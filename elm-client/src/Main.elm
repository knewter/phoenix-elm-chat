module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class, type')
import Html.Events exposing (onInput, onClick, onSubmit)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Presence exposing (PresenceState, syncState, syncDiff, presenceStateDecoder, presenceDiffDecoder)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Debug
import Dict exposing (Dict)
import Chat
import OutMessage
import Styles
import Types exposing (User, Message)


type alias UserPresence =
    { online_at : String
    , device : String
    }


type alias Model =
    { username : String
    , chats : Dict String Chat.Model
    , phxSocket : Maybe (Phoenix.Socket.Socket Msg)
    , phxPresences : PresenceState UserPresence
    , users : List User
    , currentChat : Maybe String
    }


type Msg
    = JoinChannel String
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SetUsername String
    | ConnectSocket
    | HandlePresenceState JE.Value
    | HandlePresenceDiff JE.Value
    | HandleChatJoinCommand JE.Value
    | ReceiveChatMessage String JE.Value
    | ChatMsg String Chat.Msg
    | ChatWithUser User
    | ShowChat String


initialModel : Model
initialModel =
    { username = ""
    , chats =
        Dict.empty
    , phxSocket = Nothing
    , phxPresences = Dict.empty
    , users = []
    , currentChat = Nothing
    }


socketServer : String -> String
socketServer username =
    "ws://localhost:4000/socket/websocket?username=" ++ username


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket username =
    Phoenix.Socket.init (socketServer username)
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "presence_state" "room:lobby" HandlePresenceState
        |> Phoenix.Socket.on "presence_diff" "room:lobby" HandlePresenceDiff



--|> Phoenix.Socket.on "chat:join" "room:lobby" HandleChatJoinCommand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JoinChannel channelName ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just modelPhxSocket ->
                    let
                        channel =
                            Phoenix.Channel.init channelName

                        ( phxSocket, phxJoinCmd ) =
                            Phoenix.Socket.join channel modelPhxSocket

                        phxSocket2 =
                            phxSocket
                                |> Phoenix.Socket.on "new:msg" channelName (ReceiveChatMessage channelName)

                        initialChatModel =
                            Chat.initialModel

                        newChat =
                            { initialChatModel | topic = channelName }

                        newChats =
                            model.chats
                                |> Dict.insert channelName newChat
                    in
                        ( { model
                            | phxSocket = Just phxSocket2
                            , chats = newChats
                            , currentChat = Just channelName
                          }
                        , Cmd.map PhoenixMsg phxJoinCmd
                        )

        PhoenixMsg msg ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just modelPhxSocket ->
                    let
                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.update msg modelPhxSocket
                    in
                        ( { model | phxSocket = Just phxSocket }
                        , Cmd.map PhoenixMsg phxCmd
                        )

        SetUsername username ->
            { model | username = username } ! []

        ConnectSocket ->
            let
                _ =
                    Debug.log "ConnectSocket" (toString model)

                controlChannelName' =
                    controlChannelName model.username

                controlChannel =
                    Phoenix.Channel.init controlChannelName'

                phxSocketInit =
                    (initPhxSocket model.username)

                ( phxSocket, phxJoinCmd ) =
                    Phoenix.Socket.join controlChannel phxSocketInit

                phxSocket2 =
                    phxSocket
                        |> Phoenix.Socket.on "chat:join" controlChannelName' HandleChatJoinCommand
            in
                ( { model | phxSocket = Just phxSocket2 }
                , Cmd.map PhoenixMsg phxJoinCmd
                )

        HandlePresenceState raw ->
            case JD.decodeValue (presenceStateDecoder userPresenceDecoder) raw of
                Ok presenceState ->
                    let
                        newPresenceState =
                            model.phxPresences |> syncState presenceState

                        users =
                            Dict.keys presenceState
                                |> List.map User
                    in
                        { model | users = users, phxPresences = newPresenceState } ! []

                Err error ->
                    let
                        _ =
                            Debug.log "Error" error
                    in
                        model ! []

        HandlePresenceDiff raw ->
            case JD.decodeValue (presenceDiffDecoder userPresenceDecoder) raw of
                Ok presenceDiff ->
                    let
                        newPresenceState =
                            model.phxPresences |> syncDiff presenceDiff

                        users =
                            Dict.keys newPresenceState
                                |> List.map User
                    in
                        { model | users = users, phxPresences = newPresenceState } ! []

                Err error ->
                    let
                        _ =
                            Debug.log "Error" error
                    in
                        model ! []

        HandleChatJoinCommand raw ->
            let
                _ =
                    Debug.log "here" 1
            in
                case JD.decodeValue chatJoinDecoder raw of
                    Ok channelToJoin ->
                        update (JoinChannel channelToJoin) model

                    Err error ->
                        let
                            _ =
                                Debug.log "Error" error
                        in
                            model ! []

        ReceiveChatMessage channelName chatMessage ->
            case model.chats |> Dict.get channelName of
                Nothing ->
                    model ! []

                Just chatModel ->
                    let
                        ( newChat, chatCmd, outMsg ) =
                            Chat.update (Chat.ReceiveMessage chatMessage) chatModel

                        newChats =
                            Dict.insert channelName newChat model.chats

                        newModel =
                            { model | chats = newChats }

                        newCmd =
                            Cmd.map (ChatMsg channelName) chatCmd

                        ( newModel', newCmd' ) =
                            handleChatOutMsg channelName outMsg ( newModel, newCmd )
                    in
                        ( newModel', newCmd' )

        ChatMsg channelName chatMsg ->
            case Dict.get channelName model.chats of
                Nothing ->
                    model ! []

                Just chatModel ->
                    let
                        ( newChat, chatCmd, outMsg ) =
                            Chat.update chatMsg chatModel

                        newChats =
                            Dict.insert channelName newChat model.chats

                        newModel =
                            { model | chats = newChats }

                        newCmd =
                            Cmd.map (ChatMsg channelName) chatCmd

                        ( newModel', newCmd' ) =
                            handleChatOutMsg channelName outMsg ( newModel, newCmd )
                    in
                        ( newModel', newCmd' )

        ChatWithUser user ->
            {-
               This is really:

               - Join a channel for this particular 2-way user chat.
            -}
            let
                channel =
                    twoWayChatChannelFor model.username user.name
            in
                update (JoinChannel channel) model

        ShowChat channel ->
            { model | currentChat = Just channel } ! []


controlChannelName : String -> String
controlChannelName username =
    "control:" ++ username


chatJoinDecoder : JD.Decoder String
chatJoinDecoder =
    let
        _ =
            Debug.log "zomg got here" 1
    in
        ("channel" := JD.string)



{-
   To determine the 2-way chat channel for these two users, sort them alphabetically, and insert a "<->" between them, prepending with "room:".
   For example:
       > twoWayChatChannelFor "alice" "bob" == twoWayChatChannelFor "bob" "alice"
       > twoWayChatChannelFor "alice" "bob" == "room:alice<->bob"
-}


twoWayChatChannelFor : String -> String -> String
twoWayChatChannelFor user1 user2 =
    case user1 < user2 of
        True ->
            "room:" ++ user1 ++ "<->" ++ user2

        False ->
            "room:" ++ user2 ++ "<->" ++ user1


handleChatOutMsg : String -> Maybe Chat.OutMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleChatOutMsg channelName maybeOutMsg ( model, cmd ) =
    case maybeOutMsg of
        Nothing ->
            ( model, cmd )

        Just outMsg ->
            case outMsg of
                Chat.Say message ->
                    case model.phxSocket of
                        Nothing ->
                            ( model, cmd )

                        Just modelPhxSocket ->
                            let
                                payload =
                                    (JE.object [ ( "body", JE.string message ) ])

                                push' =
                                    Phoenix.Push.init "new:msg" channelName
                                        |> Phoenix.Push.withPayload payload

                                ( phxSocket, phxCmd ) =
                                    Phoenix.Socket.push push' modelPhxSocket
                            in
                                ( { model
                                    | phxSocket = Just phxSocket
                                  }
                                , Cmd.batch
                                    [ cmd
                                    , Cmd.map PhoenixMsg phxCmd
                                    ]
                                )


userPresenceDecoder : JD.Decoder UserPresence
userPresenceDecoder =
    JD.object2 UserPresence
        ("online_at" := JD.string)
        ("device" := JD.string)


lobbyManagementView : Html Msg
lobbyManagementView =
    button [ onClick (JoinChannel "room:lobby") ] [ text "Join lobby" ]


chatInterfaceView : Model -> Html Msg
chatInterfaceView model =
    let
        compiled =
            Styles.compile Styles.css

        { class } =
            Styles.mainNamespace
    in
        div [ class [ Styles.ChatClientContainer ] ]
            [ node "style" [ type' "text/css" ] [ text compiled.css ]
            , lobbyManagementView
            , div [ class [ Styles.ChatWindowContainer ] ]
                [ rosterView model
                  --, App.map ChatMsg <| Chat.view model.chat
                , chatsView model
                ]
            ]


chatView : ( String, Chat.Model ) -> Html Msg
chatView ( channelName, chatModel ) =
    div [] [ App.map (ChatMsg channelName) (Chat.view chatModel) ]


chatsView : Model -> Html Msg
chatsView model =
    case model.currentChat of
        Nothing ->
            div [] []

        Just currentChat ->
            case Dict.get currentChat model.chats of
                Nothing ->
                    div [] []

                Just chat ->
                    chatView ( currentChat, chat )


rosterView : Model -> Html Msg
rosterView model =
    let
        { class } =
            Styles.mainNamespace
    in
        div [ class [ Styles.Roster ] ]
            [ h3 [] [ text "Contacts" ]
            , ul []
                (List.map
                    userView
                    model.users
                )
            ]


userView : User -> Html Msg
userView user =
    let
        { class } =
            Styles.mainNamespace
    in
        li
            [ class [ Styles.RosterUser ]
            , onClick (ChatWithUser user)
            ]
            [ text user.name ]


setUsernameView : Html Msg
setUsernameView =
    form [ onSubmit ConnectSocket ]
        [ input [ onInput SetUsername, placeholder "Enter a username" ] [] ]


view : Model -> Html Msg
view model =
    case model.phxSocket of
        Nothing ->
            setUsernameView

        _ ->
            chatInterfaceView model


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.phxSocket of
        Nothing ->
            Sub.none

        Just phxSocket ->
            Phoenix.Socket.listen phxSocket PhoenixMsg


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )
