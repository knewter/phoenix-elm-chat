module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class, type', style)
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
import Material
import Material.Scheme
import Material.Layout as Layout
import Material.List as List
import Material.Options as Options exposing (when)
import Material.Color as Color
import Material.Icon as Icon
import Material.Badge as Badge


type alias UserPresence =
    { online_at : String
    , device : String
    }


type alias ChatWrapper =
    { model : Chat.Model
    , totalMessages : Int
    , seenMessages : Int
    }


type alias Model =
    { username : String
    , chats : Dict String ChatWrapper
    , phxSocket : Maybe (Phoenix.Socket.Socket Msg)
    , phxPresences : PresenceState UserPresence
    , users : List User
    , currentChat : Maybe String
    , mdl : Material.Model
    }


type Msg
    = JoinChannel String
    | ShowChannel String
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
    | Mdl (Material.Msg Msg)


initialModel : Model
initialModel =
    { username = ""
    , chats =
        Dict.empty
    , phxSocket = Nothing
    , phxPresences = Dict.empty
    , users = []
    , currentChat = Nothing
    , mdl = Material.model
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowChannel channelName ->
            let
                ( newModel, newCmd ) =
                    update (JoinChannel channelName) model

                newModel' =
                    case newModel.chats |> Dict.get channelName of
                        Nothing ->
                            newModel

                        Just chatWrapper ->
                            let
                                totalMessages =
                                    List.length chatWrapper.model.messages

                                seenMessages =
                                    totalMessages

                                newChatWrapper =
                                    { chatWrapper
                                        | totalMessages = totalMessages
                                        , seenMessages = seenMessages
                                    }

                                newChats =
                                    Dict.insert channelName newChatWrapper newModel.chats
                            in
                                { newModel | chats = newChats }
            in
                ( { newModel'
                    | currentChat = Just channelName
                  }
                , newCmd
                )

        JoinChannel channelName ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just modelPhxSocket ->
                    case Dict.member channelName model.chats of
                        True ->
                            { model | currentChat = Just channelName } ! []

                        False ->
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

                                newChatWrapper =
                                    ChatWrapper newChat 0 0

                                newChats =
                                    model.chats
                                        |> Dict.insert channelName newChatWrapper
                            in
                                ( { model
                                    | phxSocket = Just phxSocket2
                                    , chats = newChats
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

                Just chatWrapper ->
                    let
                        ( newChat, chatCmd, outMsg ) =
                            Chat.update (Chat.ReceiveMessage chatMessage) chatWrapper.model

                        totalMessages =
                            List.length newChat.messages

                        seenMessages =
                            case model.currentChat == Just channelName of
                                True ->
                                    totalMessages

                                False ->
                                    chatWrapper.seenMessages

                        newChatWrapper =
                            { chatWrapper
                                | model = newChat
                                , totalMessages = totalMessages
                                , seenMessages = seenMessages
                            }

                        newChats =
                            Dict.insert channelName newChatWrapper model.chats

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

                Just chatWrapper ->
                    let
                        ( newChat, chatCmd, outMsg ) =
                            Chat.update chatMsg chatWrapper.model

                        -- Obviously we should pull this out into a function
                        totalMessages =
                            List.length newChat.messages

                        seenMessages =
                            case model.currentChat == Just channelName of
                                True ->
                                    totalMessages

                                False ->
                                    chatWrapper.seenMessages

                        newChatWrapper =
                            { chatWrapper
                                | model = newChat
                                , totalMessages = totalMessages
                                , seenMessages = seenMessages
                            }

                        newChats =
                            Dict.insert channelName newChatWrapper model.chats

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
                update (ShowChannel channel) model

        ShowChat channel ->
            { model | currentChat = Just channel } ! []

        Mdl msg' ->
            Material.update msg' model


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


chatView : ( String, Chat.Model ) -> Html Msg
chatView ( channelName, chatModel ) =
    App.map (ChatMsg channelName) (Chat.view chatModel)


chatsView : Model -> Html Msg
chatsView model =
    case model.currentChat of
        Nothing ->
            div [] []

        Just currentChat ->
            case Dict.get currentChat model.chats of
                Nothing ->
                    div [] []

                Just chatWrapper ->
                    chatView ( currentChat, chatWrapper.model )


roomsView : Model -> Html Msg
roomsView model =
    List.ul
        []
        (List.map
            (roomView model)
            knownRooms
        )


roomView : Model -> String -> Html Msg
roomView model name =
    let
        isListening =
            Dict.member name model.chats

        iconName =
            case isListening of
                True ->
                    "label"

                False ->
                    "label_outline"

        channelName =
            case isListening of
                True ->
                    case Dict.get name model.chats of
                        Just chatWrapper ->
                            case chatWrapper.totalMessages - chatWrapper.seenMessages of
                                0 ->
                                    text name

                                n ->
                                    Options.span
                                        [ Badge.add <| toString n ]
                                        [ text name ]

                        Nothing ->
                            text name

                False ->
                    text name
    in
        List.li
            [ Options.attribute <| onClick (ShowChannel name)
            , Options.css "cursor" "pointer"
            , Color.text Color.accent `when` (model.currentChat == Just name)
            ]
            [ List.content
                []
                [ List.icon iconName []
                , channelName
                ]
            ]


rosterView : Model -> Html Msg
rosterView model =
    List.ul
        []
        (List.map
            (userView model)
            model.users
        )


userView : Model -> User -> Html Msg
userView model user =
    let
        chatChannel =
            twoWayChatChannelFor model.username user.name

        isListening =
            Dict.member chatChannel model.chats
    in
        List.li
            [ Options.attribute <| onClick (ChatWithUser user)
            , Options.css "cursor" "pointer"
            , Color.text Color.accent `when` (model.currentChat == Just chatChannel)
            ]
            [ List.content
                []
                [ List.avatarImage ("https://api.adorable.io/avatars/285/" ++ user.name ++ ".png") []
                , text user.name
                ]
            ]


setUsernameView : Html Msg
setUsernameView =
    form [ onSubmit ConnectSocket ]
        [ input [ onInput SetUsername, placeholder "Enter a username" ] [] ]


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedDrawer
            ]
            { header = [ viewHeader model ]
            , drawer = [ viewDrawer model ]
            , tabs = ( [], [] )
            , main =
                [ div
                    [ style [ ( "padding", "1rem" ) ] ]
                    [ viewBody model ]
                ]
            }


viewHeader : Model -> Html Msg
viewHeader model =
    -- h1 [ style [ ( "padding", "1rem" ) ] ] [ text "Phoenix Elm Chat" ] ]
    Layout.row
        []
        [ Layout.title [] [ text "Phoenix Elm Chat" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "https://github.com/knewter/phoenix-elm-chat" ]
                [ span [] [ text "github" ] ]
            ]
        ]


viewDrawer : Model -> Html Msg
viewDrawer model =
    case model.phxSocket of
        Nothing ->
            div [] []

        Just _ ->
            div []
                [ rosterView model
                , roomsView model
                ]


viewBody : Model -> Html Msg
viewBody model =
    case model.phxSocket of
        Nothing ->
            setUsernameView

        _ ->
            chatsView model


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


knownRooms : List String
knownRooms =
    [ "room:lobby"
    , "room:random"
    , "room:gifs"
    ]
