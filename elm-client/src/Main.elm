module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)
import Dict exposing (Dict)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Presence exposing (PresenceState, syncState, syncDiff, presenceStateDecoder, presenceDiffDecoder)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Dict exposing (Dict)
import Chat
import Debug


type alias Model =
    { chats : Dict String Chat.Model
    , username : String
    , phxSocket : Maybe (Phoenix.Socket.Socket Msg)
    , phxPresences : PresenceState Chat.UserPresence
    }


type Msg
    = JoinChannel String
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveChatMessage String JE.Value
    | SetUsername String
    | ConnectSocket
    | HandlePresenceState JE.Value
    | HandlePresenceDiff JE.Value
    | ChatMsg String Chat.Msg


initialModel : Model
initialModel =
    { chats = Dict.empty
    , username = ""
    , phxSocket = Nothing
    , phxPresences = Dict.empty
    }


socketServer : String -> String
socketServer username =
    "ws://localhost:4000/socket/websocket?username=" ++ username


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket username =
    Phoenix.Socket.init (socketServer username)
        |> Phoenix.Socket.withDebug


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
                            Phoenix.Socket.on "new:msg" channelName (ReceiveChatMessage channelName) phxSocket

                        initialChatModel =
                            Chat.initialModel

                        newChat =
                            { initialChatModel | topic = channelName }

                        newChats =
                            model.chats
                                |> Dict.insert channelName newChat
                    in
                        { model
                            | phxSocket = Just phxSocket2
                            , chats = newChats
                        }
                            ! [ Cmd.map PhoenixMsg phxJoinCmd ]

        SetUsername username ->
            { model | username = username } ! []

        ConnectSocket ->
            { model | phxSocket = Just (initPhxSocket model.username) } ! []

        HandlePresenceState raw ->
            model ! []

        HandlePresenceDiff raw ->
            model ! []

        PhoenixMsg _ ->
            model ! []

        ReceiveChatMessage channelName chatMessage ->
            case model.chats |> Dict.get channelName of
                Nothing ->
                    model ! []

                Just chat ->
                    let
                        ( ( chatModel, chatCmd ), maybeOutMsg ) =
                            Chat.update (Chat.ReceiveMessage chatMessage) chat

                        newChats =
                            model.chats |> Dict.insert channelName chatModel
                    in
                        { model | chats = newChats } ! []

        ChatMsg channelName chatMsg ->
            case Dict.get channelName model.chats of
                Nothing ->
                    model ! []

                Just chatModel ->
                    let
                        ( ( chatModel, chatCmd ), outMsg ) =
                            Chat.update chatMsg chatModel

                        newChats =
                            Dict.insert channelName chatModel model.chats

                        newModel =
                            { model | chats = newChats }

                        newCmd =
                            Cmd.map (ChatMsg channelName) chatCmd

                        ( newModel', newCmd' ) =
                            handleChatOutMsg outMsg channelName ( newModel, newCmd )
                    in
                        ( newModel', newCmd' )


handleChatOutMsg : Maybe Chat.OutMsg -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleChatOutMsg maybeOutMsg channelName ( model, cmd ) =
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


userPresenceDecoder : JD.Decoder Chat.UserPresence
userPresenceDecoder =
    JD.object2 Chat.UserPresence
        ("online_at" := JD.string)
        ("device" := JD.string)


viewMessage : Chat.ChatMessage -> Html Msg
viewMessage message =
    div [ class "message" ]
        [ span [ class "user" ] [ text (message.user ++ ": ") ]
        , span [ class "body" ] [ text message.body ]
        ]


lobbyManagementView : Model -> Html Msg
lobbyManagementView model =
    div []
        [ button [ onClick (JoinChannel "room:lobby") ] [ text "Join room:lobby" ]
        , button [ onClick (JoinChannel "room:lobby2") ] [ text "Join room:lobby2" ]
        ]


chatViewListItem : ( String, Chat.Model ) -> Html Msg
chatViewListItem ( channelName, chatModel ) =
    li [] [ App.map (ChatMsg channelName) (Chat.view chatModel) ]


chatsView : Model -> Html Msg
chatsView model =
    let
        chatViews =
            model.chats
                |> Dict.toList
                |> List.map chatViewListItem
    in
        ul [] chatViews


chatInterfaceView : Model -> Html Msg
chatInterfaceView model =
    div []
        [ lobbyManagementView model
        , chatsView model
        ]


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
