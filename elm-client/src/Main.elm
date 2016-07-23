module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
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


type alias UserPresence =
    { online_at : String
    , device : String
    }


type alias Model =
    { username : String
    , chat : Chat.Model
    , phxSocket : Maybe (Phoenix.Socket.Socket Msg)
    , phxPresences : PresenceState UserPresence
    }


type Msg
    = JoinChannel String
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SetUsername String
    | ConnectSocket
    | HandlePresenceState JE.Value
    | HandlePresenceDiff JE.Value
    | ReceiveChatMessage String JE.Value
    | ChatMsg Chat.Msg


initialModel : Model
initialModel =
    { username = ""
    , chat = Chat.initialModel
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
        |> Phoenix.Socket.on "presence_state" "room:lobby" HandlePresenceState
        |> Phoenix.Socket.on "presence_diff" "room:lobby" HandlePresenceDiff


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

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.join channel modelPhxSocket

                        phxSocket2 =
                            Phoenix.Socket.on "new:msg" channelName (ReceiveChatMessage channelName) phxSocket
                    in
                        ( { model | phxSocket = Just phxSocket2 }
                        , Cmd.map PhoenixMsg phxCmd
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
            { model | phxSocket = Just (initPhxSocket model.username) } ! []

        HandlePresenceState raw ->
            model ! []

        HandlePresenceDiff raw ->
            model ! []

        ReceiveChatMessage channelName chatMessage ->
            let
                ( newChat, maybeChatOutMsg ) =
                    model.chat
                        |> Chat.update (Chat.ReceiveMessage chatMessage)
            in
                case maybeChatOutMsg of
                    Nothing ->
                        { model | chat = newChat } ! []

                    Just chatOutMsg ->
                        handleChatOutMsg chatOutMsg model

        ChatMsg chatMsg ->
            let
                ( newChat, maybeChatOutMsg ) =
                    model.chat |> Chat.update chatMsg
            in
                case maybeChatOutMsg of
                    Nothing ->
                        { model | chat = newChat } ! []

                    Just chatOutMsg ->
                        handleChatOutMsg chatOutMsg model


handleChatOutMsg : Chat.OutMsg -> Model -> ( Model, Cmd Msg )
handleChatOutMsg outMsg model =
    case outMsg of
        Chat.Say something ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just modelPhxSocket ->
                    let
                        payload =
                            (JE.object [ ( "body", JE.string something ) ])

                        push' =
                            Phoenix.Push.init "new:msg" "room:lobby"
                                |> Phoenix.Push.withPayload payload

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.push push' modelPhxSocket
                    in
                        ( { model
                            | phxSocket = Just phxSocket
                          }
                        , Cmd.map PhoenixMsg phxCmd
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
    div []
        [ lobbyManagementView
        , App.map ChatMsg <| Chat.view model.chat
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
