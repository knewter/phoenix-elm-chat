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
    , chat : Chat.Model
    , phxSocket : Maybe (Phoenix.Socket.Socket Msg)
    , phxPresences : PresenceState UserPresence
    , users : List User
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
    | ChatWithUser User


initialModel : Model
initialModel =
    { username = "knewter"
    , chat =
        Chat.initialModel
        --, phxSocket = Nothing
    , phxSocket = Just (initPhxSocket "knewter")
    , phxPresences = Dict.empty
    , users = []
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
            model ! []

        ReceiveChatMessage channelName chatMessage ->
            model.chat
                |> Chat.update (Chat.ReceiveMessage chatMessage)
                |> OutMessage.mapComponent
                    (\newChat -> { model | chat = newChat })
                |> OutMessage.mapCmd ChatMsg
                |> OutMessage.evaluateMaybe handleChatOutMsg Cmd.none

        ChatMsg chatMsg ->
            model.chat
                |> Chat.update chatMsg
                |> OutMessage.mapComponent
                    (\newChat -> { model | chat = newChat })
                |> OutMessage.mapCmd ChatMsg
                |> OutMessage.evaluateMaybe handleChatOutMsg Cmd.none

        ChatWithUser user ->
            {-
               This is really:

               - Join a channel for this chat, we'll hardcode it for now as "room:userchat"
            -}
            update (JoinChannel "room:userchat") model


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
    let
        compiled =
            Styles.compile Styles.css
    in
        div []
            [ node "style" [ type' "text/css" ] [ text compiled.css ]
            , lobbyManagementView
            , rosterView model
            , App.map ChatMsg <| Chat.view model.chat
            ]


rosterView : Model -> Html Msg
rosterView model =
    let
        { class } =
            Styles.mainNamespace
    in
        div [ class [ Styles.Roster ] ]
            (List.map userView model.users)


userView : User -> Html Msg
userView user =
    let
        { class } =
            Styles.mainNamespace
    in
        div
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
