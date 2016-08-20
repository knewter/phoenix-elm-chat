module Main exposing (..)

import View
import Model exposing (Model, UserPresence, ChatWrapper)
import Msg exposing (Msg(..))
import Utils exposing (twoWayChatChannelFor)
import Html.App as App
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
import Material.Snackbar as Snackbar
import Task


init : ( Model, Cmd Msg )
init =
    ( Model.initialModel, Material.init Mdl )


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
                                -- Don't dig into that data structure it's dumb/wrong
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
                                        |> Phoenix.Socket.on "history:list" channelName (ReceiveChatHistory channelName)

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
                                , Cmd.batch
                                    [ Cmd.map PhoenixMsg phxJoinCmd
                                    , Task.perform identity identity (Task.succeed <| GetHistory channelName)
                                    ]
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
                    let
                        ( model, cmd ) =
                            update (JoinChannel channelToJoin) model

                        ( snackbar, snackCmd ) =
                            Snackbar.add (Snackbar.snackbar (Just (ShowChannel channelToJoin)) ("You were joined to " ++ channelToJoin) "Show Channel") model.snackbar
                    in
                        ( { model | snackbar = snackbar }
                        , Cmd.batch
                            [ cmd
                            , Cmd.map Snackbar snackCmd
                            ]
                        )

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

        ReceiveChatHistory channelName chatHistory ->
            case model.chats |> Dict.get channelName of
                Nothing ->
                    model ! []

                Just chatWrapper ->
                    let
                        ( newChat, chatCmd, outMsg ) =
                            Chat.update (Chat.ReceiveHistory chatHistory) chatWrapper.model

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

        Snackbar (Snackbar.Click (Just msg)) ->
            let
                ( newModel, cmd ) =
                    update msg model

                ( snackbar, snackCmd ) =
                    Snackbar.update (Snackbar.Click (Just msg)) newModel.snackbar
            in
                { newModel | snackbar = snackbar } ! [ cmd, Cmd.map Snackbar snackCmd ]

        Snackbar msg' ->
            let
                ( snackbar, snackCmd ) =
                    Snackbar.update msg' model.snackbar
            in
                { model | snackbar = snackbar } ! [ Cmd.map Snackbar snackCmd ]

        SelectTab num ->
            { model | selectedTab = num } ! []

        GetHistory channelName ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just modelPhxSocket ->
                    let
                        push' =
                            Phoenix.Push.init "history:fetch" channelName
                                |> Phoenix.Push.withPayload (JE.object [])

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.push push' modelPhxSocket
                    in
                        ( { model
                            | phxSocket = Just phxSocket
                          }
                        , Cmd.batch
                            [ Cmd.map PhoenixMsg phxCmd
                            ]
                        )


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
                                    Chat.encodeMessage message

                                push' =
                                    Phoenix.Push.init "new:msg" channelName
                                        |> Phoenix.Push.withPayload payload

                                ( phxSocket, phxCmd ) =
                                    Phoenix.Socket.push push' modelPhxSocket

                                ( snackbar', snackCmd ) =
                                    Snackbar.add (Snackbar.toast Nothing "You sent a message") model.snackbar
                            in
                                ( { model
                                    | phxSocket = Just phxSocket
                                    , snackbar = snackbar'
                                  }
                                , Cmd.batch
                                    [ cmd
                                    , Cmd.map PhoenixMsg phxCmd
                                    , Cmd.map Snackbar snackCmd
                                    ]
                                )


userPresenceDecoder : JD.Decoder UserPresence
userPresenceDecoder =
    JD.object2 UserPresence
        ("online_at" := JD.string)
        ("device" := JD.string)


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        phxSub =
            case model.phxSocket of
                Nothing ->
                    Sub.none

                Just phxSocket ->
                    Phoenix.Socket.listen phxSocket PhoenixMsg
    in
        Sub.batch
            [ Material.subscriptions Mdl model
            , phxSub
            ]
