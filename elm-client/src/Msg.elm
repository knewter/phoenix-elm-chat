module Msg exposing (Msg(..))

import Phoenix.Socket
import Json.Encode as JE
import Json.Decode as JD
import Chat
import Types exposing (User, Message)
import Material
import Material.Snackbar as Snackbar


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
    | ReceiveChatHistory String JE.Value
    | ChatMsg String Chat.Msg
    | ChatWithUser User
    | ShowChat String
    | Mdl (Material.Msg Msg)
    | Snackbar (Snackbar.Msg (Maybe Msg))
    | SelectTab Int
    | GetHistory String
