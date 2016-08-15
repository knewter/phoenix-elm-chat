module Model exposing (..)

import Phoenix.Socket
import Chat
import Material
import Phoenix.Presence exposing (PresenceState)
import Types exposing (User, Message)
import Msg exposing (Msg)
import Dict exposing (Dict)


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
