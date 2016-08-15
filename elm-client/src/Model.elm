module Model exposing (..)

import Phoenix.Socket
import Chat
import Phoenix.Presence exposing (PresenceState)
import Types exposing (User, Message)
import Dict exposing (Dict)
import Msg exposing (Msg)
import Material
import Material.Snackbar as Snackbar


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
    , snackbar : Snackbar.Model (Maybe Msg)
    , selectedTab : Int
    }


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
    , snackbar = Snackbar.model
    , selectedTab = 0
    }
