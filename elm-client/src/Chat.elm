module Chat exposing (..)

import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))


-- Our model will track a list of messages and the text for our new message to
-- send.  We only support chatting in a single channel for now.


type alias User =
    { name : String
    }


type alias UserPresence =
    { online_at : String
    , device : String
    }


type alias ChatMessage =
    { user : String
    , body : String
    }


type alias Model =
    { topic : String
    , newMessage : String
    , messages : List ChatMessage
    , username : String
    , users : List User
    }


type Msg
    = SetNewMessage String
    | SendMessage
    | ReceiveMessage JE.Value


type OutMsg
    = Say String


initialModel : Model
initialModel =
    { topic = ""
    , newMessage = ""
    , messages = []
    , username = ""
    , users = []
    }


update : Msg -> Model -> ( ( Model, Cmd Msg ), Maybe OutMsg )
update msg model =
    case msg of
        SetNewMessage string ->
            ( { model | newMessage = string } ! [], Nothing )

        SendMessage ->
            ( { model | newMessage = "" } ! [], Just (Say model.newMessage) )

        ReceiveMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    ( { model | messages = model.messages ++ [ chatMessage ] } ! [], Nothing )

                Err error ->
                    ( model ! [], Nothing )


viewMessage : ChatMessage -> Html Msg
viewMessage message =
    div [ class "message" ]
        [ span [ class "user" ] [ text (message.user ++ ": ") ]
        , span [ class "body" ] [ text message.body ]
        ]


messageListView : Model -> Html Msg
messageListView model =
    div [ class "messages" ]
        (List.map viewMessage model.messages)


messageInputView : Model -> Html Msg
messageInputView model =
    form [ onSubmit SendMessage ]
        [ input [ placeholder "Message...", onInput SetNewMessage, value model.newMessage ] [] ]


userListView : Model -> Html Msg
userListView model =
    ul [ class "users" ]
        (List.map userView model.users)


userView : User -> Html Msg
userView user =
    li []
        [ text user.name
        ]


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text ("Chat: " ++ model.topic) ]
        , messageListView model
        , messageInputView model
        , userListView model
        ]


init : String -> ( Model, Cmd Msg )
init topic =
    ( { initialModel | topic = topic }, Cmd.none )


chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
    JD.object2 ChatMessage
        (JD.oneOf
            [ ("user" := JD.string)
            , JD.succeed "anonymous"
            ]
        )
        ("body" := JD.string)
