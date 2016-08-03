module Chat exposing (view, initialModel, update, Model, Msg(..), OutMsg(..))

import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))


type InternalMsg
    = SetNewMessage String
    | ReceiveMessage JE.Value


type OutMsg
    = Say String


type Msg
    = ForSelf InternalMsg
    | ForParent OutMsg


type alias Model =
    { newMessage : String
    , messages : List Message
    , users : List User
    }


type alias User =
    { name : String
    }


type alias Message =
    { user : String
    , body : String
    }


initialModel : Model
initialModel =
    { newMessage = ""
    , messages = []
    , users = []
    }


type alias TranslationDictionary msg =
    { onInternalMsg : InternalMsg -> msg
    , onSay : String -> msg
    }


update : Msg -> Model -> ( Model, Maybe OutMsg )
update msg model =
    case msg of
        SetNewMessage string ->
            ( { model | newMessage = string }
            , Nothing
            )

        SendMessage ->
            ( { model | newMessage = "" }
            , Just <| Say model.newMessage
            )

        ReceiveMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    ( { model | messages = model.messages ++ [ chatMessage ] }
                    , Nothing
                    )

                Err error ->
                    ( model
                    , Nothing
                    )


view : Model -> Html Msg
view model =
    div []
        [ messageListView model
        , messageInputView model
        , userListView model
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


viewMessage : Message -> Html Msg
viewMessage message =
    div [ class "message" ]
        [ span [ class "user" ] [ text (message.user ++ ": ") ]
        , span [ class "body" ] [ text message.body ]
        ]


chatMessageDecoder : JD.Decoder Message
chatMessageDecoder =
    JD.object2 Message
        (JD.oneOf
            [ ("user" := JD.string)
            , JD.succeed "anonymous"
            ]
        )
        ("body" := JD.string)
