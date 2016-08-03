module Chat exposing (view, initialModel, update, Model, TranslationDictionary, InternalMsg(..), translator, Translator)

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


type alias Translator parentMsg =
    Msg -> parentMsg


translator : TranslationDictionary parentMsg -> Translator parentMsg
translator { onInternalMsg, onSay } msg =
    case msg of
        ForSelf internal ->
            onInternalMsg internal

        ForParent (Say something) ->
            onSay something


update : InternalMsg -> Model -> Model
update msg model =
    case msg of
        SetNewMessage string ->
            { model | newMessage = string }

        ReceiveMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    { model | messages = model.messages ++ [ chatMessage ] }

                Err error ->
                    model


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
    form [ onSubmit (ForParent (Say model.newMessage)) ]
        [ input [ placeholder "Message...", onInput (\a -> ForSelf (SetNewMessage a)), value model.newMessage ] [] ]


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
