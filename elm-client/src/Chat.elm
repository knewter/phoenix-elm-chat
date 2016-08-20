module Chat exposing (view, initialModel, update, Model, Msg(..), OutMsg(..), encodeMessage)

import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Styles
import Types exposing (User, Message)
import Material
import Material.Card as Card
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Textfield as Textfield
import Material.List as List
import Markdown


type Msg
    = SetNewMessage String
    | ReceiveMessage JE.Value
    | ReceiveHistory JE.Value
    | SendMessage
    | Mdl (Material.Msg Msg)


type OutMsg
    = Say String


type alias Model =
    { newMessage : String
    , topic : String
    , messages : List Message
    , users : List User
    , mdl : Material.Model
    }


initialModel : Model
initialModel =
    { newMessage = ""
    , topic = ""
    , messages = []
    , users = []
    , mdl = Material.model
    }


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        SetNewMessage string ->
            ( { model | newMessage = string }
            , Cmd.none
            , Nothing
            )

        SendMessage ->
            ( { model | newMessage = "" }
            , Cmd.none
            , Just <| Say model.newMessage
            )

        ReceiveMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    ( { model | messages = model.messages ++ [ chatMessage ] }
                    , Cmd.none
                    , Nothing
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        ReceiveHistory raw ->
            case JD.decodeValue chatHistoryDecoder raw of
                Ok chatMessages ->
                    ( { model | messages = model.messages ++ chatMessages }
                    , Cmd.none
                    , Nothing
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        Mdl msg' ->
            let
                ( newModel, newCmd ) =
                    Material.update msg' model
            in
                ( newModel
                , newCmd
                , Nothing
                )


view : Model -> Html Msg
view model =
    Card.view
        [ Options.css "width" "100%"
        , Elevation.e2
        ]
        [ Card.title
            [ Color.background Color.primary
            , Color.text Color.white
            ]
            [ Card.head [] [ text model.topic ] ]
        , Card.text
            []
            [ messageListView model
            , messageInputView model
            ]
        ]


messageListView : Model -> Html Msg
messageListView model =
    List.ul
        []
        (List.map viewMessage model.messages)


viewMessage : Message -> Html Msg
viewMessage message =
    List.li
        [ List.withBody ]
        [ List.content
            []
            [ List.avatarImage ("https://api.adorable.io/avatars/285/" ++ message.user ++ ".png") []
            , text message.user
            , List.body
                []
                [ Markdown.toHtml [] message.body
                ]
            ]
        ]


messageInputView : Model -> Html Msg
messageInputView model =
    form
        [ onSubmit SendMessage
        ]
        [ Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.onInput SetNewMessage
            , Textfield.value model.newMessage
            , Textfield.label "Type a message..."
            ]
        ]


userView : User -> Html Msg
userView user =
    li []
        [ text user.name
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


chatHistoryDecoder : JD.Decoder (List Message)
chatHistoryDecoder =
    JD.object1 identity
        ("history" := (JD.list chatMessageDecoder))


encodeMessage : String -> JE.Value
encodeMessage message =
    (JE.object [ ( "body", JE.string message ) ])
