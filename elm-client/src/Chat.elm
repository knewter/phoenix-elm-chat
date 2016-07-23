module Chat exposing (view, initialModel, update, Model, Msg(..))

import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))


type Msg
    = SetNewMessage String
    | ReceiveMessage JE.Value
    | SendMessage


type alias Model =
    { newMessage : String
    , messages : List ChatMessage
    , users : List User
    }


type alias User =
    { name : String
    }


type alias ChatMessage =
    { user : String
    , body : String
    }


initialModel : Model
initialModel =
    { newMessage = ""
    , messages = []
    , users = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetNewMessage string ->
            { model | newMessage = string }

        SendMessage ->
            -- case model.phxSocket of
            --     Nothing ->
            --         model ! []
            --
            --     Just modelPhxSocket ->
            --         let
            --             payload =
            --                 (JE.object [ ( "body", JE.string model.newMessage ) ])
            --
            --             push' =
            --                 Phoenix.Push.init "new:msg" "room:lobby"
            --                     |> Phoenix.Push.withPayload payload
            --
            --             ( phxSocket, phxCmd ) =
            --                 Phoenix.Socket.push push' modelPhxSocket
            --         in
            --             ( { model
            --                 | newMessage = ""
            --                 , phxSocket = Just phxSocket
            --               }
            --             , Cmd.map PhoenixMsg phxCmd
            --             )
            model

        ReceiveMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    { model | messages = model.messages ++ [ chatMessage ] }

                Err error ->
                    model


view model =
    div []
        [ messageListView model
        , messageInputView model
        , userListView model
        ]



--messageListView : Model -> Html Msg


messageListView model =
    div [ class "messages" ]
        (List.map viewMessage model.messages)



--messageInputView : Model -> Html Msg


messageInputView model =
    --form [ onSubmit SendMessage ]
    form []
        --[ input [ placeholder "Message...", onInput SetNewMessage, value model.newMessage ] [] ]
        [ input [ placeholder "Message...", value model.newMessage ] [] ]



--userListView : Model -> Html Msg


userListView model =
    ul [ class "users" ]
        (List.map userView model.users)



--userView : User -> Html Msg


userView user =
    li []
        [ text user.name
        ]



--viewMessage : ChatMessage -> Html Msg


viewMessage message =
    div [ class "message" ]
        [ span [ class "user" ] [ text (message.user ++ ": ") ]
        , span [ class "body" ] [ text message.body ]
        ]


chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
    JD.object2 ChatMessage
        (JD.oneOf
            [ ("user" := JD.string)
            , JD.succeed "anonymous"
            ]
        )
        ("body" := JD.string)
