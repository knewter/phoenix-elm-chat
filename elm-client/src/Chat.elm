module Chat exposing (view)

import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)


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
