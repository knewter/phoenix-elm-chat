module View exposing (view)

import Msg exposing (Msg(..))
import Model exposing (Model)
import Chat
import Utils exposing (twoWayChatChannelFor)
import Types exposing (User)
import Html.App as App
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class, type', style)
import Html.Events exposing (onInput, onClick, onSubmit)
import Material.Scheme
import Material.Layout as Layout
import Material.List as List
import Material.Options as Options exposing (when)
import Material.Color as Color
import Material.Icon as Icon
import Material.Badge as Badge
import Material.Grid exposing (grid, size, cell, Device(..))


chatView : ( String, Chat.Model ) -> Html Msg
chatView ( channelName, chatModel ) =
    grid []
        [ cell [ size All 8 ]
            [ App.map (ChatMsg channelName) (Chat.view chatModel) ]
        , cell [ size All 4 ]
            [ text "Sidebar here..." ]
        ]


chatsView : Model -> Html Msg
chatsView model =
    case model.currentChat of
        Nothing ->
            div [] []

        Just currentChat ->
            case Dict.get currentChat model.chats of
                Nothing ->
                    div [] []

                Just chatWrapper ->
                    chatView ( currentChat, chatWrapper.model )


roomsView : Model -> Html Msg
roomsView model =
    List.ul
        []
        (List.map
            (roomView model)
            knownRooms
        )


roomView : Model -> String -> Html Msg
roomView model name =
    let
        isListening =
            Dict.member name model.chats

        iconName =
            case isListening of
                True ->
                    "label"

                False ->
                    "label_outline"

        channelName =
            case isListening of
                True ->
                    case Dict.get name model.chats of
                        Just chatWrapper ->
                            case chatWrapper.totalMessages - chatWrapper.seenMessages of
                                0 ->
                                    text name

                                n ->
                                    Options.span
                                        [ Badge.add <| toString n ]
                                        [ text name ]

                        Nothing ->
                            text name

                False ->
                    text name
    in
        List.li
            [ Options.attribute <| onClick (ShowChannel name)
            , Options.css "cursor" "pointer"
            , Color.text Color.accent `when` (model.currentChat == Just name)
            ]
            [ List.content
                []
                [ List.icon iconName []
                , channelName
                ]
            ]


rosterView : Model -> Html Msg
rosterView model =
    List.ul
        []
        (List.map
            (userView model)
            model.users
        )


userView : Model -> User -> Html Msg
userView model user =
    let
        chatChannel =
            twoWayChatChannelFor model.username user.name

        isListening =
            Dict.member chatChannel model.chats
    in
        List.li
            [ Options.attribute <| onClick (ChatWithUser user)
            , Options.css "cursor" "pointer"
            , Color.text Color.accent `when` (model.currentChat == Just chatChannel)
            ]
            [ List.content
                []
                [ List.avatarImage ("https://api.adorable.io/avatars/285/" ++ user.name ++ ".png") []
                , text user.name
                ]
            ]


setUsernameView : Html Msg
setUsernameView =
    form [ onSubmit ConnectSocket ]
        [ input [ onInput SetUsername, placeholder "Enter a username" ] [] ]


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedDrawer
            ]
            { header = [ viewHeader model ]
            , drawer = [ viewDrawer model ]
            , tabs = ( [], [] )
            , main =
                [ div
                    [ style [ ( "padding", "1rem" ) ] ]
                    [ viewBody model ]
                ]
            }


viewHeader : Model -> Html Msg
viewHeader model =
    -- h1 [ style [ ( "padding", "1rem" ) ] ] [ text "Phoenix Elm Chat" ] ]
    Layout.row
        []
        [ Layout.title [] [ text "Phoenix Elm Chat" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "https://github.com/knewter/phoenix-elm-chat" ]
                [ span [] [ text "github" ] ]
            ]
        ]


viewDrawer : Model -> Html Msg
viewDrawer model =
    case model.phxSocket of
        Nothing ->
            div [] []

        Just _ ->
            div []
                [ rosterView model
                , roomsView model
                ]


viewBody : Model -> Html Msg
viewBody model =
    case model.phxSocket of
        Nothing ->
            setUsernameView

        _ ->
            chatsView model


knownRooms : List String
knownRooms =
    [ "room:lobby"
    , "room:random"
    , "room:gifs"
    ]
