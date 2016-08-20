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
import Markdown
import Material.Scheme
import Material.Layout as Layout
import Material.List as List
import Material.Options as Options exposing (when)
import Material.Color as Color
import Material.Icon as Icon
import Material.Badge as Badge
import Material.Grid exposing (grid, size, cell, Device(..))
import Material.Table as Table
import Material.Snackbar as Snackbar
import Material.Tabs as Tabs
import Material.Dialog as Dialog
import Material.Button as Button


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
                    [ viewBody model
                    , Snackbar.view model.snackbar |> App.map Snackbar
                    ]
                ]
            }


chatView : ( String, Chat.Model ) -> Html Msg
chatView ( channelName, chatModel ) =
    App.map (ChatMsg channelName) (Chat.view chatModel)


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


viewHeader : Model -> Html Msg
viewHeader model =
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
                [ Tabs.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Tabs.ripple
                    , Tabs.onSelectTab SelectTab
                    , Tabs.activeTab model.selectedTab
                    ]
                    [ Tabs.label
                        [ Options.center ]
                        [ text "Chats" ]
                    , Tabs.label
                        [ Options.center ]
                        [ text "People" ]
                    ]
                    [ case model.selectedTab of
                        0 ->
                            roomsView model

                        1 ->
                            rosterView model

                        _ ->
                            text "404"
                    ]
                , Button.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Dialog.openOn "click" ]
                    [ text "About" ]
                , Dialog.view []
                    [ Dialog.title [] [ text "About" ]
                    , Dialog.content []
                        [ p [] [ text "A rather full-featured demo application from DailyDrip" ]
                        ]
                    , Dialog.actions []
                        [ Button.render Mdl
                            [ 2 ]
                            model.mdl
                            [ Dialog.closeOn "click" ]
                            [ text "Close" ]
                        ]
                    ]
                ]


viewBody : Model -> Html Msg
viewBody model =
    case model.phxSocket of
        Nothing ->
            setUsernameView

        _ ->
            grid []
                [ cell [ size All 6, size Tablet 8 ]
                    [ chatsView model
                    ]
                , cell [ size All 6, size Tablet 8 ]
                    [ viewSidebar model ]
                ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        examples =
            [ ( "**bold**", "Bold some text", "This is **bold**" )
            , ( "*emphasize*", "Emphasize some text", "This is *emphasized*" )
            , ( "[link text](url)", "Make a link", "Here's a [link](http://dailydrip.com)." )
            , ( "![alt text](image)", "Make an image", "![megaman](http://mugenarchive.com/forums/34f814e2d7eeb9a2d05cba1245ab0bf6/images/megaman_engine_ver_0_3_by_icepony64_thumb.gif)" )
            ]
    in
        Table.table
            [ Options.css "width" "100%" ]
            [ Table.thead []
                [ Table.tr []
                    [ Table.th [] [ text "Syntax" ]
                    , Table.th [] [ text "Description" ]
                    , Table.th [] [ text "Example" ]
                    ]
                ]
            , Table.tbody []
                (List.map
                    (\( syntax, description, example ) ->
                        Table.tr []
                            [ Table.td [] [ text syntax ]
                            , Table.td [] [ text description ]
                            , Table.td [] [ Markdown.toHtml [] example ]
                            ]
                    )
                    examples
                )
            ]


knownRooms : List String
knownRooms =
    [ "room:lobby"
    , "room:random"
    , "room:gifs"
    ]
