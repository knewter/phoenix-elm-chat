module Styles exposing (css, mainNamespace, compile, CssIds(..), CssClasses(..))

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Css.Elements
import Html.CssHelpers exposing (withNamespace)
import Styles.Variables as V


type CssIds
    = NotAThingYet


type CssClasses
    = Chat
    | ChatsList
    | Messages
    | Message
    | MessageUser
    | MessageBody
    | ChatInput
    | Roster
    | RosterUser
    | ChatClientContainer
    | ChatWindowContainer
    | Listening


compile =
    Css.compile


mainNamespace : Html.CssHelpers.Namespace String class id msg
mainNamespace =
    withNamespace "main"


css : Css.Stylesheet
css =
    (stylesheet << namespace mainNamespace.name)
        [ (.) ChatClientContainer
            [ fontFamily sansSerif
            , descendants
                [ everything
                    [ boxSizing borderBox
                    ]
                ]
            ]
        , (.) ChatWindowContainer
            [ displayFlex
            , flexDirection row
            ]
        , (.) ChatsList
            [ children
                [ Css.Elements.li
                    [ property "list-style-type" "none"
                    ]
                ]
            ]
        , (.) Chat
            [ backgroundColor V.chatBackgroundColor
            , display inlineBlock
            , width (em 20)
            , children
                [ Css.Elements.h2
                    [ margin (px 0)
                    , backgroundColor V.primaryColor
                    , padding (em 0.5)
                    , color V.primaryTextColor
                    ]
                ]
            ]
        , (.) Messages
            [ padding (em 1)
            ]
        , (.) Message
            []
        , (.) MessageUser
            [ fontWeight bold ]
        , (.) MessageBody
            [ color V.messageBodyColor ]
        , (.) Roster
            [ width (em 10)
            , display inlineBlock
            , padding (em 1)
            , children
                [ Css.Elements.ul
                    [ margin (px 0)
                    , padding (px 0)
                    , children
                        [ Css.Elements.li
                            [ property "list-style-type" "none"
                            ]
                        ]
                    ]
                ]
            ]
        , (.) RosterUser
            [ padding (em 0.5)
            , color (rgb 100 100 100)
            , cursor pointer
            , (withClass Listening)
                [ fontWeight bold
                ]
            ]
        , (.) ChatInput
            [ width (pct 100)
            ]
        ]
