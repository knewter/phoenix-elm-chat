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
    | Messages
    | Message
    | MessageUser
    | MessageBody
    | Roster
    | RosterUser


compile =
    Css.compile


mainNamespace : Html.CssHelpers.Namespace String class id msg
mainNamespace =
    withNamespace "main"


css : Css.Stylesheet
css =
    (stylesheet << namespace mainNamespace.name)
        [ Css.Elements.body
            [ fontFamily sansSerif
            ]
        , (.) Chat
            [ backgroundColor V.chatBackgroundColor
            , display inlineBlock
            , width (Css.em 20)
            ]
        , (.) Messages
            [ padding (Css.em 1)
            ]
        , (.) Message
            []
        , (.) MessageUser
            [ fontWeight bold ]
        , (.) MessageBody
            [ color V.messageBodyColor ]
        , (.) Roster
            [ width (Css.em 10)
            , display inlineBlock
            , padding (Css.em 1)
            ]
        , (.) RosterUser
            [ padding (Css.em 0.5)
            , color (rgb 100 100 100)
            , cursor pointer
            ]
        ]
