module Widget exposing (Msg, view, viewWidgetWrap)

import Browser
import Css exposing (backgroundColor, color, hex, int, maxWidth, px, rgba, zIndex)
import Css.Transitions exposing (background)
import Html.Styled exposing (Html, a, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, target)
import Tailwind.Utilities as TW


type alias Widget =
    { content : Html Msg
    }



-- UPDATE


type Msg
    = Change String


update : Widget -> Msg -> Widget
update model msg =
    case msg of
        Change newContent ->
            model



-- VIEW


view : (Widget -> msg) -> Widget -> Html msg
view toMsg widget =
    let
        updateAndWrap msg =
            toMsg (update widget msg)
    in
    div
        [ css
            [ maxWidth <| px 900
            , TW.mx_auto
            , TW.absolute
            , TW.inset_0
            , backgroundColor <| hex "#262635"
            , TW.text_white
            , zIndex <| int 999999
            , TW.overflow_hidden
            ]
        ]
        [ text "Ура" ]


viewWidgetWrap : Html msg -> Html msg
viewWidgetWrap content =
    div
        [ css
            [ maxWidth <| px 660
            , TW.mx_auto
            , TW.absolute
            , TW.inset_0
            , backgroundColor <| hex "#262635"
            , TW.text_white
            , zIndex <| int 999999
            , TW.overflow_hidden
            ]
        ]
        [ widgetAbout, content ]


widgetAbout : Html msg
widgetAbout =
    a
        [ href "#"
        , target "_blank"
        , css
            [ TW.absolute
            , TW.text_xs
            , TW.right_4
            , TW.bottom_4
            , zIndex <| int 500
            , color <| hex "#aaa"
            , TW.no_underline
            , backgroundColor <| rgba 255 255 255 0.05
            , TW.py_1
            , TW.px_2
            , TW.rounded_full
            ]
        ]
        [ text "хочу такой виджет" ]
