module WheelofFortune exposing (..)

import Browser
import Css exposing (Style, animationDuration, animationIterationCount, animationName, border3, borderColor4, borderRadius, borderStyle, borderWidth, borderWidth4, boxShadow3, boxShadow4, deg, height, hex, infinite, int, left, num, padding4, pct, position, property, px, right, rotate, sec, solid, top, transform, transparent, width, zIndex)
import Css.Animations as CssAnimation exposing (custom, keyframes)
import Css.Global exposing (children, descendants, everything, global, selector, typeSelector)
import Html.Styled exposing (Html, div, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import List.Extra
import Svg.Styled as Svg exposing (svg, switch)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Utilities as TW


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


type alias Item =
    { title : String
    , width : Float
    , height : Float
    , color : String
    }


initItems : List Item
initItems =
    [ Item "Шапка" 200 200 "#01d3ad"
    , Item "Шарф" 200 200 "#eee"
    , Item "Кепка" 200 200 "#0074ad"
    , Item "Носок" 200 200 "#0074ad"
    , Item "Шарф2" 200 200 "#eee"
    , Item "Кепка2" 200 200 "#0074ad"
    , Item "Носок2" 200 200 "#0074ad"
    , Item "Носок2" 200 200 "#0074ad"
    ]



-- MODEL


type alias Model =
    { items : List Item
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = initItems
      }
    , Cmd.none
    )


type Msg
    = Run


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewWheelWrap
        ]


viewWheelWrapSvg : Float -> Html Msg
viewWheelWrapSvg size =
    svg
        [ SvgAttr.viewBox <| "0 0 " ++ String.fromFloat size ++ " " ++ String.fromFloat size
        , SvgAttr.width <| String.fromFloat size
        , SvgAttr.height <| String.fromFloat size
        ]
        [ viewSegmentSvg 2 ]


viewSegmentSvg : Float -> Html Msg
viewSegmentSvg s =
    Svg.g []
        [ Svg.path [ SvgAttr.stroke "gray", SvgAttr.strokeWidth "2", SvgAttr.d "M115 115 115 60 A55 55 0 0 1 170 115Z", SvgAttr.fill "#f00" ] []
        ]



--- HTML


viewWheelWrap : Html Msg
viewWheelWrap =
    div
        [ css
            [ TW.relative
            , cubeCss px 474
            , TW.bg_gray_400
            , TW.rounded_full
            , descendants
                [ typeSelector "*"
                    [ TW.box_border
                    , TW.leading_none
                    , TW.text_left
                    ]
                ]
            ]
        ]
        [ viewWheelCenter
        , viewWheelArrow
        , viewWheel
        ]


viewWheelCenter : Html Msg
viewWheelCenter =
    div
        [ css
            [ TW.absolute
            , cubeCss pct 20
            , left <| pct 40
            , top <| pct 40
            , TW.bg_white
            , TW.rounded_full
            , boxShadow4 (px -1) (px 1) (px 7) (hex "#888")
            , zIndex <| int 100
            ]
        ]
        []


viewWheelArrow : Html Msg
viewWheelArrow =
    div
        [ css
            [ cubeCss px 0
            , borderStyle solid
            , borderWidth4 (px 14) (px 40) (px 14) (px 0)
            , borderColor4 transparent (hex "#fff") transparent transparent
            , property "filter" "drop-shadow(-3px 3px 3px rgba(0,0,0,.2))"
            , TW.absolute
            , right <| px 1
            , TW.inset_y_0
            , TW.m_auto
            , zIndex <| int 100
            ]
        ]
        []


viewWheel : Html Msg
viewWheel =
    let
        wait =
            Css.batch
                [ property "animation-timing-function" "linear"
                , property "animation-iteration-count" "infinite"
                , animationDuration (sec 30)
                ]
    in
    div
        [ css
            [ TW.relative
            , cubeCss pct 100
            , animationDuration (sec 6.5)
            , animationIterationCount (num 1)
            , property "animation-timing-function" "ease-out"
            , property "animation-fill-mode" "forwards"
            , transform (rotate <| deg -22.5)
            , wait
            , animationName
                (keyframes
                    [ ( 0, [ CssAnimation.property "transform" "rotate(0)" ] )
                    , ( 100, [ CssAnimation.property "transform" "rotate(360deg)" ] )
                    ]
                )
            ]
        ]
        [ viewWheellInner ]


viewWheellInner : Html Msg
viewWheellInner =
    div [ css [ TW.absolute, cubeCss pct 100, borderRadius <| pct 50, TW.overflow_hidden, border3 (px 18) solid (hex "#fff") ] ]
        [ viewWheellPart (List.take 4 initItems) True
        , viewWheellPart (List.drop 4 initItems) False
        ]


viewWheellPart : List Item -> Bool -> Html Msg
viewWheellPart items group =
    let
        num =
            if group then
                0

            else
                4

        pos =
            if group then
                left <| px 0

            else
                left <| pct 50
    in
    div [ css [ TW.absolute, TW.w_1over2, TW.h_full, TW.overflow_hidden, pos ] ]
        (List.indexedMap (\id -> viewWheellSection <| id + num) items)


viewWheellSection : Int -> Item -> Html Msg
viewWheellSection id i =
    div
        [ css
            [ TW.absolute
            , width <| pct 200
            , TW.h_full
            , styleSlice id
            ]
        ]
        [ span
            [ css
                [ TW.text_white
                , TW.absolute
                , zIndex <| int 30
                , property "transform-origin" "0 0"
                , TW.w_1over2
                , styleSection id
                ]
            ]
            [ span
                [ css
                    [ TW.absolute
                    , property "transform" "translateY(-50%) rotate(180deg)"
                    , TW.text_center
                    , TW.block
                    , TW.w_full
                    , padding4 (pct 0) (pct 8) (pct 1) (pct 36)
                    , TW.box_border
                    , TW.text_base
                    ]
                ]
                [ text i.title ]
            ]
        ]


styleSlice : Int -> Style
styleSlice id =
    case id of
        0 ->
            Css.batch
                [ property "background-image" "linear-gradient(-45deg, transparent 50%, transparent 50%), linear-gradient(-90deg, transparent 50%, #0b14de 50%)"
                ]

        1 ->
            Css.batch
                [ property "background-image" "linear-gradient(0deg, transparent 50%, transparent 50%), linear-gradient(-45deg, transparent 50%, #252ef7 50%)"
                ]

        2 ->
            Css.batch
                [ property "background-image" "linear-gradient(45deg, transparent 50%, transparent 50%), linear-gradient(0deg, transparent 50%, #0b14de 50%)"
                ]

        3 ->
            Css.batch
                [ property "background-image" "linear-gradient(90deg, transparent 50%, transparent 50%), linear-gradient(45deg, transparent 50%, #252ef7 50%)"
                ]

        4 ->
            Css.batch
                [ left <| pct -100
                , property "background-image" "linear-gradient(135deg, transparent 50%, transparent 50%), linear-gradient(90deg, transparent 50%, #0b14de 50%)"
                ]

        5 ->
            Css.batch
                [ left <| pct -100
                , property "background-image" "linear-gradient(180deg, transparent 50%, transparent 50%), linear-gradient(135deg, transparent 50%, #252ef7 50%)"
                ]

        6 ->
            Css.batch
                [ left <| pct -100
                , property "background-image" "linear-gradient(225deg, transparent 50%, transparent 50%), linear-gradient(180deg, transparent 50%, #0b14de 50%)"
                ]

        7 ->
            Css.batch
                [ left <| pct -100
                , property "background-image" "linear-gradient(270deg, transparent 50%, transparent 50%), linear-gradient(225deg, transparent 50%, #252ef7 50%)"
                ]

        _ ->
            Css.batch []


styleSection : Int -> Style
styleSection id =
    case id of
        0 ->
            Css.batch
                [ left <| pct 30.7
                , top <| pct 96.3
                , transform (rotate <| deg -67.5)
                ]

        1 ->
            Css.batch
                [ left <| pct 3.5
                , top <| pct 69.1
                , transform (rotate <| deg -22.5)
                ]

        2 ->
            Css.batch
                [ left <| pct 3.4
                , top <| pct 30.5
                , transform (rotate <| deg 22.5)
                ]

        3 ->
            Css.batch
                [ left <| pct 30.9
                , top <| pct 3.2
                , transform (rotate <| deg 67.5)
                ]

        4 ->
            Css.batch
                [ left <| pct 69.4
                , top <| pct 3.5
                , transform (rotate <| deg 112.5)
                ]

        5 ->
            Css.batch
                [ left <| pct 96.3
                , top <| pct 30.9
                , transform (rotate <| deg 157.5)
                ]

        6 ->
            Css.batch
                [ left <| pct 96.5
                , top <| pct 69.3
                , transform (rotate <| deg 202.5)
                ]

        7 ->
            Css.batch
                [ left <| pct 69.2
                , top <| pct 96.6
                , transform (rotate <| deg 247.5)
                ]

        _ ->
            Css.batch []


cubeCss r size =
    Css.batch
        [ width <| r size
        , height <| r size
        ]
