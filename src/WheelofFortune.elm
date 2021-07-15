module WheelofFortune exposing (..)

import Browser
import Css exposing (Style, animationDuration, animationIterationCount, animationName, backgroundColor, backgroundImage, backgroundPosition, backgroundPosition2, border3, borderColor4, borderRadius, borderStyle, borderWidth, borderWidth4, bottom, boxShadow3, boxShadow4, center, deg, float, fontFamilies, fontSize, height, hex, infinite, int, left, maxWidth, none, nthChild, num, opacity, padding4, pct, position, property, pt, px, qt, right, rotate, sansSerif, sec, solid, top, transform, transparent, url, width, zIndex)
import Css.Animations as CssAnimation exposing (Property, custom, keyframes)
import Css.Global exposing (children, descendants, everything, global, selector, typeSelector)
import Css.Transitions exposing (background, easeInOut, transition)
import Delay
import Html.Styled exposing (Html, br, button, div, input, node, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, disabled, href, placeholder, rel, style, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import List.Extra
import Mask
import PhoneNumber
import PhoneNumber.Countries exposing (countryRU)
import Svg.Styled as Svg exposing (svg, switch)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Utilities as TW exposing (rounded_full)
import Widget


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
    , Item "Носок3" 200 200 "#0074ad"
    ]



-- MODEL


type alias Model =
    { items : List Item
    , winnerID : Int
    , isStartGame : Bool
    , phone : String
    , errorPhone : Bool
    , step : Int
    , isOpen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = initItems
      , winnerID = 4
      , isStartGame = False
      , phone = ""
      , errorPhone = False
      , step = 1
      , isOpen = False
      }
    , Cmd.none
    )


type Msg
    = StartFortune
    | Message
    | InputtedPhone String
    | Modal Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartFortune ->
            if validNorwegianNumber (unMask <| model.phone) then
                ( { model | isStartGame = True, errorPhone = False, step = 2 }, Delay.after 6500 Message )

            else
                ( { model | errorPhone = True }, Cmd.none )

        Message ->
            ( { model | step = 3 }, Cmd.none )

        InputtedPhone phone ->
            ( { model | phone = Mask.number "(###) ###-####" phone }, Cmd.none )

        Modal is ->
            ( { model | isOpen = is }, Cmd.none )


unMask : String -> String
unMask str =
    str
        |> String.replace "(" ""
        |> String.replace ")" ""
        |> String.replace "-" ""
        |> String.replace " " ""


validNorwegianNumber : String -> Bool
validNorwegianNumber number =
    PhoneNumber.valid
        { defaultCountry = countryRU
        , otherCountries = []
        , types = PhoneNumber.anyType
        }
        number



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css
            [ fontFamilies [ qt "IBM Plex Sans", .value sansSerif ]
            , descendants
                [ typeSelector "*"
                    [ TW.box_border
                    , TW.leading_none
                    ]
                ]
            ]
        ]
        [ node "link" [ href "https://fonts.googleapis.com/css?family=IBM+Plex+Sans:500,600&display=swap", rel "stylesheet" ] []
        , viewBonusesGame model
        , div
            [ css
                [ TW.fixed
                , TW.inset_y_0
                , TW.w_full
                , maxWidth <| px 660
                , transition
                    [ Css.Transitions.left 1000
                    , Css.Transitions.transform3 500 0 easeInOut
                    ]
                ]
            , if model.isOpen then
                style "left" "0"

              else
                style "left" "-660px"
            ]
            [ Widget.viewWidgetWrap <| viewWheelWrap model ]
        ]


viewBonusesGame : Model -> Html Msg
viewBonusesGame model =
    case model.isOpen of
        True ->
            div [] []

        False ->
            viewButtonBonusGame model


viewButtonBonusGame : Model -> Html Msg
viewButtonBonusGame model =
    button
        [ css
            [ TW.fixed
            , zIndex <| int 999999
            , cubeCss px 100
            , TW.cursor_pointer
            , left <| px 10
            , bottom <| px -30
            , backgroundImage (url "img/gift.png")
            , TW.bg_center
            , TW.bg_contain
            , TW.origin_bottom_right
            , TW.bg_no_repeat
            , TW.border_0
            , backgroundColor transparent
            ]
        , onClick <| Modal True
        ]
        []


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


viewWheelWrap : Model -> Html Msg
viewWheelWrap model =
    let
        leftWrap : Html Msg -> Html Msg
        leftWrap content =
            div [ css [ TW.absolute, TW.inset_y_0, TW.left_0, TW.flex, TW.items_center, TW.text_center, zIndex <| int 1 ] ] [ content ]

        rightWrap : Html Msg -> Html Msg
        rightWrap content =
            div [ css [ TW.absolute, TW.inset_y_0, left <| px 310, right <| px 50, TW.flex, TW.items_center, TW.text_left, zIndex <| int 2, maxWidth <| px 400 ] ] [ content ]
    in
    div []
        [ leftWrap
            (div
                [ css
                    [ TW.relative
                    , cubeCss px 474
                    , left <| px -237
                    , TW.bg_gray_400
                    , TW.rounded_full
                    ]
                ]
                [ viewWheelCenter
                , viewWheelArrow
                , viewWheel model
                ]
            )
        , rightWrap <| viewFormAndStepClient model
        ]


viewFormAndStepClient : Model -> Html Msg
viewFormAndStepClient model =
    case model.step of
        1 ->
            div [ css [ TW.grid, TW.gap_4 ] ]
                [ viewTitle "Крутите \nколесо!"
                , viewDesc "Введите свой номер телефона, \nчтобы выиграть приз!"
                , div [ css [ TW.grid, TW.gap_3, TW.grid_cols_2 ] ]
                    [ div [ css [ TW.col_span_2, TW.flex, TW.items_center ] ]
                        [ div [ css [ TW.mr_2 ] ] [ text "+7" ]
                        , input
                            [ onInput InputtedPhone
                            , value model.phone
                            , type_ "text"
                            , placeholder "Введите номер телефона"
                            , css
                                [ TW.w_full
                                , height <| px 50
                                , TW.border_0
                                , TW.outline_none
                                , TW.px_2
                                , TW.text_base
                                , fontFamilies [ qt "IBM Plex Sans", .value sansSerif ]
                                , boolCss model.errorPhone (Css.batch [ TW.border_2, TW.border_red_700 ])
                                ]
                            ]
                            []
                        ]
                    , button [ onClick StartFortune, css [ styleButton, backgroundColor <| hex "#252ef7" ] ] [ text "Крутить" ]
                    , button [ onClick <| Modal False, css [ styleButton ] ] [ text "Закрыть" ]
                    ]
                , div [ css [ fontSize <| px 10, TW.opacity_20 ] ] [ text "Нажимая на кнопку, вы даете согласие на обработку своих персональных данных и соглашаетесь с Политикой конфиденциальности" ]
                ]

        2 ->
            viewLoader

        3 ->
            let
                getTitleWinnerBonus =
                    List.Extra.getAt model.winnerID model.items
            in
            case getTitleWinnerBonus of
                Just i ->
                    div [ css [ TW.grid, TW.gap_4 ] ]
                        [ viewTitle i.title
                        , viewDesc "Поздравляем! \nмы скоро свяжемся с Вами!"
                        ]

                Nothing ->
                    div [] [ viewTitle "Error" ]

        _ ->
            div [] [ text "Error" ]


viewLoader : Html Msg
viewLoader =
    let
        animation : Int -> List Style
        animation num =
            case num of
                1 ->
                    [ property "animation-iteration-count" "infinite"
                    , animationDuration (sec 0.6)
                    , animationName
                        (keyframes
                            [ ( 0, [ CssAnimation.property "transform" "scale(0)" ] )
                            , ( 100, [ CssAnimation.property "transform" "scale(1)" ] )
                            ]
                        )
                    ]

                2 ->
                    [ property "animation-iteration-count" "infinite"
                    , animationDuration (sec 0.6)
                    , animationName
                        (keyframes
                            [ ( 0, [ CssAnimation.property "transform" "translate(0,0)" ] )
                            , ( 100, [ CssAnimation.property "transform" "translate(24px,0)" ] )
                            ]
                        )
                    ]

                3 ->
                    [ property "animation-iteration-count" "infinite"
                    , animationDuration (sec 0.6)
                    , animationName
                        (keyframes
                            [ ( 0, [ CssAnimation.property "transform" "scale(1)" ] )
                            , ( 100, [ CssAnimation.property "transform" "scale(0)" ] )
                            ]
                        )
                    ]

                _ ->
                    []
    in
    div
        [ css
            [ TW.relative
            , cubeCss px 80
            , TW.mx_auto
            , descendants
                [ typeSelector "div"
                    [ TW.absolute
                    , top <| px 33
                    , cubeCss px 13
                    , TW.rounded_full
                    , TW.bg_white
                    , property "animation-timing-function" "cubic-bezier(0,1,1,0)"
                    ]
                ]
            ]
        ]
        [ div [ css [ nthChild "1" <| animation 1 ++ [ left <| px 8 ] ] ] []
        , div [ css [ nthChild "2" <| animation 2 ++ [ left <| px 8 ] ] ] []
        , div [ css [ nthChild "3" <| animation 2 ++ [ left <| px 32 ] ] ] []
        , div [ css [ nthChild "4" <| animation 3 ++ [ left <| px 56 ] ] ] []
        ]


viewTitle : String -> Html Msg
viewTitle str =
    div [ css [ TW.font_black, TW.text_5xl ] ] (htmlAddedBrFromString str)


viewDesc : String -> Html Msg
viewDesc str =
    div [ css [ TW.text_lg ] ]
        (htmlAddedBrFromString str)


boolCss : Bool -> Style -> Style
boolCss is style =
    if is then
        style

    else
        Css.batch []


styleButton =
    Css.batch
        [ TW.text_center
        , TW.text_lg
        , TW.px_4
        , TW.py_2
        , backgroundColor <| hex "#feffff12"
        , TW.text_white
        , TW.border_0
        , fontFamilies [ qt "IBM Plex Sans", .value sansSerif ]
        , TW.cursor_pointer
        ]


htmlAddedBrFromString : String -> List (Html msg)
htmlAddedBrFromString str =
    List.intersperse (br [] []) (List.map text (String.lines str))


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


viewWheel : Model -> Html Msg
viewWheel model =
    let
        animat =
            if model.isStartGame then
                styleWinner model.winnerID

            else
                Css.batch
                    [ property "animation-timing-function" "linear"
                    , property "animation-iteration-count" "infinite"
                    , animationDuration (sec 30)
                    , animationName
                        (keyframes
                            [ ( 0, [ CssAnimation.property "transform" "rotate(0)" ] )
                            , ( 100, [ CssAnimation.property "transform" "rotate(360deg)" ] )
                            ]
                        )
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
            , animat
            ]
        ]
        [ viewWheelInner model.items ]


viewWheelInner : List Item -> Html Msg
viewWheelInner items =
    div [ css [ TW.absolute, cubeCss pct 100, borderRadius <| pct 50, TW.overflow_hidden, border3 (px 18) solid (hex "#fff") ] ]
        [ viewWheelPart (List.take 4 items) True
        , viewWheelPart (List.drop 4 items) False
        ]


viewWheelPart : List Item -> Bool -> Html Msg
viewWheelPart items group =
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
        (List.indexedMap (\id -> viewWheelSection <| id + num) items)


viewWheelSection : Int -> Item -> Html Msg
viewWheelSection id i =
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


styleWinner : Int -> Style
styleWinner id =
    let
        rotate : Float -> Style
        rotate endRotate =
            Css.batch
                [ animationName
                    (keyframes
                        [ ( 0, [ CssAnimation.property "transform" "rotate(-22.5deg)" ] )
                        , ( 100, [ CssAnimation.property "transform" ("rotate(" ++ String.fromFloat endRotate ++ "deg)") ] )
                        ]
                    )
                ]
    in
    case id of
        0 ->
            rotate 967

        1 ->
            rotate 922.5

        2 ->
            rotate 1237.5

        3 ->
            rotate 1192.5

        4 ->
            rotate 1147.5

        5 ->
            rotate 1102.5

        6 ->
            rotate 1057.5

        7 ->
            rotate 1012.5

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
