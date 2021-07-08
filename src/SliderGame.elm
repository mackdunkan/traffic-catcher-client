module SliderGame exposing (..)

import Browser
import Css exposing (absolute, backgroundColor, border3, height, hex, hidden, int, left, maxWidth, minWidth, overflow, position, px, relative, solid, width, zIndex)
import Delay
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, disabled, property, style)
import Html.Styled.Events exposing (onClick)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }



-- MODEL


type alias Model =
    { items : List Item
    , position : Position
    , pos : Int
    , delays : List Int
    }


type alias Position =
    { previousItem : Int
    , currentItem : Int
    , nextItem : Int
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
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = initItems
      , position = Position 0 1 2
      , pos = 0
      , delays = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Run
    | Step
    | StartAt Int
    | Next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            let
                position =
                    next model.position (List.length model.items)
            in
            ( { model | position = position }, Cmd.none )

        Run ->
            ( model, Random.generate StartAt <| Random.int 35 65 )

        StartAt s ->
            update Step { model | delays = makeDelays s }

        Step ->
            let
                position =
                    next model.position (List.length model.items)
            in
            case model.delays of
                [] ->
                    ( model, Cmd.none )

                d :: ds ->
                    ( { model
                        | position = position
                        , delays = ds
                      }
                    , Delay.after d Step
                    )


makeDelays : Int -> List Int
makeDelays d =
    if d > 500 then
        []

    else
        d :: makeDelays (d + d // 10)


next : Position -> Int -> Position
next posit totalItem =
    let
        total =
            totalItem - 1

        index i =
            if i + 1 == total then
                total

            else if i + 1 > total then
                0

            else
                i + 1
    in
    Position (index posit.previousItem) (index posit.currentItem) (index posit.nextItem)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ css
                [ border3 (px 2) solid (hex "ffb900")
                , position relative
                , height <| px 200
                , width <| px 430
                , overflow hidden
                ]
            ]
            (List.indexedMap (\i -> viewItem i model.position) model.items)
        , div []
            [ button [ onClick Run, disabled (model.delays /= []) ] [ text "Run" ]
            , button [ onClick Next, disabled (model.delays /= []) ] [ text "Next" ]
            ]
        ]


viewItem : Int -> Position -> Item -> Html Msg
viewItem idx posit i =
    div
        [ style "transition" "all .5s ease-out"
        , css
            [ maxWidth <| px i.width
            , minWidth <| px i.width
            , height <| px i.height
            , backgroundColor <| hex i.color
            , position absolute
            , if idx == posit.currentItem then
                Css.batch
                    [ left <| px (i.width / 2)
                    , zIndex <| int 3
                    ]

              else if idx == posit.previousItem then
                Css.batch
                    [ left <| px (negate <| i.width / 2 + 15)
                    , zIndex <| int 1
                    ]

              else if idx == posit.nextItem then
                Css.batch
                    [ left <| px (i.width + i.width / 2 + 15)
                    , zIndex <| int 2
                    ]

              else
                left <| px -9999
            ]
        ]
        [ text i.title
        , text <| String.fromInt idx
        ]
