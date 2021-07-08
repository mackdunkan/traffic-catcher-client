module Main exposing (main)

import Array
import Browser
import Css exposing (absolute, after, alignItems, auto, backgroundColor, border, border3, borderBox, borderRadius, bottom, boxSizing, center, color, cursor, display, displayFlex, fixed, flex, height, hex, hidden, int, justifyContent, left, linearGradient, none, num, opacity, overflowY, padding, padding2, pct, pointer, pointerEvents, position, property, px, right, sec, solid, textAlign, toTop, top, visibility, visible, width, zIndex)
import Css.Transitions as Transitions exposing (background, transition)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (action, css, href, id, type_)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Icon as Icon
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline as Pip exposing (required)
import Random
import RemoteData exposing (RemoteData, WebData)


type alias Bonuse =
    { id : String
    , title : String
    }


type alias Client =
    { name : String
    , phone : String
    }


type alias Model =
    { bonuses : WebData (List Bonuse)
    , winnerBonus : Maybe Bonuse
    , startGame : Bool
    , client : Client
    , sendClient : WebData Client
    , isActiveModal : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bonuses = RemoteData.NotAsked
      , winnerBonus = Nothing
      , startGame = False
      , client = Client "" ""
      , sendClient = RemoteData.NotAsked
      , isActiveModal = False
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = SendHttpRequest
    | DataReceived (WebData (List Bonuse))
    | DataClient (WebData Client)
    | Roll Int
    | WinnerBonus Int
    | SavePhone String
    | SaveName String
    | SendCreateClient
    | ModalClose


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | isActiveModal = True, bonuses = RemoteData.Loading }, getBonusesAction )

        DataReceived response ->
            ( { model | bonuses = response }, Cmd.none )

        Roll length ->
            ( { model | startGame = True }, Random.generate WinnerBonus (Random.int 0 length) )

        WinnerBonus winnerBonus ->
            let
                bonuseWinner =
                    getBonuseIdWinner model winnerBonus
            in
            ( { model | winnerBonus = bonuseWinner }, Cmd.none )

        SavePhone phone ->
            ( { model | client = Client model.client.name phone }, Cmd.none )

        SaveName name ->
            ( { model | client = Client name model.client.phone }, Cmd.none )

        SendCreateClient ->
            ( { model | sendClient = RemoteData.Loading }, getCreateClientAction model.client.phone model.client.name model.winnerBonus )

        DataClient response ->
            ( { model | sendClient = response }, Cmd.none )

        ModalClose ->
            ( { model | isActiveModal = False }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.winnerBonus of
        Nothing ->
            div []
                [ button
                    [ css
                        [ position fixed
                        , right <| px 100
                        , bottom <| px 100
                        , backgroundColor <| hex "#e51d16"
                        , color <| hex "#fff"
                        , border <| px 0
                        , padding <| px 10
                        , cursor pointer
                        ]
                    , onClick SendHttpRequest
                    ]
                    [ text "Получить бонусы" ]
                , viewBonusesOrError model
                ]

        Just winner ->
            div []
                [ viewWinnerBonus winner
                , viewFormClientOrError model
                ]


getBonuseIdWinner : Model -> Int -> Maybe Bonuse
getBonuseIdWinner model id =
    case model.bonuses of
        RemoteData.Success bonuses ->
            Array.get id <| Array.fromList bonuses

        _ ->
            Nothing


viewFormClientOrError : Model -> Html Msg
viewFormClientOrError model =
    case model.sendClient of
        RemoteData.NotAsked ->
            viewFormClient

        RemoteData.Loading ->
            h3 [] [ text "Отправляем ваши контакты..." ]

        RemoteData.Success client ->
            viewClientSent client

        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)


viewClientSent : Client -> Html Msg
viewClientSent client =
    div []
        [ h4 [] [ text client.name ]
        , p [] [ text "Ваши контактные данные успешно отправлены!" ]
        ]


viewFormClient : Html Msg
viewFormClient =
    div []
        [ h4 [] [ text "Заполните форму чтобы получить бонус" ]
        , Html.form [ onSubmit SendCreateClient, action "javascript:void(0);" ]
            [ div []
                [ text "Name"
                , input [ id "name", type_ "text", onInput SaveName ] []
                ]
            , div []
                [ text "Телефон"
                , input [ id "phone", type_ "tel", onInput SavePhone ] []
                ]
            , div []
                [ button [ type_ "submit" ]
                    [ text "Получить подарок" ]
                ]
            ]
        ]


viewWinnerBonus : Bonuse -> Html Msg
viewWinnerBonus bonus =
    div [] [ text <| "Вы выиграли " ++ bonus.title ]


viewBonusesOrError : Model -> Html Msg
viewBonusesOrError model =
    viewModal model.isActiveModal
        (case model.bonuses of
            RemoteData.NotAsked ->
                text ""

            RemoteData.Loading ->
                h3 [ css [ color <| hex "#fff" ] ] [ text "Загрузка..." ]

            RemoteData.Success bonuses ->
                viewBonuses bonuses

            RemoteData.Failure httpError ->
                viewError (buildErrorMessage httpError)
        )


viewModal : Bool -> Html Msg -> Html Msg
viewModal isActive content =
    div
        [ css
            [ position fixed
            , top <| px 0
            , left <| px 0
            , width <| pct 100
            , height <| pct 100
            , property "visibility" <|
                if isActive then
                    "visible"

                else
                    "hidden"
            , opacity <|
                if isActive then
                    num 1

                else
                    num 0
            , property "pointer-events" <|
                if isActive then
                    "auto"

                else
                    "none"
            , transition
                [ Transitions.visibility2 0 0.3
                , Transitions.opacity2 0.3 0
                , Transitions.zIndex2 0 0.3
                ]
            , after
                [ property "content" "''"
                , position absolute
                , left <| px 0
                , bottom <| px 0
                , width <| pct 100
                , height <| px 60
                , pointerEvents none
                , property "background" "linear-gradient(to top, #34383c, rgba(52, 56, 60, 0))"
                ]
            ]
        ]
        [ div []
            [ button
                [ css
                    [ position fixed
                    , zIndex <| Css.int 1
                    , top <| px 20
                    , right <| pct 5
                    , height <| px 50
                    , width <| px 50
                    , borderRadius <| pct 50
                    , border3 (px 1) solid (hex "#333")
                    , backgroundColor <| hex "#fff"
                    , cursor <| pointer
                    ]
                , onClick ModalClose
                ]
                [ Icon.close
                ]
            , span
                [ css
                    [ position fixed
                    , top <| px 0
                    , left <| px 0
                    , backgroundColor <| hex "#34383c"
                    , width <| pct 100
                    , height <| pct 100
                    , zIndex <| Css.int -1
                    ]
                ]
                []
            ]
        , div
            [ css
                [ height <| pct 100
                , width <| pct 100
                , padding <| pct 5
                , textAlign left
                , overflowY auto
                , property "-webkit-font-smoothing" "antialiased"
                , property "-webkit-font-smoothing" "grayscale"
                , boxSizing borderBox
                , displayFlex
                , alignItems center
                , justifyContent center
                ]
            ]
            [ content ]
        ]


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewBonuses : List Bonuse -> Html Msg
viewBonuses bonuses =
    div []
        [ h3 [] [ text "Бонусы" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewPost bonuses)
        , buttonStartGame <| List.length bonuses
        ]


buttonStartGame : Int -> Html Msg
buttonStartGame totalBonuses =
    div []
        [ button [ onClick <| Roll <| totalBonuses ] [ text "Старт" ]
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Title" ]
        ]


viewPost : Bonuse -> Html Msg
viewPost bonuse =
    tr []
        [ td []
            [ text bonuse.id ]
        , td []
            [ text bonuse.title ]
        ]


bonuseDecoder : Decoder Bonuse
bonuseDecoder =
    Decode.succeed Bonuse
        |> required "id" string
        |> required "title" string


createUserDecoder : Decoder Client
createUserDecoder =
    Decode.succeed Client
        |> required "name" string
        |> required "phone" string


getBonusesAction : Cmd Msg
getBonusesAction =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://localhost:8000/api/Bonuses?siteId=a6e96d3d-cac1-4c68-a492-ffca9b78fe0b"
        , body = Http.emptyBody
        , expect =
            list bonuseDecoder
                |> Http.expectJson (RemoteData.fromResult >> DataReceived)
        , timeout = Nothing
        , tracker = Nothing
        }


getCreateClientAction : String -> String -> Maybe Bonuse -> Cmd Msg
getCreateClientAction phone name bonus =
    let
        bonusId =
            case bonus of
                Just bon ->
                    bon.id

                Nothing ->
                    ""
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "http://localhost:8000/api/CreateClient?siteId=a6e96d3d-cac1-4c68-a492-ffca9b78fe0b&bonuseId=" ++ bonusId ++ "&phone=" ++ phone ++ "&name=" ++ name
        , body = Http.emptyBody
        , expect =
            createUserDecoder
                |> Http.expectJson (RemoteData.fromResult >> DataClient)
        , timeout = Nothing
        , tracker = Nothing
        }


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
