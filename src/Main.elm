module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (action, href, id, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bonuses = RemoteData.NotAsked
      , winnerBonus = Nothing
      , startGame = False
      , client = Client "" ""
      , sendClient = RemoteData.NotAsked
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | bonuses = RemoteData.Loading }, getBonusesAction )

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


view : Model -> Html Msg
view model =
    case model.winnerBonus of
        Nothing ->
            div []
                [ button [ onClick SendHttpRequest ]
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
    case model.bonuses of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Загрузка..." ]

        RemoteData.Success bonuses ->
            viewBonuses bonuses

        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)


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
