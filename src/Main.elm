module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)



---- MODEL ----


type Model
    = Loading
    | Running GameState
    | Error Error


type alias GameState =
    { phrase : String
    , guesses : Set String
    }


init : ( Model, Cmd Msg )
init =
    ( Loading
    , fetchNewPhrase
    )



---- UPDATE ----


type Msg
    = Guess String
    | Restart
    | NewPhrase (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess ch ->
            case model of
                Running gameState ->
                    ( Running { gameState | guesses = Set.insert ch gameState.guesses }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Restart ->
            case model of
                Running _ ->
                    ( Loading, fetchNewPhrase )

                _ ->
                    ( model, Cmd.none )

        NewPhrase result ->
            case result of
                Ok newPhrase ->
                    ( Running { guesses = Set.empty, phrase = newPhrase }, Cmd.none )

                Err emsg ->
                    ( Error emsg, Cmd.none )


fetchNewPhrase : Cmd Msg
fetchNewPhrase =
    Http.get
        { url = "https://snapdragon-fox.glitch.me/word"
        , expect = Http.expectJson NewPhrase wordDecoder
        }


wordDecoder : Decoder String
wordDecoder =
    Decode.field "word" Decode.string



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            span [] [ text "Loading..." ]

        Running gameState ->
            viewGameState gameState

        Error emsg ->
            span [] [ text <| "Error Occurred : " ++ Debug.toString emsg ]


viewGameState : GameState -> Html Msg
viewGameState gameState =
    let
        phraseHtml =
            gameState.phrase
                |> String.toUpper
                |> String.split ""
                |> List.map
                    (\ch ->
                        if ch == " " then
                            " "

                        else if Set.member ch gameState.guesses then
                            ch

                        else
                            "_"
                    )
                |> List.map
                    (\ch ->
                        span [] [ text ch ]
                    )
                |> div []

        buttonsHtml =
            "abcdefghijklmnopqrstuvwxyz"
                |> String.toUpper
                |> String.split ""
                |> List.map
                    (\ch ->
                        button [ onClick <| Guess ch ] [ text ch ]
                    )
                |> div []

        invalidHtml =
            Set.toList gameState.guesses
                |> List.map
                    (\ch ->
                        if String.contains ch (String.toUpper gameState.phrase) then
                            ""

                        else
                            ch
                    )
                |> List.map
                    (\ch ->
                        if ch /= "" then
                            span [ style "color" "red" ] [ text ch ]

                        else
                            Html.text ""
                    )
                |> div []
    in
    div []
        [ phraseHtml
        , buttonsHtml
        , invalidHtml
        , button [ onClick Restart ] [ text "Restart" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
