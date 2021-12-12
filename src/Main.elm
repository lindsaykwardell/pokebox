module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Pokeapi exposing (Pokedex, PokemonResourceResponse)


init : ( Pokedex, Cmd Msg )
init =
    let
        ( pokedex, load ) =
            Pokeapi.init
    in
    ( pokedex, load PageLoaded )


type Msg
    = PageLoaded (Result Http.Error PokemonResourceResponse)
    | LoadNextPage
    | LoadPreviousPage


update : Msg -> Pokedex -> ( Pokedex, Cmd Msg )
update msg model =
    case msg of
        PageLoaded result ->
            ( { model | list = result }, Cmd.none )

        LoadNextPage ->
            let
                ( pokedex, load ) =
                    Pokeapi.nextPage model
            in
            ( pokedex, load PageLoaded )

        LoadPreviousPage ->
            let
                ( pokedex, load ) =
                    Pokeapi.previousPage model
            in
            ( pokedex, load PageLoaded )


view : Pokedex -> Html Msg
view model =
    div []
        [ case model.list of
            Err err ->
                case err of
                    Http.BadBody val ->
                        text val

                    _ ->
                        text "Error"

            Ok list ->
                div [ class "flex flex-col items-center"]
                    (Pokeapi.viewList list.results
                        ++ [ div [ class "flex gap-4" ]
                                [ button
                                    [ class "bg-blue-500 text-white p-2 rounded-lg w-32", onClick LoadPreviousPage ]
                                    [ text "Load previous"
                                    ]
                                , button
                                    [ class "bg-blue-500 text-white p-2 rounded-lg w-32", onClick LoadNextPage ]
                                    [ text "Load next"
                                    ]
                                ]
                           ]
                    )
        ]


main : Program () Pokedex Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = \_ -> Sub.none }
