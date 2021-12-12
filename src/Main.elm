module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Pokeapi exposing (Pokedex, Pokemon, PokemonResourceResponse)


init : ( Model, Cmd Msg )
init =
    let
        ( pokedex, load ) =
            Pokeapi.init
    in
    ( { pokedex = pokedex, query = Nothing }, load PageLoaded )


type alias Model =
    { pokedex : Pokedex
    , query : Maybe String
    }


type Msg
    = PageLoaded (Result Http.Error PokemonResourceResponse)
    | LoadNextPage
    | LoadPreviousPage
    | LoadPokemon String
    | PokemonLoaded (Result Http.Error Pokemon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageLoaded result ->
            ( { model | pokedex = Pokeapi.updateList model.pokedex result }, Cmd.none )

        LoadNextPage ->
            let
                ( pokedex, load ) =
                    Pokeapi.nextPage model.pokedex
            in
            ( { model | pokedex = pokedex }, load PageLoaded )

        LoadPreviousPage ->
            let
                ( pokedex, load ) =
                    Pokeapi.previousPage model.pokedex
            in
            ( { model | pokedex = pokedex }, load PageLoaded )

        LoadPokemon url ->
            let
                load =
                    Pokeapi.queryPokemon { url = url, expect = PokemonLoaded }
            in
            ( model, load )

        PokemonLoaded result ->
            let
                pokedex =
                    Pokeapi.updatePokemon model.pokedex result
            in
            ( { model | pokedex = pokedex }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ case model.pokedex.list of
            Err err ->
                case err of
                    Http.BadBody val ->
                        text val

                    _ ->
                        text "Error"

            Ok list ->
                div [ class "flex flex-col items-center" ]
                    (Pokeapi.viewList model.pokedex list.results (\url -> LoadPokemon url)
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


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = \_ -> Sub.none }
