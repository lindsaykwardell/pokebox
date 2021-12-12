module Main exposing (main)

import BillsPc exposing (PcStatus, StoredPokemon, decodeStoredPokemon, storePokemon)
import Browser
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Http
import Pokeapi exposing (Pokedex, Pokemon, PokemonResourceResponse)


init : ( Model, Cmd Msg )
init =
    let
        ( pokedex, load ) =
            Pokeapi.init
    in
    ( { pokedex = pokedex, query = Nothing, pcStatus = BillsPc.Idle }, load PageLoaded )


type alias Model =
    { pokedex : Pokedex
    , query : Maybe String
    , pcStatus : PcStatus
    }


type Msg
    = PageLoaded (Result Http.Error PokemonResourceResponse)
    | LoadNextPage
    | LoadPreviousPage
    | LoadPokemon String
    | PokemonLoaded (Result Http.Error Pokemon)
    | InputQuery String
    | StorePokemon Pokemon
    | PokemonStored (Result Http.Error StoredPokemon)


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

        InputQuery query ->
            if String.length query > 0 then
                ( { model | query = Just query }, Cmd.none )

            else
                ( { model | query = Nothing }, Cmd.none )

        StorePokemon pokemon ->
            ( model, storePokemon pokemon PokemonStored )

        PokemonStored result ->
            case result of
                Result.Ok storedPokemon ->
                    ( { model
                        | pcStatus = BillsPc.PokemonStored storedPokemon
                      }
                    , Cmd.none
                    )

                Result.Err error ->
                    ( { model
                        | pcStatus = BillsPc.Error error
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center gap-2" ]
        [ div [ class "border-b border-black pb-4 flex flex-col items-center gap-4" ]
            [ label [] [ text "Search for a Pokemon", input [ class "border-2", onInput InputQuery ] [] ]
            , button
                [ class "bg-blue-500 text-white p-2 rounded-lg w-32"
                , onClick <| Pokeapi.searchPokemon model.query LoadPokemon
                ]
                [ text "Search" ]
            , Pokeapi.viewPokemon model.pokedex.openPokemon
            , case model.pokedex.openPokemon of
                Nothing ->
                    text ""

                Just pokemon ->
                    button
                        [ class "bg-blue-500 text-white p-2 rounded-lg w-40"
                        , onClick <| StorePokemon pokemon
                        ]
                        [ text "Store in Bill's PC" ]
            , case model.pcStatus of
                BillsPc.Idle ->
                    text ""

                BillsPc.PokemonStored storedPokemon ->
                    text <| storedPokemon.name ++ " was stored in box " ++ String.fromInt storedPokemon.boxId

                BillsPc.Error _ ->
                    text "Something went wrong! Unable to store your Pokemon."
            ]
        , case model.pokedex.list of
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
