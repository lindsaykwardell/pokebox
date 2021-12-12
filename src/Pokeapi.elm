module Pokeapi exposing (Pokedex, Pokemon, PokemonResource, PokemonResourceResponse, init, nextPage, previousPage, viewList)

import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Pokedex =
    { limit : Int
    , offset : Int
    , list : Result Http.Error PokemonResourceResponse
    , openPokemon : Maybe Pokemon
    }


type alias PokemonResource =
    { name : String
    , url : String
    }


type alias PokemonResourceResponse =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List PokemonResource
    }


decodePokemonResourceResponse : Decoder PokemonResourceResponse
decodePokemonResourceResponse =
    Decode.succeed PokemonResourceResponse
        |> Decode.required "count" Decode.int
        |> Decode.required "next" (Decode.nullable Decode.string)
        |> Decode.required "previous" (Decode.nullable Decode.string)
        |> Decode.required "results" (Decode.list decodePokemonResource)


decodePokemonResource : Decoder PokemonResource
decodePokemonResource =
    Decode.succeed PokemonResource
        |> Decode.required "name" Decode.string
        |> Decode.required "url" Decode.string


viewList : List PokemonResource -> List (Html msg)
viewList list =
    List.map
        (\pokemon ->
            Html.div [] [ Html.text pokemon.name ]
        )
        list


type alias Pokemon =
    { id : Int
    , name : String
    , base_experience : Int
    , height : Int
    , is_default : Bool
    , order : Int
    , weight : Int
    }


decodePokemon : Decoder Pokemon
decodePokemon =
    Decode.succeed Pokemon
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.required "base_experience" Decode.int
        |> Decode.required "height" Decode.int
        |> Decode.required "is_default" Decode.bool
        |> Decode.required "order" Decode.int
        |> Decode.required "weight" Decode.int


offset : Int
offset =
    20


init : ( Pokedex, (Result Http.Error PokemonResourceResponse -> msg) -> Cmd msg )
init =
    let
        pokedex =
            { limit = 20
            , offset = 0
            , list = Ok { count = 0, next = Nothing, previous = Nothing, results = [] }
            , openPokemon = Nothing
            }
    in
    ( pokedex
    , queryList pokedex
    )


nextPage : Pokedex -> ( Pokedex, (Result Http.Error PokemonResourceResponse -> msg) -> Cmd msg )
nextPage pokedex =
    let
        updatedPokedex =
            { pokedex | offset = pokedex.offset + offset }
    in
    ( updatedPokedex
    , queryList updatedPokedex
    )


previousPage : Pokedex -> ( Pokedex, (Result Http.Error PokemonResourceResponse -> msg) -> Cmd msg )
previousPage pokedex =
    let
        updatedPokedex =
            { pokedex
                | offset =
                    if pokedex.offset - offset < 0 then
                        0

                    else
                        pokedex.offset - offset
            }
    in
    ( updatedPokedex
    , queryList updatedPokedex
    )



-- PokeAPI


rootUrl : String
rootUrl =
    "http://pokeapi.co/api/v2/pokemon/"


queryList :
    Pokedex
    -> (Result Http.Error PokemonResourceResponse -> msg)
    -> Cmd msg
queryList pokedex expect =
    let
        url =
            rootUrl
                ++ "?limit="
                ++ String.fromInt pokedex.limit
                ++ "&offset="
                ++ String.fromInt pokedex.offset
    in
    Http.get
        { url = url
        , expect = Http.expectJson expect decodePokemonResourceResponse
        }


queryPokemon :
    { id : Int
    , expect : Result Http.Error Pokemon -> msg
    }
    -> Cmd msg
queryPokemon { id, expect } =
    let
        url =
            rootUrl
                ++ String.fromInt id
                ++ "/"
    in
    Http.get
        { url = url
        , expect = Http.expectJson expect decodePokemon
        }