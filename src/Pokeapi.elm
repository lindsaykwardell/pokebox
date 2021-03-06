module Pokeapi exposing
    ( Pokedex
    , Pokemon
    , PokemonResource
    , PokemonResourceResponse
    , init
    , nextPage
    , previousPage
    , queryPokemon
    , searchPokemon
    , updateList
    , updatePokemon
    , viewList
    , viewPokemon
    )

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import String.Extra as String


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


viewList : Pokedex -> List PokemonResource -> (String -> msg) -> List (Html msg)
viewList _ list clickAction =
    List.map
        (\pokemon ->
            Html.div [ Attrs.class "text-center" ]
                [ Html.button [ onClick (clickAction pokemon.url) ] [ Html.text (String.toSentenceCase pokemon.name) ]
                ]
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


viewPokemon : Maybe Pokemon -> Html msg
viewPokemon maybePokemon =
    Html.div [ Attrs.class "bg-gray-200 border-2 border-black rounded-lg w-32 h-32 flex justify-center items-center" ]
        [ case maybePokemon of
            Nothing ->
                Html.div [ Attrs.class "text-center italic text-gray-800" ]
                    [ Html.text "No Pokemon Selected" ]

            Just pokemon ->
                Html.img
                    [ Attrs.src ("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" ++ String.fromInt pokemon.id ++ ".png") ]
                    []
        ]


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


updateList : Pokedex -> Result Http.Error PokemonResourceResponse -> Pokedex
updateList pokedex response =
    { pokedex
        | list = response
    }


updatePokemon : Pokedex -> Result Http.Error Pokemon -> Pokedex
updatePokemon pokedex response =
    case response of
        Ok pokemon ->
            { pokedex
                | openPokemon = Just pokemon
            }

        Err _ ->
            { pokedex | openPokemon = Nothing }


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
    "https://pokeapi.co/api/v2/pokemon/"


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
    { url : String
    , expect : Result Http.Error Pokemon -> msg
    }
    -> Cmd msg
queryPokemon { url, expect } =
    Http.get
        { url = url
        , expect = Http.expectJson expect decodePokemon
        }


searchPokemon : Maybe String -> (String -> msg) -> msg
searchPokemon maybeQuery msg =
    let
        url =
            rootUrl
                ++ (case maybeQuery of
                        Nothing ->
                            ""

                        Just query ->
                            query
                   )
    in
        msg url