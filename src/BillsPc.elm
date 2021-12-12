module BillsPc exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Pokeapi exposing (Pokemon)


type alias StoredPokemon =
    { id : Int
    , name : String
    , boxId : Int
    }

type PcStatus 
    = Idle
    | PokemonStored StoredPokemon
    | Error Http.Error

decodeStoredPokemon : Decoder StoredPokemon
decodeStoredPokemon =
    Decode.succeed StoredPokemon
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.required "boxId" Decode.int


storePokemon : Pokemon -> (Result Http.Error StoredPokemon -> msg) -> Cmd msg
storePokemon pokemon expect =
    Http.post
        { url = "/api/save?boxId=1"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "name"
                      , Encode.string pokemon.name
                      )
                    ]
                )
        , expect = Http.expectJson expect decodeStoredPokemon
        }
