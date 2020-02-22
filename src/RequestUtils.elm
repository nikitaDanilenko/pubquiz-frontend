module RequestUtils exposing (..)

import Constants             exposing (credentialsParam)
import Crypto.Hash           exposing (sha512)
import Json.Encode as Encode exposing (encode, object)
import Types                 exposing (jsonEncCredentials)
import Url.Builder

type alias User = String
type alias SessionKey = String
type alias RestParam = String
type alias RestValue = String
type alias RestKey = String

-- | Takes the list of all parameters and adds a credentials parameter,
--   where the user is the supplied user and the signature is computed from
--   the given session key and the encoded values (in this order).
encodeWithSignature : User -> SessionKey -> List (String, Encode.Value) -> RestParam
encodeWithSignature u sk params =
    let signature = sha512 (String.concat [sk, mkJSONParams params])
        credentials = { user = u, signature = signature }
    in mkJSONParams ((credentialsParam, jsonEncCredentials credentials) :: params)

mkJSONParams : List (String, Encode.Value) -> RestParam
mkJSONParams ps = ps |> List.map (\(k, v) -> (k, encode 0 v)) |> mkParams

mkParams : List (RestKey, RestValue) -> RestParam
mkParams kvs =
    let done = Url.Builder.relative [] (List.map (\(k, v) -> Url.Builder.string k v) kvs)
    in String.dropLeft 1 done