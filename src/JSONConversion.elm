module JSONConversion exposing (..)

import Constants             exposing (credentialsParam, dataParam)
import Crypto.Hash           exposing (sha512)
import Json.Encode as Encode exposing (encode, object)
import Types                 exposing (jsonEncCredentials)

type alias User = String
type alias SessionKey = String

encodeWithSignature : User -> SessionKey -> List (String, Encode.Value) -> Encode.Value
encodeWithSignature u sk params =
    let data = object params
        signature = sha512 (String.concat [sk, encode 0 data])
        credentials = { user = u, signature = signature }
    in object [(dataParam, data), (credentialsParam, jsonEncCredentials credentials)]

