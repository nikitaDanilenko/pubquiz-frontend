module JSONConversion exposing (..)

import Constants             exposing (credentialsParam)
import Crypto.Hash           exposing (sha512)
import Json.Encode as Encode exposing (encode, object)
import Types                 exposing (jsonEncCredentials)

type alias User = String
type alias SessionKey = String

-- | Takes the list of all parameters and adds a credentials parameter,
--   where the user is the supplied user and the signature is computed from
--   the given session key and the encoded values (in this order).
encodeWithSignature : User -> SessionKey -> List (String, Encode.Value) -> Encode.Value
encodeWithSignature u sk params =
    let signature = sha512 (String.concat [sk, encode 0 (object params)])
        credentials = { user = u, signature = signature }
    in object ((credentialsParam, jsonEncCredentials credentials) :: params)

