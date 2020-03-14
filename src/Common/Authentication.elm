module Common.Authentication exposing (..)

import Common.WireUtil exposing (RestParam, SessionKey, mkJSONParams)
import Common.Constants exposing (credentialsParam)
import Common.Types exposing (UserName, jsonEncCredentials)
import Crypto.Hash exposing (sha512)
import Json.Encode as Encode


type alias Authentication =
    { userName : UserName
    , sessionKey : SessionKey
    }


empty : Authentication
empty =
    { userName = ""
    , sessionKey = ""
    }


updateUserName : Authentication -> UserName -> Authentication
updateUserName authentication userName =
    { authentication | userName = userName }


updateSessionKey : Authentication -> SessionKey -> Authentication
updateSessionKey authentication sessionKey =
    { authentication | sessionKey = sessionKey }

{-| Takes the list of all parameters and adds a credentials parameter,
where the user is the supplied user and the signature is computed from
the given session key and the encoded values (in this order).
-}
encodeWithSignature : Authentication -> List ( String, Encode.Value ) -> RestParam
encodeWithSignature authentication params =
    let
        signature =
            sha512 (String.concat [ authentication.sessionKey, mkJSONParams params ])

        credentials =
            { user = authentication.userName, signature = signature }
    in
    mkJSONParams (( credentialsParam, jsonEncCredentials credentials ) :: params)