module Common.Authentication exposing (..)

import Base64
import Common.Types exposing (Credentials, UserName)
import Common.WireUtil exposing (RestParam, SessionKey)
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
mkCredentials : Authentication -> Encode.Value -> Credentials
mkCredentials authentication value =
    { user = authentication.userName, signature = sha512 (Base64.encode (Encode.encode 0 value)) }
