module Common.Authentication exposing (..)

import Common.Types exposing (UserName)
import Input.RequestUtils exposing (SessionKey)


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
