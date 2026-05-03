module Pages.BackOffice.Login.Page exposing
    ( Model
    , Msg(..)
    , lenses
    )

{-| Back office login page types.
-}

import Api.Types exposing (LoginResponse)
import Monocle.Lens exposing (Lens)
import OpenApi.Common


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    , isSubmitting : Bool
    }


lenses :
    { username : Lens Model String
    , password : Lens Model String
    , error : Lens Model (Maybe String)
    , isSubmitting : Lens Model Bool
    }
lenses =
    { username = Lens .username (\b a -> { a | username = b })
    , password = Lens .password (\b a -> { a | password = b })
    , error = Lens .error (\b a -> { a | error = b })
    , isSubmitting = Lens .isSubmitting (\b a -> { a | isSubmitting = b })
    }


type Msg
    = SetUsername String
    | SetPassword String
    | Submit
    | GotLoginResponse (Result (OpenApi.Common.Error () String) LoginResponse)
