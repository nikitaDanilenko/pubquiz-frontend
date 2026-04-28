module Input.NewUser exposing (..)

import Common.Types exposing (Password, UserName)


type alias NewUser =
    { user : UserName
    , password1 : Password
    , password2 : Password
    }


empty : NewUser
empty =
    { user = ""
    , password1 = ""
    , password2 = ""
    }


type NewUserField
    = UserField
    | PasswordField1
    | PasswordField2


update : NewUser -> NewUserField -> String -> NewUser
update newUser fld text =
    case fld of
        UserField ->
            { newUser | user = text }

        PasswordField1 ->
            { newUser | password1 = text }

        PasswordField2 ->
            { newUser | password2 = text }


isValid : NewUser -> Bool
isValid newUser =
    not (String.isEmpty newUser.user)
        && newUser.password1
        == newUser.password2
        && not (String.isEmpty newUser.password1)
