module Input.NewUser exposing ( .. )

import Common.Types exposing (Password, UserName)

type alias NewUser = {
        user : UserName,
        password1 : Password,
        password2 : Password
    }

empty : NewUser
empty = {
    user = "",
    password1 = "",
    password2 = ""  
  }

type NewUserField = UserField | PasswordField1 | PasswordField2

update : NewUser -> NewUserField -> String -> NewUser
update nu fld text =
    case fld of
        UserField -> { nu | user = text }
        PasswordField1 -> { nu | password1 = text }
        PasswordField2 -> { nu | password2 = text }

isValid : NewUser -> Bool
isValid nu = 
    not (String.isEmpty nu.user) && 
    nu.password1 == nu.password2 && 
    not (String.isEmpty nu.password1)