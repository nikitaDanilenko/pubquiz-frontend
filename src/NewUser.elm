module NewUser exposing ( .. )

import Base exposing ( User, Password )

type alias NewUser = {
        user : User,
        password1 : Password,
        password2 : Password
    }

emptyUser : NewUser
emptyUser = {
    user = "",
    password1 = "",
    password2 = ""  
  }

type NewUserField = UserField | PasswordField1 | PasswordField2

update : NewUserField -> String -> NewUser -> NewUser
update fld text nu = 
    case fld of
        UserField -> { nu | user = text }
        PasswordField1 -> { nu | password1 = text }
        PasswordField2 -> { nu | password2 = text }

isValid : NewUser -> Bool
isValid nu = 
    not (String.isEmpty nu.user) && 
    nu.password1 == nu.password2 && 
    not (String.isEmpty nu.password1)