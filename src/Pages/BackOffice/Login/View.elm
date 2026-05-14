module Pages.BackOffice.Login.View exposing (view)

import Html exposing (Html, button, form, h1, input, label, p, section, text)
import Html.Attributes exposing (class, disabled, for, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Maybe.Extra
import Pages.BackOffice.Login.Page as Page


view : Page.Model -> Html Page.Msg
view model =
    section [ class "login" ]
        [ h1 [] [ text "Back Office Login" ]
        , viewForm model
        , viewError model.error
        ]


viewForm : Page.Model -> Html Page.Msg
viewForm model =
    let
        isDisabled =
            model.isSubmitting || String.isEmpty model.username || String.isEmpty model.password
    in
    form [ class "login-form", onSubmit Page.Submit ]
        [ label [ for "username" ] [ text "Username" ]
        , input [ type_ "text", id "username", placeholder "Username", value model.username, onInput Page.SetUsername, disabled model.isSubmitting ] []
        , label [ for "password" ] [ text "Password" ]
        , input [ type_ "password", id "password", placeholder "Password", value model.password, onInput Page.SetPassword, disabled model.isSubmitting ] []
        , button [ type_ "submit", class "login-button", disabled isDisabled ]
            [ text (buttonText model) ]
        ]


buttonText : Page.Model -> String
buttonText model =
    if model.isSubmitting then
        "Logging in..."

    else
        "Login"


viewError : Maybe String -> Html msg
viewError =
    Maybe.Extra.unwrap (text "") (\error -> p [ class "login-error" ] [ text error ])
