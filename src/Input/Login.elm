module Input.Login exposing (..)

import Common.ConnectionUtil exposing (addFeedbackLabel)
import Common.Types exposing (Password, UserName)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (autocomplete, class, for, id, type_)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)


type alias Model =
    { user : UserName
    , password : Password
    , feedback : String
    }


type Msg
    = SetUser UserName
    | SetPassword Password
    | Login


init : Model
init =
    { user = ""
    , password = ""
    , feedback = ""
    }


view : Model -> Html Msg
view md =
    div [ id "initialMain" ]
        [ div [ id "userField" ]
            [ label [ for "user" ] [ text "User name" ]
            , input [ autocomplete True, onInput SetUser, onEnter Login ] []
            ]
        , div [ id "passwordField" ]
            [ label [ for "password" ] [ text "Password" ]
            , input [ type_ "password", autocomplete True, onInput SetPassword, onEnter Login ] []
            ]
        , div [ id "fetchButton" ]
            [ button [ class "button", onClick Login ] [ text "Login" ] ]
        , addFeedbackLabel md.feedback
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                SetUser userName ->
                    { model | user = userName }

                SetPassword password ->
                    { model | password = password }

                Login ->
                    model
    in
    ( newModel, Cmd.none )
