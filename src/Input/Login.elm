module Input.Login exposing (..)

import Common.ConnectionUtil exposing (addFeedbackLabel, encodeBody)
import Common.Constants exposing (loginApi, passwordParam, userParam)
import Common.Types exposing (Password, UserName, jsonDecUserHash, jsonEncPassword, jsonEncUserName)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (autocomplete, class, for, id, type_)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Http
import Input.Model exposing (ErrorOr)
import Input.RequestUtils exposing (SessionKey, mkJSONParams)


type alias Model =
    { user : UserName
    , password : Password
    , feedback : String
    }


type Msg
    = SetUser UserName
    | SetPassword Password
    | Login
    | LoggedIn (ErrorOr SessionKey)


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
    case msg of
        SetUser userName ->
            ( { model | user = userName }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            ( model, login model.user model.password )

        LoggedIn _ ->
            ( model, Cmd.none )


login : UserName -> Password -> Cmd Msg
login user password =
    Http.post
        { url = loginApi
        , expect = Http.expectJson LoggedIn jsonDecUserHash
        , body = encodeBody (mkJSONParams [ ( userParam, jsonEncUserName user ), ( passwordParam, jsonEncPassword password ) ])
        }
