module Input.Login exposing (..)

import Common.Constants exposing (loginApi, passwordParam, userParam)
import Common.HttpUtil as HttpUtil
import Common.Types exposing (Password, UserName, jsonDecUserHash, jsonEncPassword, jsonEncUserName)
import Common.Util exposing (ErrorOr)
import Common.WireUtil exposing (SessionKey, addFeedbackLabel, encodeBody, mkJSONParams)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (autocomplete, class, for, id, type_)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Http


type alias Model =
    { user : UserName
    , password : Password
    , feedback : String
    }


updateUser : Model -> UserName -> Model
updateUser model userName =
    { model | user = userName }


updatePassword : Model -> Password -> Model
updatePassword model password =
    { model | password = password }


updateFeedback : Model -> String -> Model
updateFeedback model feedback =
    { model | feedback = feedback }


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
            ( updateUser model userName, Cmd.none )

        SetPassword password ->
            ( updatePassword model password, Cmd.none )

        Login ->
            ( model, login model.user model.password )

        LoggedIn _ ->
            ( model, Cmd.none )


login : UserName -> Password -> Cmd Msg
login user password =
    Http.post
        { url = loginApi
        , expect = HttpUtil.expectJson LoggedIn jsonDecUserHash
        , body = encodeBody (mkJSONParams [ ( userParam, jsonEncUserName user ), ( passwordParam, jsonEncPassword password ) ])
        }
