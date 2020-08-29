module Input.ConfirmLock exposing (..)

import Common.Authentication exposing (Authentication, encodeWithSignature)
import Common.Constants exposing (actionParam, lockApi, quizIdParam)
import Common.HttpUtil as HttpUtil
import Common.Types exposing (Action(..), DbQuizId, QuizInfo, jsonEncAction, jsonEncDbQuizId)
import Common.Util as Util exposing (ErrorOr)
import Common.WireUtil exposing (encodeBody, errorToString)
import Html exposing (Html, button, div, label, text)
import Html.Attributes exposing (class, for, id)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { quizInfo : QuizInfo
    , authentication : Authentication
    , feedback : String
    }


updateFeedback : Model -> String -> Model
updateFeedback model feedback =
    { model | feedback = feedback }


type Msg
    = Back
    | Lock
    | Locked (ErrorOr ())


init : QuizInfo -> Authentication -> ( Model, Cmd Msg )
init quizInfo authentication =
    ( { quizInfo = quizInfo, authentication = authentication, feedback = "" }, Cmd.none )


view : Model -> Html Msg
view md =
    div [ id "confirmView" ]
        [ label [ for "lockWarning" ]
            [ text
                (String.concat
                    [ "You are about to lock "
                    , md.quizInfo.quizIdentifier.name
                    , ". "
                    , "This cannot be undone. Please confirm."
                    ]
                )
            ]
        , button [ class "backButton", onClick Back ]
            [ text "Back" ]
        , button [ class "lockButton", onClick Lock ]
            [ text "Yes, lock" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Back ->
            ( model, Cmd.none )

        Lock ->
            ( model, postLock model.authentication model.quizInfo.quizId )

        Locked response ->
            ( Util.foldResultWith (errorToString >> updateFeedback model) (always model) response, Cmd.none )


postLock : Authentication -> DbQuizId -> Cmd Msg
postLock authentication qid =
    let
        params =
            encodeWithSignature authentication [ ( quizIdParam, jsonEncDbQuizId qid ), ( actionParam, jsonEncAction LockA ) ]
    in
    Http.post
        { url = lockApi
        , body = encodeBody params
        , expect = HttpUtil.expectWhatever Locked
        }
