module Common.WireUtil exposing (..)

import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings)
import Common.Util as Util exposing (getMsg)
import Html exposing (Attribute, Html, div, form, input, label, text)
import Html.Attributes exposing (action, for, id, type_)
import Http exposing (Error(..))
import Input.Model exposing (ErrorOr)
import Json.Encode as Encode exposing (encode)
import Url.Builder


type alias User =
    String


type alias SessionKey =
    String


type alias RestParam =
    String


type alias RestValue =
    String


type alias RestKey =
    String


mkJSONParams : List ( String, Encode.Value ) -> RestParam
mkJSONParams ps =
    ps |> List.map (\( k, v ) -> ( k, encode 0 v )) |> mkParams


mkParams : List ( RestKey, RestValue ) -> RestParam
mkParams kvs =
    let
        done =
            Url.Builder.relative [] (List.map (\( k, v ) -> Url.Builder.string k v) kvs)
    in
    String.dropLeft 1 done


getLabelsWith : (ErrorOr Labels -> msg) -> DbQuizId -> Cmd msg
getLabelsWith f =
    getMsg getLabelsApi f jsonDecLabels


getQuizInfoWith : (ErrorOr QuizInfo -> msg) -> DbQuizId -> Cmd msg
getQuizInfoWith f =
    getMsg getQuizInfoApi f jsonDecQuizInfo


getQuizRatingsWith : (ErrorOr QuizRatings -> msg) -> DbQuizId -> Cmd msg
getQuizRatingsWith f =
    getMsg getQuizRatingsApi f jsonDecQuizRatings


useOrFetchWith : (DbQuizId -> Cmd msg) -> Maybe a -> DbQuizId -> Cmd msg
useOrFetchWith dft mA qid =
    Util.foldMaybe (dft qid) (always Cmd.none) mA


encodeBody : String -> Http.Body
encodeBody =
    Http.stringBody "application/x-www-form-urlencoded"


linkButton : String -> List (Attribute msg) -> List (Html msg) -> Html msg
linkButton link attrs children =
    form [ action link ]
        [ input (type_ "submit" :: attrs) children ]


addFeedbackLabel : String -> Html msg
addFeedbackLabel feedback =
    div [ id "feedbackArea" ]
        [ label [ for "feedbackLabel" ] [ text feedback ] ]


errorToString : Http.Error -> String
errorToString err =
    case err of
        BadUrl url ->
            String.concat [ "Bad URL: ", url ]

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadStatus s ->
            String.concat [ "Bad status: ", String.fromInt s ]

        BadBody str ->
            String.concat [ "Bad body: ", str ]
