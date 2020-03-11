module Common.ConnectionUtil exposing (..)

import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings)
import Common.Util as Util exposing (getMsg)
import Html exposing (Attribute, Html, div, form, input, label, text)
import Html.Attributes exposing (action, for, id, type_)
import Input.Model exposing (ErrorOr)


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


linkButton : String -> List (Attribute msg) -> List (Html msg) -> Html msg
linkButton link attrs children =
    form [ action link ]
        [ input (type_ "submit" :: attrs) children ]

addFeedbackLabel : String -> Html msg
addFeedbackLabel feedback =
    div [ id "feedbackArea" ]
        [ label [ for "feedbackLabel" ] [ text feedback ] ]