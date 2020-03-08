module Common.ConnectionUtil exposing (..)

import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings)
import Common.Util exposing (getMsg)
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
