module Common.ConnectionUtil exposing (..)

import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings)
import Common.Util as Util exposing (getMsg)
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