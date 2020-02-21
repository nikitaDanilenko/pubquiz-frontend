module Types exposing (..)

import Date exposing ( Date, toIsoString )
import Json.Encode exposing ( Value, object, int, string )

type alias TeamNumber = {
    teamNumber : Int
    }

teamNumberEncoder : TeamNumber -> Value
teamNumberEncoder tn = object [("teamNumber", int tn.teamNumber)]

type alias QuizName = {
    quizName : String
    }

quizNameEncoder : QuizName -> Value
quizNameEncoder qn = object [("quizName", string qn.quizName)]

type alias Place = {
    place : String
    }

placeEncoder : Place -> Value
placeEncoder p = object [("place", string p.place)]

type alias QuizDate = {
    quizDate : Date
    }

quizDateEncoder : QuizDate -> Value
quizDateEncoder qd = object [("quizDate", string (toIsoString qd.quizDate))]