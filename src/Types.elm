module Types exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias TeamNumber  = Int

jsonDecTeamNumber : Json.Decode.Decoder ( TeamNumber )
jsonDecTeamNumber =
    Json.Decode.int

jsonEncTeamNumber : TeamNumber -> Value
jsonEncTeamNumber  val = Json.Encode.int val



type alias RoundNumber  = Int

jsonDecRoundNumber : Json.Decode.Decoder ( RoundNumber )
jsonDecRoundNumber =
    Json.Decode.int

jsonEncRoundNumber : RoundNumber -> Value
jsonEncRoundNumber  val = Json.Encode.int val



type alias Code  = String

jsonDecCode : Json.Decode.Decoder ( Code )
jsonDecCode =
    Json.Decode.string

jsonEncCode : Code -> Value
jsonEncCode  val = Json.Encode.string val



type alias TeamName  = String

jsonDecTeamName : Json.Decode.Decoder ( TeamName )
jsonDecTeamName =
    Json.Decode.string

jsonEncTeamName : TeamName -> Value
jsonEncTeamName  val = Json.Encode.string val



type alias QuizName  = String

jsonDecQuizName : Json.Decode.Decoder ( QuizName )
jsonDecQuizName =
    Json.Decode.string

jsonEncQuizName : QuizName -> Value
jsonEncQuizName  val = Json.Encode.string val



type alias Place  = String

jsonDecPlace : Json.Decode.Decoder ( Place )
jsonDecPlace =
    Json.Decode.string

jsonEncPlace : Place -> Value
jsonEncPlace  val = Json.Encode.string val



type alias QuizDate  = Day

jsonDecQuizDate : Json.Decode.Decoder ( QuizDate )
jsonDecQuizDate =
    jsonDecDay

jsonEncQuizDate : QuizDate -> Value
jsonEncQuizDate  val = jsonEncDay val



type alias RoundLabel  = String

jsonDecRoundLabel : Json.Decode.Decoder ( RoundLabel )
jsonDecRoundLabel =
    Json.Decode.string

jsonEncRoundLabel : RoundLabel -> Value
jsonEncRoundLabel  val = Json.Encode.string val



type alias TeamLabel  = String

jsonDecTeamLabel : Json.Decode.Decoder ( TeamLabel )
jsonDecTeamLabel =
    Json.Decode.string

jsonEncTeamLabel : TeamLabel -> Value
jsonEncTeamLabel  val = Json.Encode.string val



type alias OwnPointsLabel  = String

jsonDecOwnPointsLabel : Json.Decode.Decoder ( OwnPointsLabel )
jsonDecOwnPointsLabel =
    Json.Decode.string

jsonEncOwnPointsLabel : OwnPointsLabel -> Value
jsonEncOwnPointsLabel  val = Json.Encode.string val



type alias MaxReachedLabel  = String

jsonDecMaxReachedLabel : Json.Decode.Decoder ( MaxReachedLabel )
jsonDecMaxReachedLabel =
    Json.Decode.string

jsonEncMaxReachedLabel : MaxReachedLabel -> Value
jsonEncMaxReachedLabel  val = Json.Encode.string val



type alias MaxReachableLabel  = String

jsonDecMaxReachableLabel : Json.Decode.Decoder ( MaxReachableLabel )
jsonDecMaxReachableLabel =
    Json.Decode.string

jsonEncMaxReachableLabel : MaxReachableLabel -> Value
jsonEncMaxReachableLabel  val = Json.Encode.string val



type alias BackToChartViewLabel  = String

jsonDecBackToChartViewLabel : Json.Decode.Decoder ( BackToChartViewLabel )
jsonDecBackToChartViewLabel =
    Json.Decode.string

jsonEncBackToChartViewLabel : BackToChartViewLabel -> Value
jsonEncBackToChartViewLabel  val = Json.Encode.string val



type alias MainLabel  = String

jsonDecMainLabel : Json.Decode.Decoder ( MainLabel )
jsonDecMainLabel =
    Json.Decode.string

jsonEncMainLabel : MainLabel -> Value
jsonEncMainLabel  val = Json.Encode.string val



type alias OwnPageLabel  = String

jsonDecOwnPageLabel : Json.Decode.Decoder ( OwnPageLabel )
jsonDecOwnPageLabel =
    Json.Decode.string

jsonEncOwnPageLabel : OwnPageLabel -> Value
jsonEncOwnPageLabel  val = Json.Encode.string val



type alias ViewPreviousLabel  = String

jsonDecViewPreviousLabel : Json.Decode.Decoder ( ViewPreviousLabel )
jsonDecViewPreviousLabel =
    Json.Decode.string

jsonEncViewPreviousLabel : ViewPreviousLabel -> Value
jsonEncViewPreviousLabel  val = Json.Encode.string val



type alias CumulativeLabel  = String

jsonDecCumulativeLabel : Json.Decode.Decoder ( CumulativeLabel )
jsonDecCumulativeLabel =
    Json.Decode.string

jsonEncCumulativeLabel : CumulativeLabel -> Value
jsonEncCumulativeLabel  val = Json.Encode.string val



type alias IndividualRoundsLabel  = String

jsonDecIndividualRoundsLabel : Json.Decode.Decoder ( IndividualRoundsLabel )
jsonDecIndividualRoundsLabel =
    Json.Decode.string

jsonEncIndividualRoundsLabel : IndividualRoundsLabel -> Value
jsonEncIndividualRoundsLabel  val = Json.Encode.string val



type alias ProgressionLabel  = String

jsonDecProgressionLabel : Json.Decode.Decoder ( ProgressionLabel )
jsonDecProgressionLabel =
    Json.Decode.string

jsonEncProgressionLabel : ProgressionLabel -> Value
jsonEncProgressionLabel  val = Json.Encode.string val



type alias PlacementLabel  = String

jsonDecPlacementLabel : Json.Decode.Decoder ( PlacementLabel )
jsonDecPlacementLabel =
    Json.Decode.string

jsonEncPlacementLabel : PlacementLabel -> Value
jsonEncPlacementLabel  val = Json.Encode.string val



type alias PlaceLabel  = String

jsonDecPlaceLabel : Json.Decode.Decoder ( PlaceLabel )
jsonDecPlaceLabel =
    Json.Decode.string

jsonEncPlaceLabel : PlaceLabel -> Value
jsonEncPlaceLabel  val = Json.Encode.string val



type alias PointsLabel  = String

jsonDecPointsLabel : Json.Decode.Decoder ( PointsLabel )
jsonDecPointsLabel =
    Json.Decode.string

jsonEncPointsLabel : PointsLabel -> Value
jsonEncPointsLabel  val = Json.Encode.string val



type alias RoundWinnerLabel  = String

jsonDecRoundWinnerLabel : Json.Decode.Decoder ( RoundWinnerLabel )
jsonDecRoundWinnerLabel =
    Json.Decode.string

jsonEncRoundWinnerLabel : RoundWinnerLabel -> Value
jsonEncRoundWinnerLabel  val = Json.Encode.string val



type alias UserName  = String

jsonDecUserName : Json.Decode.Decoder ( UserName )
jsonDecUserName =
    Json.Decode.string

jsonEncUserName : UserName -> Value
jsonEncUserName  val = Json.Encode.string val



type alias UserSalt  = String

jsonDecUserSalt : Json.Decode.Decoder ( UserSalt )
jsonDecUserSalt =
    Json.Decode.string

jsonEncUserSalt : UserSalt -> Value
jsonEncUserSalt  val = Json.Encode.string val



type alias UserHash  = String

jsonDecUserHash : Json.Decode.Decoder ( UserHash )
jsonDecUserHash =
    Json.Decode.string

jsonEncUserHash : UserHash -> Value
jsonEncUserHash  val = Json.Encode.string val



type alias Day  =
   { year: Int
   , month: Int
   , day: Int
   }

jsonDecDay : Json.Decode.Decoder ( Day )
jsonDecDay =
   Json.Decode.succeed (\pyear pmonth pday -> {year = pyear, month = pmonth, day = pday})
   |> required "year" (Json.Decode.int)
   |> required "month" (Json.Decode.int)
   |> required "day" (Json.Decode.int)

jsonEncDay : Day -> Value
jsonEncDay  val =
   Json.Encode.object
   [ ("year", Json.Encode.int val.year)
   , ("month", Json.Encode.int val.month)
   , ("day", Json.Encode.int val.day)
   ]

