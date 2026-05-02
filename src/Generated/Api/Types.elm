module Api.Types exposing
    ( AddTeamsCommand, ChangeSettingsCommand, CorrectScoreCommand, Day, LoginRequest, LoginResponse, QuizActive
    , QuizIdentifier, QuizMetaData, QuizSettings, QuizSummary, RecordRoundScoresCommand, RenameTeamCommand
    , Round, ScoreBoard, ScoreEntry, SetTeamActiveCommand, Team, TeamScore
    )

{-|


## Aliases

@docs AddTeamsCommand, ChangeSettingsCommand, CorrectScoreCommand, Day, LoginRequest, LoginResponse, QuizActive
@docs QuizIdentifier, QuizMetaData, QuizSettings, QuizSummary, RecordRoundScoresCommand, RenameTeamCommand
@docs Round, ScoreBoard, ScoreEntry, SetTeamActiveCommand, Team, TeamScore

-}

import Date


type alias TeamScore =
    { points : Float, teamNumber : Int }


type alias Team =
    { active : Bool, name : String, number : Int }


type alias SetTeamActiveCommand =
    { active : Bool, teamNumber : Int }


type alias ScoreEntry =
    { points : Float, roundNumber : Int, teamNumber : Int }


type alias ScoreBoard =
    { scores : List ScoreEntry, teams : List Team }


type alias Round =
    { displayMaxPoints : Float, number : Int, numberOfQuestions : Int }


type alias RenameTeamCommand =
    { newName : String, teamNumber : Int }


type alias RecordRoundScoresCommand =
    { roundNumber : Int, scores : List TeamScore }


type alias QuizActive =
    { identifier : QuizIdentifier
    , quizId : Int
    , rounds : List Round
    , scoreBoard : ScoreBoard
    }


type alias QuizSummary =
    { active : Bool, identifier : QuizIdentifier, quizId : Int }


type alias QuizSettings =
    { numberOfTeams : Int, questionsPerRound : List Int }


type alias QuizMetaData =
    { identifier : QuizIdentifier, settings : QuizSettings }


type alias QuizIdentifier =
    { date : Day, name : String, place : String }


type alias LoginResponse =
    { token : String }


type alias LoginRequest =
    { password : String, username : String }


type alias Day =
    Date.Date


type alias CorrectScoreCommand =
    { points : Float, roundNumber : Int, teamNumber : Int }


type alias ChangeSettingsCommand =
    { newIdentifier : QuizIdentifier }


type alias AddTeamsCommand =
    { additionalTeams : Int }
