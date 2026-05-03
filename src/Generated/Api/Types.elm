module Api.Types exposing
    ( AddTeamsCommand, ChangeSettingsCommand, CorrectScoreCommand, Day, LoginRequest, QuizActive, QuizIdentifier
    , QuizMetaData, QuizSettings, QuizSummary, RecordRoundScoresCommand, RenameTeamCommand, Round, ScoreBoard
    , ScoreEntry, SetTeamActiveCommand, Team, TeamScore
    , BackofficeQuizIdAddTeams_Error(..), BackofficeQuizIdChangeSettings_Error(..), BackofficeQuizIdCorrectScore_Error(..)
    , BackofficeQuizIdRecordRoundScores_Error(..), BackofficeQuizIdRenameTeam_Error(..)
    , BackofficeQuizIdSetTeamActive_Error(..)
    )

{-|


## Aliases

@docs AddTeamsCommand, ChangeSettingsCommand, CorrectScoreCommand, Day, LoginRequest, QuizActive, QuizIdentifier
@docs QuizMetaData, QuizSettings, QuizSummary, RecordRoundScoresCommand, RenameTeamCommand, Round, ScoreBoard
@docs ScoreEntry, SetTeamActiveCommand, Team, TeamScore


## Errors

@docs BackofficeQuizIdAddTeams_Error, BackofficeQuizIdChangeSettings_Error, BackofficeQuizIdCorrectScore_Error
@docs BackofficeQuizIdRecordRoundScores_Error, BackofficeQuizIdRenameTeam_Error
@docs BackofficeQuizIdSetTeamActive_Error

-}

import Date


type BackofficeQuizIdAddTeams_Error
    = BackofficeQuizIdAddTeams_400 ()
    | BackofficeQuizIdAddTeams_404 ()


type BackofficeQuizIdChangeSettings_Error
    = BackofficeQuizIdChangeSettings_400 ()
    | BackofficeQuizIdChangeSettings_404 ()


type BackofficeQuizIdCorrectScore_Error
    = BackofficeQuizIdCorrectScore_400 ()
    | BackofficeQuizIdCorrectScore_404 ()


type BackofficeQuizIdRecordRoundScores_Error
    = BackofficeQuizIdRecordRoundScores_400 ()
    | BackofficeQuizIdRecordRoundScores_404 ()


type BackofficeQuizIdRenameTeam_Error
    = BackofficeQuizIdRenameTeam_400 ()
    | BackofficeQuizIdRenameTeam_404 ()


type BackofficeQuizIdSetTeamActive_Error
    = BackofficeQuizIdSetTeamActive_400 ()
    | BackofficeQuizIdSetTeamActive_404 ()


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
