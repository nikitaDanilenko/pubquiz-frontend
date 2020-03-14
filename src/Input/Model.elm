module Input.Model exposing (..)

import Common.Copy exposing (LabelsField)
import Common.Types exposing (Activity(..), DbQuizId, Header, Labels, Password, Place, QuizDate, QuizIdentifier, QuizInfo, QuizName, QuizRatings, QuizSettings, Ratings, RoundNumber, TeamNumber, UserHash, UserName)
import Http exposing (Error(..))
import Input.NewUser exposing (NewUser, NewUserField)
import Input.Validity exposing (Validity)



-- todo: split model in various models? This may simplify the handling of default values.


type alias Model =
    { user : UserName
    , password : Password
    , oneWayHash : String
    , quizzes : List QuizInfo
    , currentQuizInfo : QuizInfo
    , currentQuizSettings : QuizSettings
    , currentQuizRatings : QuizRatings
    , isValidQuizUpdate : Validity
    , displayState : DisplayState
    , newUser : NewUser
    , feedback : String
    }















type Edited
    = ContentsE
    | LabelsE



{- The different types of possible states the page can transition. -}


type DisplayState
    = Initial -- The state at the beginning of the application.
    | Editing Edited -- The view where one sees the current quiz values and can update those.
    | Authenticating -- The view presented for the authentication of a user.
    | Selecting -- In this view you see all available quizzes.
    | ConfirmingLock
    | CreatingQuiz
    | CreatingUser


type alias ErrorOr a =
    Result Http.Error a


type TeamUpdateSetting
    = InitialTU
    | IntermediateTU


type ResponseWithFeedback
    = GotAll (ErrorOr (List QuizInfo))
    | GotQuizRatings (ErrorOr QuizRatings)
    | GotLabels (ErrorOr Labels)
    | Logged (ErrorOr UserHash)
    | CreatedQuiz (ErrorOr QuizInfo)


type ResponsePure
    = Updated
    | Locked
    | CreatedUser


type Msg
    = GetAll
    | GetSingle DbQuizId
    | PostUpdate DbQuizId QuizRatings
    | AcknowledgeLock
    | LockQuiz DbQuizId
    | SetUser UserName
    | SetPassword Password
    | SetTeamsInQuiz TeamUpdateSetting String
    | UpdatePoints RoundNumber TeamNumber String -- Points for round, team, value.
    | SetMaxPoints RoundNumber String -- Points for round, value
    | UpdateQuestions Int String
    | AddRound
    | SetRoundsNumber String
    | StartCreatingQuiz
    | StartCreatingUser
    | CreateQuiz
    | CreateUser
    | SetNewQuizName QuizName
    | SetNewQuizDate String
    | SetNewQuizPlace Place
    | SetNewUserParam NewUserField String
    | Login
    | LabelsUpdate LabelsField String
    | SetTeamName TeamNumber String
    | GetLabels
    | PostQuizSettingsUpdate DbQuizId QuizSettings
    | ResponseF ResponseWithFeedback
    | ResponseP ResponsePure (ErrorOr ())



