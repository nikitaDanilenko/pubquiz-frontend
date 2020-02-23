module Model exposing ( .. )

import Copy exposing (LabelsField)
import Http exposing     ( Error ( .. ) )

import NewUser exposing  ( NewUser, NewUserField )
import Quiz exposing     ( Quiz )
import Types exposing (Labels, Password, Place, QuizDate, QuizName, QuizPDN, QuizSettings, UserName)
import Validity exposing ( Validity )

type alias Model = 
    {
        user : UserName,
        password : Password,
        oneWayHash : String,
        quizzes : List QuizName,
        editing : QuizName,
        currentQuiz : Quiz,
        isValidQuizUpdate : Validity,
        displayState : DisplayState,
        createQuizPDN : QuizPDN,
        quizSettings : QuizSettings,
        newUser : NewUser,
        feedback : String
    }

initialModelFunction : () -> (Model, Cmd Msg)
initialModelFunction () = (initialModel, Cmd.none)

initialModel : Model
initialModel = { user = "",
                 password = "",
                 oneWayHash = "",
                 quizzes = [], 
                 editing = "",
                 currentQuiz = Quiz.empty,
                 isValidQuizUpdate = Validity.default,
                 displayState = Initial,
                 createQuizPDN = defaultQuizPDN,
                 quizSettings = defaultQuizSettings,
                 newUser = NewUser.emptyUser,
                 feedback = "" 
               }

defaultQuizPDN : QuizPDN
defaultQuizPDN = {
    place = "",
    date = { year = 2100, month = 1, day = 1 },
    name = defaultLabels.mainLabel
  }

defaultQuizSettings : QuizSettings
defaultQuizSettings = {
    rounds = defaultRounds,
    numberOfTeams = defaultNumberOfTeams,
    labels = defaultLabels
  }

defaultLabels : Labels
defaultLabels = {
    roundLabel = "Runde",
    teamLabel = "Gruppe",
    ownPointsLabel = "Erreichte Punkte",
    maxReachedLabel = (String.concat ["Erreichte H", String.fromChar (Char.fromCode 246), "chstpunktzahl"]),
    maxReachableLabel = "Erreichbare Punkte",
    backToChartView = "Gesamtwertung",
    mainLabel = "Quiz",
    ownPageLabel = "Eigene Punkte",
    viewPrevious = "Alle Quizzes",
    cumulativeLabel = "Gesamtpunktzahl",
    individualRoundsLabel = "Punkte pro Runde",
    progressionLabel = "Verlauf",
    placementLabel = "Platzierung",
    placeLabel = "Platz",
    pointsLabel = "Punkte",
    roundWinnerLabel = "Rundensieger"
  }

defaultNumberOfTeams : Int
defaultNumberOfTeams = 8

defaultQuestionNumber : Int
defaultQuestionNumber = 8

defaultRounds : List Int
defaultRounds = List.repeat 4 defaultQuestionNumber

type Edited = ContentsE | LabelsE

{- The different types of possible states the page can transition. -}
type DisplayState = Initial -- The state at the beginning of the application.
                  | Editing Edited -- The view where one sees the current quiz values and can update those.
                  | Authenticating -- The view presented for the authentication of a user.
                  | Selecting -- In this view you see all available quizzes.
                  | ConfirmingLock
                  | CreatingQuiz
                  | CreatingUser

type alias ErrorOr a = Result Http.Error a

type TeamUpdateSetting = InitialTU | IntermediateTU

type ResponseWithFeedback = GotAll
                          | GotSingleQuiz
                          | GotSingleLabels
                          | Logged

type ResponsePure = Updated
                  | Locked
                  | CreatedQuiz
                  | CreatedUser


type Msg = GetAll 
         | GetSingle QuizName
         | PostUpdate QuizName String
         | AcknowledgeLock
         | Lock QuizName
         | SetUser UserName
         | SetPassword Password
         | SetTeamsInQuiz TeamUpdateSetting String
         | UpdatePoints Int Int String -- Points for round, team, value.
         | SetMaxPoints Int String    -- Points for round, value
         | UpdateQuestions Int String
         | AddRound
         | SetRoundsNumber String
         | StartCreatingQuiz
         | StartCreatingUser
         | CreateQuiz
         | CreateUser
         | SetNewQuizName QuizName
         | SetNewUserParam NewUserField String
         | Login
         | LabelsUpdate LabelsField String
         | SetTeamName Int String
         | GetLabels
         | PostQuizSettingsUpdate QuizName (List Int) Int Labels
         | ResponseF ResponseWithFeedback (ErrorOr String)
         | ResponseP ResponsePure (ErrorOr ())



errorToString : Http.Error -> String
errorToString err = case err of
     BadUrl url   -> String.concat [ "Bad URL: ", url ]
     Timeout      -> "Timeout"
     NetworkError -> "Network error"
     BadStatus s  -> String.concat [ "Bad status: ", String.fromInt s ]
     BadBody str  -> String.concat [ "Bad body: ", str ]