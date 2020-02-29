module Input.Model exposing (..)

import Common.Copy exposing (LabelsField)
import Common.QuizRatings as QuizRatings
import Common.Types exposing (Activity(..), DbQuizId, Header, Labels, Password, Place, QuizDate, QuizIdentifier, QuizInfo, QuizName, QuizRatings, QuizSettings, Ratings, RoundNumber, TeamNumber, UserHash, UserName)
import Date exposing (Date)
import Http exposing (Error(..))
import Input.NewUser as NewUser exposing (NewUser, NewUserField)
import Input.Validity as Validity exposing (Validity)



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


initialModelFunction : () -> ( Model, Cmd Msg )
initialModelFunction () =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { user = ""
    , password = ""
    , oneWayHash = ""
    , quizzes = []
    , currentQuizInfo = defaultQuizInfo
    , currentQuizSettings = defaultQuizSettings
    , currentQuizRatings = QuizRatings.empty
    , isValidQuizUpdate = Validity.default
    , displayState = Initial
    , newUser = NewUser.emptyUser
    , feedback = ""
    }


defaultQuizInfo : QuizInfo
defaultQuizInfo =
    { quizId = -1
    , quizIdentifier = defaultQuizIdentifier
    , active = Inactive
    , fullSheetPath = ""
    , qrOnlyPath = ""
    }


defaultQuizIdentifier : QuizIdentifier
defaultQuizIdentifier =
    { place = ""
    , date = "2100-01-01"
    , name = "Quiz"
    }


defaultQuizSettings : QuizSettings
defaultQuizSettings =
    { rounds = defaultRounds
    , numberOfTeams = defaultNumberOfTeams
    , labels = defaultLabels
    }


defaultLabels : Labels
defaultLabels =
    { roundLabel = "Runde"
    , teamLabel = "Gruppe"
    , ownPointsLabel = "Erreichte Punkte"
    , maxReachedLabel = String.concat [ "Erreichte H", String.fromChar (Char.fromCode 246), "chstpunktzahl" ]
    , maxReachableLabel = "Erreichbare Punkte"
    , backToChartView = "Gesamtwertung"
    , ownPageLabel = "Eigene Punkte"
    , viewPrevious = "Alle Quizzes"
    , cumulativeLabel = "Gesamtpunktzahl"
    , individualRoundsLabel = "Punkte pro Runde"
    , progressionLabel = "Verlauf"
    , placementLabel = "Platzierung"
    , placeLabel = "Platz"
    , pointsLabel = "Punkte"
    , roundWinnerLabel = "Rundensieger"
    }


defaultNumberOfTeams : Int
defaultNumberOfTeams =
    8


defaultQuestionNumber : Int
defaultQuestionNumber =
    8


defaultRounds : List Int
defaultRounds =
    List.repeat 4 defaultQuestionNumber


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