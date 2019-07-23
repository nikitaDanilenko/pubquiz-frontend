module Model exposing ( .. )

import Http exposing    ( Error ( .. ) )

import Base   exposing  ( User, Password )
import Labels exposing  ( Labels, defaultLabels )
import NewUser exposing ( NewUser, NewUserField )
import Quiz exposing    ( Quiz, empty )

type alias Model = 
    {
        user : User,
        password : Password,
        oneWayHash : String,
        quizzes : List QuizName,
        editing : QuizName,
        currentQuiz : Quiz,
        teamsInQuiz : Int,
        isValidQuizUpdate : Bool,
        numberOfRounds : Int,
        displayState : DisplayState,
        createName : QuizName,
        labels : Labels,
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
                 teamsInQuiz = 8,
                 isValidQuizUpdate = False,
                 numberOfRounds = 4,
                 displayState = Initial, 
                 createName = "",
                 labels = defaultLabels,
                 newUser = NewUser.emptyUser,
                 feedback = "" 
               }

type Edited = ContentsE | LabelsE

{- The different types of possible states the page can transition. -}
type DisplayState = Initial -- The state at the beginning of the application.
                  | Editing Edited -- The view where one sees the current quiz values and can update those.
                  | Authenticating -- The view presented for the authentication of a user.
                  | Selecting -- In this view you see all available quizzes.
                  | ConfirmingLock
                  | CreatingQuiz
                  | CreatingUser

type alias QuizName = String

type alias ErrorOr a = Result Http.Error a

type TeamUpdateSetting = InitialTU | IntermediateTU

type Msg = GetAll 
         | GotAll (ErrorOr String)
         | GetSingle QuizName
         | GotSingle (ErrorOr String)
         | PostUpdate QuizName String
         | AcknowledgeLock
         | Lock QuizName
         | SetUser User
         | SetPassword Password
         | SetPoints String String
         | SetTeamsInQuiz TeamUpdateSetting String
         | UpdatePoints Int Int String -- Points for round, team, value.
         | SetMaxPoints Int String    -- Points for round, value
         | AddRound
         | SetRoundsNumber String
         | LocationChange
         | Updated (ErrorOr ())
         | Locked (ErrorOr ())
         | StartCreatingQuiz
         | StartCreatingUser
         | CreateQuiz
         | Created (ErrorOr ())
         | CreateUser
         | CreatedUser (ErrorOr ())
         | SetNewQuizName QuizName
         | SetNewUserParam NewUserField String
         | Login
         | Logged (ErrorOr String)
         | LabelsUpdate LabelsField String
         | SetTeamName Int String
         | GetLabels
         | GotLabels (ErrorOr String)
         | PostLabelUpdate QuizName Labels

type LabelsField = RoundField
                 | TeamField
                 | OwnPointsField
                 | MaxReachedField
                 | MaxReachableField
                 | BackField
                 | MainField
                 | OwnPageField
                 | ViewQuizzesField
                 | CumulativeField
                 | IndividualField
                 | ProgressionField
                 | PlacementField
                 | PlaceField
                 | PointsField
                 | RoundWinnerField

errorToString : Http.Error -> String
errorToString err = case err of
     BadUrl url   -> String.concat [ "Bad URL: ", url ]
     Timeout      -> "Timeout"
     NetworkError -> "Network error"
     BadStatus s  -> String.concat [ "Bad status: ", String.fromInt s ]
     BadBody str  -> String.concat [ "Bad body: ", str ]