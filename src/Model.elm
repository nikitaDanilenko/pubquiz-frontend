module Model exposing ( .. )

import Http exposing    ( Error ( .. ) )

import Base   exposing  ( User, Password )
import Labels exposing  ( Labels, defaultLabels )
import NewUser exposing ( NewUser, NewUserField )

type alias Model = 
    {
        user : User,
        password : Password,
        oneWayHash : String,
        quizzes : List QuizName,
        editing : QuizName,
        currentPoints : String,
        numberOfRounds : String,
        displayState : DisplayState,
        createName : QuizName,
        labels : Labels,
        newUser : NewUser,
        feedback : String
    }

initialModel : () -> (Model, Cmd Msg)
initialModel () = ({ user = "",
                     password = "",
                     oneWayHash = "", 
                     quizzes = [], 
                     editing = "",
                     currentPoints = "",
                     numberOfRounds = "",
                     displayState = Initial, 
                     createName = "",
                     labels = defaultLabels,
                     newUser = NewUser.emptyUser,
                     feedback = "" 
                     }, Cmd.none)

{- The different types of possible states the page can transition. -}
type DisplayState = Initial -- The state at the beginning of the application.
                  | Editing -- The view where one sees the current quiz values and can update those.
                  | Authenticating -- The view presented for the authentication of a user.
                  | Selecting -- In this view you see all available quizzes.
                  | ConfirmingLock
                  | CreatingQuiz
                  | CreatingUser

type alias QuizName = String

type alias Quiz = 
    {
        name : String,
        rounds : List Round
    }

type alias Round = 
    {
        maxPoints : Float,
        teamPoints : List Float
    }

type Msg = GetAll 
         | GotAll (Result Http.Error String)
         | GetSingle QuizName
         | GotSingle (Result Http.Error String)
         | PostUpdate QuizName String
         | AcknowledgeLock
         | Lock QuizName
         | SetUser User
         | SetPassword Password
         | SetPoints String
         | SetRoundsNumber String
         | LocationChange
         | Updated (Result Http.Error ())
         | Locked (Result Http.Error ())
         | StartCreatingQuiz
         | StartCreatingUser
         | CreateQuiz
         | Created (Result Http.Error ())
         | CreateUser
         | CreatedUser (Result Http.Error ())
         | SetNewQuizName QuizName
         | SetNewUserParam NewUserField String
         | Login
         | Logged (Result Http.Error String)
         | LabelsUpdate LabelsField String

type LabelsField = RoundField
                 | GroupField
                 | OwnPointsField
                 | MaxReachedField
                 | MaxReachableField
                 | BackField
                 | MainField
                 | OwnPageField

errorToString : Http.Error -> String
errorToString err = case err of
     BadUrl url   -> String.concat [ "Bad URL: ", url ]
     Timeout      -> "Timeout"
     NetworkError -> "Network error"
     BadStatus s  -> String.concat [ "Bad status: ", String.fromInt s ]
     BadBody str  -> String.concat [ "Bad body: ", str ]