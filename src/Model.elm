module Model exposing ( .. )

import Http exposing ( Error ( .. ) )

type alias Model = 
    {
        user : User,
        password : Password,
        quizzes : List QuizName,
        editing : QuizName,
        currentPoints : String,
        displayState : DisplayState,
        createName : QuizName,
        feedback : String
    }

initialModel : () -> (Model, Cmd Msg)
initialModel () = ({ user = "",
                     password = "", 
                     quizzes = [], 
                     editing = "",
                     currentPoints = "",
                     displayState = Initial, 
                     createName = "",
                     feedback = "" 
                     }, Cmd.none)

{- The different types of possible states the page can transition. -}
type DisplayState = Initial -- The state at the beginning of the application.
                  | Editing -- The view where one sees the current quiz values and can update those.
                  | Authenticating -- The view presented for the authentication of a user.
                  | Selecting -- In this view you see all available quizzes.
                  | ConfirmingLock
                  | Creating

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

type alias User = String 
type alias Password = String

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
         | LocationChange
         | Updated (Result Http.Error ())
         | Locked (Result Http.Error ())
         | StartCreating
         | Create QuizName
         | Created (Result Http.Error ())
         | SetNewQuizName QuizName
         | Login
         | Logged (Result Http.Error String)

errorToString : Http.Error -> String
errorToString err = case err of
     BadUrl url   -> String.concat [ "Bad URL: ", url ]
     Timeout      -> "Timeout"
     NetworkError -> "Network error"
     BadStatus s  -> String.concat [ "Bad status: ", String.fromInt s ]
     BadBody str  -> String.concat [ "Bad body: ", str ]