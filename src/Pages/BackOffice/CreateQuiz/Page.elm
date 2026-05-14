module Pages.BackOffice.CreateQuiz.Page exposing
    ( Model
    , Msg(..)
    , lenses
    )

import Api.Types exposing (Quiz)
import Monocle.Lens exposing (Lens)
import OpenApi.Common


type alias Model =
    { name : String
    , date : String
    , place : String
    , numberOfRounds : Int
    , questionsPerRound : List Int
    , numberOfTeams : Int
    , isSubmitting : Bool
    , error : Maybe String
    }


lenses :
    { name : Lens Model String
    , date : Lens Model String
    , place : Lens Model String
    , numberOfRounds : Lens Model Int
    , questionsPerRound : Lens Model (List Int)
    , numberOfTeams : Lens Model Int
    , isSubmitting : Lens Model Bool
    , error : Lens Model (Maybe String)
    }
lenses =
    { name = Lens .name (\b a -> { a | name = b })
    , date = Lens .date (\b a -> { a | date = b })
    , place = Lens .place (\b a -> { a | place = b })
    , numberOfRounds = Lens .numberOfRounds (\b a -> { a | numberOfRounds = b })
    , questionsPerRound = Lens .questionsPerRound (\b a -> { a | questionsPerRound = b })
    , numberOfTeams = Lens .numberOfTeams (\b a -> { a | numberOfTeams = b })
    , isSubmitting = Lens .isSubmitting (\b a -> { a | isSubmitting = b })
    , error = Lens .error (\b a -> { a | error = b })
    }


type Msg
    = SetName String
    | SetDate String
    | SetPlace String
    | SetNumberOfRounds String
    | SetQuestionsForRound Int String
    | SetNumberOfTeams String
    | Submit
    | GotCreateResponse (Result (OpenApi.Common.Error () String) Quiz)
