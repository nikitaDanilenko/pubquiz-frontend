module Pages.Public.Quiz.Page exposing
    ( Flags
    , Hovering
    , Model
    , Msg(..)
    , lenses
    )

import Api.Types exposing (QuizActive)
import Chart.Item as CI
import Http
import Monocle.Lens exposing (Lens)
import Util.Tristate exposing (Tristate)


type alias Hovering =
    List (CI.One { round : Int } CI.Any)


type alias Model =
    { quizId : Int
    , quiz : Tristate Http.Error QuizActive
    , hovering : Hovering
    }


lenses :
    { quizId : Lens Model Int
    , quiz : Lens Model (Tristate Http.Error QuizActive)
    , hovering : Lens Model Hovering
    }
lenses =
    { quizId = Lens .quizId (\b a -> { a | quizId = b })
    , quiz = Lens .quiz (\b a -> { a | quiz = b })
    , hovering = Lens .hovering (\b a -> { a | hovering = b })
    }


type Msg
    = GotQuiz (Result Http.Error QuizActive)
    | OnHover Hovering


type alias Flags =
    { quizId : Int
    , apiBase : String
    }
