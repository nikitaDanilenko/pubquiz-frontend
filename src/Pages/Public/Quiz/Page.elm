module Pages.Public.Quiz.Page exposing
    ( Flags
    , Model
    , Msg(..)
    , lenses
    )

import Api.Types exposing (QuizActive)
import Http
import Monocle.Lens exposing (Lens)
import Util.Tristate exposing (Tristate)


type alias Model =
    { quizId : Int
    , quiz : Tristate Http.Error QuizActive
    }


lenses :
    { quizId : Lens Model Int
    , quiz : Lens Model (Tristate Http.Error QuizActive)
    }
lenses =
    { quizId = Lens .quizId (\b a -> { a | quizId = b })
    , quiz = Lens .quiz (\b a -> { a | quiz = b })
    }


type Msg
    = GotQuiz (Result Http.Error QuizActive)


type alias Flags =
    { quizId : Int
    , apiBase : String
    }
