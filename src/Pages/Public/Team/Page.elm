module Pages.Public.Team.Page exposing
    ( Flags
    , Model
    , Msg(..)
    , lenses
    )

{-| Team detail page state.
-}

import Api.Types exposing (QuizActive)
import Http
import Monocle.Lens exposing (Lens)
import Util.Tristate exposing (Tristate)


type alias Model =
    { quizId : Int
    , teamNumber : Int
    , quiz : Tristate Http.Error QuizActive
    }


lenses :
    { quizId : Lens Model Int
    , teamNumber : Lens Model Int
    , quiz : Lens Model (Tristate Http.Error QuizActive)
    }
lenses =
    { quizId = Lens .quizId (\b a -> { a | quizId = b })
    , teamNumber = Lens .teamNumber (\b a -> { a | teamNumber = b })
    , quiz = Lens .quiz (\b a -> { a | quiz = b })
    }


type Msg
    = GotQuiz (Result Http.Error QuizActive)


type alias Flags =
    { quizId : Int
    , teamNumber : Int
    , apiBase : String
    }
