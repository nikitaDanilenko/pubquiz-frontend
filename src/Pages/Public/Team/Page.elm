module Pages.Public.Team.Page exposing
    ( Flags
    , Model
    , Msg(..)
    , lenses
    )

import Api.Types exposing (Quiz)
import Monocle.Lens exposing (Lens)
import OpenApi.Common
import Util.Tristate exposing (Tristate)


type alias Model =
    { quizId : Int
    , teamNumber : Int
    , quiz : Tristate (OpenApi.Common.Error () String) Quiz
    }


lenses :
    { quizId : Lens Model Int
    , teamNumber : Lens Model Int
    , quiz : Lens Model (Tristate (OpenApi.Common.Error () String) Quiz)
    }
lenses =
    { quizId = Lens .quizId (\b a -> { a | quizId = b })
    , teamNumber = Lens .teamNumber (\b a -> { a | teamNumber = b })
    , quiz = Lens .quiz (\b a -> { a | quiz = b })
    }


type Msg
    = GotQuiz (Result (OpenApi.Common.Error () String) Quiz)


type alias Flags =
    { quizId : Int
    , teamNumber : Int
    }
