module Pages.BackOffice.Overview.Page exposing
    ( Model
    , Msg(..)
    , lenses
    )

import Api.Types exposing (QuizSummary)
import Monocle.Lens exposing (Lens)
import OpenApi.Common
import Util.Tristate exposing (Tristate)


type alias Model =
    { quizzes : Tristate (OpenApi.Common.Error () String) (List QuizSummary)
    }


lenses :
    { quizzes : Lens Model (Tristate (OpenApi.Common.Error () String) (List QuizSummary))
    }
lenses =
    { quizzes = Lens .quizzes (\b a -> { a | quizzes = b })
    }


type Msg
    = GotQuizzes (Result (OpenApi.Common.Error () String) (List QuizSummary))
