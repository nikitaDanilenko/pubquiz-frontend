module Pages.Public.Quiz.Page exposing
    ( Flags
    , Hovering
    , Model
    , Msg(..)
    , StatsHovering
    , lenses
    )

import Api.Types exposing (QuizActive)
import Chart.Item as CI
import Monocle.Lens exposing (Lens)
import OpenApi.Common
import Util.Tristate exposing (Tristate)


type alias Hovering =
    List (CI.One { round : Int } CI.Any)


type alias StatsHovering =
    List (CI.One { round : Int, min : Float, max : Float, average : Float, median : Float } CI.Any)


type alias Model =
    { quizId : Int
    , quiz : Tristate (OpenApi.Common.Error () String) QuizActive
    , hovering : Hovering
    , statsHovering : StatsHovering
    }


lenses :
    { quizId : Lens Model Int
    , quiz : Lens Model (Tristate (OpenApi.Common.Error () String) QuizActive)
    , hovering : Lens Model Hovering
    , statsHovering : Lens Model StatsHovering
    }
lenses =
    { quizId = Lens .quizId (\b a -> { a | quizId = b })
    , quiz = Lens .quiz (\b a -> { a | quiz = b })
    , hovering = Lens .hovering (\b a -> { a | hovering = b })
    , statsHovering = Lens .statsHovering (\b a -> { a | statsHovering = b })
    }


type Msg
    = GotQuiz (Result (OpenApi.Common.Error () String) QuizActive)
    | OnHover Hovering
    | OnStatsHover StatsHovering


type alias Flags =
    { quizId : Int
    }
