module Pages.Public.QuizSheets.Page exposing (Model, Msg(..))

import Api.Types exposing (Quiz)
import OpenApi.Common


type alias Model =
    { quizId : Int
    , quiz : Maybe Quiz
    , isLoading : Bool
    , error : Maybe String
    , baseUrl : String
    }


type Msg
    = GotQuiz (Result (OpenApi.Common.Error () String) Quiz)
