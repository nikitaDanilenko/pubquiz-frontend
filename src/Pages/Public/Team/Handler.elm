module Pages.Public.Team.Handler exposing (init, update)

{-| Team detail page logic.
-}

import Api.Json
import Http
import Pages.Public.Team.Page as Page
import Result.Extra
import Util.Tristate as Tristate


init : Page.Flags -> ( Page.Model, Cmd Page.Msg )
init flags =
    ( { quizId = flags.quizId
      , teamNumber = flags.teamNumber
      , quiz = Tristate.initial
      }
    , fetchQuiz flags.apiBase flags.quizId
    )


update : Page.Msg -> Page.Model -> ( Page.Model, Cmd Page.Msg )
update msg model =
    case msg of
        Page.GotQuiz result ->
            ( Page.lenses.quiz.set (Result.Extra.unpack Tristate.failed Tristate.ready result) model
            , Cmd.none
            )


fetchQuiz : String -> Int -> Cmd Page.Msg
fetchQuiz apiBase quizId =
    Http.get
        { url = String.concat [ apiBase, "/public/", String.fromInt quizId ]
        , expect = Http.expectJson Page.GotQuiz Api.Json.decodeQuizActive
        }
