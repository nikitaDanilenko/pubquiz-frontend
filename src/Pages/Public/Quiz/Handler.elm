module Pages.Public.Quiz.Handler exposing (init, update)

{-| Quiz page logic.
-}

import Api.Json
import Http
import Pages.Public.Quiz.Page as Page
import Result.Extra
import Util.Tristate as Tristate


init : Page.Flags -> ( Page.Model, Cmd Page.Msg )
init flags =
    ( { quizId = flags.quizId
      , quiz = Tristate.initial
      , hovering = []
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

        Page.OnHover hovering ->
            ( Page.lenses.hovering.set hovering model
            , Cmd.none
            )


fetchQuiz : String -> Int -> Cmd Page.Msg
fetchQuiz apiBase quizId =
    Http.get
        { url = apiBase ++ "/public/" ++ String.fromInt quizId
        , expect = Http.expectJson Page.GotQuiz Api.Json.decodeQuizActive
        }
