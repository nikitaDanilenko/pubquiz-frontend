module Pages.Public.QuizSheets.Handler exposing (init, update)

import Api.Api
import Pages.Public.QuizSheets.Page as Page


init : { quizId : Int, baseUrl : String } -> ( Page.Model, Cmd Page.Msg )
init params =
    ( { quizId = params.quizId
      , quiz = Nothing
      , isLoading = True
      , error = Nothing
      , baseUrl = params.baseUrl
      }
    , Api.Api.publicQuizId
        { params = { quizId = params.quizId }
        , toMsg = Page.GotQuiz
        }
    )


update : Page.Msg -> Page.Model -> ( Page.Model, Cmd Page.Msg )
update msg model =
    case msg of
        Page.GotQuiz result ->
            case result of
                Ok quiz ->
                    ( { model | quiz = Just quiz, isLoading = False }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to load quiz", isLoading = False }
                    , Cmd.none
                    )
