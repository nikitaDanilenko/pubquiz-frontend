module Output.QuizOutput exposing (..)

import Browser
import Common.Constants exposing (getQuizRatingsApi, teamQueryParam, teamTableApi)
import Common.Types exposing (DbQuizId, QuizInfo, TeamQuery, jsonDecQuizRatings, jsonDecTeamTableInfo, jsonEncTeamQuery)
import Common.Util exposing (getAllWith, getMsg, getMsgWith)
import Input.Model exposing (ErrorOr)
import Output.Model as Model exposing (Model, Msg(..), SubModel(..), initialModelFunction)
import Output.Views exposing (view)


main : Program () Model Msg
main =
    Browser.document
        { init = initialModelFunction
        , view = \model -> { title = Model.titleFor model, body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.subModel ) of
        ( GetQuizRatings quizInfo, _ ) ->
            ( model, getQuizRatings quizInfo )

        ( GotQuizRatings quizInfo qrCandidate, _ ) ->
            ( updateSubModel (Result.map (\quizRatings -> QuizModel quizRatings quizInfo) qrCandidate) model, Cmd.none )

        ( GetTeamTable, QuizModel _ _ ) ->
            ( model, getTeamTable model.teamQuery )

        ( GotTeamTable teamTableCandidate, QuizModel _ quizInfo ) ->
            ( updateSubModel (Result.map (\table -> TableModel table quizInfo) teamTableCandidate) model, Cmd.none )

        ( GetAllQuizzes, QuizModel _ _ ) ->
            ( model, getAllQuizzes )

        ( GotAllQuizzes quizzesCandidate, QuizModel _ _ ) ->
            ( updateSubModel (Result.map AllModel quizzesCandidate) model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateSubModel : ErrorOr SubModel -> Model -> Model
updateSubModel sub model =
    case sub of
        Ok subModel ->
            { model | subModel = subModel }

        Err _ ->
            model


getQuizRatings : QuizInfo -> Cmd Msg
getQuizRatings quizInfo =
    getMsg getQuizRatingsApi (GotQuizRatings quizInfo) jsonDecQuizRatings quizInfo.quizId


getTeamTable : TeamQuery -> Cmd Msg
getTeamTable =
    getMsgWith jsonEncTeamQuery teamQueryParam teamTableApi GotTeamTable jsonDecTeamTableInfo


getAllQuizzes : Cmd Msg
getAllQuizzes =
    getAllWith GotAllQuizzes
