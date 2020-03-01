module Output.QuizOutput exposing (..)

import Browser
import Common.Constants exposing (getQuizRatingsApi, teamQueryParam, teamTableApi)
import Common.Types exposing (DbQuizId, TeamQuery, jsonDecQuizRatings, jsonDecTeamTable, jsonEncTeamQuery)
import Common.Util exposing (getAllWith, getMsg, getMsgWith)
import Output.Model as Model exposing (Model(..), Msg(..), initialModelFunction)
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
    case ( msg, model ) of
        ( GetQuizRatings qid, AllModel _ _ ) ->
            ( model, getQuizRatings qid )

        --( GotQuizRatings qrCandidate, _ ) ->
        --   case qrCandidate of
        --     Ok quizRatings ->
        ( GetTeamTable teamQuery, QuizModel _ _ _ _ ) ->
            ( model, getTeamTable teamQuery )

        ( GotTeamTable teamTableCandidate, QuizModel _ quizInfo _ labels ) ->
            let
                newModel =
                    case teamTableCandidate of
                        Ok teamTable ->
                            TableModel teamTable quizInfo labels

                        Err _ ->
                            model
            in
            ( newModel, Cmd.none )

        ( GetAllQuizzes, QuizModel _ _ _ _ ) ->
            ( model, getAllQuizzes )

        ( GotAllQuizzes quizzesCandidate, QuizModel _ _ _ labels ) ->
            let
                newModel =
                    case Debug.log "candidate" quizzesCandidate of
                        Ok quizInfos ->
                            AllModel quizInfos labels

                        Err _ ->
                            model
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


getQuizRatings : DbQuizId -> Cmd Msg
getQuizRatings =
    getMsg getQuizRatingsApi GotQuizRatings jsonDecQuizRatings


getTeamTable : TeamQuery -> Cmd Msg
getTeamTable =
    getMsgWith jsonEncTeamQuery teamQueryParam teamTableApi GotTeamTable jsonDecTeamTable


getAllQuizzes : Cmd Msg
getAllQuizzes =
    getAllWith GotAllQuizzes
