module Output.QuizOutput exposing (..)

import Browser
import Common.Constants exposing (getQuizRatingsApi, teamQueryParam, teamTableApi)
import Common.Types exposing (DbQuizId, QuizInfo, TeamQuery, jsonDecQuizRatings, jsonDecTeamTable, jsonEncTeamQuery)
import Common.Util exposing (getAllWith, getMsg, getMsgWith)
import Output.Model as Model exposing (Model, Msg(..), QuizModelKind, SubModel(..), initialModelFunction)
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
        ( GetQuizRatings qid, AllModel _ ) ->
            ( model, getQuizRatings qid )

        ( GotQuizRatings qid qrCandidate, AllModel _ ) ->
            let
                newModel =
                    case qrCandidate of
                        Ok quizRatings ->
                            QuizModel quizRatings qid

                        Err _ ->
                            model.subModel
            in
            ( { model | subModel = newModel }, Cmd.none )

        ( GetTeamTable, QuizModel _ _ ) ->
            ( model, getTeamTable model.teamQuery )

        ( GotTeamTable teamTableCandidate, QuizModel _ quizInfo ) ->
            let
                newModel =
                    case teamTableCandidate of
                        Ok teamTable ->
                            TableModel teamTable quizInfo

                        Err _ ->
                            model.subModel
            in
            ( { model | subModel = newModel }, Cmd.none )

        ( GetAllQuizzes, QuizModel _ _ ) ->
            ( model, getAllQuizzes )

        ( GotAllQuizzes quizzesCandidate, QuizModel _ _ ) ->
            let
                newModel =
                    case Debug.log "candidate" quizzesCandidate of
                        Ok quizInfos ->
                            AllModel quizInfos

                        Err _ ->
                            model.subModel
            in
            ( { model | subModel = newModel }, Cmd.none )

        _ ->
            ( model, Cmd.none )


getQuizRatings : QuizInfo -> Cmd Msg
getQuizRatings quizInfo =
    getMsg getQuizRatingsApi (GotQuizRatings quizInfo) jsonDecQuizRatings quizInfo.quizId


getTeamTable : TeamQuery -> Cmd Msg
getTeamTable =
    getMsgWith jsonEncTeamQuery teamQueryParam teamTableApi GotTeamTable jsonDecTeamTable


getAllQuizzes : Cmd Msg
getAllQuizzes =
    getAllWith GotAllQuizzes
