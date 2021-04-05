module Output.Quiz exposing (Model, Msg, init, update, view)

import Chartjs.Chart exposing (chart)
import Common.QuizRatings as QuizRatings
import Common.Ranking exposing (RoundRankingPerTeam, RoundRankings, RoundWinner, TeamsRanking, ratingsToRankings)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamQuery)
import Common.Util as Util exposing (ErrorOr)
import Common.WireUtil exposing (getLabelsWith, getQuizInfoWith, getQuizRatingsWith, linkButton, loadingSymbol, mkPlacementTables, mkTeamQueryLink, useOrFetchWith)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class, for, id)
import Input.QuizValues as QuizValues
import Output.Charts as Charts
import Output.Colors as Colors exposing (mkColors)
import Output.OutputUtil exposing (fragmentUrl, mkFullQuizName)


type alias Model =
    { labels : Labels
    , teamQueryCandidate : Maybe TeamQuery
    , quizRatings : QuizRatings
    , quizInfo : QuizInfo
    , status : Status
    }


updateLabels : Model -> Labels -> Model
updateLabels model labels =
    { model | labels = labels, status = updateLabelsSet model.status True }


updateQuizRatings : Model -> QuizRatings -> Model
updateQuizRatings model quizRatings =
    { model | quizRatings = quizRatings, status = updateQuizRatingsSet model.status True }


updateQuizInfo : Model -> QuizInfo -> Model
updateQuizInfo model quizInfo =
    { model | quizInfo = quizInfo, status = updateQuizInfoSet model.status True }


{-| Without the status, the charts are initialised with whatever values are contained in the initial
models only. The precise background is unclear.
Using a status is a simple workaround since we only create charts once every value has been set.
-}
type alias Status =
    { labelsSet : Bool
    , quizRatingsSet : Bool
    , quizInfoSet : Bool
    }


loading : Status
loading =
    Status False False False


isFinished : Status -> Bool
isFinished s =
    List.all identity [ s.labelsSet, s.quizRatingsSet, s.quizInfoSet ]


updateLabelsSet : Status -> Bool -> Status
updateLabelsSet s b =
    { s | labelsSet = b }


updateQuizRatingsSet : Status -> Bool -> Status
updateQuizRatingsSet s b =
    { s | quizRatingsSet = b }


updateQuizInfoSet : Status -> Bool -> Status
updateQuizInfoSet s b =
    { s | quizInfoSet = b }


type Msg
    = GotQuizRatings (ErrorOr QuizRatings)
    | GotLabels (ErrorOr Labels)
    | GotQuizInfo (ErrorOr QuizInfo)


init : Maybe Labels -> Maybe TeamQuery -> DbQuizId -> ( Model, Cmd Msg )
init mLabels mTeamQuery qid =
    ( { labels = Maybe.withDefault QuizValues.defaultLabels mLabels
      , teamQueryCandidate = mTeamQuery
      , quizRatings = QuizRatings.default
      , quizInfo = QuizValues.defaultQuizInfo
      , status = { loading | labelsSet = Util.isDefined mLabels }
      }
    , Cmd.batch
        [ getQuizInfoWith GotQuizInfo qid
        , useOrFetchWith (getLabelsWith GotLabels) mLabels qid
        , getQuizRatingsWith GotQuizRatings qid
        ]
    )


view : Model -> Html Msg
view model =
    if not (isFinished model.status) then
        div []
            [ loadingSymbol ]

    else
        let
            rankings =
                ratingsToRankings model.quizRatings

            roundLabels =
                List.map (\( n, _ ) -> String.join " " [ model.labels.roundLabel, String.fromInt n ]) rankings.sortedRatings

            cumulativeChart =
                Charts.cumulativeChart colors rankings.cumulative roundLabels model.labels.cumulativeLabel

            perRoundChart =
                Charts.perRoundChart colors rankings.perRound roundLabels model.labels.individualRoundsLabel

            progressionChart =
                Charts.progressionChart colors rankings.cumulative roundLabels model.labels.progressionLabel

            roundEvaluationChart =
                Charts.roundEvaluationChart Colors.evaluationColors model.quizRatings roundLabels ""

            backToTable =
                case model.teamQueryCandidate of
                    Just teamQuery ->
                        if teamQuery.teamQueryQuizId == model.quizInfo.quizId then
                            [ div [ id "backToTable" ]
                                [ linkButton
                                    (mkTeamQueryLink teamQuery)
                                    [ class "ownPointsButton" ]
                                    [ text model.labels.ownPointsLabel ]
                                ]
                            ]

                        else
                            []

                    Nothing ->
                        []

            colors =
                model.quizRatings.header
                    |> List.filter (.teamInfoActivity >> QuizValues.isActive)
                    |> List.length
                    |> mkColors
        in
        div [ id "quizView" ]
            (div [ id "quizTitle" ] [ label [ for "quizTitleLabel" ] [ text (mkFullQuizName model.quizInfo.quizIdentifier) ] ]
                :: div [ id "rankings" ] (mkPlacementTables rankings model.labels)
                :: div [ id "charts" ]
                    [ div [ id "cumulativeChart" ]
                        [ chart 800 600 cumulativeChart ]
                    , div [ id "perRoundChart" ]
                        [ chart 800 600 perRoundChart ]
                    , div [ id "progressionChart" ]
                        [ chart 800 600 progressionChart ]
                    , div [ id "roundEvaluationChart" ]
                        [ chart 800 600 roundEvaluationChart ]
                    ]
                :: div [ id "allQuizzes" ]
                    [ linkButton
                        (fragmentUrl [ "" ])
                        [ class "allQuizzesButton" ]
                        [ text model.labels.viewPrevious ]
                    ]
                :: backToTable
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                GotQuizRatings quizRatingsCandidate ->
                    Util.foldResult model (updateQuizRatings model) quizRatingsCandidate

                GotLabels labelsCandidate ->
                    Util.foldResult model (updateLabels model) labelsCandidate

                GotQuizInfo quizInfoCandidate ->
                    Util.foldResult model (updateQuizInfo model) quizInfoCandidate
    in
    ( newModel, Cmd.none )
