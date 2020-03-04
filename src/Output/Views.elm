module Output.Views exposing (..)

import Chartjs.Chart exposing (chart)
import Color.Convert
import Common.Ranking exposing (ratingsToRankings)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamLine, TeamQuery, TeamTable, TeamTableInfo)
import Common.Util as Util
import Html exposing (Html, button, div, h1, table, td, text, th, tr)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import List.Extra
import Output.Charts as Charts
import Output.Colors exposing (mkColors)
import Output.Model exposing (Model, Msg(..), SubModel(..), mkFullQuizName)


view : Model -> Html Msg
view model =
    case model.subModel of
        TableModel teamTableInfo quizInfo ->
            tableView teamTableInfo quizInfo model.labels

        QuizModel quizRatings quizInfo ->
            quizView quizRatings
                (if model.teamQuery.teamQueryQuizId == quizInfo.quizId then
                    Possible

                 else
                    Impossible
                )
                model.labels

        AllModel quizInfos ->
            allView quizInfos


type BackToTable
    = Possible
    | Impossible


tableView : TeamTableInfo -> QuizInfo -> Labels -> Html Msg
tableView teamTableInfo quizInfo labels =
    let
        colors =
            mkColors teamTableInfo.teamTableInfoNumberOfTeams

        colorSetting =
            Util.foldMaybe []
                (\c -> [ style "color" (Color.Convert.colorToCssRgba c) ])
                (List.Extra.getAt teamTableInfo.teamTableInfoTeamNumber colors)
    in
    div [ id "tableView" ]
        [ div [ id "ownPoints" ]
            [ h1 colorSetting
                [ text teamTableInfo.teamTableInfoTeamName
                , text ": "
                , text (showStanding (standing teamTableInfo.teamTable))
                ]
            ]
        , div [ id "pointsTable" ]
            [ table
                []
                (tr []
                    [ th [] [ text labels.roundLabel ]
                    , th [] [ text labels.ownPointsLabel ]
                    , th [] [ text labels.maxReachedLabel ]
                    , th [] [ text labels.maxReachableLabel ]
                    ]
                    :: List.map mkHTMLLine teamTableInfo.teamTable
                )
            ]
        , div [ id "quizRatings" ]
            [ button [ class "quizRatingsButton", onClick (GetQuizRatings quizInfo) ] [ text labels.backToChartView ] ]
        ]


mkHTMLLine : TeamLine -> Html Msg
mkHTMLLine ti =
    tr []
        [ td [] [ text (String.fromInt ti.roundNumber) ]
        , td [] [ text (String.fromFloat ti.reachedPoints) ]
        , td [] [ text (String.fromFloat ti.maximumPoints) ]
        , td [] [ text (String.fromFloat ti.reachablePoints) ]
        ]


standing : TeamTable -> ( Float, Float )
standing =
    List.foldr (\tl ( reached, reachable ) -> ( tl.reachedPoints + reached, tl.reachablePoints + reachable )) ( 0, 0 )


showStanding : ( Float, Float ) -> String
showStanding ( reached, reachable ) =
    String.join "/" [ String.fromFloat reached, String.fromFloat reachable ]


quizView : QuizRatings -> BackToTable -> Labels -> Html Msg
quizView quizRatings btt labels =
    let
        rankings = ratingsToRankings quizRatings.ratings

        sortedHeader =
            List.sortBy .teamInfoNumber quizRatings.header

        roundLabels =
            List.map (\( n, _ ) -> String.join " " [ labels.roundLabel, String.fromInt n ]) rankings.sortedRatings

        backToTable =
            case btt of
                Possible ->
                    [ div [ id "backToTable" ]
                        [ button [ class "ownPointsButton", onClick GetTeamTable ]
                            [ text labels.ownPointsLabel ]
                        ]
                    ]

                Impossible ->
                    []

        colors =
            mkColors (List.length quizRatings.header)
    in
    div [ id "charts" ]
        ([ div [ id "cumulativeChart" ]
            [ chart 798 599 (Charts.cumulativeChart sortedHeader colors rankings.cumulative roundLabels labels.cumulativeLabel) ]
         , div [ id "perRoundChart" ]
            [ chart 798 599 (Charts.perRoundChart sortedHeader colors rankings.perRound roundLabels labels.individualRoundsLabel) ]
         , div [ id "progressionChart" ]
            [ chart 798 599 (Charts.progressionChart sortedHeader colors rankings.cumulative roundLabels labels.progressionLabel) ]
         , div [ id "allQuizzes" ]
            [ button [ class "allQuizzesButton", onClick GetAllQuizzes ] [ text labels.viewPrevious ] ]
         ]
            ++ backToTable
        )


allView : List QuizInfo -> Html Msg
allView quizInfos =
    div [ id "allQuizzes" ]
        (List.map mkQuizInfoButton quizInfos)


mkQuizInfoButton : QuizInfo -> Html Msg
mkQuizInfoButton quizInfo =
    div []
        [ button [ class "quizInfoButton", onClick (GetQuizRatings quizInfo) ]
            [ text (mkFullQuizName quizInfo.quizIdentifier) ]
        ]
