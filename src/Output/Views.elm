module Output.Views exposing (..)

import Chartjs.Chart exposing (chart)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamLine, TeamTable)
import Html exposing (Html, button, div, table, td, text, th, tr)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import List.Extra exposing (transpose)
import Output.Charts as Charts
import Output.Model exposing (Model(..), Msg(..), mkFullQuizName)


view : Model -> Html Msg
view model =
    case model of
        TableModel teamTable quizInfo labels ->
            tableView teamTable quizInfo.quizId labels

        QuizModel quizRatings _ labels ->
            quizView quizRatings labels

        AllModel quizInfos _ ->
            allView quizInfos


tableView : TeamTable -> DbQuizId -> Labels -> Html Msg
tableView teamTable qid labels =
    div [ id "tableView" ]
        [ div [ id "ownPoints" ]
            -- todo: Add colour parameter
            [ text (showStanding (standing teamTable)) ]
        , div [ id "pointsTable" ]
            [ table
                []
                (tr []
                    [ th [] [ text labels.roundLabel ]
                    , th [] [ text labels.ownPointsLabel ]
                    , th [] [ text labels.maxReachedLabel ]
                    , th [] [ text labels.maxReachableLabel ]
                    ]
                    :: List.map mkHTMLLine teamTable
                )
            ]
        , div [ id "quizRatings" ]
            [ button [ class "quizRatingsButton", onClick (GetQuizRatings qid) ] [ text labels.backToChartView ] ]
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


quizView : QuizRatings -> Labels -> Html Msg
quizView quizRatings labels =
    let
        sortedRatings =
            List.sortBy Tuple.first quizRatings.ratings

        sortedHeader =
            List.sortBy .teamInfoNumber quizRatings.header

        roundLabels = List.map (\( n, _ ) -> String.join " " [ labels.roundLabel, String.fromInt n ]) sortedRatings

        rearranged =
            transpose (List.map (\( rn, rat ) -> rat.points |> List.sortBy .teamNumber |> List.map (\x -> ( rn, x ))) sortedRatings)
    in
    div [id "charts"]
        [ div [ id "perRoundChart" ]
              [ chart 798 599 (Charts.perRoundChart sortedHeader rearranged roundLabels labels) ],
          div [ id "cumulativeChart"]
              [ chart 798 599 (Charts.cumulativeChart sortedHeader rearranged roundLabels labels)]
        ]


allView : List QuizInfo -> Html Msg
allView quizInfos =
    div [ id "allQuizzes" ]
        (List.map mkQuizInfoButton quizInfos)


mkQuizInfoButton : QuizInfo -> Html Msg
mkQuizInfoButton quizInfo =
    div []
        [ button [ class "quizInfoButton", onClick (GetQuizRatings quizInfo.quizId) ]
            [ text (mkFullQuizName quizInfo.quizIdentifier) ]
        ]
