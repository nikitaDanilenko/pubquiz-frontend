module Output.Views exposing (..)

import Chartjs.Chart exposing (chart)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamLine, TeamTable)
import Html exposing (Html, button, div, table, td, text, th, tr)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import List.Extra exposing (scanl, transpose)
import Output.Charts as Charts
import Output.Model exposing (Model(..), Msg(..), QuizModelKind(..), mkFullQuizName)


view : Model -> Html Msg
view model =
    case model of
        TableModel teamTable quizInfo labels ->
            tableView teamTable quizInfo.quizId labels

        QuizModel quizRatings _ kind labels ->
            quizView quizRatings labels kind

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


quizView : QuizRatings -> Labels -> QuizModelKind -> Html Msg
quizView quizRatings labels kind =
    let
        sortedRatings =
            List.sortBy Tuple.first quizRatings.ratings

        sortedHeader =
            List.sortBy .teamInfoNumber quizRatings.header

        roundLabels =
            List.map (\( n, _ ) -> String.join " " [ labels.roundLabel, String.fromInt n ]) sortedRatings

        rearranged =
            transpose (List.map (\( rn, rat ) -> rat.points |> List.sortBy .teamNumber |> List.map (\x -> ( rn, x ))) sortedRatings)

        perRoundPoints =
            List.map (List.map (\a -> a |> Tuple.second |> .rating)) rearranged

        cumulativePoints =
            List.map (\xs -> xs |> scanl (\( _, nextTr ) current -> current + nextTr.rating) 0 |> List.drop 1) rearranged

        backToTable = case kind of
                        Current teamQuery -> [ div [id "backToTable"] [ button [class "ownPointsButton", onClick (GetTeamTable teamQuery)]
                                               [ text labels.ownPointsLabel ] ] ]
                        Other -> []
    in
    div [ id "charts" ]
        ([ div [ id "perRoundChart" ]
            [ chart 798 599 (Charts.perRoundChart sortedHeader perRoundPoints roundLabels labels.individualRoundsLabel) ]
        , div [ id "cumulativeChart" ]
            [ chart 798 599 (Charts.cumulativeChart sortedHeader cumulativePoints roundLabels labels.cumulativeLabel) ]
        , div [ id "progressionChart" ]
            [ chart 798 599 (Charts.progressionChart sortedHeader cumulativePoints roundLabels labels.progressionLabel) ]
        , div [ id "allQuizzes" ]
              [ button [ class "allQuizzesButton", onClick GetAllQuizzes] [ text labels.viewPrevious ] ]
        ] ++ backToTable)


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
