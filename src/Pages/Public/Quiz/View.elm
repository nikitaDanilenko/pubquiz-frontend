module Pages.Public.Quiz.View exposing (view)

{-| Quiz page view.
-}

import Api.Types exposing (QuizActive, Round, ScoreEntry, Team)
import Chart as C
import Chart.Attributes as CA
import Date
import Html exposing (Html, h1, h2, li, ol, p, section, text)
import Html.Attributes exposing (class)
import List.Extra
import Maybe.Extra
import Pages.Public.Quiz.Page as Page
import Util.Colors as Colors
import Util.Tristate as Tristate


view : Page.Model -> Html Page.Msg
view model =
    Tristate.fold
        { onInitial = viewLoading
        , onReady = viewQuiz
        , onFailed = viewError
        }
        model.quiz


viewLoading : Html msg
viewLoading =
    section [ class "loading" ]
        [ p [] [ text "Loading quiz..." ]
        ]


viewError : a -> Html msg
viewError _ =
    section [ class "error" ]
        [ h1 [] [ text "Error" ]
        , p [] [ text "Failed to load quiz. It may not exist or the server is unavailable." ]
        ]


viewQuiz : QuizActive -> Html msg
viewQuiz quiz =
    let
        activeTeams =
            quiz.scoreBoard.teams
                |> List.filter .active
                |> List.sortBy .number

        rounds =
            quiz.rounds |> List.sortBy .number

        scores =
            quiz.scoreBoard.scores

        teamData =
            prepareTeamData activeTeams rounds scores
    in
    section [ class "quiz" ]
        [ viewHeader quiz
        , viewRanking teamData
        , viewProgressionChart teamData
        , viewCumulativeBarChart teamData rounds
        , viewPerRoundBarChart teamData rounds
        , viewRoundStatisticsChart rounds scores
        ]


type alias TeamData =
    { team : Team
    , roundScores : List Float
    , cumulativeScores : List Float
    , total : Float
    }


prepareTeamData : List Team -> List Round -> List ScoreEntry -> List TeamData
prepareTeamData teams rounds scores =
    teams
        |> List.map
            (\team ->
                let
                    roundScores =
                        rounds
                            |> List.map
                                (\r ->
                                    scores
                                        |> List.Extra.find (\s -> s.teamNumber == team.number && s.roundNumber == r.number)
                                        |> Maybe.Extra.unwrap 0 .points
                                )

                    cumulativeScores =
                        roundScores
                            |> List.foldl (\score acc -> acc ++ [ Maybe.Extra.unwrap score ((+) score) (List.Extra.last acc) ]) []

                    total =
                        List.sum roundScores
                in
                { team = team
                , roundScores = roundScores
                , cumulativeScores = cumulativeScores
                , total = total
                }
            )


type alias RankedTeam =
    { rank : Int
    , team : Team
    , total : Float
    }


computeRanking : List TeamData -> List RankedTeam
computeRanking teamData =
    teamData
        |> List.sortBy (.total >> negate)
        |> List.Extra.groupWhile (\a b -> a.total == b.total)
        |> List.foldl
            (\( first, rest ) ( rank, acc ) ->
                let
                    group =
                        first :: rest

                    ranked =
                        group |> List.map (\td -> { rank = rank, team = td.team, total = td.total })
                in
                ( rank + List.length group, acc ++ ranked )
            )
            ( 1, [] )
        |> Tuple.second


viewRanking : List TeamData -> Html msg
viewRanking teamData =
    let
        ranked =
            computeRanking teamData
    in
    section [ class "ranking" ]
        [ h2 [] [ text "Ranking" ]
        , ol [ class "ranking-list" ]
            (ranked
                |> List.map
                    (\r ->
                        li [ class "ranking-item" ]
                            [ text <| String.fromInt r.rank ++ ". "
                            , text <| teamName r.team
                            , text <| " (" ++ formatPoints r.total ++ ")"
                            ]
                    )
            )
        ]



-- CHART 1: PROGRESSION LINE CHART


type alias DataPoint =
    { x : Float
    , y : Float
    }


viewProgressionChart : List TeamData -> Html msg
viewProgressionChart teamData =
    let
        totalTeams =
            List.length teamData
    in
    section [ class "chart progression-chart" ]
        [ h2 [] [ text "Progression" ]
        , C.chart
            [ CA.height 300
            , CA.width 600
            ]
            ([ C.xLabels [ CA.withGrid ]
             , C.yLabels [ CA.withGrid ]
             ]
                ++ (teamData
                        |> List.indexedMap
                            (\i td ->
                                C.series .x
                                    [ C.interpolated .y [ CA.color (teamColor totalTeams i) ] [ CA.circle ]
                                    ]
                                    (td.cumulativeScores |> List.indexedMap (\ri score -> { x = toFloat (ri + 1), y = score }))
                            )
                   )
            )
        ]


viewCumulativeBarChart : List TeamData -> List Round -> Html msg
viewCumulativeBarChart teamData rounds =
    let
        totalTeams =
            List.length teamData
    in
    section [ class "chart cumulative-chart" ]
        [ h2 [] [ text "Cumulative Points by Round" ]
        , C.chart
            [ CA.height 300
            , CA.width 600
            ]
            [ C.xLabels [ CA.withGrid ]
            , C.yLabels [ CA.withGrid ]
            , C.bars
                [ CA.roundTop 0.2 ]
                (teamData
                    |> List.indexedMap
                        (\i _ ->
                            C.bar (.cumulativeScores >> List.Extra.getAt i >> Maybe.withDefault 0)
                                [ CA.color (teamColor totalTeams i) ]
                        )
                )
                (rounds
                    |> List.indexedMap
                        (\ri r ->
                            { round = r.number
                            , cumulativeScores = teamData |> List.map (\td -> List.Extra.getAt ri td.cumulativeScores |> Maybe.withDefault 0)
                            }
                        )
                )
            ]
        ]



-- CHART 3: PER-ROUND BAR CHART


viewPerRoundBarChart : List TeamData -> List Round -> Html msg
viewPerRoundBarChart teamData rounds =
    let
        totalTeams =
            List.length teamData
    in
    section [ class "chart per-round-chart" ]
        [ h2 [] [ text "Points per Round" ]
        , C.chart
            [ CA.height 300
            , CA.width 600
            ]
            [ C.xLabels [ CA.withGrid ]
            , C.yLabels [ CA.withGrid ]
            , C.bars
                [ CA.roundTop 0.2 ]
                (teamData
                    |> List.indexedMap
                        (\i _ ->
                            C.bar (.roundScores >> List.Extra.getAt i >> Maybe.withDefault 0)
                                [ CA.color (teamColor totalTeams i) ]
                        )
                )
                (rounds
                    |> List.indexedMap
                        (\ri r ->
                            { round = r.number
                            , roundScores = teamData |> List.map (\td -> List.Extra.getAt ri td.roundScores |> Maybe.withDefault 0)
                            }
                        )
                )
            ]
        ]


type alias RoundStats =
    { round : Int
    , min : Float
    , max : Float
    , average : Float
    , median : Float
    }


computeRoundStats : List Round -> List ScoreEntry -> List RoundStats
computeRoundStats rounds scores =
    rounds
        |> List.map
            (\r ->
                let
                    roundScores =
                        scores
                            |> List.filter (\s -> s.roundNumber == r.number)
                            |> List.map .points
                            |> List.sort

                    count =
                        List.length roundScores

                    minVal =
                        List.minimum roundScores |> Maybe.withDefault 0

                    maxVal =
                        List.maximum roundScores |> Maybe.withDefault 0

                    avg =
                        if count > 0 then
                            List.sum roundScores / toFloat count

                        else
                            0

                    med =
                        if count == 0 then
                            0

                        else if modBy 2 count == 1 then
                            List.Extra.getAt (count // 2) roundScores |> Maybe.withDefault 0

                        else
                            let
                                lower =
                                    List.Extra.getAt (count // 2 - 1) roundScores |> Maybe.withDefault 0

                                upper =
                                    List.Extra.getAt (count // 2) roundScores |> Maybe.withDefault 0
                            in
                            (lower + upper) / 2
                in
                { round = r.number
                , min = minVal
                , max = maxVal
                , average = avg
                , median = med
                }
            )


viewRoundStatisticsChart : List Round -> List ScoreEntry -> Html msg
viewRoundStatisticsChart rounds scores =
    let
        stats =
            computeRoundStats rounds scores
    in
    section [ class "chart statistics-chart" ]
        [ h2 [] [ text "Round Statistics" ]
        , C.chart
            [ CA.height 300
            , CA.width 600
            ]
            [ C.xLabels [ CA.withGrid ]
            , C.yLabels [ CA.withGrid ]
            , C.bars
                [ CA.roundTop 0.2 ]
                [ C.bar .min [ CA.color Colors.statisticsColors.min ]
                    |> C.named "Min"
                , C.bar .average [ CA.color Colors.statisticsColors.average ]
                    |> C.named "Average"
                , C.bar .median [ CA.color Colors.statisticsColors.median ]
                    |> C.named "Median"
                , C.bar .max [ CA.color Colors.statisticsColors.max ]
                    |> C.named "Max"
                ]
                stats
            , C.legendsAt .max .max [ CA.alignRight ] []
            ]
        ]


viewHeader : QuizActive -> Html msg
viewHeader quiz =
    let
        identifier =
            quiz.identifier

        dateStr =
            Date.format "d MMMM y" identifier.date
    in
    Html.header []
        [ h1 [] [ text identifier.name ]
        , p [ class "quiz-meta" ]
            [ text identifier.place
            , text " · "
            , text dateStr
            ]
        ]


teamName : Team -> String
teamName team =
    if String.isEmpty team.name then
        "Team " ++ String.fromInt team.number

    else
        team.name


formatPoints : Float -> String
formatPoints points =
    if toFloat (round points) == points then
        String.fromInt (round points)

    else
        String.fromFloat points


teamColor : Int -> Int -> String
teamColor total =
    Colors.interpolateColor total >> Colors.toHex
