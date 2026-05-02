module Pages.Public.Quiz.View exposing (view)

{-| Quiz page view.
-}

import Api.Types exposing (QuizActive, Round, ScoreEntry, Team)
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Date
import Html exposing (Html, a, h1, h2, li, p, s, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, href, style)
import List.Extra
import Maybe.Extra
import Pages.Public.Quiz.Page as Page
import Stat
import Util.Colors as Colors
import Util.Theme as Theme exposing (Theme)
import Util.Tristate as Tristate


view : Theme -> Page.Model -> Html Page.Msg
view theme model =
    Tristate.fold
        { onInitial = viewLoading
        , onReady = viewQuiz theme model.hovering
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


viewQuiz : Theme -> Page.Hovering -> QuizActive -> Html Page.Msg
viewQuiz theme hovering quiz =
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
        , viewRanking quiz.quizId teamData
        , viewProgressionChart theme teamData
        , viewCumulativeBarChart theme hovering teamData rounds
        , viewPerRoundBarChart theme hovering teamData rounds
        , viewRoundStatisticsChart theme rounds scores
        ]


type alias TeamData =
    { team : Team
    , roundScores : List Float
    , cumulativeScores : List Float
    , total : Float
    }


prepareTeamData : List Team -> List Round -> List ScoreEntry -> List TeamData
prepareTeamData teams rounds scores =
    let
        toTeamData team =
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
                        |> List.foldl (\score acc -> List.concat [ acc, [ Maybe.Extra.unwrap score ((+) score) (List.Extra.last acc) ] ]) []

                total =
                    List.sum roundScores
            in
            { team = team
            , roundScores = roundScores
            , cumulativeScores = cumulativeScores
            , total = total
            }
    in
    teams |> List.map toTeamData


type alias RankedTeam =
    { rank : Int
    , team : Team
    , total : Float
    }


computeRanking : List TeamData -> List RankedTeam
computeRanking teamData =
    let
        assignRank ( first, rest ) ( rank, acc ) =
            let
                group =
                    first :: rest

                ranked =
                    group |> List.map (\td -> { rank = rank, team = td.team, total = td.total })
            in
            ( rank + List.length group, List.concat [ acc, ranked ] )
    in
    teamData
        |> List.sortBy (.total >> negate)
        |> List.Extra.groupWhile (\a b -> a.total == b.total)
        |> List.foldl assignRank ( 1, [] )
        |> Tuple.second


viewRanking : Int -> List TeamData -> Html msg
viewRanking quizId teamData =
    let
        ranked =
            computeRanking teamData
    in
    section [ class "ranking" ]
        [ h2 [] [ text "Ranking" ]
        , table [ class "ranking-table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "#" ]
                    , th [] [ text "Team" ]
                    , th [] [ text "Points" ]
                    ]
                ]
            , tbody []
                (ranked
                    |> List.map
                        (\r ->
                            tr []
                                [ td [] [ text (String.fromInt r.rank) ]
                                , td []
                                    [ a [ href (teamUrl quizId r.team.number) ]
                                        [ text (teamName r.team) ]
                                    ]
                                , td [] [ text (formatPoints r.total) ]
                                ]
                        )
                )
            ]
        ]


teamUrl : Int -> Int -> String
teamUrl quizId teamNumber =
    String.concat [ "/quizzes/", String.fromInt quizId, "/teams/", String.fromInt teamNumber ]


viewProgressionChart : Theme -> List TeamData -> Html Page.Msg
viewProgressionChart theme teamData =
    let
        totalTeams =
            List.length teamData
    in
    section [ class "chart progression-chart" ]
        [ h2 [] [ text "Progression" ]
        , C.chart
            [ CA.height 300
            , CA.width 600
            , CA.margin { top = 10, bottom = 30, left = 0, right = 0 }
            ]
            (List.concat
                [ [ C.xLabels [ CA.withGrid, CA.color (Theme.labelColor theme) ]
                  , C.yLabels [ CA.withGrid, CA.color (Theme.labelColor theme) ]
                  ]
                , teamData
                    |> List.indexedMap
                        (\i td ->
                            C.series .x
                                [ C.interpolated .y [ CA.color (teamColor totalTeams i) ] [ CA.circle, CA.size 8 ]
                                    |> C.named (teamName td.team)
                                ]
                                (td.cumulativeScores |> List.indexedMap (\ri score -> { x = toFloat (ri + 1), y = score }))
                        )
                ]
            )
        , viewTeamLegend teamData
        ]


viewCumulativeBarChart : Theme -> Page.Hovering -> List TeamData -> List Round -> Html Page.Msg
viewCumulativeBarChart theme hovering teamData rounds =
    let
        totalTeams =
            List.length teamData

        roundData =
            rounds |> List.map (\r -> { round = r.number })

        getScore ri i =
            teamData
                |> List.Extra.getAt i
                |> Maybe.andThen (\td -> List.Extra.getAt ri td.cumulativeScores)
                |> Maybe.withDefault 0
    in
    section [ class "chart cumulative-chart" ]
        [ h2 [] [ text "Cumulative Points by Round" ]
        , C.chart
            [ CA.height 300
            , CA.width 600
            , CA.margin { top = 10, bottom = 30, left = 0, right = 0 }
            , CE.onMouseMove Page.OnHover (CE.getNearest CI.any)
            , CE.onMouseLeave (Page.OnHover [])
            ]
            [ C.xLabels [ CA.noGrid, CA.color (Theme.labelColor theme) ]
            , C.yLabels [ CA.withGrid, CA.color (Theme.labelColor theme) ]
            , C.bars
                [ CA.roundTop 0.2 ]
                (teamData
                    |> List.indexedMap
                        (\i td ->
                            C.bar (\datum -> getScore (datum.round - 1) i)
                                [ CA.color (teamColor totalTeams i) ]
                                |> C.named (teamName td.team)
                        )
                )
                roundData
            , C.each hovering <|
                \_ item ->
                    [ C.tooltip item [ CA.onTop ] [] [ Html.text (String.concat [ CI.getName item, ": ", formatPoints (CI.getY item) ]) ] ]
            ]
        , viewTeamLegend teamData
        ]



-- CHART 3: PER-ROUND BAR CHART


viewPerRoundBarChart : Theme -> Page.Hovering -> List TeamData -> List Round -> Html Page.Msg
viewPerRoundBarChart theme hovering teamData rounds =
    let
        totalTeams =
            List.length teamData

        roundData =
            rounds |> List.map (\r -> { round = r.number })

        getScore ri i =
            teamData
                |> List.Extra.getAt i
                |> Maybe.andThen (\td -> List.Extra.getAt ri td.roundScores)
                |> Maybe.withDefault 0
    in
    section [ class "chart per-round-chart" ]
        [ h2 [] [ text "Points per Round" ]
        , C.chart
            [ CA.height 300
            , CA.width 600
            , CA.margin { top = 10, bottom = 30, left = 0, right = 0 }
            , CE.onMouseMove Page.OnHover (CE.getNearest CI.any)
            , CE.onMouseLeave (Page.OnHover [])
            ]
            [ C.xLabels [ CA.noGrid, CA.color (Theme.labelColor theme) ]
            , C.yLabels [ CA.withGrid, CA.color (Theme.labelColor theme) ]
            , C.bars
                [ CA.roundTop 0.2 ]
                (teamData
                    |> List.indexedMap
                        (\i td ->
                            C.bar (\datum -> getScore (datum.round - 1) i)
                                [ CA.color (teamColor totalTeams i) ]
                                |> C.named (teamName td.team)
                        )
                )
                roundData
            , C.each hovering <|
                \_ item ->
                    [ C.tooltip item [ CA.onTop ] [] [ Html.text (String.concat [ CI.getName item, ": ", formatPoints (CI.getY item) ]) ] ]
            ]
        , viewTeamLegend teamData
        ]


type alias RoundStats =
    { round : Int
    , min : Float
    , max : Float
    , average : Float
    , median : Float
    }


toRoundStats : List ScoreEntry -> Round -> RoundStats
toRoundStats scores r =
    let
        roundScores =
            scores
                |> List.filterMap
                    (\s ->
                        if s.roundNumber == r.number then
                            Just s.points

                                else
                                    Nothing
                            )
                        |> List.sort

        minVal =
            List.minimum roundScores |> Maybe.withDefault 0

        maxVal =
            List.maximum roundScores |> Maybe.withDefault 0

        avg =
            Stat.average roundScores
                |> Maybe.withDefault 0

        med =
            Stat.median roundScores
                |> Maybe.withDefault 0
    in
    { round = r.number
    , min = minVal
    , max = maxVal
    , average = avg
    , median = med
    }


computeRoundStats : List Round -> List ScoreEntry -> List RoundStats
computeRoundStats rounds scores =
    rounds |> List.map (toRoundStats scores)


viewRoundStatisticsChart : Theme -> List Round -> List ScoreEntry -> Html msg
viewRoundStatisticsChart theme rounds scores =
    let
        stats =
            computeRoundStats rounds scores
    in
    section [ class "chart statistics-chart" ]
        [ h2 [] [ text "Round Statistics" ]
        , C.chart
            [ CA.height 300
            , CA.width 600
            , CA.margin { top = 10, bottom = 30, left = 0, right = 0 }
            ]
            [ C.xLabels [ CA.noGrid, CA.color (Theme.labelColor theme) ]
            , C.yLabels [ CA.withGrid, CA.color (Theme.labelColor theme) ]
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
            ]
        , viewStatisticsLegend
        ]


viewStatisticsLegend : Html msg
viewStatisticsLegend =
    ul [ class "statistics-legend" ]
        [ legendItem "min" "Min"
        , legendItem "average" "Average"
        , legendItem "median" "Median"
        , legendItem "max" "Max"
        ]


legendItem : String -> String -> Html msg
legendItem colorKey label =
    li
        [ class "legend-item"
        , attribute "data-stat" colorKey
        ]
        [ text label ]


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
        String.concat [ "Team ", String.fromInt team.number ]

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


viewTeamLegend : List TeamData -> Html msg
viewTeamLegend teamData =
    let
        totalTeams =
            List.length teamData
    in
    ul [ class "team-legend" ]
        (teamData
            |> List.indexedMap
                (\i td ->
                    li [ class "legend-item" ]
                        [ span
                            [ class "legend-color"
                            , style "background-color" (teamColor totalTeams i)
                            ]
                            []
                        , text (teamName td.team)
                        ]
                )
        )
