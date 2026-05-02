module Pages.Public.Quiz.View exposing (view)

{-| Quiz page view.
-}

import Api.Types exposing (QuizActive, Round, ScoreEntry, Team)
import Chart as C
import Chart.Attributes as CA
import Date
import Html exposing (Html, h1, h2, li, ol, p, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import List.Extra
import Maybe.Extra
import Pages.Public.Quiz.Page as Page
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
        , viewProgressionChart teamData rounds
        , viewCumulativeBarChart teamData rounds
        , viewPerRoundBarChart teamData rounds
        , viewRoundStatisticsChart rounds scores
        , viewScoreTable quiz
        ]



-- DATA PREPARATION


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



-- RANKING


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


viewProgressionChart : List TeamData -> List Round -> Html msg
viewProgressionChart teamData _ =
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
                                    [ C.interpolated .y [ CA.color (teamColor i) ] [ CA.circle ]
                                    ]
                                    (td.cumulativeScores |> List.indexedMap (\ri score -> { x = toFloat (ri + 1), y = score }))
                            )
                   )
            )
        ]



-- CHART 2: CUMULATIVE BAR CHART


viewCumulativeBarChart : List TeamData -> List Round -> Html msg
viewCumulativeBarChart teamData rounds =
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
                                [ CA.color (teamColor i) ]
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
                                [ CA.color (teamColor i) ]
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



-- CHART 4: ROUND STATISTICS


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
                [ C.bar .min [ CA.color "#e15759" ]
                    |> C.named "Min"
                , C.bar .average [ CA.color "#f28e2c" ]
                    |> C.named "Average"
                , C.bar .median [ CA.color "#76b7b2" ]
                    |> C.named "Median"
                , C.bar .max [ CA.color "#59a14f" ]
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


viewScoreTable : QuizActive -> Html msg
viewScoreTable quiz =
    let
        activeTeams =
            quiz.scoreBoard.teams
                |> List.filter .active
                |> List.sortBy .number

        rounds =
            quiz.scoreBoard.scores
                |> List.map .roundNumber
                |> List.Extra.unique
                |> List.sort

        roundInfos =
            quiz.rounds
                |> List.sortBy .number
    in
    section [ class "score-table-section" ]
        [ h2 [] [ text "Detailed Scores" ]
        , table [ class "score-table" ]
            [ viewTableHeader rounds roundInfos
            , viewTableBody activeTeams rounds quiz.scoreBoard.scores roundInfos
            ]
        ]


viewTableHeader : List Int -> List Round -> Html msg
viewTableHeader rounds roundInfos =
    let
        roundHeaders =
            rounds
                |> List.map
                    (\roundNum ->
                        let
                            maxPoints =
                                roundInfos
                                    |> List.Extra.find (\r -> r.number == roundNum)
                                    |> Maybe.map .displayMaxPoints
                                    |> Maybe.withDefault 0
                        in
                        th [ class "round-header" ]
                            [ text ("R" ++ String.fromInt roundNum)
                            , Html.br [] []
                            , Html.small [] [ text ("/" ++ formatPoints maxPoints) ]
                            ]
                    )

        totalMaxPoints =
            roundInfos |> List.map .displayMaxPoints |> List.sum
    in
    thead []
        [ tr []
            (th [ class "team-header" ] [ text "Team" ]
                :: roundHeaders
                ++ [ th [ class "total-header" ]
                        [ text "Total"
                        , Html.br [] []
                        , Html.small [] [ text ("/" ++ formatPoints totalMaxPoints) ]
                        ]
                   ]
            )
        ]


viewTableBody : List Team -> List Int -> List ScoreEntry -> List Round -> Html msg
viewTableBody teams rounds scores roundInfos =
    let
        teamRows =
            teams
                |> List.map (viewTeamRow rounds scores roundInfos)
                |> List.sortBy Tuple.first
                |> List.reverse
                |> List.map Tuple.second
    in
    tbody [] teamRows


viewTeamRow : List Int -> List ScoreEntry -> List Round -> Team -> ( Float, Html msg )
viewTeamRow rounds scores _ team =
    let
        roundScores =
            rounds
                |> List.map
                    (\roundNum ->
                        scores
                            |> List.Extra.find (\s -> s.teamNumber == team.number && s.roundNumber == roundNum)
                            |> Maybe.Extra.unwrap 0 .points
                    )

        total =
            List.sum roundScores

        cells =
            roundScores
                |> List.map
                    (\pts ->
                        td [ class "score-cell" ] [ text (formatPoints pts) ]
                    )
    in
    ( total
    , tr [ class "team-row" ]
        (td [ class "team-name" ] [ text (teamName team) ]
            :: cells
            ++ [ td [ class "total-cell" ] [ text (formatPoints total) ] ]
        )
    )



-- HELPERS


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


teamColor : Int -> String
teamColor index =
    let
        colors =
            [ "#4e79a7"
            , "#f28e2c"
            , "#e15759"
            , "#76b7b2"
            , "#59a14f"
            , "#edc949"
            , "#af7aa1"
            , "#ff9da7"
            , "#9c755f"
            , "#bab0ab"
            ]
    in
    List.Extra.getAt (modBy (List.length colors) index) colors
        |> Maybe.withDefault "#4e79a7"
