module Pages.Public.Quiz.View exposing (view)

{-| Quiz page view.
-}

import Api.Types exposing (QuizActive, Round, ScoreBoard, ScoreEntry, Team)
import Date
import Html exposing (Html, h1, p, section, table, tbody, td, text, th, thead, tr)
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
    section [ class "quiz" ]
        [ viewHeader quiz
        , viewScoreTable quiz
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
    table [ class "score-table" ]
        [ viewTableHeader rounds roundInfos
        , viewTableBody activeTeams rounds quiz.scoreBoard.scores roundInfos
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
                            |> List.Extra.find
                                (\s -> s.teamNumber == team.number && s.roundNumber == roundNum)
                            |> Maybe.Extra.unwrap 0 .points
                    )

        total =
            roundScores
                |> List.sum

        cells =
            roundScores
                |> List.map
                    (\maybePoints ->
                        td [ class "score-cell" ]
                            [ text (formatPoints maybePoints) ]
                    )

        teamName =
            if String.isEmpty team.name then
                String.concat [ "Team", " ", String.fromInt team.number ]

            else
                team.name
    in
    ( total
    , tr [ class "team-row" ]
        (td [ class "team-name" ] [ text teamName ]
            :: cells
            ++ [ td [ class "total-cell" ] [ text (formatPoints total) ] ]
        )
    )


formatPoints : Float -> String
formatPoints points =
    if toFloat (round points) == points then
        String.fromInt (round points)

    else
        String.fromFloat points
