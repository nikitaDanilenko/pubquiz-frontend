module Pages.Public.Team.View exposing (view)

{-| Team detail page view.
-}

import Api.Types exposing (QuizActive, QuizIdentifier, Round, ScoreEntry, Team)
import Date
import Html exposing (Html, a, article, em, h1, h2, h3, p, section, small, text)
import Html.Attributes exposing (class, href, style)
import List.Extra
import Pages.Public.Team.Page as Page
import Util.Colors as Colors
import Util.Tristate as Tristate


view : Page.Model -> Html Page.Msg
view model =
    Tristate.fold
        { onInitial = viewLoading
        , onReady = viewTeam model.teamNumber
        , onFailed = viewError
        }
        model.quiz


viewLoading : Html msg
viewLoading =
    section [ class "loading" ]
        [ p [] [ text "Loading team data..." ]
        ]


viewError : a -> Html msg
viewError _ =
    section [ class "error" ]
        [ h1 [] [ text "Error" ]
        , p [] [ text "Failed to load team data." ]
        ]


viewTeam : Int -> QuizActive -> Html msg
viewTeam teamNumber quiz =
    let
        activeTeams =
            quiz.scoreBoard.teams
                |> List.filter .active
                |> List.sortBy .number

        totalTeams =
            List.length activeTeams

        teamIndex =
            activeTeams
                |> List.Extra.findIndex (\t -> t.number == teamNumber)
                |> Maybe.withDefault 0

        teamColor =
            Colors.interpolateColor totalTeams teamIndex
                |> Colors.toHex

        team =
            activeTeams
                |> List.Extra.find (\t -> t.number == teamNumber)

        rounds =
            quiz.rounds |> List.sortBy .number

        scores =
            quiz.scoreBoard.scores

        roundData =
            computeRoundData teamNumber rounds scores activeTeams

        totalReached =
            roundData |> List.map .ownPoints |> List.sum

        totalReachable =
            roundData |> List.map .maxReachable |> List.sum
    in
    section [ class "team-view" ]
        [ viewHeader quiz.identifier
        , viewTeamHeader teamColor team teamNumber totalReached totalReachable
        , viewRoundTable roundData
        , viewBackLink quiz.quizId
        ]


viewHeader : QuizIdentifier -> Html msg
viewHeader identifier =
    let
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


viewTeamHeader : String -> Maybe Team -> Int -> Float -> Float -> Html msg
viewTeamHeader color maybeTeam teamNumber reached reachable =
    let
        name =
            maybeTeam
                |> Maybe.map
                    (\t ->
                        if String.isEmpty t.name then
                            String.concat [ "Team ", String.fromInt t.number ]

                        else
                            t.name
                    )
                |> Maybe.withDefault (String.concat [ "Team ", String.fromInt teamNumber ])
    in
    section [ class "team-header" ]
        [ h2 [ style "color" color ]
            [ text name
            , text ": "
            , text (formatScore reached reachable)
            ]
        ]


formatScore : Float -> Float -> String
formatScore reached reachable =
    String.concat [ formatPoints reached, "/", formatPoints reachable ]


formatPoints : Float -> String
formatPoints points =
    if toFloat (round points) == points then
        String.fromInt (round points)

    else
        String.fromFloat points


type alias RoundData =
    { roundNumber : Int
    , ownPoints : Float
    , maxReached : Float
    , maxReachable : Float
    , placeInRound : Int
    , placeAfterRound : Int
    }


computeRoundData : Int -> List Round -> List ScoreEntry -> List Team -> List RoundData
computeRoundData teamNumber rounds scores activeTeams =
    let
        activeTeamNumbers =
            activeTeams |> List.map .number

        cumulativeScores =
            computeCumulativeScores rounds scores activeTeamNumbers
    in
    rounds
        |> List.indexedMap
            (\roundIndex r ->
                let
                    roundScores =
                        scores
                            |> List.filter (\s -> s.roundNumber == r.number && List.member s.teamNumber activeTeamNumbers)

                    ownPoints =
                        roundScores
                            |> List.Extra.find (\s -> s.teamNumber == teamNumber)
                            |> Maybe.map .points
                            |> Maybe.withDefault 0

                    maxReached =
                        roundScores
                            |> List.map .points
                            |> List.maximum
                            |> Maybe.withDefault 0

                    placeInRound =
                        computePlace teamNumber roundScores

                    cumulativeAtRound =
                        cumulativeScores
                            |> List.Extra.getAt roundIndex
                            |> Maybe.withDefault []

                    placeAfterRound =
                        computePlaceCumulative teamNumber cumulativeAtRound
                in
                { roundNumber = r.number
                , ownPoints = ownPoints
                , maxReached = maxReached
                , maxReachable = r.displayMaxPoints
                , placeInRound = placeInRound
                , placeAfterRound = placeAfterRound
                }
            )


computePlace : Int -> List ScoreEntry -> Int
computePlace teamNumber scores =
    let
        sorted =
            scores
                |> List.sortBy (.points >> negate)
                |> List.Extra.groupWhile (\a b -> a.points == b.points)

        findRank groups rank =
            case groups of
                [] ->
                    0

                ( first, rest ) :: remaining ->
                    let
                        group =
                            first :: rest
                    in
                    if List.any (\s -> s.teamNumber == teamNumber) group then
                        rank

                    else
                        findRank remaining (rank + List.length group)
    in
    findRank sorted 1


computePlaceCumulative : Int -> List ( Int, Float ) -> Int
computePlaceCumulative teamNumber cumulativeScores =
    let
        sorted =
            cumulativeScores
                |> List.sortBy (Tuple.second >> negate)
                |> List.Extra.groupWhile (\a b -> Tuple.second a == Tuple.second b)

        findRank groups rank =
            case groups of
                [] ->
                    0

                ( first, rest ) :: remaining ->
                    let
                        group =
                            first :: rest
                    in
                    if List.any (\( tn, _ ) -> tn == teamNumber) group then
                        rank

                    else
                        findRank remaining (rank + List.length group)
    in
    findRank sorted 1


computeCumulativeScores : List Round -> List ScoreEntry -> List Int -> List (List ( Int, Float ))
computeCumulativeScores rounds scores activeTeamNumbers =
    let
        getScoreForRound roundNum teamNum =
            scores
                |> List.Extra.find (\s -> s.roundNumber == roundNum && s.teamNumber == teamNum)
                |> Maybe.map .points
                |> Maybe.withDefault 0

        accumulate roundNum previousCumulative =
            activeTeamNumbers
                |> List.map
                    (\teamNum ->
                        let
                            prev =
                                previousCumulative
                                    |> List.Extra.find (\( tn, _ ) -> tn == teamNum)
                                    |> Maybe.map Tuple.second
                                    |> Maybe.withDefault 0

                            current =
                                getScoreForRound roundNum teamNum
                        in
                        ( teamNum, prev + current )
                    )
    in
    rounds
        |> List.foldl
            (\r acc ->
                let
                    previous =
                        List.Extra.last acc |> Maybe.withDefault []

                    newCumulative =
                        accumulate r.number previous
                in
                List.concat [ acc, [ newCumulative ] ]
            )
            []


viewRoundTable : List RoundData -> Html msg
viewRoundTable roundData =
    let
        withPrevious =
            List.indexedMap
                (\i rd ->
                    ( rd, List.Extra.getAt (i - 1) roundData )
                )
                roundData
    in
    section [ class "round-cards" ]
        (withPrevious |> List.map viewRoundCard)


viewRoundCard : ( RoundData, Maybe RoundData ) -> Html msg
viewRoundCard ( rd, maybePrevious ) =
    let
        overallChange =
            maybePrevious
                |> Maybe.map (\prev -> prev.placeAfterRound - rd.placeAfterRound)
    in
    article [ class "round-card" ]
        [ h3 [ class "round-title" ]
            [ text (String.concat [ "Round ", String.fromInt rd.roundNumber ]) ]
        , p [ class "round-line" ]
            [ em [] [ text "Points: " ]
            , text (String.concat [ formatPoints rd.ownPoints, " / ", formatPoints rd.maxReachable ])
            , small [] [ text (String.concat [ " (max: ", formatPoints rd.maxReached, ")" ]) ]
            ]
        , p [ class "round-line" ]
            [ em [] [ text "Place: " ]
            , text (ordinal rd.placeInRound)
            ]
        , p [ class "round-line" ]
            [ em [] [ text "Overall: " ]
            , text (ordinal rd.placeAfterRound)
            , text " "
            , text (trendIndicator overallChange)
            ]
        ]


trendIndicator : Maybe Int -> String
trendIndicator maybeChange =
    case maybeChange of
        Nothing ->
            ""

        Just change ->
            if change > 0 then
                "↑"

            else if change < 0 then
                "↓"

            else
                "·"


ordinal : Int -> String
ordinal n =
    let
        suffix =
            if n >= 11 && n <= 13 then
                "th"

            else
                case modBy 10 n of
                    1 ->
                        "st"

                    2 ->
                        "nd"

                    3 ->
                        "rd"

                    _ ->
                        "th"
    in
    String.concat [ String.fromInt n, suffix ]


viewBackLink : Int -> Html msg
viewBackLink quizId =
    section [ class "quiz-link" ]
        [ a [ href (String.concat [ "/quizzes/", String.fromInt quizId ]) ]
            [ text "To quiz" ]
        ]
