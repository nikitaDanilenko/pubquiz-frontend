module Common.WireUtil exposing
    ( addFeedbackLabel
    , errorToString
    , getLabelsWith
    , getQuizInfoWith
    , getQuizRatingsWith
    , linkButton
    , loadingSymbol
    , mkPlacement
    , mkPlacementTables
    , mkTeamQueryLink
    , useOrFetchWith
    )

import Bootstrap.Button
import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi, quizIdParam, teamCodeParam, teamNumberParam)
import Common.Ranking exposing (RankingsWithSorted, RoundRankings, RoundWinner, TeamsRanking, rankingToPlacement, roundRankingsToRoundWinners)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, Ratings, TeamQuery, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings)
import Common.Util as Util exposing (ErrorOr, getMsg)
import Html exposing (Attribute, Html, div, label, table, td, text, tr)
import Html.Attributes exposing (for, href, id)
import Http exposing (Error(..))
import List.Extra exposing (maximumBy)
import Loading
import Output.OutputUtil exposing (fragmentUrl)


getLabelsWith : (ErrorOr Labels -> msg) -> DbQuizId -> Cmd msg
getLabelsWith f =
    getMsg getLabelsApi f jsonDecLabels


getQuizInfoWith : (ErrorOr QuizInfo -> msg) -> DbQuizId -> Cmd msg
getQuizInfoWith f =
    getMsg getQuizInfoApi f jsonDecQuizInfo


getQuizRatingsWith : (ErrorOr QuizRatings -> msg) -> DbQuizId -> Cmd msg
getQuizRatingsWith f =
    getMsg getQuizRatingsApi f jsonDecQuizRatings


useOrFetchWith : (DbQuizId -> Cmd msg) -> Maybe a -> DbQuizId -> Cmd msg
useOrFetchWith dft mA qid =
    Util.foldMaybe (dft qid) (always Cmd.none) mA


linkButton : String -> List (Attribute msg) -> List (Html msg) -> Html msg
linkButton link attrs children =
    Bootstrap.Button.linkButton
        [ Bootstrap.Button.primary
        , Bootstrap.Button.attrs (href link :: attrs)
        ]
        children


addFeedbackLabel : String -> Html msg
addFeedbackLabel feedback =
    div [ id "feedbackArea" ]
        [ label [ for "feedbackLabel" ] [ text feedback ] ]


errorToString : Http.Error -> String
errorToString err =
    case err of
        BadUrl url ->
            String.concat [ "Bad URL: ", url ]

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadStatus s ->
            String.concat [ "Bad status: ", String.fromInt s ]

        BadBody str ->
            str


mkPlacementTables : RankingsWithSorted -> Labels -> List (Html msg)
mkPlacementTables rankings labels =
    [ mkPlacementsHtml rankings.cumulative labels.placementLabel labels.placeLabel labels.pointsLabel
    , mkRoundWinners rankings.perRound labels.roundWinnerLabel labels.roundLabel labels.pointsLabel
    ]


type alias Placements =
    { perRound : List TeamsRanking
    , overall : List TeamsRanking
    }


mkPlacement : RoundRankings -> List TeamsRanking
mkPlacement roundRankings =
    let
        zeroRating =
            ( 1, { teamNumber = 1, rating = 0 } )

        findCurrent =
            .teamRatings >> maximumBy Tuple.first >> Maybe.withDefault zeroRating >> Tuple.second

        currentRanking =
            List.map (\perTeam -> { teamName = perTeam.teamName, teamRating = findCurrent perTeam }) roundRankings

        placement =
            rankingToPlacement currentRanking
    in
    placement


mkPlacementsHtml : RoundRankings -> String -> String -> String -> Html msg
mkPlacementsHtml rrs wordForPlacement wordForPlace wordForPoints =
    div [ id "placements" ]
        [ label [ for "placementsLabel" ] [ text wordForPlacement ]
        , table [ id "placementsTable" ]
            (List.map (mkPlacementsTableLine wordForPlace wordForPoints) (mkPlacement rrs))
        ]


mkPlacementsTableLine : String -> String -> TeamsRanking -> Html msg
mkPlacementsTableLine wordForPlace wordForPoints teamsRanking =
    tr []
        [ td []
            [ text
                (String.concat
                    [ String.join " "
                        [ wordForPlace
                        , String.fromInt teamsRanking.position
                        , String.concat [ "(", String.fromFloat teamsRanking.points, " ", wordForPoints, ")" ]
                        ]
                    , ":"
                    ]
                )
            ]
        , td []
            [ text (String.join ", " (List.map .teamName teamsRanking.teamNamesWithNumbers)) ]
        ]


mkRoundWinners : RoundRankings -> String -> String -> String -> Html msg
mkRoundWinners rr wordForRoundWinner wordForRound wordForPoints =
    let
        roundWinners =
            roundRankingsToRoundWinners rr
    in
    div [ id "roundWinners" ]
        [ label [ for "roundWinnersLabel" ] [ text wordForRoundWinner ]
        , table [ id "roundWinnersTable" ]
            (List.map (mkRoundWinnersTableLine wordForRound wordForPoints) roundWinners)
        ]


mkRoundWinnersTableLine : String -> String -> RoundWinner -> Html msg
mkRoundWinnersTableLine wordForRound wordForPoints rw =
    tr []
        [ td []
            [ text
                (String.concat
                    [ wordForRound
                    , " "
                    , String.fromInt rw.roundNumber
                    , " ("
                    , String.fromFloat rw.points
                    , " "
                    , wordForPoints
                    , "):"
                    ]
                )
            ]
        , td [] [ text (String.join ", " rw.teamNames) ]
        ]


loadingSymbol : Html msg
loadingSymbol =
    Loading.render Loading.Spinner Loading.defaultConfig Loading.On


mkTeamQueryLink : TeamQuery -> String
mkTeamQueryLink teamQuery =
    fragmentUrl
        [ quizIdParam
        , String.fromInt teamQuery.teamQueryQuizId
        , teamNumberParam
        , String.fromInt teamQuery.teamQueryTeamNumber
        , teamCodeParam
        , teamQuery.teamQueryTeamCode
        ]
