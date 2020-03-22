module Common.WireUtil exposing
    ( RestKey
    , RestParam
    , RestValue
    , SessionKey
    , User
    , addFeedbackLabel
    , encodeBody
    , errorToString
    , getLabelsWith
    , getQuizInfoWith
    , getQuizRatingsWith
    , linkButton
    , mkJSONParams
    , mkParams
    , mkPlacementTables
    , useOrFetchWith
    )

import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi)
import Common.Ranking exposing (RankingsWithSorted, RoundRankings, RoundWinner, TeamsRanking, rankingToPlacement, roundRankingsToRoundWinners)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, Ratings, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings)
import Common.Util as Util exposing (ErrorOr, getMsg)
import Html exposing (Attribute, Html, div, form, input, label, table, td, text, tr)
import Html.Attributes exposing (action, for, id, type_)
import Http exposing (Error(..))
import Json.Encode as Encode exposing (encode)
import List.Extra exposing (maximumBy)
import Url.Builder


type alias User =
    String


type alias SessionKey =
    String


type alias RestParam =
    String


type alias RestValue =
    String


type alias RestKey =
    String


mkJSONParams : List ( String, Encode.Value ) -> RestParam
mkJSONParams ps =
    ps |> List.map (\( k, v ) -> ( k, encode 0 v )) |> mkParams


mkParams : List ( RestKey, RestValue ) -> RestParam
mkParams kvs =
    let
        done =
            Url.Builder.relative [] (List.map (\( k, v ) -> Url.Builder.string k v) kvs)
    in
    String.dropLeft 1 done


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


encodeBody : String -> Http.Body
encodeBody =
    Http.stringBody "application/x-www-form-urlencoded"


linkButton : String -> List (Attribute msg) -> List (Html msg) -> Html msg
linkButton link attrs children =
    form [ action link ]
        [ input (type_ "submit" :: attrs) children ]


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
            String.concat [ "Bad body: ", str ]


mkPlacementTables : RankingsWithSorted -> Labels -> List (Html msg)
mkPlacementTables rankings labels =
    [ mkPlacements rankings.cumulative labels.placementLabel labels.placeLabel labels.pointsLabel
    , mkRoundWinners rankings.perRound labels.roundWinnerLabel labels.roundLabel labels.pointsLabel
    ]


mkPlacements : RoundRankings -> String -> String -> String -> Html msg
mkPlacements rrs wordForPlacement wordForPlace wordForPoints =
    let
        zeroRating =
            ( 1, { teamNumber = 1, rating = 0 } )

        findCurrent =
            .teamRatings >> maximumBy Tuple.first >> Maybe.withDefault zeroRating >> Tuple.second

        currentRanking =
            List.map (\perTeam -> { teamName = perTeam.teamName, teamRating = findCurrent perTeam }) rrs

        placement =
            rankingToPlacement currentRanking
    in
    div [ id "placements" ]
        [ label [ for "placementsLabel" ] [ text wordForPlacement ]
        , table [ id "placementsTable" ]
            (List.map (mkPlacementsTableLine wordForPlace wordForPoints) placement)
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
            [ text (String.join ", " teamsRanking.teamNames) ]
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
