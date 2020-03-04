module Common.Ranking exposing (..)

import Common.Types exposing (Ratings, RoundNumber, TeamName, TeamRating)
import List.Extra exposing (scanl, transpose)


type alias RoundRanking =
    { roundNumber : RoundNumber
    , teamRatings : List TeamRating
    }


roundRankingToPointsByTeam : RoundRanking -> List Float
roundRankingToPointsByTeam =
    .teamRatings >> List.sortBy .teamNumber >> List.map .rating


type alias RoundRankings =
    List RoundRanking


ratingsToRankings : Ratings -> { sortedRatings : Ratings, perRound : RoundRankings, cumulative : RoundRankings }
ratingsToRankings ratings =
    let
        sortedRatings =
            List.sortBy Tuple.first ratings

        rearranged =
            transpose (List.map (\( rn, rat ) -> rat.points |> List.sortBy .teamNumber |> List.map (\x -> ( rn, x ))) sortedRatings)

        roundRankings =
            List.indexedMap (\i l -> { roundNumber = 1 + i, teamRatings = List.map Tuple.second l }) rearranged

        cumulativeRankings =
            List.indexedMap
                (\i l ->
                    { roundNumber = 1 + i
                    , teamRatings =
                        l
                            |> scanl (\( _, nextTr ) current -> current + nextTr.rating) 0
                            |> List.drop 1
                            |> List.indexedMap (\tn r -> { teamNumber = tn, rating = r })
                    }
                )
                rearranged
    in
    { sortedRatings = sortedRatings, perRound = roundRankings, cumulative = cumulativeRankings }

type alias RoundWinner = {
  roundNumber : RoundNumber,
  teamName : TeamName,
  points : Float
 }

--rankingsToRoundWinners : RoundRanking -> List RoundWinner
--rankingsToRoundWinners =

-- todo: This function should also produce the team names.
-- Assuming that the ranking contains sorted teamRatings by team,
-- we can get the team names by zipping (smartly) with the header of the quiz.
rankingToPlacement : RoundRanking -> (RoundNumber, List (Int, TeamRating))
rankingToPlacement rr = (rr.roundNumber, rr.teamRatings |> List.sortBy .rating |> List.indexedMap (\i p -> (1 + i, p)))