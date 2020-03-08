module Common.Ranking exposing (..)

import Common.Types exposing (Header, Ratings, RoundNumber, TeamName, TeamNumber, TeamRating)
import Common.Util as Util
import List.Extra exposing (maximumBy, scanl, transpose)


type alias RoundRankingPerTeam =
    { teamNumber : TeamNumber
    , teamName : TeamName
    , teamRatings : List ( RoundNumber, TeamRating )
    }


roundRankingPerTeamToPointsPerTeam : RoundRankingPerTeam -> List Float
roundRankingPerTeamToPointsPerTeam =
    .teamRatings >> List.sortBy (Tuple.second >> .teamNumber) >> List.map (Tuple.second >> .rating)


type alias RoundRankings =
    List RoundRankingPerTeam


ratingsToRankings : Ratings -> Header -> { sortedRatings : Ratings, perRound : RoundRankings, cumulative : RoundRankings }
ratingsToRankings ratings header =
    let
        sortedRatings =
            ratings
                |> List.sortBy Tuple.first
                |> List.map (Tuple.mapSecond (\tr -> { tr | points = List.sortBy .teamNumber tr.points }))

        sortedHeader =
            List.sortBy .teamInfoNumber header

        rearranged =
            transpose (List.map (\( rn, rat ) -> rat.points |> List.sortBy .teamNumber |> List.map (\x -> ( rn, x ))) sortedRatings)

        -- We process the header twice for simplicity of representation.
        -- It is possible to achieve the same result with only one traversal,
        -- but the corresponding steps are more technical.
        roundRankings =
            List.map2 (\l ti -> { teamNumber = ti.teamInfoNumber, teamName = ti.teamInfoName, teamRatings = l }) rearranged sortedHeader

        cumulativeRankings =
            List.map2
                (\l ti ->
                    { teamNumber = ti.teamInfoNumber
                    , teamName = ti.teamInfoName
                    , teamRatings =
                        l
                            |> scanl (\( rn, nextTr ) ( _, current ) -> ( rn, addNamedTeamRatings nextTr current )) ( 1, defaultTeamRating )
                            |> List.drop 1
                    }
                )
                rearranged
                sortedHeader
    in
    { sortedRatings = sortedRatings, perRound = roundRankings, cumulative = cumulativeRankings }


type alias RoundWinner =
    { roundNumber : RoundNumber
    , teamNames : List TeamName
    , points : Float
    }


type alias NamedTeamRating =
    { teamRating : TeamRating
    , teamName : TeamName
    }


defaultTeamRating : TeamRating
defaultTeamRating =
    { rating = 0, teamNumber = 1 }


addNamedTeamRatings : TeamRating -> TeamRating -> TeamRating
addNamedTeamRatings tr1 tr2 =
    { tr1 | rating = tr1.rating + tr2.rating }


type alias TeamsRanking =
    { position : Int
    , points : Float
    , teamNames : List TeamName
    }


roundRankingsToRoundWinners : RoundRankings -> List RoundWinner
roundRankingsToRoundWinners rankings =
    let
        roundInside =
            List.map (\perTeam -> List.map (\( rn, r ) -> { roundNumber = rn, teamName = perTeam.teamName, teamRating = r }) perTeam.teamRatings) rankings

        rearranged =
            transpose roundInside

        findWinner : List { roundNumber : RoundNumber, teamName : TeamName, teamRating : TeamRating } -> RoundWinner
        findWinner ratings =
            let
                max =
                    ratings |> maximumBy (.teamRating >> .rating) |> Maybe.withDefault { roundNumber = 1, teamName = "", teamRating = defaultTeamRating }

                maxPoints =
                    max.teamRating.rating
            in
            { points = maxPoints
            , teamNames = ratings |> List.filter (\t -> t.teamRating.rating == maxPoints) |> List.map .teamName
            , roundNumber = max.roundNumber
            }
    in
    List.map findWinner rearranged


rankingToPlacement : List NamedTeamRating -> List TeamsRanking
rankingToPlacement numberedTeamRatings =
    let
        rating =
            .teamRating >> .rating

        pointsOf =
            List.head >> Util.foldMaybe 0 rating

        placed =
            numberedTeamRatings
                |> List.sortBy rating
                |> List.reverse
                |> Util.groupBy (\x y -> rating x == rating y)
                |> List.indexedMap (\i cs -> { position = 1 + i, points = pointsOf cs, teamNames = List.map .teamName cs })
    in
    placed
