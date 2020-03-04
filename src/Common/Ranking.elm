module Common.Ranking exposing (..)

import Common.Types exposing (RoundNumber, TeamRating)


type alias RoundRanking =
    { roundNumber : RoundNumber
    , teamRatings : List TeamRating
    }

roundRankingToPointsByTeam : RoundRanking -> List Float
roundRankingToPointsByTeam = .teamRatings >> List.sortBy .teamNumber >> List.map .rating

type alias RoundRankings = List RoundRanking
