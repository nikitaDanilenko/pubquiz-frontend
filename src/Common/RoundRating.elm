module Common.RoundRating exposing (..)

import Basics.Extra exposing (flip)
import Common.Copy exposing (updateTeamRatingPoints)
import Common.Types exposing (RoundRating, TeamNumber, TeamRating)
import List.Extra exposing (updateIf)


empty : RoundRating
empty =
    { reachableInRound = 0, points = [] }


updateReachableInRound : RoundRating -> Float -> RoundRating
updateReachableInRound roundRating reachableInRound =
    { roundRating | reachableInRound = reachableInRound }


updatePoints : RoundRating -> List TeamRating -> RoundRating
updatePoints roundRating points =
    { roundRating | points = points }


emptyOfSize : Int -> RoundRating
emptyOfSize n =
    { reachableInRound = 0
    , points = List.map (\i -> { teamNumber = i, rating = 0 }) (List.range 1 n)
    }


update : TeamNumber -> Float -> RoundRating -> RoundRating
update tn ps rr =
    updateIf (\teamRating -> teamRating.teamNumber == tn) (flip updateTeamRatingPoints ps) rr.points
        |> updatePoints rr
