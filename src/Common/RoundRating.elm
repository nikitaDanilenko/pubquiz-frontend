module Common.RoundRating exposing (..)

import Common.Types exposing (RoundRating, TeamNumber)
import Common.Util exposing (adjustToSize)


empty : RoundRating
empty =
    { reachableInRound = 0, points = [] }


emptyOfSize : Int -> RoundRating
emptyOfSize n =
    { reachableInRound = 0
    , points = List.map (\i -> { teamNumber = i, rating = 0 }) (List.range 1 n)
    }


adjustTo : Int -> RoundRating -> RoundRating
adjustTo n rr =
    { rr | points = adjustToSize n rr.points }


arePointsValid : RoundRating -> Bool
arePointsValid rr =
    List.all (\x -> x.rating <= rr.reachableInRound) rr.points


update : TeamNumber -> Float -> RoundRating -> RoundRating
update tn ps rr =
    let
        newPoints =
            List.map (\tr -> if tr.teamNumber == tn then { tr | rating = ps } else tr) rr.points
    in
    { rr | points = newPoints }
