module RoundRating exposing (..)

import Types exposing (RoundRating)
import Util exposing (adjustToSize, updateIndex)


empty : RoundRating
empty =
    { reachableInRound = 0, points = [] }


emptyOfSize : Int -> RoundRating
emptyOfSize n =
    { reachableInRound = 0
    , points = List.indexedMap (\i z -> { teamNumber = i, rating = z }) (List.repeat n 0)
    }


adjustTo : Int -> RoundRating -> RoundRating
adjustTo n rr =
    { rr | points = adjustToSize n rr.points }


arePointsValid : RoundRating -> Bool
arePointsValid rr =
    List.all (\x -> x.rating <= rr.reachableInRound) rr.points


update : Int -> Float -> RoundRating -> RoundRating
update i ps rr =
    let
        newPoints =
            updateIndex i { teamNumber = i, rating = ps } rr.points
    in
    { rr | points = newPoints }
