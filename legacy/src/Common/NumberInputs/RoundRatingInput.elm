module Common.NumberInputs.RoundRatingInput exposing
    ( RoundRatingInput
    , emptyForHeader
    , fromRoundRating
    , toRoundRating
    , updatePoints
    , updateReachableInRound
    )

import Basics.Extra exposing (flip)
import Common.FromInput as FromInput exposing (FromInput)
import Common.NumberInputs.TeamRatingInput as TeamRatingInput exposing (TeamRatingInput, fromTeamRating, toTeamRating)
import Common.NumberInputs.Util as Util exposing (pointsFromInputWith)
import Common.Types exposing (Header, RoundRating, TeamNumber)
import Common.Util
import List.Extra exposing (updateIf)


type alias RoundRatingInput =
    { reachableInRound : FromInput Float
    , points : List TeamRatingInput
    }


toRoundRating : RoundRatingInput -> RoundRating
toRoundRating rri =
    { reachableInRound = rri.reachableInRound.value
    , points = rri.points |> List.map toTeamRating
    }


fromRoundRating : RoundRating -> RoundRatingInput
fromRoundRating rr =
    let
        minValue =
            maximumWith .rating rr.points
    in
    { reachableInRound = pointsFromInputWith (Util.atLeast minValue) rr.reachableInRound
    , points = rr.points |> List.map (fromTeamRating rr.reachableInRound)
    }


updateReachableInRound : RoundRatingInput -> String -> RoundRatingInput
updateReachableInRound roundRating reachableInRound =
    FromInput.lift updateReachableInRoundOnly roundRating.reachableInRound reachableInRound roundRating


updateReachableInRoundOnly : RoundRatingInput -> FromInput Float -> RoundRatingInput
updateReachableInRoundOnly rri reachableInRound =
    { rri
        | reachableInRound = reachableInRound
        , points = List.map (flip TeamRatingInput.updateMaxPoints reachableInRound.value) rri.points
    }


updatePoints : RoundRatingInput -> TeamNumber -> String -> RoundRatingInput
updatePoints rri teamNumber rating =
    rri
        |> (\input -> updatePointsOnly input teamNumber rating)
        |> (\input ->
                let
                    minValue =
                        maximumWith (.rating >> .value) input.points
                in
                { input | reachableInRound = FromInput.updateCheck input.reachableInRound (Util.atLeast minValue) }
           )


updatePointsOnly : RoundRatingInput -> TeamNumber -> String -> RoundRatingInput
updatePointsOnly rri teamNumber rating =
    { rri | points = updateIf (\tri -> tri.teamNumber == teamNumber) (flip TeamRatingInput.updateRating rating) rri.points }


emptyForHeader : Header -> RoundRatingInput
emptyForHeader header =
    { reachableInRound = pointsFromInputWith (always True) 0
    , points = List.map (.teamInfoNumber >> TeamRatingInput.zeroTeamRating) header
    }


maximumWith : (a -> Float) -> List a -> Float
maximumWith floatValue xs =
    xs |> List.Extra.maximumBy floatValue |> Common.Util.foldMaybe 0 floatValue
