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
import Common.NumberInputs.Util exposing (pointsFromInput, pointsFromInputWith)
import Common.Types exposing (Header, RoundRating, TeamNumber)
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
    { reachableInRound = pointsFromInputWith (always True) rr.reachableInRound
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
    { rri | points = updateIf (\tri -> tri.teamNumber == teamNumber) (flip TeamRatingInput.updateRating rating) rri.points }


emptyForHeader : Header -> RoundRatingInput
emptyForHeader header =
    { reachableInRound = pointsFromInput 0 0
    , points = List.map (.teamInfoNumber >> TeamRatingInput.zeroTeamRating) header
    }
