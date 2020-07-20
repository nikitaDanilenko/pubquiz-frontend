module Common.NumberInputs.RoundRatingInput exposing (..)

import Basics.Extra exposing (flip)
import Common.FromInput as FromInput exposing (FromInput)
import Common.NumberInputs.TeamRatingInput as TeamRatingInput exposing (TeamRatingInput, fromTeamRating, toTeamRating)
import Common.NumberInputs.Util exposing (pointsFromInput)
import Common.Types exposing (RoundRating, TeamNumber)
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
    { reachableInRound = pointsFromInput rr.reachableInRound
    , points = rr.points |> List.map fromTeamRating
    }


updateReachableInRound : RoundRatingInput -> String -> RoundRatingInput
updateReachableInRound roundRating reachableInRound =
    { roundRating | reachableInRound = FromInput.updateText roundRating.reachableInRound reachableInRound }

updatePoints: RoundRatingInput -> TeamNumber -> String -> RoundRatingInput
updatePoints rri teamNumber rating =
  {rri | points = updateIf (\tri -> tri.teamNumber == teamNumber) (flip TeamRatingInput.updateRating rating) rri.points}