module Common.NumberInputs.RatingsInput exposing (..)

import Basics.Extra exposing (flip)
import Common.NumberInputs.RoundRatingInput as RoundRatingInput exposing (RoundRatingInput, fromRoundRating, toRoundRating)
import Common.Types exposing (Ratings, RoundNumber, RoundRating, TeamNumber, TeamRating)
import List.Extra exposing (updateIf)


type alias RatingsInput =
    List ( RoundNumber, RoundRatingInput )


toRatings : RatingsInput -> Ratings
toRatings =
    List.map (Tuple.mapSecond toRoundRating)


fromRatings : Ratings -> RatingsInput
fromRatings =
    List.map (Tuple.mapSecond fromRoundRating)


updateMax : RoundNumber -> String -> RatingsInput -> RatingsInput
updateMax roundNumber maxPoints =
    updateIf (\( rn, _ ) -> rn == roundNumber) (Tuple.mapSecond (flip RoundRatingInput.updateReachableInRound maxPoints))


updatePoints : RoundNumber -> TeamNumber -> String -> RatingsInput -> RatingsInput
updatePoints roundNumber tn ps =
    updateIf (\( rn, _ ) -> rn == roundNumber) (Tuple.mapSecond (\rri -> RoundRatingInput.updatePoints rri tn ps))

addRound : RoundRatingInput -> RatingsInput -> RatingsInput
addRound r ri =
    let
        rNum =
            1 + Maybe.withDefault 0 (List.maximum (List.map Tuple.first ri))
    in
        ri ++ [ ( rNum, r ) ]