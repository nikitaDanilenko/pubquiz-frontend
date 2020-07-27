module Common.NumberInputs.TeamRatingInput exposing
    ( TeamRatingInput
    , fromTeamRating
    , toTeamRating
    , updateRating
    , zeroTeamRating
    )

import Common.FromInput as FromInput exposing (FromInput)
import Common.NumberInputs.Util exposing (pointsFromInput)
import Common.Types exposing (TeamNumber, TeamRating)


type alias TeamRatingInput =
    { teamNumber : TeamNumber
    , rating : FromInput Float
    }


toTeamRating : TeamRatingInput -> TeamRating
toTeamRating tri =
    { teamNumber = tri.teamNumber, rating = tri.rating.value }


fromTeamRating : TeamRating -> TeamRatingInput
fromTeamRating tr =
    { teamNumber = tr.teamNumber
    , rating = pointsFromInput tr.rating
    }


updateOnlyRating : TeamRatingInput -> FromInput Float -> TeamRatingInput
updateOnlyRating tri rating =
    { tri | rating = rating }


updateRating : TeamRatingInput -> String -> TeamRatingInput
updateRating tri rating =
    FromInput.lift updateOnlyRating tri.rating rating tri


zeroTeamRating : TeamNumber -> TeamRatingInput
zeroTeamRating tn =
    { teamNumber = tn, rating = pointsFromInput 0 }
