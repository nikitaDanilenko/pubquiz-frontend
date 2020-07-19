module Input.RatingsInput exposing (..)

import Common.FromInput as FromInput exposing (FromInput)
import Common.Types exposing (Ratings, RoundNumber, RoundRating, TeamNumber, TeamRating)
import List.Extra
import Parser


type alias RatingsInput =
    List ( RoundNumber, RoundRatingInput )


type alias RoundRatingInput =
    { reachableInRound : FromInput Float
    , points : List TeamRatingInput
    }


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


toRatings : RatingsInput -> Ratings
toRatings =
    List.map (Tuple.mapSecond toRoundRating)


fromRatings : Ratings -> RatingsInput
fromRatings =
    List.map (Tuple.mapSecond fromRoundRating)


parsePoints : String -> Result String Float
parsePoints string =
    let
        actualString =
            string |> String.toList |> List.Extra.dropWhile (\c -> c == '0') |> String.fromList
    in
    if String.isEmpty actualString then
        Ok 0

    else
        Parser.run Parser.float actualString |> Result.mapError (\_ -> "Not a decimal number")


partial : String -> Bool
partial str =
    List.length (String.split "." str) <= 2 && String.all (\c -> c == '.' || Char.isDigit c) str


pointsFromInput : Float -> FromInput Float
pointsFromInput value =
    FromInput.emptyText
        { ifEmptyValue = 0
        , value = value
        , parse = parsePoints
        , isPartial = partial
        }
