module Common.NumberInputs.Util exposing (..)

import Common.FromInput exposing (FromInput)
import List.Extra
import Parser


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


pointsFromInput : Float -> Float -> FromInput Float
pointsFromInput maxValue =
    pointsFromInputWith (nonNegativeMax maxValue)


pointsFromInputWith : (Float -> Bool) -> Float -> FromInput Float
pointsFromInputWith check value =
    { ifEmptyValue = 0
    , value = value
    , text = String.fromFloat value
    , parse = parsePoints
    , partial = partial
    , check = check
    }


nonNegativeMax : Float -> Float -> Bool
nonNegativeMax maxValue x =
    x >= 0 && x <= maxValue

atLeast : Float -> Float -> Bool
atLeast minValue x = x >= minValue
