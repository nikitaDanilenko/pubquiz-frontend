module Common.NumberInputs.Util exposing (..)

import Common.FromInput as FromInput exposing (FromInput)
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


pointsFromInput : Float -> FromInput Float
pointsFromInput value =
    FromInput.emptyText
        { ifEmptyValue = 0
        , value = value
        , parse = parsePoints
        , isPartial = partial
        }
