module Round exposing ( .. )

import Parser exposing ( succeed, float, symbol, sequence, Trailing ( .. ), Parser, (|.),
                         (|=), end )
import Util exposing   ( isParserSuccess, blanks, adjustToSize, foldMaybe )

type alias Round = 
    {
        maxPoints : Float,
        teamPoints : List Float
    }

empty : Round
empty = { maxPoints = 0, teamPoints = [] }

emptyOfSize : Int -> Round
emptyOfSize n = { maxPoints = 0, teamPoints = List.repeat n 0 }

adjustTo : Int -> Round -> Round
adjustTo n rd = { rd | teamPoints = adjustToSize n rd.teamPoints }

update : Int -> Float -> Round -> Round
update i ps rd = 
  let newPoints = List.indexedMap (\j p -> if i == j then ps else p) rd.teamPoints
  in { rd | teamPoints = newPoints }

toString : Round -> String
toString rd = String.join " " (String.fromFloat rd.maxPoints :: 
                               ":" :: 
                               List.map String.fromFloat rd.teamPoints
                              )

roundParser : Parser Round
roundParser = succeed Round 
                         |. blanks 
                         |= float
                         |. blanks
                         |. symbol ":"
                         |. blanks
                         |= sequence {
                              start = "",
                              separator = "",
                              end = "",
                              spaces = blanks,
                              item = float,
                              trailing = Optional
                            }

isValidRound : String -> Bool
isValidRound = isParserSuccess roundParser

areValidPoints : Round -> Bool
areValidPoints rd = foldMaybe False (\m -> m <= rd.maxPoints) (List.maximum rd.teamPoints)