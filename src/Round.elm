module Round exposing ( .. )

import Parser exposing ( succeed, float, symbol, sequence, Trailing ( .. ), Parser, (|.),
                         (|=), end )
import Util exposing   ( isParserSuccess, blanks )

type alias Round = 
    {
        maxPoints : Float,
        teamPoints : List Float
    }

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