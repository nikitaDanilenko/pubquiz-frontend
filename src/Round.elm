module Round exposing ( .. )

import Parser exposing ( succeed, spaces, float, symbol, sequence, Trailing ( .. ), Parser, (|.),
                         (|=), end )
import Util exposing   ( isParserSuccess )

type alias Round = 
    {
        maxPoints : Float,
        teamPoints : List Float
    }

toString : Round -> String
toString rd = String.join " " (String.fromFloat rd.maxPoints :: 
                               ":" :: 
                               List.map String.fromFloat rd.teamPoints
                              )

roundParser : Parser Round
roundParser = succeed Round 
                         |. spaces 
                         |= float
                         |. spaces
                         |. symbol ":"
                         |= sequence {
                              start = "",
                              separator = "",
                              end = "",
                              spaces = spaces,
                              item = float,
                              trailing = Optional
                            }
                         |. end

isValidRound : String -> Bool
isValidRound = isParserSuccess roundParser