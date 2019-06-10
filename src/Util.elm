module Util exposing ( .. )

import Parser exposing ( Parser, run, chompWhile )

escapeHTML : String -> String
escapeHTML str = case String.uncons str of
    Nothing      -> ""
    Just (c, cs) -> 
        let escapedC = 
              case c of
                'ä' -> "&auml;"
                'Ä' -> "&Auml;"
                'ö' -> "&ouml;"
                'Ö' -> "&Ouml;"
                'ü' -> "&uuml;"
                'Ü' -> "&Uuml;"
                'ß' -> "&szlig;"
                _   -> String.fromChar c
        in String.concat [escapedC, escapeHTML cs]

foldMaybe : b -> (a -> b) -> Maybe a -> b
foldMaybe empty f m = Maybe.withDefault empty (Maybe.map f m)

splitFirstLast : String -> (String, List String)
splitFirstLast text = 
  case String.lines text of
    []      -> ("", [])
    l :: ls -> (l, ls)

isParserSuccess : Parser a -> String -> Bool
isParserSuccess p text = foldMaybe False (\_ -> True) (Result.toMaybe (run p text))

blanks : Parser ()
blanks = chompWhile (\c -> c == ' ' || c == '\r')