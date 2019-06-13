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

{- Fills a list with zeroes in the back if the list is not long enough, 
   otherwise return the prefix of the list with the given length. -}
adjustToSize : Int -> List Float -> List Float
adjustToSize n fs = 
    let fsLength = List.length fs
    in if fsLength > n then List.take n fs else fs ++ List.repeat (n - fsLength) 0

isValidInternalQuizChar : Char -> Bool
isValidInternalQuizChar c = Char.isAlphaNum c || List.member c ['_', '-']

isValidInternalQuizName : String -> Bool
isValidInternalQuizName = String.all isValidInternalQuizChar