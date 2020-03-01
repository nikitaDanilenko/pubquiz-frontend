module Common.Util exposing ( .. )

import Common.Constants exposing (quizIdParam)
import Common.Types exposing (DbQuizId, TeamRating, jsonEncDbQuizId)
import Http exposing (Error)
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder exposing (string)

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

{- Fills a list with zeroes in the back if the list is not long enough, 
   otherwise return the prefix of the list with the given length. -}
adjustToSize : Int -> List TeamRating -> List TeamRating
adjustToSize n = adjustToSizeWith (List.indexedMap (\i r -> {teamNumber = i, rating = r}) (List.repeat n 0))

adjustToSizeWith : List a -> List a -> List a
adjustToSizeWith dft lst =
    let combine : List a -> List a -> List a
        combine l r = 
            case (l, r) of
                ([], rest) -> rest
                (_, []) -> []
                (x :: xs, _ :: ys) -> x :: combine xs ys
    in combine lst dft

updateIndex : Int -> a -> List a -> List a
updateIndex i y = List.indexedMap (\j x -> if i == j then y else x)

isValidQuizName : String -> Bool
isValidQuizName n = not (String.isEmpty n)

find : (a -> Bool) -> List a -> Maybe a
find p l = case l of
  [] -> Nothing
  (x :: xs) -> if p x then Just x else find p xs

getMsg : String -> (Result Error a -> msg) -> Decode.Decoder a -> DbQuizId -> Cmd msg
getMsg path action decoder quizId =
    Http.get
        { url = Url.Builder.relative [ path ] [ string quizIdParam (Encode.encode 0 (jsonEncDbQuizId quizId)) ]
        , expect = Http.expectJson action decoder
        }

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y