module Quiz exposing ( .. )

import Parser exposing ( succeed, sequence, Trailing ( .. ), Parser, (|.), DeadEnd,
                         (|=), end, run, symbol , chompWhile, oneOf, variable, chompIf,
                         getChompedString )
import Result exposing ( andThen )
import Set

import Round exposing  ( Round, isValidRound, roundParser )
import Util exposing   ( splitFirstLast, isParserSuccess, blanks )

type alias TeamWithOptionalName = (String, Maybe String)
type alias Header = List TeamWithOptionalName

type alias Quiz = 
    {
        header : Header,
        rounds : List Round
    }

empty : Quiz
empty = {
    header = [],
    rounds = []
  }

adjustTo : Int -> Quiz -> Quiz
adjustTo n quiz = { quiz | rounds = List.map (Round.adjustTo n) quiz.rounds }

update : Int -> Int -> Float -> Quiz -> Quiz
update round group points quiz =
  let change : Int -> Round -> Round
      change i r = if i == round then Round.update group points r else r
      
      updatedRounds = List.indexedMap change quiz.rounds
  in { quiz | rounds = updatedRounds }

updateMax : Int -> Float -> Quiz -> Quiz
updateMax rd m quiz =
  let updatedRounds = List.indexedMap (\i r -> if i == rd then { r | maxPoints = m } else r) 
                                      quiz.rounds
  in { quiz | rounds = updatedRounds }

addRound : Round -> Quiz -> Quiz
addRound r q = { q | rounds = q.rounds ++ [r] }

numberOfGroups : Quiz -> Int
numberOfGroups quiz = 
  Maybe.withDefault 0 (List.maximum (List.map (\r -> List.length r.teamPoints) quiz.rounds))

toString : Quiz -> String
toString quiz = String.join "\n" (headerToString quiz :: roundsToStrings quiz)

toEditableString : Quiz -> String
toEditableString quiz = String.join "\n" (roundsToStrings quiz)

headerToString : Quiz -> String
headerToString quiz = 
  let sep = String.fromChar teamNameSeparator

      withMaybeName : String -> Maybe String -> String
      withMaybeName c mn = case mn of
        Nothing -> c
        Just n  -> String.concat ["(", c, "|", sep, n, sep, ")"]
  in String.join " " (List.map (\(c, mn) -> withMaybeName c mn) quiz.header)

roundsToStrings : Quiz -> List String
roundsToStrings quiz = List.map Round.toString quiz.rounds

updateHeader : Header -> Quiz -> Quiz
updateHeader h q = { q | header = h }

parseQuiz : String -> Result (List DeadEnd) Quiz
parseQuiz text = 
    let (header, rs) = splitFirstLast text
        headerResult = run headerParser header
    in andThen (\h -> run (quizParser h) (String.replace "," "." (String.join "\n" rs)))
               (run headerParser header)

alphaNumericParser : Parser String
alphaNumericParser = getChompedString (chompIf Char.isAlphaNum)

codeParser : Parser String
codeParser = 
  succeed (\c cs -> String.concat (c :: cs))
    |= alphaNumericParser
    |= sequence {
        start = "",
        separator = "",
        end = "",
        spaces = chompWhile (\_ -> False),
        item = alphaNumericParser,
        trailing = Optional
       }

teamNameSeparator : Char
teamNameSeparator = '\\'

notSeparator : Char -> Bool
notSeparator c = c /= teamNameSeparator

separatorParser : Parser String
separatorParser = getChompedString (chompIf (\c -> c == teamNameSeparator))

codeWithNameParser : Parser (String, String)
codeWithNameParser = 
  succeed Tuple.pair
    |. blanks
    |. symbol "("
    |. blanks
    |= codeParser
    |. blanks
    |. symbol "|"
    |. blanks
    |. separatorParser
    |= variable {
         start = notSeparator,
         inner = notSeparator,
         reserved = Set.empty
       }
    |. separatorParser
    |. blanks
    |. symbol ")"

codeWithMaybeNameParser : Parser TeamWithOptionalName
codeWithMaybeNameParser = oneOf [
    Parser.map (\s -> (s, Nothing)) codeParser,
    Parser.map (\(c, n) -> (c, Just n)) codeWithNameParser
  ]

headerParser : Parser Header
headerParser = 
  succeed identity 
    |= sequence {
         start = "", 
         separator = " ",
         end = "",
         spaces = chompWhile (\_ -> False),
         item = codeWithMaybeNameParser,
         trailing = Optional
       } 
    |. end

quizParser : List (String, Maybe String) -> Parser Quiz
quizParser header = 
  succeed (Quiz header)
    |. blanks
    |= sequence {
         start = "", 
         separator = "\n",
         end = "",
         spaces = blanks,
         item = roundParser,
         trailing = Optional
       }
    |. end

isValidRoundsText : String -> Bool
isValidRoundsText text = 
  let (header, rounds) = splitFirstLast text
  in List.all isValidRound rounds 