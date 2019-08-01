module Quiz exposing ( .. )

import Parser exposing ( succeed, sequence, Trailing ( .. ), Parser, (|.), DeadEnd,
                         (|=), end, run, symbol, chompWhile, oneOf, variable, getChompedString,
                         chompIf )
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
update round team points quiz =
  let change : Int -> Round -> Round
      change i r = if i == round then Round.update team points r else r
      
      updatedRounds = List.indexedMap change quiz.rounds
  in { quiz | rounds = updatedRounds }

updateMax : Int -> Float -> Quiz -> Quiz
updateMax rd m quiz =
  let updatedRounds = List.indexedMap (\i r -> if i == rd then { r | maxPoints = m } else r) 
                                      quiz.rounds
  in { quiz | rounds = updatedRounds }

getRound : Int -> Quiz -> Round
getRound n q = Maybe.withDefault Round.empty (List.head (List.drop n q.rounds))

arePointsValid : Quiz -> Bool
arePointsValid q = List.all Round.arePointsValid q.rounds

updateTeamName : Int -> String -> Quiz -> Quiz
updateTeamName i newName quiz =
  let inTWON : String -> TeamWithOptionalName -> TeamWithOptionalName
      inTWON name (code, _) = (code, Just name)

      inHeader : Int -> String -> Header -> Header
      inHeader pos name =  
        List.indexedMap (\k twon -> if k == pos then inTWON name twon else twon) 
  in { quiz | header = inHeader i newName quiz.header }

addRound : Round -> Quiz -> Quiz
addRound r q = { q | rounds = q.rounds ++ [r] }

maxNumberOfTeams : Quiz -> Int
maxNumberOfTeams quiz = List.length quiz.header

numberOfTeams : Quiz -> Int
numberOfTeams quiz = 
  let max = maxNumberOfTeams quiz
  in Maybe.withDefault max (List.maximum (List.map (\r -> List.length r.teamPoints) quiz.rounds))

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
alphaNumericParser = parseIf Char.isAlphaNum

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

parseIf : (Char -> Bool) -> Parser String
parseIf p = getChompedString (chompIf p)

parseWhile : (Char -> Bool) -> Parser String
parseWhile p = getChompedString (chompWhile p)

separatorParser : Parser String
separatorParser = parseIf (\c -> c == teamNameSeparator)

anyNonSeparatorParser : Parser String
anyNonSeparatorParser = parseWhile notSeparator

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
    |= anyNonSeparatorParser
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