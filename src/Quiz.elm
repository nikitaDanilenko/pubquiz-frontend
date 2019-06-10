module Quiz exposing ( .. )

import Parser exposing ( succeed, spaces, sequence, Trailing ( .. ), Parser, (|.), DeadEnd,
                         (|=), end, run )

import Round exposing  ( Round, isValidRound, roundParser )
import Util exposing   ( splitFirstLast, isParserSuccess )

type alias Quiz = 
    {
        header : List String,
        rounds : List Round
    }

empty : Quiz
empty = {
    header = [],
    rounds = []
  }

toString : Quiz -> String
toString quiz = String.join "\n" (headerToString quiz :: roundsToStrings quiz)

toEditableString : Quiz -> String
toEditableString quiz = String.join "\n" (roundsToStrings quiz)

headerToString : Quiz -> String
headerToString quiz = String.join " " quiz.header

roundsToStrings : Quiz -> List String
roundsToStrings quiz = List.map Round.toString quiz.rounds

updateHeader : String -> Quiz -> Quiz
updateHeader text q = { q | header = String.words text }

parseQuiz : String -> Result (List DeadEnd) Quiz
parseQuiz text = 
    let (header, rs) = splitFirstLast text
    in run (quizParser header) (String.join "\n" rs)

quizParser : String -> Parser Quiz
quizParser header = succeed (Quiz (String.words header))
                      |. spaces
                      |= sequence {
                           start = "", 
                           separator = "",
                           end = "",
                           spaces = spaces,
                           item = roundParser,
                           trailing = Optional
                         }
                      |. end

isValidRoundsText : String -> Bool
isValidRoundsText text = 
  let (header, rounds) = splitFirstLast text
  in List.all isValidRound rounds 