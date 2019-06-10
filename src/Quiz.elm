module Quiz exposing ( .. )

import Parser exposing ( succeed, sequence, Trailing ( .. ), Parser, (|.), DeadEnd,
                         (|=), end, run )

import Round exposing  ( Round, isValidRound, roundParser )
import Util exposing   ( splitFirstLast, isParserSuccess, blanks )

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
headerToString quiz = String.join " " quiz.header

roundsToStrings : Quiz -> List String
roundsToStrings quiz = List.map Round.toString quiz.rounds

updateHeader : String -> Quiz -> Quiz
updateHeader text q = { q | header = String.words text }

parseQuiz : String -> Result (List DeadEnd) Quiz
parseQuiz text = 
    let (header, rs) = splitFirstLast text
    in run (quizParser header) (String.replace "," "." (String.join "\n" rs))

quizParser : String -> Parser Quiz
quizParser header = succeed (Quiz (String.words header))
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