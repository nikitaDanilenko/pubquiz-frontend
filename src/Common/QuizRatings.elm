module Common.QuizRatings exposing (..)

import Common.RoundRating as RoundRating
import Common.Types exposing (DbQuizId, Header, QuizRatings, Ratings, RoundNumber, RoundRating, TeamName, TeamNumber)
import Common.Util as Util


empty : QuizRatings
empty =
    { header = defaultHeader
    , ratings = defaultRatings
    }

updateQuizRatingsRatings : QuizRatings -> Ratings -> QuizRatings
updateQuizRatingsRatings quizRatings ratings =
    { quizRatings | ratings = ratings }


updateQuizRatingsHeader : QuizRatings -> Header -> QuizRatings
updateQuizRatingsHeader quizRatings header =
    { quizRatings | header = header }

defaultRatings : Ratings
defaultRatings =
    []


defaultHeader : Header
defaultHeader =
    []


adjustTo : Int -> QuizRatings -> QuizRatings
adjustTo n qr =
    { qr | ratings = List.map (\( rn, rr ) -> ( rn, RoundRating.adjustTo n rr )) qr.ratings }


update : QuizRatings -> RoundNumber -> TeamNumber -> Float -> QuizRatings
update quizRatings round team points =
    let
        change : RoundNumber -> RoundRating -> RoundRating
        change rn rr =
            if rn == round then
                RoundRating.update team points rr

            else
                rr

        updatedRatings =
            List.map (\( rn, rr ) -> ( rn, change rn rr )) quizRatings.ratings
    in
    updateQuizRatingsRatings quizRatings updatedRatings


updateMax : Int -> Float -> QuizRatings -> QuizRatings
updateMax rd m quizRatings =
    let
        updatedRatings =
            List.map
                (\( rn, rr ) ->
                    ( rn
                    , if rn == rd then
                        { rr | reachableInRound = m }

                      else
                        rr
                    )
                )
                quizRatings.ratings
    in
    updateQuizRatingsRatings quizRatings updatedRatings


getRound : RoundNumber -> QuizRatings -> RoundRating
getRound n q =
    Util.foldMaybe RoundRating.empty Tuple.second (Util.find (\( tn, _ ) -> tn == n) q.ratings)



-- todo: check necessity


arePointsValid : QuizRatings -> Bool
arePointsValid q =
    List.all (\p -> p |> Tuple.second |> RoundRating.arePointsValid) q.ratings



-- todo: check empty team names


updateTeamName : TeamNumber -> TeamName -> QuizRatings -> QuizRatings
updateTeamName tNum tName quiz =
    let
        inHeader : TeamNumber -> TeamName -> Header -> Header
        inHeader pos name =
            List.map
                (\ti ->
                    if ti.teamInfoNumber == pos then
                        { ti | teamInfoName = name }

                    else
                        ti
                )
    in
    { quiz | header = inHeader tNum tName quiz.header }


addRound : RoundRating -> QuizRatings -> QuizRatings
addRound r q =
    let
        rNum =
            1 + Maybe.withDefault 0 (List.maximum (List.map Tuple.first q.ratings))
    in
    { q | ratings = q.ratings ++ [ ( rNum, r ) ] }


maxNumberOfTeams : QuizRatings -> Int
maxNumberOfTeams quiz =
    List.length quiz.header


numberOfTeams : QuizRatings -> Int
numberOfTeams quiz =
    let
        max =
            maxNumberOfTeams quiz
    in
    Maybe.withDefault max (List.maximum (List.map (\( _, rr ) -> List.length rr.points) quiz.ratings))
