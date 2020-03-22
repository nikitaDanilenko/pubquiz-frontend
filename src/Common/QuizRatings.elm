module Common.QuizRatings exposing (..)

import Basics.Extra exposing (flip)
import Common.Copy exposing (updateTeamInfoTeamName)
import Common.RoundRating as RoundRating
import Common.Types exposing (DbQuizId, Header, QuizRatings, Ratings, RoundNumber, RoundRating, TeamName, TeamNumber)
import Common.Util as Util
import List.Extra exposing (updateIf)


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


-- todo: check empty team names


updateTeamName : TeamNumber -> TeamName -> QuizRatings -> QuizRatings
updateTeamName teamNumber teamName quizRatings =
    quizRatings.header
        |> updateIf (\teamInfo -> teamInfo.teamInfoNumber == teamNumber) (flip updateTeamInfoTeamName teamName)
        |> updateQuizRatingsHeader quizRatings


addRound : RoundRating -> QuizRatings -> QuizRatings
addRound r q =
    let
        rNum =
            1 + Maybe.withDefault 0 (List.maximum (List.map Tuple.first q.ratings))
    in
    { q | ratings = q.ratings ++ [ ( rNum, r ) ] }