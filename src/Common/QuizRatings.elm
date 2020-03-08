module Common.QuizRatings exposing (..)

import Common.RoundRating as RoundRating
import Common.Types exposing (DbQuizId, Header, QuizRatings, Ratings, RoundNumber, RoundRating, TeamName, TeamNumber)
import Common.Util as Util


empty : QuizRatings
empty =
    { header = defaultHeader
    , ratings = defaultRatings
    }


defaultRatings : Ratings
defaultRatings =
    []


defaultHeader : Header
defaultHeader =
    []


adjustTo : Int -> QuizRatings -> QuizRatings
adjustTo n qr =
    { qr | ratings = List.map (\( rn, rr ) -> ( rn, RoundRating.adjustTo n rr )) qr.ratings }


update : RoundNumber -> TeamNumber -> Float -> QuizRatings -> QuizRatings
update round team points quiz =
    let
        change : RoundNumber -> RoundRating -> RoundRating
        change rn rr =
            if rn == round then
                RoundRating.update team points rr

            else
                rr

        updatedRatings =
            List.map (\( rn, rr ) -> ( rn, change rn rr )) quiz.ratings
    in
    { quiz | ratings = updatedRatings }


updateMax : Int -> Float -> QuizRatings -> QuizRatings
updateMax rd m quiz =
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
                quiz.ratings
    in
    { quiz | ratings = updatedRatings }


getRound : RoundNumber -> QuizRatings -> RoundRating
getRound n q =
    Util.foldMaybe RoundRating.empty Tuple.second (Util.find (\( tn, _ ) -> tn == n) q.ratings)


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

