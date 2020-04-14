module Common.QuizRatings exposing (..)

import Basics.Extra exposing (flip)
import Common.Copy exposing (updateTeamInfoTeamName)
import Common.RoundRating as RoundRating
import Common.Types exposing (DbQuizId, Header, QuizRatings, Ratings, RoundNumber, RoundRating, TeamName, TeamNumber, TeamRating)
import Common.Util as Util
import List.Extra exposing (updateIf)


default : QuizRatings
default =
    { header = defaultHeader
    , ratings = defaultRatings
    }


updateRatings : QuizRatings -> Ratings -> QuizRatings
updateRatings quizRatings ratings =
    { quizRatings | ratings = ratings }


updateHeader : QuizRatings -> Header -> QuizRatings
updateHeader quizRatings header =
    { quizRatings | header = header }


defaultRatings : Ratings
defaultRatings =
    []


defaultHeader : Header
defaultHeader =
    []


update : QuizRatings -> RoundNumber -> TeamNumber -> Float -> QuizRatings
update quizRatings round team points =
    updateIf (\( rn, _ ) -> rn == round) (Tuple.mapSecond (RoundRating.update team points)) quizRatings.ratings
        |> updateRatings quizRatings


updateMax : RoundNumber -> Float -> QuizRatings -> QuizRatings
updateMax roundNumber maxPoints quizRatings =
    updateIf (\( rn, _ ) -> rn == roundNumber) (Tuple.mapSecond (flip RoundRating.updateReachableInRound maxPoints)) quizRatings.ratings
        |> updateRatings quizRatings


getRound : RoundNumber -> QuizRatings -> RoundRating
getRound n q =
    Util.foldMaybe RoundRating.empty Tuple.second (List.Extra.find (\( tn, _ ) -> tn == n) q.ratings)


updateTeamName : TeamNumber -> TeamName -> QuizRatings -> QuizRatings
updateTeamName teamNumber teamName quizRatings =
    quizRatings.header
        |> updateIf (\teamInfo -> teamInfo.teamInfoNumber == teamNumber) (flip updateTeamInfoTeamName teamName)
        |> updateHeader quizRatings


addRound : RoundRating -> QuizRatings -> QuizRatings
addRound r q =
    let
        rNum =
            1 + Maybe.withDefault 0 (List.maximum (List.map Tuple.first q.ratings))
    in
    { q | ratings = q.ratings ++ [ ( rNum, r ) ] }


cumulative : QuizRatings -> QuizRatings
cumulative quizRatings =
    let
        addTeamRatings : List TeamRating -> List TeamRating -> List TeamRating
        addTeamRatings =
            Util.intersectWith (\x y -> { teamNumber = x.teamNumber, rating = x.rating + y.rating })
                .teamNumber
                identity
                .teamNumber
                identity

        addRoundRatings : RoundRating -> RoundRating -> RoundRating
        addRoundRatings x y =
            { reachableInRound = x.reachableInRound + y.reachableInRound
            , points = addTeamRatings x.points y.points
            }
    in
    { quizRatings
        | ratings =
            quizRatings.ratings
                |> List.sortBy Tuple.first
                |> List.map
                    (Tuple.mapSecond
                        (\roundRating ->
                            { roundRating
                                | points =
                                    roundRating.points |> List.sortBy .teamNumber
                            }
                        )
                    )
                |> List.Extra.scanl
                    (\( roundNumber, rating ) ( _, ratingAcc ) -> ( roundNumber, addRoundRatings rating ratingAcc ))
                    ( 0, RoundRating.emptyForHeader quizRatings.header )
                |> List.drop 1
    }
