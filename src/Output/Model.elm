module Output.Model exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Common.Types exposing (Activity(..), Code, DbQuizId, Labels, QuizIdentifier, QuizInfo, QuizRatings, TeamNumber, TeamQuery, TeamTable, TeamTableInfo)
import Input.Model exposing (ErrorOr)
import Url exposing (Url)

type Msg
    = GetQuizRatings QuizInfo
    | GotQuizRatings QuizInfo (ErrorOr QuizRatings)
    | GetTeamTable
    | GotTeamTable (ErrorOr TeamTableInfo)
    | GetAllQuizzes
    | GotAllQuizzes (ErrorOr (List QuizInfo))
    | ClickedLink UrlRequest
    | ChangedUrl Url
    | GetLabels DbQuizId
    | GotLabels DbQuizId (ErrorOr Labels)
    | GotQuizInfo (ErrorOr QuizInfo)


--initialModel : Model
--initialModel =
--    let
--        quizInfo =
--            Input.Model.defaultQuizInfo
--    in
--    QuizModel Input.Model.defaultLabels
--              (Just { teamQueryQuizId = 1, teamQueryTeamNumber = 1, teamQueryTeamCode = "d1215d" })
--              testRatings
--              { quizInfo | quizId = 1 }


testRatings : QuizRatings
testRatings =
    { header =
        [ { teamInfoName = "G1", teamInfoCode = "", teamInfoNumber = 1, teamInfoActivity = Active }
        , { teamInfoName = "Gruppe 2", teamInfoCode = "", teamInfoNumber = 2, teamInfoActivity = Active }
        ]
    , ratings =
        [ ( 1, { reachableInRound = 8, points = [ { teamNumber = 1, rating = 2 }, { teamNumber = 2, rating = 5 } ] } )
        , ( 2, { reachableInRound = 9, points = [ { teamNumber = 1, rating = 7 }, { teamNumber = 2, rating = 3 } ] } )
        , ( 3, { reachableInRound = 10, points = [ { teamNumber = 1, rating = 4 }, { teamNumber = 2, rating = 6 } ] } )
        ]
    }
