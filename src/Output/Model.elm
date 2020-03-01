module Output.Model exposing (..)

import Common.Types exposing (Activity(..), Code, DbQuizId, Labels, QuizIdentifier, QuizInfo, QuizRatings, TeamNumber, TeamQuery, TeamTable)
import Input.Model exposing (ErrorOr)

--todo: extract labels
type Model
    = TableModel TeamTable QuizInfo Labels
    | QuizModel QuizRatings QuizInfo QuizModelKind Labels
    | AllModel (List QuizInfo) Labels

type QuizModelKind = Current TeamQuery | Other

titleFor : Model -> String
titleFor model =
    case model of
        TableModel _ quizInfo labels ->
            String.join " - " [ mkFullQuizName quizInfo.quizIdentifier, labels.ownPointsLabel ]

        QuizModel _ quizInfo _ labels ->
            String.join " - " [ mkFullQuizName quizInfo.quizIdentifier, labels.backToChartView ]

        AllModel _ labels ->
            labels.viewPrevious


mkFullQuizName : QuizIdentifier -> String
mkFullQuizName idf =
    String.join " "
        [ String.concat [ idf.date, ":" ]
        , idf.name
        , String.concat [ "(", idf.place, ")" ]
        ]


type Msg
    = GetQuizRatings DbQuizId
    | GotQuizRatings (ErrorOr QuizRatings)
    | GetTeamTable TeamQuery
    | GotTeamTable (ErrorOr TeamTable)
    | GetAllQuizzes
    | GotAllQuizzes (ErrorOr (List QuizInfo))


initialModelFunction : () -> ( Model, Cmd Msg )
initialModelFunction _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    QuizModel testRatings Input.Model.defaultQuizInfo (Current { teamQueryQuizId = 1, teamQueryTeamNumber = 1, teamQueryTeamCode = "" }) Input.Model.defaultLabels

testRatings : QuizRatings
testRatings = {
  header = [{teamInfoName = "G1", teamInfoCode = "", teamInfoNumber = 1, teamInfoActivity = Active},
            {teamInfoName = "Gruppe 2", teamInfoCode = "", teamInfoNumber = 2, teamInfoActivity = Active}],
  ratings = [(1, {reachableInRound = 8, points = [{ teamNumber = 1, rating = 2 }, { teamNumber = 2, rating = 5 }]}),
             (2, {reachableInRound = 9, points = [{ teamNumber = 1, rating = 7 }, { teamNumber = 2, rating = 3 }]}),
             (2, {reachableInRound = 10, points = [{ teamNumber = 1, rating = 4 }, { teamNumber = 2, rating = 6 }]})
            ]
  }
