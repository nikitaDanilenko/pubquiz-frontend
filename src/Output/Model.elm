module Output.Model exposing (..)

import Common.Types exposing (Activity(..), Code, DbQuizId, Labels, QuizIdentifier, QuizInfo, QuizRatings, TeamNumber, TeamQuery, TeamTable)
import Input.Model exposing (ErrorOr)



--todo: extract labels


type alias Model =
    { teamQuery : TeamQuery
    , labels : Labels
    , subModel : SubModel
    }


type SubModel
    = TableModel TeamTable QuizInfo
    | QuizModel QuizRatings QuizInfo
    | AllModel (List QuizInfo)


type QuizModelKind
    = Current
    | Other


titleFor : Model -> String
titleFor model =
    case model.subModel of
        TableModel _ quizInfo ->
            String.join " - " [ mkFullQuizName quizInfo.quizIdentifier, model.labels.ownPointsLabel ]

        QuizModel _ quizInfo ->
            String.join " - " [ mkFullQuizName quizInfo.quizIdentifier, model.labels.backToChartView ]

        AllModel _ ->
            model.labels.viewPrevious


mkFullQuizName : QuizIdentifier -> String
mkFullQuizName idf =
    String.join " "
        [ String.concat [ idf.date, ":" ]
        , idf.name
        , String.concat [ "(", idf.place, ")" ]
        ]


type Msg
    = GetQuizRatings QuizInfo
    | GotQuizRatings QuizInfo (ErrorOr QuizRatings)
    | GetTeamTable
    | GotTeamTable (ErrorOr TeamTable)
    | GetAllQuizzes
    | GotAllQuizzes (ErrorOr (List QuizInfo))


initialModelFunction : () -> ( Model, Cmd Msg )
initialModelFunction _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
  let quizInfo =  Input.Model.defaultQuizInfo
  in  Model { teamQueryQuizId = 1, teamQueryTeamNumber = 1, teamQueryTeamCode = "d1215d" } Input.Model.defaultLabels (QuizModel testRatings { quizInfo | quizId = 1 })


testRatings : QuizRatings
testRatings =
    { header =
        [ { teamInfoName = "G1", teamInfoCode = "", teamInfoNumber = 1, teamInfoActivity = Active }
        , { teamInfoName = "Gruppe 2", teamInfoCode = "", teamInfoNumber = 2, teamInfoActivity = Active }
        ]
    , ratings =
        [ ( 1, { reachableInRound = 8, points = [ { teamNumber = 1, rating = 2 }, { teamNumber = 2, rating = 5 } ] } )
        , ( 2, { reachableInRound = 9, points = [ { teamNumber = 1, rating = 7 }, { teamNumber = 2, rating = 3 } ] } )
        , ( 2, { reachableInRound = 10, points = [ { teamNumber = 1, rating = 4 }, { teamNumber = 2, rating = 6 } ] } )
        ]
    }
