module Output.Model exposing (..)

import Common.Types exposing (Code, DbQuizId, Labels, QuizIdentifier, QuizInfo, QuizRatings, TeamNumber, TeamQuery, TeamTable)
import Input.Model


type Model
    = TableModel TeamTable QuizInfo Labels
    | QuizModel QuizRatings QuizInfo Labels
    | AllModel (List QuizInfo) Labels


titleFor : Model -> String
titleFor model =
    case model of
        TableModel _ quizInfo labels ->
            String.join " - " [ mkFullQuizName quizInfo.quizIdentifier, labels.ownPointsLabel ]

        QuizModel _ quizInfo labels ->
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
    = Any


initialModelFunction : () -> ( Model, Cmd Msg )
initialModelFunction _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    AllModel [] Input.Model.defaultLabels
