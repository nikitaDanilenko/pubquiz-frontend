module Input.QuizValues exposing (..)

import Basics
import Common.Copy exposing (LabelsField(..))
import Common.Types exposing (Activity(..), Labels, NumberOfQuestions, Place, QuestionsInQuiz, QuestionsInRound, QuizDate, QuizIdentifier, QuizInfo, QuizName, QuizSettings, RoundNumber)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, for, id, min, placeholder, step, type_, value)
import Html.Events exposing (onInput)
import List.Extra exposing (setIf)
import Parser exposing (int, run)


type Msg
    = LabelsUpdate LabelsField String
    | SetQuizName QuizName
    | SetQuizDate QuizDate
    | SetQuizPlace Place
    | SetRoundsNumber String
    | SetTeamsInQuiz String
    | SetQuestions Int String


mkCreationForm :
    (Msg -> msg)
    -> QuizIdentifier
    -> QuizSettings
    -> Html.Attribute msg
    -> Labels
    -> List (Html msg)
mkCreationForm wrapMsg quizIdentifier quizSettings createOnEnter labels =
    let
        associations =
            [ ( "Label for rounds", RoundField, labels.roundLabel )
            , ( "Label for teams", TeamField, labels.teamLabel )
            , ( "Label for own points", OwnPointsField, labels.ownPointsLabel )
            , ( "Label for maximum reached points", MaxReachedField, labels.maxReachedLabel )
            , ( "Label for maximum reachable points", MaxReachableField, labels.maxReachableLabel )
            , ( "Label for 'back to chart'", BackField, labels.backToChartView )
            , ( "Label for own page", OwnPageField, labels.ownPageLabel )
            , ( "Label for 'view quizzes' button", ViewPreviousField, labels.viewPrevious )
            , ( "Label for cumulative points", CumulativeField, labels.cumulativeLabel )
            , ( "Label for individual points", IndividualRoundsField, labels.individualRoundsLabel )
            , ( "Label for progression", ProgressionField, labels.progressionLabel )
            , ( "Label for placement", PlacementField, labels.placementLabel )
            , ( "Label for place", PlaceField, labels.placeLabel )
            , ( "Label for points", PointsField, labels.pointsLabel )
            , ( "Label for round winner", RoundWinnerField, labels.roundWinnerLabel )
            ]

        mkInput : String -> LabelsField -> String -> Html msg
        mkInput lbl fld dft =
            div [ id (createIdByField fld) ]
                [ label [] [ text lbl ]
                , input [ onInput (LabelsUpdate fld >> wrapMsg), type_ "text", value dft, createOnEnter ] []
                ]

        mkIdentifierPart : String -> String -> String -> String -> String -> String -> (String -> msg) -> Html msg
        mkIdentifierPart divId labelFor description inputType example currentValue onInputFct =
            div [ id divId ]
                [ label [ for labelFor ] [ text description ]
                , input
                    [ onInput onInputFct
                    , value currentValue
                    , type_ inputType
                    , createOnEnter
                    , placeholder example
                    ]
                    []
                ]
    in
    [ mkIdentifierPart "quizNameDiv" "quizName" "Quiz name" "text" "e.g. Quiz" quizIdentifier.name (SetQuizName >> wrapMsg)
    , mkIdentifierPart "quizDateDiv" "quizDate" "Quiz date" "date" "e.g. 2020-01-01" quizIdentifier.date (SetQuizDate >> wrapMsg)
    , mkIdentifierPart "quizPlaceDiv" "quizPlace" "Quiz place" "text" "e.g. Cheers" quizIdentifier.place (SetQuizPlace >> wrapMsg)
    , div [ id "roundsNumberDiv" ]
        [ label [ for "roundsNumber" ]
            [ text "Number of regular rounds" ]
        , input
            [ onInput (SetRoundsNumber >> wrapMsg)
            , class "roundsSpinner"
            , type_ "number"
            , min "1"
            , step "1"
            , createOnEnter
            , value (String.fromInt (List.length quizSettings.questionsInQuiz))
            ]
            []
        ]
    , div [ id "questionLabel" ]
        [ label [ id "questionsPerRound" ]
            [ text "Questions per round" ]
        ]
    , div [ id "questionArea" ]
        [ mkQuestionsForm (\i -> SetQuestions i >> wrapMsg) createOnEnter quizSettings.questionsInQuiz ]
    , div [ id "teamNumberArea" ]
        [ label [ for "teamNumber" ] [ text "Number of teams" ]
        , input
            [ onInput (SetTeamsInQuiz >> wrapMsg)
            , class "teamsSpinner"
            , type_ "number"
            , min "1"
            , createOnEnter
            , value (String.fromInt quizSettings.numberOfTeams)
            ]
            []
        ]
    , div [ id "labelsForm" ]
        (List.map (\( lbl, fld, dft ) -> mkInput lbl fld dft) associations)
    ]


createIdByField : LabelsField -> String
createIdByField fld =
    case fld of
        RoundField ->
            "roundField"

        TeamField ->
            "teamField"

        OwnPointsField ->
            "ownPointsField"

        MaxReachedField ->
            "maxReachedField"

        MaxReachableField ->
            "maxReachableField"

        BackField ->
            "backField"

        OwnPageField ->
            "ownPageField"

        ViewPreviousField ->
            "viewPreviousField"

        CumulativeField ->
            "cumulativeField"

        IndividualRoundsField ->
            "individualRoundsField"

        ProgressionField ->
            "progressionField"

        PlacementField ->
            "placementField"

        PlaceField ->
            "placeField"

        PointsField ->
            "pointsField"

        RoundWinnerField ->
            "roundWinnerField"


mkQuestionsForm : (Int -> String -> msg) -> Html.Attribute msg -> QuestionsInQuiz -> Html msg
mkQuestionsForm updateQuestions createOnEnter questionsInQuiz =
    div [ id "perRound" ]
        (List.concat
            (List.map
                (\questionsInRound ->
                    [ div [ class "roundQuestionLine" ]
                        [ label [ class "roundNumber" ]
                            [ text (String.join " " [ "Round", String.fromInt questionsInRound.questionsInRoundRoundNumber ]) ]
                        , input
                            [ value (String.fromInt questionsInRound.questionsInRoundNumberOfQuestions)
                            , onInput (updateQuestions questionsInRound.questionsInRoundRoundNumber)
                            , class "questionSpinner"
                            , type_ "number"
                            , min "1"
                            , step "1"
                            , createOnEnter
                            ]
                            []
                        ]
                    ]
                )
                (List.sortBy .questionsInRoundRoundNumber questionsInQuiz)
            )
        )


isValidQuizIdentifier : QuizIdentifier -> Bool
isValidQuizIdentifier quizIdentifier =
    List.all (String.isEmpty >> not)
        [ quizIdentifier.name
        , quizIdentifier.place
        , quizIdentifier.date
        ]


defaultQuizIdentifier : QuizIdentifier
defaultQuizIdentifier =
    { place = ""
    , date = ""
    , name = ""
    }


mkQuestionsInRound : RoundNumber -> NumberOfQuestions -> QuestionsInRound
mkQuestionsInRound roundNumber numberOfQuestions =
    { questionsInRoundRoundNumber = roundNumber, questionsInRoundNumberOfQuestions = numberOfQuestions }


defaultQuestionsInQuiz : QuestionsInQuiz
defaultQuestionsInQuiz =
    List.indexedMap (\i -> mkQuestionsInRound (1 + i))
        (List.repeat defaultRoundsNumber defaultQuestionNumber)


updateQuestionsInQuizAt : QuestionsInQuiz -> QuestionsInRound -> QuestionsInQuiz
updateQuestionsInQuizAt questionsInQuiz questionsInRound =
    setIf (\qir -> qir.questionsInRoundRoundNumber == questionsInRound.questionsInRoundRoundNumber) questionsInRound questionsInQuiz


adjustToSize : QuestionsInQuiz -> Int -> QuestionsInQuiz
adjustToSize questionsInQuiz n =
    let
        sorted =
            List.sortBy .questionsInRoundRoundNumber questionsInQuiz

        length =
            List.length sorted

        make =
            n - length

        made =
            List.indexedMap (\i -> mkQuestionsInRound (1 + length + i)) (List.repeat make defaultQuestionNumber)
    in
    List.take n sorted ++ made


defaultQuizSettings : QuizSettings
defaultQuizSettings =
    { questionsInQuiz = defaultQuestionsInQuiz
    , numberOfTeams = defaultNumberOfTeams
    , labels = defaultLabels
    }


defaultLabels : Labels
defaultLabels =
    { roundLabel = "Runde"
    , teamLabel = "Gruppe"
    , ownPointsLabel = "Erreichte Punkte"
    , maxReachedLabel = String.concat [ "Erreichte H", String.fromChar (Char.fromCode 246), "chstpunktzahl" ]
    , maxReachableLabel = "Erreichbare Punkte"
    , backToChartView = "Gesamtwertung"
    , ownPageLabel = "Eigene Punkte"
    , viewPrevious = "Alle Quizzes"
    , cumulativeLabel = "Gesamtpunktzahl"
    , individualRoundsLabel = "Punkte pro Runde"
    , progressionLabel = "Verlauf"
    , placementLabel = "Platzierung"
    , placeLabel = "Platz"
    , pointsLabel = "Punkte"
    , roundWinnerLabel = "Rundensieger"
    }


defaultNumberOfTeams : Int
defaultNumberOfTeams =
    8


defaultQuestionNumber : Int
defaultQuestionNumber =
    8


defaultRoundsNumber : Int
defaultRoundsNumber =
    4


defaultQuizInfo : QuizInfo
defaultQuizInfo =
    { quizId = -1
    , quizIdentifier = defaultQuizIdentifier
    , active = Inactive
    , fullSheetPath = ""
    , qrOnlyPath = ""
    }


validatePositiveNatural : String -> Maybe Int
validatePositiveNatural txt =
    case run int txt of
        Ok n ->
            if n > 0 then
                Just n

            else
                Nothing

        _ ->
            Nothing


isActive : Activity -> Bool
isActive activity =
    case activity of
        Active ->
            True

        Inactive ->
            False


swapActivity : Activity -> Activity
swapActivity activity =
    case activity of
        Active ->
            Inactive

        Inactive ->
            Active
