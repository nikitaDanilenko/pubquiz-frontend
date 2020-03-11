module Input.QuizValues exposing (..)

import Common.Copy exposing (LabelsField(..))
import Common.Types exposing (Labels, Place, QuizDate, QuizIdentifier, QuizName, QuizSettings)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, step, type_, value)
import Html.Events exposing (onInput)


type Msg
    = LabelsUpdate LabelsField String
    | SetQuizName QuizName
    | SetQuizDate QuizDate
    | SetQuizPlace Place
    | SetRoundsNumber String
    | SetTeamsInQuiz String


mkCreationForm :
    QuizSettings
    -> Html.Attribute Msg
    -> Labels
    -> List (Html Msg)
mkCreationForm quizSettings createOnEnter labels =
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

        mkInput : String -> LabelsField -> String -> Html Msg
        mkInput lbl fld dft =
            div [ id (createIdByField fld) ]
                [ label [] [ text lbl ]
                , input [ onInput (LabelsUpdate fld), type_ "text", value dft, createOnEnter ] []
                ]

        mkIdentifierPart : String -> String -> String -> String -> String -> (String -> Msg) -> Html Msg
        mkIdentifierPart divId labelFor description inputType example onInputFct =
            div [ id divId ]
                [ label [ for labelFor ] [ text description ]
                , input [ onInput onInputFct, type_ inputType, createOnEnter, placeholder example ] []
                ]
    in
    [ mkIdentifierPart "quizNameDiv" "quizName" "Quiz name" "text" "e.g. Quiz" SetQuizName
    , mkIdentifierPart "quizDateDiv" "quizDate" "Quiz date" "date" "e.g. 2020-01-01" SetQuizDate
    , mkIdentifierPart "quizPlaceDiv" "quizPlace" "Quiz place" "text" "e.g. Cheers" SetQuizPlace
    , div [ id "roundsNumberDiv" ]
        [ label [ for "roundsNumber" ]
            [ text "Number of rounds" ]
        , input
            [ onInput SetRoundsNumber
            , class "roundsSpinner"
            , type_ "number"
            , min "1"
            , step "1"
            , createOnEnter
            , value (String.fromInt (List.length quizSettings.rounds))
            ]
            []
        ]
    , div [ id "questionLabel" ]
        [ label [ id "questionsPerRound" ]
            [ text "Questions per round" ]
        ]
    , div [ id "questionArea" ]
        [ mkQuestionsForm createOnEnter quizSettings.rounds ]
    , div [ id "teamNumberArea" ]
        [ label [ for "teamNumber" ] [ text "Number of teams" ]
        , input
            [ onInput SetTeamsInQuiz
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


mkQuestionsForm : (Int -> String -> msg) -> Html.Attribute msg -> List Int -> Html msg
mkQuestionsForm updateQuestions createOnEnter rs =
    div [ id "perRound" ]
        (List.concat
            (List.indexedMap
                (\i qs ->
                    [ div [ class "roundQuestionLine" ]
                        [ label [ class "roundNumber" ]
                            [ text (String.join " " [ "Round", String.fromInt (1 + i) ]) ]
                        , input
                            [ value (String.fromInt qs)
                            , onInput (updateQuestions i)
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
                rs
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


defaultQuizSettings : QuizSettings
defaultQuizSettings =
    { rounds = defaultRounds
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


defaultRounds : List Int
defaultRounds =
    List.repeat 4 defaultQuestionNumber
