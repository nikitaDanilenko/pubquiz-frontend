module Input.Views exposing (..)

import Common.Constants exposing (mkPath, sheetPDFPrefix)
import Common.Copy exposing (LabelsField(..))
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , input
        , label
        , node
        , text
        , tr
        )
import Html.Attributes
    exposing
        ( autocomplete
        , class
        , disabled
        , for
        , href
        , id
        , max
        , min
        , placeholder
        , rel
        , step
        , target
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Input.Model exposing (..)
import Input.NewUser exposing (NewUserField(..), isValid)
import Common.QuizRatings as QuizRatings
import Common.Types exposing (DbQuizId, Header, Labels, QuizInfo, QuizName, RoundNumber, RoundRating, TeamNumber)
import Common.Util
    exposing
        ( adjustToSize
        )
import Input.Validity as Validity


authenticationView : Model -> Html Msg
authenticationView md =
    div [ id "initialMain" ]
        [ div [ id "userField" ]
            [ label [ for "user" ] [ text "User name" ]
            , input [ autocomplete True, onInput SetUser, onEnter Login ] []
            ]
        , div [ id "passwordField" ]
            [ label [ for "password" ] [ text "Password" ]
            , input [ type_ "password", autocomplete True, onInput SetPassword, onEnter Login ] []
            ]
        , div [ id "fetchButton" ]
            [ button [ class "button", onClick Login ] [ text "Login" ] ]
        , addFeedbackLabel md
        ]


selectionView : Model -> Html Msg
selectionView md =
    let
        mkButton : QuizInfo -> Html Msg
        mkButton qi =
            button
                [ class "quizButton"
                , onClick (GetSingle qi.quizId)
                ]
                [ text qi.quizIdentifier.name ]
    in
    div [ id "quizSelectionMain" ]
        [ div [ id "selectExistingQuizzesMain" ]
            (List.map mkButton (List.filter (\q -> not (String.isEmpty q.quizIdentifier.name)) md.quizzes))
        , div [ id "createNewQuiz" ]
            [ button [ class "newQuizButton", onClick StartCreatingQuiz ] [ text "New quiz" ] ]
        , div [ id "createNewUser" ]
            [ button [ class "newUserButton", onClick StartCreatingUser ] [ text "New user" ] ]
        , addFeedbackLabel md
        ]


editingView : Model -> Html Msg
editingView md =
    let
        quizName =
            md.currentQuizInfo.quizIdentifier.name

        numberOfTeams =
            md.currentQuizSettings.numberOfTeams

        quizRatings =
            md.currentQuizRatings
    in
    div [ id "singleQuiz" ]
        ([ div [ id "editingLabel" ]
            [ label [ for "editingQuiz" ]
                [ text (String.join " " [ "Editing", quizName ]) ]
            ]
         , div [ id "teamsInQuiz" ]
            [ label [ for "teamInQuizLabel" ] [ text "Teams in the quiz" ]
            , input
                [ value (String.fromInt numberOfTeams)
                , type_ "number"
                , min "1"
                , step "1"
                , max (String.fromInt (QuizRatings.maxNumberOfTeams quizRatings))
                , onInput (SetTeamsInQuiz IntermediateTU)
                ]
                []
            ]
         , div [ id "teamNames" ]
            (label [ for "teamNamesLabel" ] [ text "Team names" ]
                :: mkTeamNameInput (List.take numberOfTeams quizRatings.header)
            )
         ]
            ++ List.map (\( rn, rr ) -> mkRoundForm rn numberOfTeams rr)
                quizRatings.ratings
            ++ [ button [ class "button", onClick AddRound ] [ text "Add round" ]
               , button [ class "button", onClick GetLabels ] [ text "Edit labels" ]
               , button [ class "backButton", onClick GetAll ] [ text "Back" ]
               , button [ class "lockButton", onClick AcknowledgeLock ] [ text "Lock" ]
               , button
                    [ class "button"
                    , onClick (PostUpdate md.currentQuizInfo.quizId quizRatings)
                    , disabled (not (Validity.isValid md.isValidQuizUpdate))
                    ]
                    [ text "Update" ]

               -- todo: Fix these links according to new structure. This holds twice: once for top level sheets, and once for RESTview
               , mkLinkToSheet "answerSheet" "Get quiz sheet" md.currentQuizInfo.fullSheetPath
               , mkLinkToSheet "qrSheet" "Get QR codes only" md.currentQuizInfo.qrOnlyPath
               -- todo: Adjust this path using a proper REST request
               , mkLinkToSheet "mainGraphPage" "View main graph page" ""
               , addFeedbackLabel md
               ]
        )


mkLinkToSheet : String -> String -> String -> Html Msg
mkLinkToSheet divId linkText file =
    div [ id divId ]
        [ a
            [ class "link"
            , href
                (mkPath
                    [ sheetPDFPrefix
                    , file
                    ]
                )
            , target "_blank"
            ]
            [ text linkText ]
        ]


confirmView : Model -> Html Msg
confirmView md =
    div [ id "confirmView" ]
        [ label [ for "lockWarning" ]
            [ text
                (String.concat
                    [ "You are about to lock "
                    , md.currentQuizInfo.quizIdentifier.name
                    , ". "
                    , "This cannot be undone. Please confirm. "
                    ]
                )
            ]
        , button [ class "backButton", onClick (GetSingle md.currentQuizInfo.quizId) ]
            [ text "Back" ]
        , button [ class "lockButton", onClick (LockQuiz md.currentQuizInfo.quizId) ]
            [ text "Yes, lock" ]
        ]


creatingQuizView : Model -> Html Msg
creatingQuizView md =
    let
        createOnEnter =
            onEnter CreateQuiz
    in
    div [ id "creatingQuizView" ]
        (mkCreationForm md createOnEnter md.currentQuizSettings.labels
            ++ [ button
                    [ class "button"
                    , onClick CreateQuiz
                    , disabled (not (isValidNewQuiz md))
                    ]
                    [ text "Create" ]
               , button [ class "backButton", onClick GetAll ] [ text "Back" ]
               , addFeedbackLabel md
               ]
        )


editingLabelsView : Model -> Html Msg
editingLabelsView md =
    let
        lbls =
            md.currentQuizSettings.labels

        edit =
            md.currentQuizInfo.quizId

        done =
            PostQuizSettingsUpdate edit md.currentQuizSettings
    in
    div [ id "editingLabelsView" ]
        (mkCreationForm md (onEnter done) lbls
            ++ [ button [ class "button", onClick done ] [ text "Update" ]
               , button [ class "backButton", onClick (GetSingle edit) ] [ text "Back" ]
               , addFeedbackLabel md
               ]
        )


creatingUserView : Model -> Html Msg
creatingUserView md =
    let
        createOnEnter =
            onEnter CreateUser
    in
    div [ id "creatingUserView" ]
        [ div [ id "creatingUser" ]
            [ label [ for "username" ] [ text "User name" ]
            , input [ onInput (SetNewUserParam UserField), createOnEnter ] []
            ]
        , div [ id "creatingPassword1" ]
            [ label [ for "password1" ] [ text "Password" ]
            , input [ onInput (SetNewUserParam PasswordField1), type_ "password", createOnEnter ] []
            ]
        , div [ id "creatingPassword2" ]
            [ label [ for "password2" ] [ text "Repeat password" ]
            , input [ onInput (SetNewUserParam PasswordField2), type_ "password", createOnEnter ] []
            ]
        , button
            [ class "button"
            , onClick CreateUser
            , disabled (not (isValid md.newUser))
            ]
            [ text "Create" ]
        , button [ class "backButton", onClick GetAll ] [ text "Back" ]
        , addFeedbackLabel md
        ]


mkCreationForm : Model -> Html.Attribute Msg -> Labels -> List (Html Msg)
mkCreationForm md createOnEnter labels =
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
    [ mkIdentifierPart "quizNameDiv" "quizName" "Quiz name" "text" "e.g. Quiz" SetNewQuizName
    , mkIdentifierPart "quizDateDiv" "quizDate" "Quiz name" "date" "e.g. 2020-01-01" SetNewQuizDate
    , mkIdentifierPart "quizPlaceDiv" "quizPlace" "Quiz name" "text" "e.g. Cheers" SetNewQuizPlace
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
            , value (String.fromInt (List.length md.currentQuizSettings.rounds))
            ]
            []
        ]
    , div [ id "questionLabel" ]
        [ label [ id "questionsPerRound" ]
            [ text "Questions per round" ]
        ]
    , div [ id "questionArea" ]
        [ mkQuestionsForm createOnEnter md.currentQuizSettings.rounds ]
    , div [ id "teamNumberArea" ]
        [ label [ for "teamNumber" ] [ text "Number of teams" ]
        , input
            [ onInput (SetTeamsInQuiz InitialTU)
            , class "teamsSpinner"
            , type_ "number"
            , min "1"
            , createOnEnter
            , value (String.fromInt md.currentQuizSettings.numberOfTeams)
            ]
            []
        ]
    , div [ id "labelsForm" ]
        (List.map (\( lbl, fld, dft ) -> mkInput lbl fld dft) associations)
    ]


addFeedbackLabel : Model -> Html Msg
addFeedbackLabel model =
    div [ id "feedbackArea" ]
        [ label [ for "feedbackLabel" ] [ text model.feedback ] ]


mkRoundForm : RoundNumber -> Int -> RoundRating -> Html Msg
mkRoundForm rn gs rr =
    div [ id "roundPoints" ]
        (label [ class "roundNumber" ]
            [ text (String.join " " [ "Round", String.fromInt rn ]) ]
            :: div [ id "maxPointsArea" ]
                [ label [ class "maxPoints" ] [ text "Obtainable" ]
                , input
                    (value (String.fromFloat rr.reachableInRound)
                        :: onInput (SetMaxPoints rn)
                        :: pointInputAttributes
                    )
                    []
                ]
            :: List.map
                (\tr ->
                    div [ class "teamPointsArea" ]
                        [ div [ class "label" ]
                            [ label [ class "pointsPerTeamLabel" ]
                                [ text
                                    (String.join " "
                                        [ "Team"
                                        , String.fromInt tr.teamNumber
                                        ]
                                    )
                                ]
                            ]
                        , div [ class "input" ]
                            [ input
                                (value (String.fromFloat tr.rating)
                                    :: onInput (UpdatePoints rn tr.teamNumber)
                                    :: max (String.fromFloat rr.reachableInRound)
                                    :: pointInputAttributes
                                )
                                []
                            ]
                        ]
                )
                (adjustToSize gs rr.points)
        )


mkQuestionsForm : Html.Attribute Msg -> List Int -> Html Msg
mkQuestionsForm createOnEnter rs =
    div [ id "perRound" ]
        (List.concat
            (List.indexedMap
                (\i qs ->
                    [ div [ class "roundQuestionLine" ]
                        [ label [ class "roundNumber" ]
                            [ text (String.join " " [ "Round", String.fromInt (1 + i) ]) ]
                        , input
                            [ value (String.fromInt qs)
                            , onInput (UpdateQuestions i)
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


mkTeamNumber : TeamNumber -> String -> String
mkTeamNumber i wordForTeam =
    String.join " " [ wordForTeam, String.fromInt i ]


mkSingleTeamNameInput : TeamNumber -> String -> Html Msg
mkSingleTeamNameInput tn name =
    div [ class "teamNameInputArea" ]
        [ label [ for "teamName" ] [ text (mkTeamNumber tn "Team") ]
        , input
            [ value name
            , onInput (SetTeamName tn)
            ]
            []
        ]


mkTeamNameInput : Header -> List (Html Msg)
mkTeamNameInput h =
    h
        |> List.sortBy (\ti -> ti.teamInfoNumber)
        |> List.map (\ti -> mkSingleTeamNameInput ti.teamInfoNumber ti.teamInfoName)


pointInputAttributes : List (Html.Attribute Msg)
pointInputAttributes =
    [ class "labeledInput", type_ "number", min "0", step "0.5" ]


wrapView : (Model -> Html Msg) -> Model -> Html Msg
wrapView viewOf model =
    div [ id "mainPage" ]
        [ node "link" [ rel "stylesheet", type_ "text/css", href "style.css" ] []
        , viewOf model
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


isValidNewQuiz : Model -> Bool
isValidNewQuiz md =
    not (String.isEmpty md.currentQuizInfo.quizIdentifier.name)
