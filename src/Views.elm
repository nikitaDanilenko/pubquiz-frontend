module Views exposing ( .. )

import Html exposing              ( Html, div, text, input, button, textarea, node, a, table, 
                                    tr, td, label )
import Html.Attributes exposing   ( id, autocomplete, class, type_, disabled, rel, href, value, max,
                                    placeholder, download, target, for, min, acceptCharset, step )            
import Html.Events exposing       ( onInput, onClick )
import Html.Events.Extra exposing ( onEnter )
import Parser exposing            ( int, float )

import Constants exposing         ( sheetPDFPrefix, sheetPDFFile, mkPath )
import Labels exposing            ( Labels )
import Model exposing             ( .. )
import NewUser exposing           ( NewUserField ( .. ), isValid )
import Quiz exposing              ( isValidRoundsText, toEditableString, Header )
import Round exposing             ( isValidRound, Round )
import Util exposing              ( isParserSuccess, splitFirstLast, adjustToSize, 
                                    isValidInternalQuizName )

authenticationView : Model -> Html Msg
authenticationView md = 
    div [ id "initialMain"]
        [ div [ id "userField" ] 
              [   label [ for "user" ] [ text "User name" ]
                , input [ autocomplete True, onInput SetUser, onEnter Login ] []
              ],
          div [ id "passwordField" ]
              [
                  label [ for "password" ] [ text "Password" ]
                , input [ type_ "password", autocomplete True, onInput SetPassword, onEnter Login ] []
              ],
          div [ id "fetchButton" ]
              [ button [ class "button", onClick Login ] [ text "Login" ] ],
          addFeedbackLabel md
        ]

selectionView : Model -> Html Msg
selectionView md =
    let mkButton : QuizName -> Html Msg
        mkButton name = button [ class "quizButton", 
                                 onClick (GetSingle name) ] 
                               [ text name ]
    in div [ id "quizSelectionMain"]
           [ div [ id "selectExistingQuizzesMain" ]
                 (List.map mkButton (List.filter (\q -> not (String.isEmpty q)) md.quizzes)),
             div [ id "createNewQuiz" ]
                 [ button [ class "newQuizButton", onClick StartCreatingQuiz ] [ text "New quiz"] ],
             div [ id "createNewUser" ]
                 [ button [ class "newUserButton", onClick StartCreatingUser ] [ text "New user" ] ],
             addFeedbackLabel md
           ]

editingView : Model -> Html Msg
editingView md =
    div [ id "singleQuiz" ]
        ([ 
          div [ id "editingLabel"] 
              [ label [ for "editingQuiz" ]
                      [ text (String.join " " ["Editing", md.editing]) ] 
              ],
          div [ id "teamsInQuiz" ]
              [ label [ for "teamInQuizLabel" ] [ text "Teams in the quiz" ],
                input [ value (String.fromInt md.teamsInQuiz), 
                        type_ "number", 
                        min "1", 
                        step "1", 
                        onInput SetTeamsInQuiz 
                      ] 
                      []
              ],
          div [ id "teamNames" ]
              (label [ for "teamNamesLabel" ] [ text "Team names"] :: 
                mkTeamNameInput md.currentQuiz.header)
         ] ++
         List.indexedMap (\i rd -> mkRoundForm i md.teamsInQuiz rd)
                         md.currentQuiz.rounds
          ++ 
         [
          button [ class "button", onClick AddRound ] [ text "Add round" ],
          {-textarea [ id "singleQuizArea", onInput (SetPoints (Quiz.headerToString md.currentQuiz)) ] 
                   [ text (toEditableString md.currentQuiz) ],-}
          button [ class "backButton", onClick GetAll ] [ text "Back" ],
          button [ class "lockButton", onClick AcknowledgeLock ] [ text "Lock" ],
          button [ class "button", 
                   onClick (PostUpdate md.editing (Quiz.toString md.currentQuiz)),
                   disabled (not md.isValidQuizUpdate) ]
                 [ text "Update" ],
          div [ id "answerSheet" ]
              [ a [ class "link",
                    href (mkPath [ sheetPDFPrefix, 
                                   md.editing, 
                                   String.join "-" [ md.editing, sheetPDFFile ]
                                 ]),
                    target "_blank"
                  ] 
                  [ text "Get quiz sheet" ] ],
          div [ id "mainGraphPage" ]
              [ a [ class "link",
                    href (mkPath [ sheetPDFPrefix, md.editing, "" ]),
                    target "_blank"
                  ] 
                  [ text "View main graph page" ] ],
          addFeedbackLabel md
         ])

confirmView : Model -> Html Msg
confirmView md =
    div [ id "confirmView" ]
        [ label [ for "lockWarning" ]
                [ text (String.concat ["You are about to lock ", md.editing, ". ",
                                       "This cannot be undone. Please confirm. "]) ],
          button [ class "backButton", onClick (GetSingle md.editing) ]
                 [ text "Back" ],
          button [ class "lockButton", onClick (Lock md.editing) ]
                 [ text "Yes, lock" ]
        ]

creatingQuizView : Model -> Html Msg
creatingQuizView md = 
  let createOnEnter = onEnter CreateQuiz
  in div [ id "creatingQuizView" ]
         [ label [ for "internalQuizName" ] [ text "Quiz name (internal)" ], 
           input [ onInput SetNewQuizName,
                   type_ "text",
                   createOnEnter,
                   placeholder "e.g. some-quiz-yyyy-mm-dd" ] [],
           div [ id "roundsNumberDiv"] 
               [ label [ for "roundsNumber" ]
                       [ text "Number of rounds" ],
                 input [ onInput SetRoundsNumber,
                         class "roundsSpinner",
                         type_ "number", 
                         min "1",
                         step "1",
                         createOnEnter,
                         value (String.fromInt md.numberOfRounds) ] [] ], 
           div [ id "teamNumberArea" ]
               [ label [ for "teamNumber" ] [ text "Number of teams" ],
                 input [ onInput SetTeamsInQuiz,
                         class "teamsSpinner",
                         type_ "number", 
                         min "1",
                         createOnEnter,
                         value (String.fromInt md.teamsInQuiz) ] [] ],
           mkCreationForm createOnEnter md.labels,
           button [ class "button", onClick CreateQuiz, 
                    disabled (not (isValidNewQuiz md)) ] [ text "Create" ] ,
           button [ class "backButton", onClick GetAll ] [ text "Back" ],
           addFeedbackLabel md
          ]

creatingUserView : Model -> Html Msg
creatingUserView md =
  let createOnEnter = onEnter CreateUser
  in div [ id "creatingUserView" ]
      [ div [ id "creatingUser"] 
            [ label [ for "username" ] [ text "User name" ],
              input [ onInput (SetNewUserParam UserField), createOnEnter ] []
            ],
        div [ id "creatingPassword1" ]
            [ label [ for "password1" ] [ text "Password" ],
              input [ onInput (SetNewUserParam PasswordField1), type_ "password", createOnEnter ] []
            ],
        div [ id "creatingPassword2" ]
            [ label [ for "password2" ] [ text "Repeat password" ],
              input [ onInput (SetNewUserParam PasswordField2), type_ "password", createOnEnter ] []
            ],
        button [ class "button", onClick CreateUser, 
                 disabled (not (isValid md.newUser)) ]
               [ text "Create" ],
        button [ class "backButton", onClick GetAll ] [ text "Back" ],
        addFeedbackLabel md
      ]

mkCreationForm : Html.Attribute Msg -> Labels -> Html Msg
mkCreationForm createOnEnter labels = 
  let associations = [("Description (external)", MainField, labels.mainLabel),
                      ("Label for rounds", RoundField, labels.roundLabel),
                      ("Label for teams", TeamField, labels.teamLabel),
                      ("Label for own points", OwnPointsField, labels.ownPointsLabel),
                      ("Label for maximum reached points", MaxReachedField, labels.maxReachedLabel),
                      ("Label for maximum reachable points", MaxReachableField, labels.maxReachableLabel),
                      ("Label for 'back to chart'", BackField, labels.backToChartView),
                      ("Label for own page", OwnPageField, labels.ownPageLabel),
                      ("Label for 'view quizzes' button", ViewQuizzesField, labels.viewQuizzesLabel),
                      ("Label for cumulative points", CumulativeField, labels.cumulativeLabel),
                      ("Label for individual points", IndividualField, labels.individualLabel),
                      ("Label for progression", ProgressionField, labels.progressionLabel)
                     ]
      mkInput : String -> LabelsField -> String -> Html Msg
      mkInput lbl fld dft = 
        div [ id (createIdByField fld) ] 
            [ label [] [ text lbl ], 
              input [ onInput (LabelsUpdate fld), type_ "text", value dft, createOnEnter ] [] ]
  in div [ id "labelsForm" ]
         (List.map (\(lbl, fld, dft) -> mkInput lbl fld dft) associations)

addFeedbackLabel : Model -> Html Msg
addFeedbackLabel model = div [ id "feedbackArea" ] 
                             [ label [ for "feedbackLabel" ] [ text model.feedback ] ]

mkRoundForm : Int -> Int -> Round -> Html Msg
mkRoundForm number gs rd = 
  div [ id "roundPoints" ]
      ( label [ class "roundNumber" ] 
              [ text (String.join " " [ "Round", String.fromInt (1 + number) ]) ] ::
        div [ id "maxPointsArea" ] 
            [ label [ class "maxPoints" ] [ text "Obtainable" ], 
              input (value (String.fromFloat rd.maxPoints) :: 
                     onInput (SetMaxPoints number) :: 
                     pointInputAttributes) 
                     []
            ] ::
        List.indexedMap (\i ps -> div [ class "teamPointsArea"]
                                      [ div [ class "label" ]
                                            [ label [ class "pointsPerTeamLabel" ] 
                                                    [ text (String.join " " 
                                                                        ["Team", 
                                                                         String.fromInt (1 + i)]) ]
                                            ],
                                        div [ class "input" ]
                                            [ input (value (String.fromFloat ps) ::
                                                     onInput (UpdatePoints number i) :: 
                                                     pointInputAttributes) 
                                                    []

                                            ]
                                      ])
                        (adjustToSize gs rd.teamPoints))

mkTeamNumber : Int -> String -> String
mkTeamNumber i wordForTeam = (String.join " " [wordForTeam, String.fromInt i])

mkSingleTeamName : Int -> String -> Maybe String -> String
mkSingleTeamName i wordForTeam = Maybe.withDefault (mkTeamNumber i wordForTeam)

mkSingleTeamNameInput : Int -> String -> Maybe String -> Html Msg
mkSingleTeamNameInput i wordForTeam mn =
  div [ class "teamNameInputArea" ]
      [ label [ for "teamName" ] [ text (mkTeamNumber (1 + i) "Team") ],
        input [ value (mkSingleTeamName (1 + i) wordForTeam mn),
                onInput (SetTeamName i) ] [ ]
       ]

mkTeamNameInput : Header -> List (Html Msg)
mkTeamNameInput = List.indexedMap (\i (_, mn) -> mkSingleTeamNameInput i "Gruppe" mn)

pointInputAttributes : List (Html.Attribute Msg)
pointInputAttributes = [ class "labeledInput", type_ "number", min "0", step "0.5" ]

wrapView : (Model -> Html Msg) -> Model -> Html Msg
wrapView viewOf model = 
  div [ id "mainPage"]
      [
        node "link" [ rel "stylesheet", type_ "text/css", href "style.css" ] [],
        viewOf model 
      ]

createIdByField : LabelsField -> String
createIdByField fld = case fld of
  RoundField -> "roundField"
  TeamField -> "teamField"
  OwnPointsField -> "ownPointsField"
  MaxReachedField -> "maxReachedField"
  MaxReachableField -> "maxReachableField"
  BackField -> "backField"
  MainField -> "mainField"
  OwnPageField -> "ownPageField"
  ViewQuizzesField -> "viewQuizzesField"
  CumulativeField -> "cumulativeField"
  IndividualField -> "individualField"
  ProgressionField -> "progressionField"

toCell : String -> Html Msg
toCell str = td [] [ text str ]

toTable : List (List String) -> Html Msg
toTable = table [] << List.map (tr [] << List.map toCell)

isValidNewQuiz : Model -> Bool
isValidNewQuiz md = not (String.isEmpty md.createName) && isValidInternalQuizName md.createName

isValidInt : String -> Bool
isValidInt = isParserSuccess int

isValidFloat : String -> Bool
isValidFloat = isParserSuccess float