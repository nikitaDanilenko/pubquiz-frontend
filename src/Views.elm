module Views exposing ( .. )

import Html exposing              ( Html, div, text, input, button, textarea, node, a, table, 
                                    tr, td, label )
import Html.Attributes exposing   ( id, autocomplete, class, type_, disabled, rel, href,
                                    placeholder, download, target, for, min )            
import Html.Events exposing       ( onInput, onClick )
import Html.Events.Extra exposing ( onEnter )

import Constants exposing         ( sheetPDFPrefix, sheetPDFFile, mkPath )
import Labels exposing            ( Labels )
import Model exposing             ( .. )
import NewUser exposing           ( NewUserField ( .. ), isValid )

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
        [ text (String.concat ["Editing ", md.editing]),
          textarea [ id "singleQuizArea", onInput SetPoints ] [ text md.currentPoints ],
          button [ class "button", onClick GetAll ] [ text "Back" ],
          button [ class "lockButton", onClick AcknowledgeLock ] [ text "Lock" ],
          button [ class "button", onClick (PostUpdate md.editing md.currentPoints) ]
                 [ text "Update" ],
          div [ id "answerSheet" ]
              [ a [ class "link",
                    href (mkPath [ sheetPDFPrefix, 
                                   md.editing, 
                                   String.concat [md.editing, "-", sheetPDFFile ]
                                 ]),
                    target "_blank"
                  ] 
                  [ text "Get quiz sheet" ] ],
          addFeedbackLabel md
        ]

confirmView : Model -> Html Msg
confirmView md =
    div [ id "confirmView" ]
        [ label [ for "lockWarning"] 
                [ text (String.concat ["You are about to lock ", md.editing, ". ",
                                       "This cannot be undone. Please confirm. "]) ],
          button [ class "button", onClick (GetSingle md.editing) ]
                 [ text "Abort" ],
          button [ class "button", onClick (Lock md.editing) ]
                 [ text "Yes, lock" ]
        ]

creatingQuizView : Model -> Html Msg
creatingQuizView md = 
  let createOnEnter = onEnter CreateQuiz
  in div [ id "creatingQuizView" ]
         [ label [ for "internalQuizName" ] [ text "Quiz name (internal)" ], 
           input [ onInput SetNewQuizName, createOnEnter ] [],
           div [ id "roundsNumberDiv"] 
               [ label [ for "roundsNumber", type_ "number", min "1" ]
                       [ text "Number of rounds" ],
                 input [ onInput SetRoundsNumber, 
                         createOnEnter,
                         placeholder (String.fromInt md.numberOfRounds) ] [] ], 
           mkCreationForm createOnEnter md.labels,
           button [ class "button", onClick CreateQuiz, 
                    disabled (String.isEmpty md.createName) ] [ text "Create" ] ,
           button [ class "button", onClick GetAll ] [ text "Back" ],
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
        button [ class "button", onClick GetAll ] [ text "Back" ],
        addFeedbackLabel md
      ]

mkCreationForm : Html.Attribute Msg -> Labels -> Html Msg
mkCreationForm createOnEnter labels = 
  let associations = [("Description (external)", MainField, labels.mainLabel),
                      ("Label for rounds", RoundField, labels.roundLabel),
                      ("Label for groups", GroupField, labels.groupLabel),
                      ("Label for own points", OwnPointsField, labels.ownPointsLabel),
                      ("Label for maximum reached points", MaxReachedField, labels.maxReachedLabel),
                      ("Label for maximum reachable points", MaxReachableField, labels.maxReachableLabel),
                      ("Label for 'back to chart'", BackField, labels.backToChartView),
                      ("Label for own page", OwnPageField, labels.ownPageLabel)
                     ]
      mkInput : String -> LabelsField -> String -> Html Msg
      mkInput lbl fld dft = 
        div [ id (createIdByField fld) ] 
            [ label [] [ text lbl ], 
              input [ onInput (LabelsUpdate fld), placeholder dft, createOnEnter ] [] ]
  in div [ id "labelsForm" ]
         (List.map (\(lbl, fld, dft) -> mkInput lbl fld dft) associations)

addFeedbackLabel : Model -> Html Msg
addFeedbackLabel model = div [ id "feedbackLabel" ] [ text model.feedback ]

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
  GroupField -> "groupField"
  OwnPointsField -> "ownPointsField"
  MaxReachedField -> "MaxReachedField"
  MaxReachableField -> "MaxReachableField"
  BackField -> "BackField"
  MainField -> "MainField"
  OwnPageField -> "OwnPageField"

toCell : String -> Html Msg
toCell str = td [] [ text str ]

toTable : List (List String) -> Html Msg
toTable = table [] << List.map (tr [] << List.map toCell)