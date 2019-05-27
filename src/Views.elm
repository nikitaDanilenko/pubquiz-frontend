module Views exposing ( .. )

import Html exposing              ( Html, div, text, input, button, textarea, node, a )
import Html.Attributes exposing   ( id, autocomplete, class, type_, disabled, rel, href,
                                    placeholder, download, target )            
import Html.Events exposing       ( onInput, onClick )
import Html.Events.Extra exposing ( onEnter )

import Constants exposing         ( sheetPDFPrefix, sheetPDFFile, mkPath )
import Labels exposing            ( Labels )
import Model exposing             ( .. )

authenticationView : Model -> Html Msg
authenticationView md = 
    div [ id "initialMain"]
        [ div [ id "userField" ] 
              [   text "User name"
                , input [ autocomplete True, onInput SetUser, onEnter Login ] []
              ],
          div [ id "passwordField" ]
              [
                  text "Password"
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
                 [ button [ class "newQuizButton", onClick StartCreating ] [ text "New quiz"] ] ,
             addFeedbackLabel md
           ]

editingView : Model -> Html Msg
editingView md =
    div [ id "singleQuiz" ]
        [ text (String.concat ["Editing ", md.editing]),
          textarea [ id "singleQuizArea", onInput SetPoints ] [ text md.currentPoints ],
          button [ class "button", onClick GetAll ] [ text "Back" ],
          button [ class "button", onClick AcknowledgeLock ] [ text "Lock" ],
          button [ class "button", onClick (PostUpdate md.editing md.currentPoints) ]
                 [ text "Update" ],
          div [ id "answerSheet" ]
              [ a [ class "link",
                    href (mkPath [ sheetPDFPrefix, md.editing, sheetPDFFile ]),
                    target "_blank"
                  ] 
                  [ text "Get quiz sheet" ] ],
          addFeedbackLabel md
        ]

confirmView : Model -> Html Msg
confirmView md =
    div [ id "confirmView" ]
        [ text (String.concat ["You are about to lock ", md.editing, ". ",
                               "This cannot be undone. Please confirm. "]),
          button [ class "button", onClick (GetSingle md.editing) ]
                 [ text "Abort" ],
          button [ class "button", onClick (Lock md.editing) ]
                 [ text "Yes, lock" ]
        ]

creatingView : Model -> Html Msg
creatingView md = 
  let createOnEnter = onEnter (Create md.createName)
  in div [ id "creatingView" ]
         [ text "Quiz name (internal)", input [ onInput SetNewQuizName, createOnEnter ] [],
           mkCreationForm createOnEnter md.labels,
           button [ class "button", onClick (Create md.createName), 
                    disabled (String.isEmpty md.createName) ] [ text "Create" ] ,
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
      mkInput : String -> LabelsField -> String -> List (Html Msg)
      mkInput lbl fld dft = 
        [ text lbl, input [ onInput (LabelsUpdate fld), placeholder dft, createOnEnter ] [] ]
  in div [ id "labelsForm" ]
         (List.concatMap (\(lbl, fld, dft) -> mkInput lbl fld dft) associations)

addFeedbackLabel : Model -> Html Msg
addFeedbackLabel model = div [ id "feedbackLabel" ] [ text model.feedback ]

wrapView : (Model -> Html Msg) -> Model -> Html Msg
wrapView viewOf model = 
  div [ id "mainPage"]
      [
        node "link" [ rel "stylesheet", type_ "text/css", href "style.css" ] [],
        viewOf model 
      ]