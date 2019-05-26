module Views exposing ( .. )

import Html exposing              ( Html, div, text, input, button, textarea, node )
import Html.Attributes exposing   ( id, autocomplete, class, type_, disabled, rel, href )            
import Html.Events exposing       ( onInput, onClick )
import Html.Events.Extra exposing ( onEnter )

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
  div [ id "creatingView" ]
      [ text "Quiz name",
        input [ onInput SetNewQuizName, onEnter (Create md.createName) ] [],
        button [ class "button", 
                 onClick (Create md.createName), 
                 disabled (String.isEmpty md.createName) 
               ] 
               [ text "Create" ] ,
        button [ class "button", onClick GetAll ] [ text "Back" ],
        addFeedbackLabel md
      ]

addFeedbackLabel : Model -> Html Msg
addFeedbackLabel model = div [ id "feedbackLabel" ] [ text model.feedback ]

wrapView : (Model -> Html Msg) -> Model -> Html Msg
wrapView viewOf model = 
  div [ id "mainPage"]
      [
        node "link" [ rel "stylesheet", type_ "text/css", href "style.css" ] [],
        viewOf model 
      ]