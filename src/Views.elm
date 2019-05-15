module Views exposing ( .. )

import Html exposing              ( Html, div, text, input, button, textarea )
import Html.Attributes exposing   ( id, autocomplete, class )            
import Html.Events exposing       ( onInput, onClick )
import Html.Events.Extra exposing ( onEnter )

import Model exposing             ( .. )

authenticationView : Model -> Html Msg
authenticationView md = 
    div [ id "initialMain"]
        [ div [ id "userField" ] 
              [   text "User name"
                , input [ autocomplete True, onInput SetUser, onEnter GetAll ] []
              ],
          div [ id "passwordField" ]
              [
                  text "Password"
                , input [ autocomplete True, onInput SetPassword, onEnter GetAll ] []
              ],
          div [ id "fetchButton" ]
              [ button [ class "button", onClick GetAll ] [ text "Login" ] ],
          div [ id "errorLabel" ] [ text md.errorMsg ]
        ]

selectionView : Model -> Html Msg
selectionView md =
    let mkButton : QuizName -> Html Msg
        mkButton name = button [ class "quizButton", 
                                 onClick (GetSingle name) ] 
                               [ text name ]
    in div [ id "allQuizzesMain" ]
           (List.map mkButton (List.filter (\q -> not (String.isEmpty q)) md.quizzes))

editingView : Model -> Html Msg
editingView md =
    div [ id "singleQuiz" ]
        [ textarea [ id "singleQuizArea", onInput SetPoints ] [ text md.currentPoints ],
          button [ class "button", onClick GetAll ] [ text "Back" ],
          button [ class "button", onClick AcknowledgeLock ] [ text "Lock" ],
          button [ class "button", onClick (PostUpdate md.editing md.currentPoints) ]
                 [ text "Update" ]
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
creatingView md = confirmView md