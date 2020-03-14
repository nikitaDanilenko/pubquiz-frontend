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
import Input.QuizValues exposing (mkCreationForm)
import Input.Validity as Validity
















-- todo: Re-use suggestions in the input field.
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








addFeedbackLabel : Model -> Html Msg
addFeedbackLabel model =
    div [ id "feedbackArea" ]
        [ label [ for "feedbackLabel" ] [ text model.feedback ] ]















