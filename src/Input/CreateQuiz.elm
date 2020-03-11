module Input.CreateQuiz exposing (..)

import Common.Authentication exposing (Authentication)
import Common.ConnectionUtil exposing (addFeedbackLabel)
import Common.Copy as Copy
import Common.Types exposing (Labels, QuizIdentifier, QuizSettings)
import Common.Util as Util
import Date
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, id)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onEnter)
import Input.QuizValues as QuizValues


type alias Model =
    { quizIdentifier : QuizIdentifier
    , quizSettings : QuizSettings
    , authentication : Authentication
    , feedback : String
    }


updateQuizSettings : Model -> QuizSettings -> Model
updateQuizSettings model quizSettings =
    { model | quizSettings = quizSettings }


updateQuizIdentifier : Model -> QuizIdentifier -> Model
updateQuizIdentifier model quizIdentifier =
    { model | quizIdentifier = quizIdentifier }


updateFeedback : Model -> String -> Model
updateFeedback model feedback =
    { model | feedback = feedback }


type Msg
    = CreateQuiz
    | Done
    | Value QuizValues.Msg


init : Authentication -> Model
init authentication =
    { quizIdentifier = QuizValues.defaultQuizIdentifier
    , quizSettings = QuizValues.defaultQuizSettings
    , authentication = authentication
    , feedback = ""
    }


view : Model -> Html Msg
view md =
    let
        createOnEnter =
            onEnter CreateQuiz
    in
    div [ id "creatingQuizView" ]
        (QuizValues.mkCreationForm md.quizSettings createOnEnter md.quizSettings.labels
            ++ [ button
                    [ class "button"
                    , onClick CreateQuiz
                    , disabled (not (QuizValues.isValidQuizIdentifier md.quizIdentifier))
                    ]
                    [ text "Create" ]
               , button [ class "backButton", onClick Done ] [ text "Back" ]
               , addFeedbackLabel md.feedback
               ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateQuiz ->
            ( model, Cmd.none )

        Done ->
            ( model, Cmd.none )

        Value valueMsg ->
            let
                newModel =
                    case valueMsg of
                        QuizValues.LabelsUpdate labelsField string ->
                            Copy.updateLabelsByField model.quizSettings.labels labelsField string
                                |> Copy.updateQuizSettingsLabels model.quizSettings
                                |> updateQuizSettings model

                        QuizValues.SetQuizName quizName ->
                            Copy.updateQuizIdentifierName model.quizIdentifier quizName
                                |> updateQuizIdentifier model

                        QuizValues.SetQuizDate quizDate ->
                            case Date.fromIsoString quizDate of
                                Ok date ->
                                    Copy.updateQuizIdentifierDate model.quizIdentifier date
                                        |> updateQuizIdentifier model

                                Err _ ->
                                    model

                        QuizValues.SetQuizPlace place ->
                            Copy.updateQuizIdentifierPlace model.quizIdentifier place
                                |> updateQuizIdentifier model

                        QuizValues.SetRoundsNumber string ->
                            case QuizValues.validatePositiveNatural string of
                                Just n ->
                                  Util.adjustToSizeWith (List.repeat n QuizValues.defaultQuestionNumber) model.quizSettings.rounds
                                    |> Copy.updateQuizSettingsRounds model.quizSettings
                                    |> updateQuizSettings model

                                Nothing ->
                                    model

                        QuizValues.SetTeamsInQuiz string ->
                            case QuizValues.validatePositiveNatural string of
                                Just n ->
                                    Copy.updateQuizSettingsNumberOfTeams model.quizSettings n
                                        |> updateQuizSettings model

                                Nothing ->
                                    model

                        QuizValues.SetQuestions int string ->
                            case QuizValues.validatePositiveNatural string of
                                Just qs ->
                                    Util.updateIndex int qs model.quizSettings.rounds
                                        |> Copy.updateQuizSettingsRounds model.quizSettings
                                        |> updateQuizSettings model

                                Nothing ->
                                    model
            in
            ( newModel, Cmd.none )
