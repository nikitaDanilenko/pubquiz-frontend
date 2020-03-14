module Input.SetQuizSettings exposing (..)

import Common.Authentication exposing (Authentication)
import Common.ConnectionUtil exposing (addFeedbackLabel, errorToString)
import Common.Constants exposing (getQuizInfoApi)
import Common.Copy as Copy
import Common.Types exposing (DbQuizId, Labels, QuizIdentifier, QuizSettings, jsonDecQuizInfo)
import Common.Util as Util exposing (getMsg)
import Date
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, id)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onEnter)
import Input.Model exposing (ErrorOr)
import Input.QuizValues as QuizValues


type alias Model =
    { quizIdentifier : QuizIdentifier
    , quizSettings : QuizSettings
    , authentication : Authentication
    , feedback : String
    , usage : UsagePlain
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
    = Commit
    | GotUpdate UpdatePart
    | Back
    | Value QuizValues.Msg


type UpdatePart
    = QuizIdentifierPart (ErrorOr QuizIdentifier)
    | QuizSettingsPart (ErrorOr QuizSettings)


type Usage
    = Create
    | Update QuizIdentifier QuizSettings

type UsagePlain = CreatePlain | UpdatePlain

init : Authentication -> Usage -> ( Model, Cmd Msg )
init authentication usage =
    let
        (quizIdentifier, quizSettings, usagePlain) =
          case usage of
            Create -> (QuizValues.defaultQuizIdentifier, QuizValues.defaultQuizSettings, CreatePlain)


            Update quizIdentifier quizSettings ->
              (quizIdentifier, quizSettings, UpdatePlain)


        initialModel =
            { quizIdentifier = quizIdentifier
            , quizSettings = quizSettings
            , authentication = authentication
            , feedback = ""
            , usage = usagePlain
            }

    in
    ( initialModel, Cmd.none )


view : Model -> Html Msg
view md =
    let
        createOnEnter =
            onEnter Commit
        buttonText =
          case md.usage of
            CreatePlain -> "Create"


            UpdatePlain -> "Update"

    in
    div [ id "creatingQuizView" ]
        (QuizValues.mkCreationForm md.quizSettings createOnEnter md.quizSettings.labels
            ++ [ button
                    [ class "button"
                    , onClick Commit
                    , disabled (not (QuizValues.isValidQuizIdentifier md.quizIdentifier))
                    ]
                    [ text buttonText ]
               , button [ class "backButton", onClick Back ] [ text "Back" ]
               , addFeedbackLabel md.feedback
               ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Commit ->
            ( model, Cmd.none )

        Back ->
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

        GotUpdate updatePart ->
            let
                newModel =
                    case updatePart of
                        QuizIdentifierPart quizIdentifierCandidate ->
                            case quizIdentifierCandidate of
                                Ok quizIdentifier ->
                                    updateQuizIdentifier model quizIdentifier

                                Err error ->
                                    updateFeedback model (errorToString error)

                        QuizSettingsPart quizSettingsCandidate ->
                            case quizSettingsCandidate of
                                Ok quizSettings ->
                                    updateQuizSettings model quizSettings

                                Err error ->
                                    updateFeedback model (errorToString error)
            in
            ( newModel, Cmd.none )

getQuizIdentifier : DbQuizId -> Cmd Msg
getQuizIdentifier =
  getMsg getQuizInfoApi (Result.map .quizIdentifier >> QuizIdentifierPart >> GotUpdate) jsonDecQuizInfo