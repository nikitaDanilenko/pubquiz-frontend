module Input.SetQuizSettings exposing (..)

import Common.Authentication exposing (Authentication)
import Common.ConnectionUtil exposing (addFeedbackLabel, encodeBody, errorToString)
import Common.Constants exposing (actionParam, newApi, quizIdParam, quizIdentifierParam, quizSettingsParam, updateQuizApi)
import Common.Copy as Copy
import Common.Types exposing (Action(..), DbQuizId, Labels, QuizIdentifier, QuizInfo, QuizSettings, jsonDecQuizInfo, jsonEncAction, jsonEncDbQuizId, jsonEncQuizIdentifier, jsonEncQuizSettings)
import Common.Util as Util
import Date
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, id)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onEnter)
import Http
import Input.Model exposing (ErrorOr)
import Input.QuizValues as QuizValues
import Input.RequestUtils exposing (encodeWithSignature)


type alias Model =
    { quizIdentifier : QuizIdentifier
    , quizSettings : QuizSettings
    , authentication : Authentication
    , feedback : String
    , usage : UseAs
    }


updateQuiz : Model -> QuizSettings -> Model


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
    | Back
    | Value QuizValues.Msg
    | Updated (ErrorOr ())
    | Created (ErrorOr QuizInfo)


type InitialiseAs
    = CreateInitial
    | UpdateInitial DbQuizId QuizIdentifier QuizSettings


type UseAs
    = CreateUsage
    | UpdateUsage DbQuizId


init : Authentication -> InitialiseAs -> ( Model, Cmd Msg )
init authentication usage =
    let
        ( quizIdentifier, quizSettings, usagePlain ) =
            case usage of
                CreateInitial ->
                    ( QuizValues.defaultQuizIdentifier, QuizValues.defaultQuizSettings, CreateUsage )

                UpdateInitial qid quizIdentifier quizSettings ->
                    ( quizIdentifier, quizSettings, UpdateUsage qid )

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
                CreateUsage ->
                    "Create"

                UpdateUsage _ ->
                    "Update"
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
            let
                command =
                    case model.usage of
                        CreateUsage ->
                            createQuiz model.authentication model.quizIdentifier model.quizSettings

                        UpdateUsage qid ->
                            updateQuiz model.authentication qid model.quizIdentifier model.quizSettings
            in
            ( model, command )

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

        Updated response ->
            let
                feedback =
                    case response of
                        Ok _ ->
                            "Update successful."

                        Err error ->
                            errorToString error
            in
            ( updateFeedback model feedback, Cmd.none )

        Created _ ->
            ( model, Cmd.none )


createQuiz : Authentication -> QuizIdentifier -> QuizSettings -> Cmd Msg
createQuiz authentication idf s =
    Http.post
        { url = newApi
        , body =
            encodeBody
                (encodeWithSignature authentication
                    [ ( quizIdentifierParam, jsonEncQuizIdentifier idf )
                    , ( quizSettingsParam, jsonEncQuizSettings s )
                    , ( actionParam, jsonEncAction CreateQuizA )
                    ]
                )
        , expect = Http.expectJson Created jsonDecQuizInfo
        }


updateQuiz : Authentication -> DbQuizId -> QuizIdentifier -> QuizSettings -> Cmd Msg
updateQuiz authentication qid quizIdentifier quizSettings =
    let
        params =
            encodeWithSignature authentication
                [ ( quizIdParam, jsonEncDbQuizId qid )
                , ( quizIdentifierParam, jsonEncQuizIdentifier quizIdentifier )
                , ( quizSettingsParam, jsonEncQuizSettings quizSettings )
                , ( actionParam, jsonEncAction UpdateSettingsA )
                ]
    in
    Http.post
        { url = updateQuizApi
        , body = encodeBody params
        , expect = Http.expectWhatever Updated
        }
