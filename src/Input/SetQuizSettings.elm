module Input.SetQuizSettings exposing
    ( CreateModel
    , Msg(..)
    , UpdateModel
    , initCreate
    , initUpdate
    , updateCreate
    , updateCreateBase
    , updateFeedback
    , updateQuizIdentifier
    , updateQuizSettings
    , updateUpdate
    , updateUpdateBase
    , viewCreate
    , viewUpdate
    )

import Basics.Extra exposing (flip)
import Common.Authentication exposing (Authentication)
import Common.Constants exposing (getQuizSettingsApi, newApi, updateQuizApi)
import Common.Copy as Copy
import Common.HttpUtil as HttpUtil
import Common.Types exposing (CreateQuizRequest, DbQuizId, Labels, QuizIdentifier, QuizInfo, QuizSettings, QuizUpdateRequest, jsonDecQuizInfo, jsonDecQuizSettings, jsonEncCreateQuizRequest, jsonEncQuizUpdateRequest)
import Common.Util exposing (ErrorOr, getMsg)
import Common.WireUtil exposing (addFeedbackLabel, errorToString, loadingSymbol)
import Date
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, id)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onEnter)
import Input.QuizValues as QuizValues exposing (Mode(..))


type alias Base =
    { quizIdentifier : QuizIdentifier
    , quizSettings : QuizSettings
    , authentication : Authentication
    , status : Status
    , feedback : String
    }


type alias Status =
    { quizSettingsLoaded : Bool
    }


hasFinishedLoading : Status -> Bool
hasFinishedLoading status =
    status.quizSettingsLoaded


updateStatusQuizSettingsLoaded : Status -> Bool -> Status
updateStatusQuizSettingsLoaded status b =
    { status | quizSettingsLoaded = b }


loaded : Status
loaded =
    { quizSettingsLoaded = True }


loading : Status
loading =
    { quizSettingsLoaded = True }


type alias CreateModel =
    { base : Base
    }


updateCreateBase : CreateModel -> Base -> CreateModel
updateCreateBase model base =
    { model | base = base }


type alias UpdateModel =
    { quizSettings : QuizSettings
    , quizInfo : QuizInfo
    , authentication : Authentication
    , status : Status
    , feedback : String
    }


baseOfUpdate : UpdateModel -> Base
baseOfUpdate updateModel =
    { quizIdentifier = updateModel.quizInfo.quizIdentifier
    , quizSettings = updateModel.quizSettings
    , authentication = updateModel.authentication
    , status = updateModel.status
    , feedback = updateModel.feedback
    }


updateUpdateBase : UpdateModel -> Base -> UpdateModel
updateUpdateBase model base =
    { model
        | quizSettings = base.quizSettings
        , quizInfo = Copy.updateQuizInfoQuizIdentifier model.quizInfo base.quizIdentifier
        , authentication = base.authentication
        , feedback = base.feedback
    }


updateQuizSettings : Base -> QuizSettings -> Base
updateQuizSettings base quizSettings =
    { base | quizSettings = quizSettings }


updateQuizIdentifier : Base -> QuizIdentifier -> Base
updateQuizIdentifier base quizIdentifier =
    { base | quizIdentifier = quizIdentifier }


updateFeedback : Base -> String -> Base
updateFeedback base feedback =
    { base | feedback = feedback }


updateStatus : Base -> Status -> Base
updateStatus base status =
    { base | status = status }


type Msg
    = Commit
    | Back
    | Value QuizValues.Msg
    | Updated (ErrorOr ())
    | Created (ErrorOr QuizInfo)
    | GotQuizSettings (ErrorOr QuizSettings)


initWith : (Authentication -> model) -> Authentication -> ( model, Cmd Msg )
initWith initial authentication =
    ( initial authentication, Cmd.none )


initCreate : Authentication -> ( CreateModel, Cmd Msg )
initCreate =
    initWith
        (\authentication ->
            { base =
                { quizSettings = QuizValues.defaultQuizSettings
                , quizIdentifier = QuizValues.defaultQuizIdentifier
                , authentication = authentication
                , status = loaded
                , feedback = ""
                }
            }
        )


initUpdate : Authentication -> QuizInfo -> ( UpdateModel, Cmd Msg )
initUpdate authentication quizInfo =
    ( { quizSettings = QuizValues.defaultQuizSettings
      , quizInfo = quizInfo
      , authentication = authentication
      , status = loading
      , feedback = ""
      }
    , getQuizSettings quizInfo.quizId
    )


viewWith : (model -> Base) -> Mode -> String -> model -> Html Msg
viewWith baseOf mode commitButtonText md =
    let
        createOnEnter =
            onEnter Commit
    in
    if not (hasFinishedLoading (baseOf md).status) then
        div [] [ loadingSymbol ]

    else
        div [ id "quizSettingsView" ]
            (QuizValues.mkCreationForm Value mode (baseOf md).quizIdentifier (baseOf md).quizSettings createOnEnter (baseOf md).quizSettings.labels
                ++ [ button
                        [ class "button"
                        , onClick Commit
                        , disabled (not (QuizValues.isValidQuizIdentifier (baseOf md).quizIdentifier))
                        ]
                        [ text commitButtonText ]
                   , button [ class "backButton", onClick Back ] [ text "Back" ]
                   , addFeedbackLabel (baseOf md).feedback
                   ]
            )


viewCreate : CreateModel -> Html Msg
viewCreate =
    viewWith .base Create "Create"


viewUpdate : UpdateModel -> Html Msg
viewUpdate =
    viewWith baseOfUpdate Update "Update"


updateWith : (Base -> Cmd Msg) -> Msg -> Base -> ( Base, Cmd Msg )
updateWith commitCommand msg base =
    case msg of
        Commit ->
            ( base, commitCommand base )

        Back ->
            ( base, Cmd.none )

        Value valueMsg ->
            let
                newModel =
                    case valueMsg of
                        QuizValues.LabelsUpdate labelsField string ->
                            Copy.updateLabelsByField base.quizSettings.labels labelsField string
                                |> Copy.updateQuizSettingsLabels base.quizSettings
                                |> updateQuizSettings base

                        QuizValues.SetQuizName quizName ->
                            Copy.updateQuizIdentifierName base.quizIdentifier quizName
                                |> updateQuizIdentifier base

                        QuizValues.SetQuizDate quizDate ->
                            case Date.fromIsoString quizDate of
                                Ok date ->
                                    Copy.updateQuizIdentifierDate base.quizIdentifier date
                                        |> updateQuizIdentifier base

                                Err error ->
                                    updateFeedback base error

                        QuizValues.SetQuizPlace place ->
                            Copy.updateQuizIdentifierPlace base.quizIdentifier place
                                |> updateQuizIdentifier base

                        QuizValues.SetRoundsNumber string ->
                            case QuizValues.validatePositiveNatural string of
                                Just n ->
                                    QuizValues.adjustToSize base.quizSettings.questionsInQuiz n
                                        |> Copy.updateQuizSettingsQuestionsInQuiz base.quizSettings
                                        |> updateQuizSettings base

                                Nothing ->
                                    base

                        QuizValues.SetTeamsInQuiz string ->
                            case QuizValues.validatePositiveNatural string of
                                Just n ->
                                    Copy.updateQuizSettingsNumberOfTeams base.quizSettings n
                                        |> updateQuizSettings base

                                Nothing ->
                                    base

                        QuizValues.SetQuestions int string ->
                            case QuizValues.validatePositiveNatural string of
                                Just qs ->
                                    QuizValues.updateQuestionsInQuizAt
                                        base.quizSettings.questionsInQuiz
                                        { questionsInRoundRoundNumber = int
                                        , questionsInRoundNumberOfQuestions = qs
                                        }
                                        |> Copy.updateQuizSettingsQuestionsInQuiz base.quizSettings
                                        |> updateQuizSettings base

                                Nothing ->
                                    base
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
            ( updateFeedback base feedback, Cmd.none )

        Created _ ->
            ( base, Cmd.none )

        GotQuizSettings quizSettingsCandidate ->
            let
                newBase =
                    case quizSettingsCandidate of
                        Ok quizSettings ->
                            updateStatusQuizSettingsLoaded base.status True
                                |> updateStatus base
                                |> flip updateQuizSettings quizSettings

                        Err error ->
                            updateFeedback base (errorToString error)
            in
            ( newBase, Cmd.none )


updateCreate : Msg -> CreateModel -> ( CreateModel, Cmd Msg )
updateCreate msg createModel =
    let
        ( updatedBase, cmd ) =
            updateWith
                (\base ->
                    createQuiz base.authentication
                        { createQuizRequestQuizIdentifier = base.quizIdentifier
                        , createQuizRequestQuizSettings = base.quizSettings
                        }
                )
                msg
                createModel.base
    in
    ( updatedBase |> updateCreateBase createModel, cmd )


updateUpdate : Msg -> UpdateModel -> ( UpdateModel, Cmd Msg )
updateUpdate msg updateModel =
    let
        ( updatedBase, cmd ) =
            updateWith
                (\base ->
                    updateQuiz base.authentication
                        { quizUpdateRequestQuizId = updateModel.quizInfo.quizId
                        , quizUpdateRequestQuizIdentifier = base.quizIdentifier
                        , quizUpdateRequestQuizSettings = base.quizSettings
                        }
                )
                msg
                (baseOfUpdate updateModel)
    in
    ( updatedBase |> updateUpdateBase updateModel, cmd )


createQuiz : Authentication -> CreateQuizRequest -> Cmd Msg
createQuiz authentication createQuizRequest =
    HttpUtil.postJsonWithCredentials
        authentication
        { url = newApi
        , body =
            jsonEncCreateQuizRequest createQuizRequest
        , expect = HttpUtil.expectJson Created jsonDecQuizInfo
        }


updateQuiz : Authentication -> QuizUpdateRequest -> Cmd Msg
updateQuiz authentication quizUpdateRequest =
    HttpUtil.postJsonWithCredentials
        authentication
        { url = updateQuizApi
        , body = jsonEncQuizUpdateRequest quizUpdateRequest
        , expect = HttpUtil.expectWhatever Updated
        }


getQuizSettings : DbQuizId -> Cmd Msg
getQuizSettings =
    getMsg getQuizSettingsApi GotQuizSettings jsonDecQuizSettings
