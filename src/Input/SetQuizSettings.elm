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

import Common.Authentication exposing (Authentication)
import Common.ConnectionUtil exposing (addFeedbackLabel, encodeBody, errorToString)
import Common.Constants
    exposing
        ( actionParam
        , newApi
        , quizIdParam
        , quizIdentifierParam
        , quizSettingsParam
        , updateQuizApi
        )
import Common.Copy as Copy
import Common.Types
    exposing
        ( Action(..)
        , DbQuizId
        , Labels
        , QuizIdentifier
        , QuizInfo
        , QuizSettings
        , jsonDecQuizInfo
        , jsonEncAction
        , jsonEncDbQuizId
        , jsonEncQuizIdentifier
        , jsonEncQuizSettings
        )
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


type alias Base =
    { quizIdentifier : QuizIdentifier
    , quizSettings : QuizSettings
    , authentication : Authentication
    , feedback : String
    }


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
    , feedback : String
    }


baseOfUpdate : UpdateModel -> Base
baseOfUpdate updateModel =
    { quizIdentifier = updateModel.quizInfo.quizIdentifier
    , quizSettings = updateModel.quizSettings
    , authentication = updateModel.authentication
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
updateQuizSettings model quizSettings =
    { model | quizSettings = quizSettings }


updateQuizIdentifier : Base -> QuizIdentifier -> Base
updateQuizIdentifier base quizIdentifier =
    { base | quizIdentifier = quizIdentifier }


updateFeedback : Base -> String -> Base
updateFeedback model feedback =
    { model | feedback = feedback }


type Msg
    = Commit
    | Back
    | Value QuizValues.Msg
    | Updated (ErrorOr ())
    | Created (ErrorOr QuizInfo)


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
                , feedback = ""
                }
            }
        )


initUpdate : Authentication -> QuizInfo -> QuizSettings -> ( UpdateModel, Cmd Msg )
initUpdate authentication quizInfo quizSettings =
    initWith
        (\auth ->
            { quizSettings = quizSettings
            , quizInfo = quizInfo
            , authentication = auth
            , feedback = ""
            }
        )
        authentication


viewWith : (model -> Base) -> String -> model -> Html Msg
viewWith baseOf commitButtonText md =
    let
        createOnEnter =
            onEnter Commit
    in
    div [ id "creatingQuizView" ]
        (QuizValues.mkCreationForm (baseOf md).quizSettings createOnEnter (baseOf md).quizSettings.labels
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
    viewWith .base "Create"


viewUpdate : UpdateModel -> Html Msg
viewUpdate =
    viewWith baseOfUpdate "Update"


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

                                Err _ ->
                                    base

                        QuizValues.SetQuizPlace place ->
                            Copy.updateQuizIdentifierPlace base.quizIdentifier place
                                |> updateQuizIdentifier base

                        QuizValues.SetRoundsNumber string ->
                            case QuizValues.validatePositiveNatural string of
                                Just n ->
                                    Util.adjustToSizeWith (List.repeat n QuizValues.defaultQuestionNumber) base.quizSettings.rounds
                                        |> Copy.updateQuizSettingsRounds base.quizSettings
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
                                    Util.updateIndex int qs base.quizSettings.rounds
                                        |> Copy.updateQuizSettingsRounds base.quizSettings
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


updateCreate : Msg -> CreateModel -> ( CreateModel, Cmd Msg )
updateCreate msg createModel =
    let
        ( updatedBase, cmd ) =
            updateWith (\base -> createQuiz base.authentication base.quizIdentifier base.quizSettings) msg createModel.base
    in
    ( updatedBase |> updateCreateBase createModel, cmd )


updateUpdate : Msg -> UpdateModel -> ( UpdateModel, Cmd Msg )
updateUpdate msg updateModel =
    let
        ( updatedBase, cmd ) =
            updateWith (\base -> updateQuiz base.authentication updateModel.quizInfo.quizId base.quizIdentifier base.quizSettings) msg (baseOfUpdate updateModel)
    in
    ( updatedBase |> updateUpdateBase updateModel, cmd )


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
