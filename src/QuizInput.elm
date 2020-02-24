module QuizInput exposing (main)

-- todo Write all out.

import Base exposing (SessionKey)
import Browser
import Constants exposing (..)
import Copy exposing (updateLabelsByField, updateQuizIdentifierName, updateQuizInfoQuizId, updateQuizInfoQuizIdentifier, updateQuizSettingsLabels, updateQuizSettingsNumberOfTeams, updateQuizSettingsRounds)
import Crypto.Hash exposing (sha512)
import Html exposing (Html)
import Http exposing (Error)
import Json.Decode as Decode
import Json.Encode as Encode
import Model exposing (..)
import NewUser exposing (NewUser)
import Parser exposing (float, int, run)
import QuizRatings
import RequestUtils exposing (RestKey, RestParam, RestValue, encodeWithSignature, mkJSONParams, mkParams)
import RoundRating
import Types exposing (Action(..), Credentials, DbQuizId, Labels, Password, QuizIdentifier, QuizName, QuizRatings, QuizSettings, UserHash, UserName, jsonDecDbQuizId, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings, jsonDecUserHash, jsonEncAction, jsonEncDbQuizId, jsonEncPassword, jsonEncQuizIdentifier, jsonEncQuizRatings, jsonEncQuizSettings, jsonEncUserName)
import Url.Builder exposing (string)
import Util exposing (adjustToSizeWith, isValidInternalQuizName, updateIndex)
import Validity
import Views exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = initialModelFunction
        , view = \model -> { title = "QuizRatings Interface", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUser u ->
            ( { model | user = u }, Cmd.none )

        SetPassword p ->
            ( { model | password = p }, Cmd.none )

        GetAll ->
            ( { model
                | displayState = Authenticating
                , currentQuizRatings = QuizRatings.empty
                , currentQuizInfo = Model.defaultQuizInfo
                , currentQuizSettings = Model.defaultQuizSettings
              }
            , getAll
            )

        ResponseF tpe ->
            case tpe of
                GotAll qis ->
                    ( { model
                        | quizzes = Result.withDefault [] qis
                        , displayState = Selecting
                        , feedback = ""
                        , currentQuizInfo = Model.defaultQuizInfo
                        , currentQuizRatings = QuizRatings.empty
                      }
                    , Cmd.none
                    )

                GotQuizRatings c ->
                    ( updateQuizByQuizRatings c model, Cmd.none )

                Logged c ->
                    ( { model | feedback = "", oneWayHash = Result.withDefault "" c }, getAll )

                GotLabels c ->
                    let
                        ( labels, feedback ) =
                            case c of
                                Ok ls ->
                                    ( ls, "" )

                                _ ->
                                    ( Model.defaultLabels
                                    , "Cannot parse server response, using default labels."
                                    )
                    in
                    ( { model
                        | feedback = feedback
                        , currentQuizSettings = updateQuizSettingsLabels model.currentQuizSettings labels
                        , displayState = Editing LabelsE
                      }
                    , Cmd.none
                    )

                CreatedQuiz c ->
                    case c of
                        Ok qid ->
                          let newQuizInfo = updateQuizInfoQuizId model.currentQuizInfo qid
                          in
                            ( { model
                                | currentQuizSettings = Model.defaultQuizSettings
                                , currentQuizInfo = updateQuizInfoQuizId model.currentQuizInfo qid
                                , quizzes = model.quizzes ++ [newQuizInfo]
                              }
                            , getQuizRatings qid
                            )

                        Err err ->
                            ( { model | feedback = errorToString err }, Cmd.none )

        ResponseP tpe (Ok _) ->
            case tpe of
                Locked ->
                    ( { model | feedback = String.concat [ "Locked ", model.currentQuizInfo.quizIdentifier.name ] }, getAll )

                Updated ->
                    ( { model | feedback = "Update successful" }, Cmd.none )

                CreatedUser ->
                    ( { model
                        | newUser = NewUser.emptyUser
                        , feedback = String.join " " [ "Created user", model.newUser.user ]
                      }
                    , getAll
                    )

        ResponseP _ (Err err) ->
            ( { model | feedback = errorToString err }, Cmd.none )

        GetSingle qid ->
            ( { model
                | currentQuizInfo = updateQuizInfoQuizId model.currentQuizInfo qid
                , feedback = ""
              }
            , getQuizRatings qid
            )

        SetTeamsInQuiz s text ->
            let
                tu =
                    processTeamUpdate s text model

                newQuizRatings =
                    QuizRatings.adjustTo tu.teams model.currentQuizRatings
            in
            ( { model
                | currentQuizSettings = updateQuizSettingsNumberOfTeams model.currentQuizSettings tu.teams
                , currentQuizRatings = newQuizRatings
                , feedback = tu.response
              }
            , Cmd.none
            )

        UpdatePoints rn tn ps ->
            let
                ( np, response ) =
                    case run float ps of
                        Ok p ->
                            let
                                maxPs =
                                    (QuizRatings.getRound rn model.currentQuizRatings).reachableInRound
                            in
                            if p <= maxPs then
                                ( p, "" )

                            else
                                ( maxPs
                                , String.join " "
                                    [ "The maximum number of points"
                                    , "in this round is"
                                    , String.fromFloat maxPs
                                    ]
                                )

                        Err _ ->
                            ( 0
                            , String.join " "
                                [ "Invalid decimal point number"
                                , "at round ="
                                , String.fromInt rn
                                , "and team ="
                                , String.concat
                                    [ String.fromInt tn
                                    , "."
                                    ]
                                , "Substituting 0."
                                ]
                            )

                newQuiz =
                    QuizRatings.update rn tn np model.currentQuizRatings

                valid =
                    QuizRatings.arePointsValid newQuiz
            in
            ( { model
                | currentQuizRatings = newQuiz
                , feedback = response
                , isValidQuizUpdate =
                    Validity.updatePoints valid model.isValidQuizUpdate
              }
            , Cmd.none
            )

        AddRound ->
            let
                newQuiz =
                    QuizRatings.addRound
                        (RoundRating.emptyOfSize model.currentQuizSettings.numberOfTeams)
                        model.currentQuizRatings
            in
            ( { model | currentQuizRatings = newQuiz }, Cmd.none )

        SetMaxPoints rd ps ->
            let
                newModel =
                    case run float ps of
                        Ok p ->
                            let
                                newQuiz =
                                    QuizRatings.updateMax rd p model.currentQuizRatings

                                valid =
                                    QuizRatings.arePointsValid newQuiz
                            in
                            { model
                                | currentQuizRatings = newQuiz
                                , isValidQuizUpdate =
                                    Validity.updatePoints valid
                                        model.isValidQuizUpdate
                            }

                        Err _ ->
                            { model | feedback = "Not a decimal point number." }
            in
            ( newModel, Cmd.none )

        PostUpdate qName points ->
            ( model, postUpdate model.user model.oneWayHash qName points )

        AcknowledgeLock ->
            ( { model | displayState = ConfirmingLock }, Cmd.none )

        LockQuiz qName ->
            ( model, postLock model.user model.oneWayHash qName )

        Login ->
            ( { model | displayState = Authenticating }
            , login model.user model.password
            )

        StartCreatingQuiz ->
            ( { model | displayState = CreatingQuiz }, Cmd.none )

        SetNewQuizName name ->
            let
                feedback =
                    if isValidInternalQuizName name then
                        ""

                    else
                        String.join " "
                            [ "Internal name contains invalid symbols:"
                            , "only characters, numbers"
                            , "_ and - are allowed."
                            ]
            in
            ( { model
                | currentQuizInfo = name |> updateQuizIdentifierName model.currentQuizInfo.quizIdentifier |> updateQuizInfoQuizIdentifier model.currentQuizInfo
                , feedback = feedback
              }
            , Cmd.none
            )

        SetRoundsNumber rs ->
            let
                newModel =
                    Util.foldMaybe { model | feedback = "Not a valid number of teams." }
                        (\r ->
                            { model
                                | currentQuizSettings = updateQuizSettingsRounds model.currentQuizSettings (adjustToSizeWith (List.repeat r Model.defaultQuestionNumber) model.currentQuizSettings.rounds)
                                , feedback = ""
                            }
                        )
                        (validatePositiveNatural rs)
            in
            ( newModel, Cmd.none )

        UpdateQuestions i txt ->
            let
                rs =
                    model.currentQuizSettings.rounds

                ( newRs, feedback ) =
                    Util.foldMaybe ( rs, "Not a natural number larger than zero." )
                        (\q -> ( updateIndex i q rs, "" ))
                        (validatePositiveNatural txt)
            in
            ( { model
                | currentQuizSettings = updateQuizSettingsRounds model.currentQuizSettings newRs
                , feedback = feedback
              }
            , Cmd.none
            )

        CreateQuiz ->
            if String.isEmpty model.currentQuizInfo.quizIdentifier.name then
                ( { model | feedback = "Empty quiz name" }, Cmd.none )

            else
                ( model
                , createNewQuiz model.user
                    model.oneWayHash
                    model.currentQuizInfo.quizIdentifier
                    model.currentQuizSettings
                )

        StartCreatingUser ->
            ( { model | newUser = NewUser.emptyUser, displayState = CreatingUser }
            , Cmd.none
            )

        SetNewUserParam fld txt ->
            let
                nu =
                    NewUser.update fld txt model.newUser
            in
            ( { model | newUser = nu }
            , Cmd.none
            )

        CreateUser ->
            ( model, createNewUser model.user model.oneWayHash model.newUser )

        LabelsUpdate fld text ->
            let
                lbls =
                    updateLabelsByField fld text model.currentQuizSettings.labels
            in
            ( { model | currentQuizSettings = updateQuizSettingsLabels model.currentQuizSettings lbls }, Cmd.none )

        SetTeamName tn teamName ->
            let
                newQuizRatings =
                    QuizRatings.updateTeamName tn teamName model.currentQuizRatings
            in
            ( { model
                | currentQuizRatings = newQuizRatings
                , feedback = ""
              }
            , Cmd.none
            )

        GetLabels ->
            ( model, getLabels model.currentQuizInfo.quizId )

        PostQuizSettingsUpdate q qs ->
            ( model, updateQuizSettings model.user model.oneWayHash q qs )


view : Model -> Html Msg
view model =
    let
        currentView =
            case model.displayState of
                Authenticating ->
                    authenticationView

                Initial ->
                    authenticationView

                Editing ContentsE ->
                    editingView

                Editing LabelsE ->
                    editingLabelsView

                Selecting ->
                    selectionView

                ConfirmingLock ->
                    confirmView

                CreatingQuiz ->
                    creatingQuizView

                CreatingUser ->
                    creatingUserView
    in
    wrapView currentView model


login : UserName -> Password -> Cmd Msg
login user password =
    Http.post
        { url = loginApi
        , expect = Http.expectJson (\x -> x |> Logged |> ResponseF) jsonDecUserHash
        , body = encodeBody (mkJSONParams [ ( userParam, jsonEncUserName user ), ( passwordParam, jsonEncPassword password ) ])
        }


getAll : Cmd Msg
getAll =
    Http.get
        { url = allApi
        , expect = Http.expectJson (\x -> x |> GotAll |> ResponseF) (Decode.list jsonDecQuizInfo)
        }


getMsg : String -> (Result Error a -> Msg) -> Decode.Decoder a -> DbQuizId -> Cmd Msg
getMsg path action decoder quizId =
    Http.get
        { url = Url.Builder.relative [ path ] [ string quizIdParam (Encode.encode 0 (jsonEncDbQuizId quizId)) ]
        , expect = Http.expectJson action decoder
        }


getLabels : DbQuizId -> Cmd Msg
getLabels =
    getMsg getLabelsApi (\x -> x |> GotLabels |> ResponseF) jsonDecLabels


getQuizRatings : DbQuizId -> Cmd Msg
getQuizRatings =
    getMsg getQuizRatingsApi (\x -> x |> GotQuizRatings |> ResponseF) jsonDecQuizRatings


postUpdate : UserName -> SessionKey -> DbQuizId -> QuizRatings -> Cmd Msg
postUpdate u sk qid quizRatings =
    let
        params =
            encodeWithSignature u
                sk
                [ ( quizIdParam, jsonEncDbQuizId qid )
                , ( quizRatingsParam, jsonEncQuizRatings quizRatings )
                ]
    in
    Http.post
        { url = updateApi
        , body = encodeBody params
        , expect = Http.expectWhatever (ResponseP Updated)
        }


postLock : UserName -> SessionKey -> DbQuizId -> Cmd Msg
postLock u sk qid =
    let
        params =
            encodeWithSignature u sk [ ( quizParam, jsonEncDbQuizId qid ), ( actionParam, jsonEncAction LockA ) ]
    in
    Http.post
        { url = lockApi
        , body = encodeBody params
        , expect = Http.expectWhatever (ResponseP Locked)
        }


createNewQuiz : UserName -> UserHash -> QuizIdentifier -> QuizSettings -> Cmd Msg
createNewQuiz u sk idf s =
    Http.post
        { url = newApi
        , body =
            encodeBody
                (encodeWithSignature u
                    sk
                    [ ( quizPDNParam, jsonEncQuizIdentifier idf )
                    , ( quizSettingsParam, jsonEncQuizSettings s )
                    , ( actionParam, jsonEncAction CreateQuizA )
                    ]
                )
        , expect = Http.expectJson (\x -> x |> CreatedQuiz |> ResponseF) jsonDecDbQuizId
        }


createNewUser : UserName -> SessionKey -> NewUser -> Cmd Msg
createNewUser u sk newUser =
    let
        params =
            mkParamsWithSignature u
                sk
                [ ( newUserParam, newUser.user )
                , ( passwordParam, newUser.password1 )
                ]
    in
    Http.post
        { url = newUserApi
        , body = encodeBody params
        , expect = Http.expectWhatever (ResponseP CreatedUser)
        }


updateQuizSettings : UserName -> SessionKey -> DbQuizId -> QuizSettings -> Cmd Msg
updateQuizSettings u sk qid settings =
    let
        params =
            encodeWithSignature u
                sk
                [ ( quizIdParam, jsonEncDbQuizId qid )
                , ( quizSettingsParam, jsonEncQuizSettings settings )
                , ( actionParam, jsonEncAction UpdateSettingsA )
                ]
    in
    Http.post
        { url = updateQuizSettingsApi
        , body = encodeBody params
        , expect = Http.expectWhatever (ResponseP Updated)
        }


updateQuizByQuizRatings : ErrorOr QuizRatings -> Model -> Model
updateQuizByQuizRatings eQuizRatings model =
    case eQuizRatings of
        Ok quizRatings ->
            let
                guess =
                    QuizRatings.numberOfTeams quizRatings

                actual =
                    if guess == 0 then
                        model.currentQuizSettings.numberOfTeams

                    else
                        guess

                pointsValid =
                    QuizRatings.arePointsValid quizRatings

                validity =
                    { pointsValid = pointsValid
                    , serverTextOK = True
                    , teamNamesValid = True
                    }
            in
            { model
                | currentQuizRatings = quizRatings
                , currentQuizSettings = updateQuizSettingsNumberOfTeams model.currentQuizSettings actual
                , currentQuizInfo = Maybe.withDefault Model.defaultQuizInfo (Util.find (\qi -> qi.quizId == model.currentQuizInfo.quizId) model.quizzes)
                , isValidQuizUpdate = validity
                , feedback = ""
                , displayState = Editing ContentsE
            }

        Err _ ->
            { model
                | isValidQuizUpdate =
                    Validity.updateServerText False model.isValidQuizUpdate
                , feedback = "Parsing error: Could not read quiz ratings from server"
                , displayState = Selecting
            }


mkWithSignature : UserName -> SessionKey -> List ( RestKey, RestValue ) -> List ( RestKey, RestValue )
mkWithSignature u key kvs =
    let
        allParams =
            ( userParam, u ) :: kvs

        sig =
            sha512 (String.concat [ key, mkParams allParams ])
    in
    ( signatureParam, sig ) :: allParams


mkParamsWithSignature : UserName -> SessionKey -> List ( RestKey, RestValue ) -> RestParam
mkParamsWithSignature u key kvs =
    mkParams (mkWithSignature u key kvs)


encodeBody : String -> Http.Body
encodeBody =
    Http.stringBody "application/x-www-form-urlencoded"


processTeamUpdate : TeamUpdateSetting -> String -> Model -> { teams : Int, response : String }
processTeamUpdate setting text model =
    let
        ( ts, r ) =
            case run int text of
                Ok n ->
                    case setting of
                        InitialTU ->
                            ( n, "" )

                        IntermediateTU ->
                            let
                                maxTeams =
                                    QuizRatings.maxNumberOfTeams model.currentQuizRatings
                            in
                            if n <= maxTeams then
                                ( n, "" )

                            else
                                ( maxTeams
                                , String.join " "
                                    [ "QuizRatings supports only"
                                    , String.fromInt maxTeams
                                    , "teams."
                                    ]
                                )

                Err _ ->
                    ( 0, "Invalid team number. Substituting 0." )
    in
    { teams = ts, response = r }


validatePositiveNatural : String -> Maybe Int
validatePositiveNatural txt =
    case run int txt of
        Ok n ->
            if n > 0 then
                Just n

            else
                Nothing

        _ ->
            Nothing
