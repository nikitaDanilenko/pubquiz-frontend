module QuizInput exposing ( main )

import Browser
import Copy exposing (updateLabelsByField, updateQuizPDNName, updateQuizSettingsLabels, updateQuizSettingsNumberOfTeams, updateQuizSettingsRounds)
import Crypto.Hash exposing     ( sha512 )
import Html exposing            ( Html )
import Http
import Json.Encode as Encode
import Json.Decode as Decode
import RequestUtils exposing (RestKey, RestParam, RestValue, encodeWithSignature, mkJSONParams, mkParams)
import Types exposing (Action(..), Credentials, DbQuizId, Labels, Password, QuizName, QuizPDN, QuizSettings, UserHash, UserName, jsonDecLabels, jsonDecUserHash, jsonEncAction, jsonEncDbQuizId, jsonEncLabels, jsonEncPassword, jsonEncQuizName, jsonEncQuizPDN, jsonEncQuizSettings, jsonEncUserName)
import Url.Builder exposing     ( string )
-- todo Write all out.
import Base exposing            ( SessionKey )
import Constants exposing       ( .. )
import Model exposing           ( .. )
import NewUser exposing         ( NewUser )
import Parser exposing          ( int, float, run )
import QuizRatings
import RoundRating
import Util exposing            ( isValidInternalQuizName, adjustToSizeWith, updateIndex )
import Validity
import Views exposing           ( .. )

main : Program () Model Msg
main =
  Browser.document
    { init          = initialModelFunction
    , view          = \model -> { title = "QuizRatings Interface", body = [ view model ] }
    , update        = update
    , subscriptions = \_ -> Sub.none
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
    SetUser u               -> ({ model | user = u }, Cmd.none)
    SetPassword p           -> ({ model | password = p}, Cmd.none)
    GetAll                  -> ({ model | displayState = Authenticating,
                                          currentQuizRatings = QuizRatings.empty,
                                          currentQuizInfo = Model.defaultQuizInfo,
                                          currentQuizSettings = Model.defaultQuizSettings },
                                 getAll)
    ResponseF tpe (Ok text) ->
      case tpe of
        GotAll    -> ({ model | quizzes = String.lines text, 
                                displayState = Selecting,
                                feedback = "",
                                currentQuizInfo = Model.defaultQuizInfo,
                                currentQuizRatings = QuizRatings.empty },
                      Cmd.none)
        GotSingleQuiz   -> let updatedModel = updateQuizByText text model
                           in ({ updatedModel | displayState = Editing ContentsE }, Cmd.none)
        Logged          -> let hash = case Decode.decodeString jsonDecUserHash text of
                                        Ok h -> h
                                        Err _ -> ""
                            in ({ model | feedback = "", oneWayHash = hash }, getAll)
        GotSingleLabels -> let (labels, feedback) =
                                 case Decode.decodeString jsonDecLabels text of
                                   Ok ls -> (ls, "")
                                   _       -> (Model.defaultLabels,
                                               "Cannot parse server response, using default labels.")
                           in ({ model | feedback = feedback, 
                                         currentQuizSettings = updateQuizSettingsLabels model.currentQuizSettings labels,
                                         displayState = Editing LabelsE }, Cmd.none)

    ResponseF _ (Err err)   -> ({ model | feedback = errorToString err}, Cmd.none)

    ResponseP tpe (Ok _)    ->
      case tpe of
        Locked      -> ({ model | feedback = String.concat ["Locked ", model.currentQuizInfo.identifier.name] }, getAll)
        Updated     -> ({ model | feedback = "Update successful"}, Cmd.none)
        CreatedQuiz -> ({ model | currentQuizSettings = Model.defaultQuizSettings },
                        getSingle model.currentQuizInfo.quizId)
        CreatedUser -> ({ model | newUser = NewUser.emptyUser, 
                                  feedback = String.join " " [ "Created user", model.newUser.user ] 
                         }, getAll)

    ResponseP _ (Err err)   -> ({ model | feedback = errorToString err}, Cmd.none)
    
    GetSingle qName         -> ({ model | currentQuizInfo = qName, feedback = "" }, getSingle qName)
    SetTeamsInQuiz s text   -> let tu = processTeamUpdate s text model
                                   newQuiz = QuizRatings.adjustTo tu.teams model.currentRatings
                               in ({ model | currentQuizSettings = updateQuizSettingsNumberOfTeams model.currentQuizSettings tu.teams,
                                             currentRatings = newQuiz,
                                             feedback = tu.response }, Cmd.none)
    UpdatePoints r g ps     -> let (np, response) =
                                    case run float ps of
                                     Ok p -> 
                                      let maxPs = (QuizRatings.getRound r model.currentRatings).maxPoints
                                      in if p <= maxPs then (p, "")
                                         else (maxPs, 
                                               String.join " " ["The maximum number of points",
                                                                "in this round is",
                                                                String.fromFloat maxPs])
                                     Err _ -> (0, 
                                               String.join " " 
                                                           ["Invalid decimal point number",
                                                            "at round =", 
                                                            String.fromInt (1 + r),
                                                            "and team =",
                                                            String.concat [String.fromInt (1 + g), 
                                                                           "."],
                                                            "Substituting 0."])
                                   newQuiz = QuizRatings.update r g np model.currentRatings
                                   valid = QuizRatings.arePointsValid newQuiz
                               in ({ model | currentRatings = newQuiz,
                                             feedback = response,
                                             isValidQuizUpdate = 
                                              Validity.updatePoints valid model.isValidQuizUpdate
                                    },
                                   Cmd.none)
    AddRound                -> let newQuiz = QuizRatings.addRound (RoundRating.emptyOfSize model.currentQuizSettings.numberOfTeams)
                                                           model.currentRatings
                               in ({ model | currentRatings = newQuiz }, Cmd.none)
    SetMaxPoints rd ps      -> let newModel =
                                    case run float ps of
                                      Ok p -> 
                                        let newQuiz = QuizRatings.updateMax rd p model.currentRatings
                                            valid = QuizRatings.arePointsValid newQuiz
                                        in { model | currentRatings = newQuiz,
                                                     isValidQuizUpdate = 
                                                       Validity.updatePoints valid 
                                                                             model.isValidQuizUpdate
                                              }
                                      Err _ -> { model | feedback = "Not a decimal point number."}
                               in (newModel, Cmd.none)
    PostUpdate qName points -> (model, postUpdate model.user model.oneWayHash qName points)
    
    
    AcknowledgeLock         -> ({ model | displayState = ConfirmingLock }, Cmd.none)
    LockQuiz qName              -> (model, postLock model.user model.oneWayHash qName)
    
    Login                   -> ({ model | displayState = Authenticating }, 
                                login model.user model.password)

    StartCreatingQuiz       -> ({ model | displayState = CreatingQuiz }, Cmd.none)
    SetNewQuizName name     -> let feedback = 
                                    if isValidInternalQuizName name then "" 
                                    else String.join " " ["Internal name contains invalid symbols:",
                                                          "only characters, numbers",
                                                          "_ and - are allowed."]
                               in ({ model | createQuizPDN = updateQuizPDNName model.createQuizPDN name,
                                             feedback = feedback }, Cmd.none)
    SetRoundsNumber rs      -> let newModel = 
                                     Util.foldMaybe { model | feedback = "Not a valid number of teams." }
                                                    (\r -> { model | currentQuizSettings = updateQuizSettingsRounds model.currentQuizSettings (adjustToSizeWith Model.defaultQuestionNumber r model.currentQuizSettings.rounds),
                                                                     feedback = "" }) 
                                                    (validatePositiveNatural rs)
                               in (newModel, Cmd.none)
    UpdateQuestions i txt   -> let rs = model.currentQuizSettings.rounds
                                   (newRs, feedback) =
                                     Util.foldMaybe (rs, "Not a natural number larger than zero.")
                                                    (\q -> (updateIndex i q rs, ""))
                                                    (validatePositiveNatural txt)
                               in ({ model | currentQuizSettings = updateQuizSettingsRounds model.currentQuizSettings newRs,
                                             feedback = feedback}, Cmd.none)

    CreateQuiz              -> if String.isEmpty (model.createQuizPDN.name)
                                then ({ model | feedback = "Empty quiz name" }, Cmd.none)
                                else
                                (model,
                                      createNewQuiz model.user
                                                    model.oneWayHash
                                                    model.createQuizPDN
                                                    model.currentQuizSettings)
    StartCreatingUser       -> ({ model | newUser = NewUser.emptyUser, displayState = CreatingUser},
                                Cmd.none)
    SetNewUserParam fld txt -> let nu = NewUser.update fld txt model.newUser
                               in ({ model | newUser =  nu }, 
                                Cmd.none)

    CreateUser              -> (model, createNewUser model.user model.oneWayHash model.newUser)
    LabelsUpdate fld text   -> let lbls = updateLabelsByField fld text model.currentQuizSettings.labels
                               in ({ model | currentQuizSettings = updateQuizSettingsLabels model.currentQuizSettings lbls }, Cmd.none)
    SetTeamName i teamName  -> let newQuiz = QuizRatings.updateTeamName i teamName model.currentRatings
                                   valid = QuizRatings.allTeamNamesValid newQuiz
                                   validity = Validity.updateTeamNames valid model.isValidQuizUpdate
                                   error = String.join " " ["Team name of team", 
                                                            String.fromInt ( 1+ i),
                                                            "contains invalid symbols",
                                                            "(", 
                                                            String.fromChar QuizRatings.teamNameSeparator,
                                                            ")." ]
                                   feedback = if valid then "" else error 
                               in ({ model | currentRatings = newQuiz,
                                             feedback = feedback,
                                             isValidQuizUpdate = validity }, Cmd.none)
    GetLabels               -> (model, getQuizLabels model.currentQuizInfo)
    PostQuizSettingsUpdate q qs
                            -> (model, updateQuizSettings model.user model.oneWayHash q qs)

view : Model -> Html Msg
view model = 
    let currentView = case model.displayState of
            Authenticating -> authenticationView
            Initial -> authenticationView
            Editing ContentsE -> editingView
            Editing LabelsE -> editingLabelsView 
            Selecting -> selectionView
            ConfirmingLock -> confirmView
            CreatingQuiz -> creatingQuizView
            CreatingUser -> creatingUserView
     in wrapView currentView model

login : UserName -> Password -> Cmd Msg
login user password = Http.post {
        url = loginApi,
        expect = Http.expectString (ResponseF Logged),
        body = encodeBody (mkJSONParams [ (userParam, jsonEncUserName user), (passwordParam, jsonEncPassword password) ])
    }

getAll : Cmd Msg
getAll = Http.get { 
    url = allApi, 
    expect = Http.expectString (ResponseF GotAll)
  }

type QuizPart = DataPart | LabelsPart

getMsg : QuizPart -> DbQuizId -> Cmd Msg
getMsg part quizId =
  let (path, action) = 
        case part of
          DataPart -> ("getQuizData", ResponseF GotSingleQuiz)
          LabelsPart -> ("getQuizLabels", ResponseF GotSingleLabels)
  in Http.get { 
        url = Url.Builder.relative [ quizApi, path ] [ string quizParam (Encode.encode 0 (jsonEncDbQuizId quizId)) ],
        expect = Http.expectString action
    }

getSingle : DbQuizId -> Cmd Msg
getSingle = getMsg DataPart

getQuizLabels : DbQuizId -> Cmd Msg
getQuizLabels = getMsg LabelsPart

postUpdate : UserName -> SessionKey -> QuizName -> String -> Cmd Msg
postUpdate u sk quizName points = 
    let params = mkParamsWithSignature u sk [(quizParam, quizName), 
                                             (roundsParam, points)]
    in Http.post {
        url = updateApi,
        body = encodeBody params,
        expect = Http.expectWhatever (ResponseP Updated)
    }

postLock : UserName -> SessionKey -> QuizName -> Cmd Msg
postLock u sk quizName = 
    let params = encodeWithSignature u sk [(quizParam, jsonEncQuizName quizName), (actionParam, jsonEncAction LockA)]
    in Http.post {
        url = lockApi,
        body = encodeBody params,
        expect = Http.expectWhatever (ResponseP Locked)
    }

createNewQuiz : UserName -> UserHash -> QuizPDN -> QuizSettings -> Cmd Msg
createNewQuiz u sk pdn s =
  Http.post {
      url = newApi,
      body = encodeBody (encodeWithSignature u sk [(quizPDNParam, jsonEncQuizPDN pdn),
                                                   (quizSettingsParam, jsonEncQuizSettings s),
                                                   (actionParam, jsonEncAction CreateQuizA)]),
      expect = Http.expectWhatever (ResponseP CreatedQuiz)
    }

createNewUser : UserName -> SessionKey -> NewUser -> Cmd Msg
createNewUser u sk newUser = 
    let params = mkParamsWithSignature u sk [(newUserParam, newUser.user), 
                                             (passwordParam, newUser.password1)]
    in Http.post {
        url = newUserApi,
        body = encodeBody params,
        expect = Http.expectWhatever (ResponseP CreatedUser)
    }

updateQuizSettings : UserName -> SessionKey -> DbQuizId -> QuizSettings -> Cmd Msg
updateQuizSettings u sk qid settings =
    let params = encodeWithSignature u sk [(quizIdParam, jsonEncDbQuizId qid),
                                           (quizSettingsParam, jsonEncQuizSettings settings),
                                           (actionParam, jsonEncAction UpdateSettingsA)]
    in Http.post {
        url = updateQuizSettingsApi,
        body = encodeBody params,
        expect = Http.expectWhatever (ResponseP Updated)
    }

updateQuizByText : String -> Model -> Model
updateQuizByText text model = 
    case QuizRatings.parseQuiz text of
        Ok quiz -> let guess = QuizRatings.numberOfTeams quiz
                       actual = if guess == 0 then model.currentQuizSettings.numberOfTeams else guess
                       pointsValid = QuizRatings.arePointsValid quiz
                       validity = { pointsValid = pointsValid,
                                    serverTextOK = True, 
                                    teamNamesValid = True }
                   in { model | currentRatings = quiz,
                                currentQuizSettings = updateQuizSettingsNumberOfTeams model.currentQuizSettings actual,
                                isValidQuizUpdate = validity,
                                feedback = ""
                   }
        Err _   -> { model | isValidQuizUpdate =
                              Validity.updateServerText False model.isValidQuizUpdate, 
                             feedback = "Parsing error"
                   }

mkWithSignature : UserName -> SessionKey -> List (RestKey, RestValue) -> List (RestKey, RestValue)
mkWithSignature u key kvs = 
    let allParams = (userParam, u) :: kvs
        sig = sha512 (String.concat [key, mkParams allParams])
    in (signatureParam, sig) :: allParams

mkParamsWithSignature : UserName -> SessionKey -> List (RestKey, RestValue) -> RestParam
mkParamsWithSignature u key kvs = mkParams (mkWithSignature u key kvs)

encodeBody : String -> Http.Body
encodeBody = Http.stringBody "application/x-www-form-urlencoded"

processTeamUpdate : TeamUpdateSetting -> String -> Model -> { teams : Int, response : String }
processTeamUpdate setting text model = 
  let (ts, r) = case run int text of
                  Ok n -> 
                    case setting of
                      InitialTU      -> (n, "")
                      IntermediateTU -> 
                        let maxTeams = QuizRatings.maxNumberOfTeams model.currentRatings
                        in if n <= maxTeams then (n, "")
                           else (maxTeams, 
                                 String.join " " ["QuizRatings supports only",
                                                  String.fromInt maxTeams, 
                                                  "teams."])
                  Err _ -> (0, "Invalid team number. Substituting 0.")
  in { teams = ts, response = r }

validatePositiveNatural : String -> Maybe Int
validatePositiveNatural txt = 
  case run int txt of
    Ok n -> if n > 0 then Just n else Nothing
    _    -> Nothing
