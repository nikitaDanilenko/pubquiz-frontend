module QuizInput exposing ( main )

import Browser
import Crypto.Hash exposing     ( sha512 )
import Html exposing            ( Html, div, node )
import Html.Attributes exposing ( rel, type_, href, id )
import Http
import Http exposing            ( get, emptyBody, post )
import Json.Encode as Encode
import Url.Builder exposing     ( string )
-- todo Write all out.
import Base exposing            ( User, Password, SessionKey )
import Constants exposing       ( .. )
import Labels exposing          ( Labels )
import Model exposing           ( .. )
import NewUser exposing         ( NewUser )
import Parser exposing          ( int, float, run )
import Quiz
import Round
import Util exposing            ( isValidInternalQuizName )
import Views exposing           ( .. )

main : Program () Model Msg
main =
  Browser.document
    { init          = initialModelFunction
    , view          = \model -> { title = "Quiz Interface", body = [ view model ] }
    , update        = update
    , subscriptions = \_ -> Sub.none
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
    SetUser u               -> ({ model | user = u }, Cmd.none)
    SetPassword p           -> ({ model | password = p}, Cmd.none)
    GetAll                  -> ({ model | displayState = Authenticating,
                                          currentQuiz = Quiz.empty,
                                          editing = "",
                                          teamsInQuiz = initialModel.teamsInQuiz }, 
                                 getAll)

    GotAll (Err err)        -> ({ model | feedback = errorToString err}, Cmd.none)
    GotAll (Ok text)        -> ({ model | quizzes = String.lines text, 
                                          displayState = Selecting,
                                          feedback = "",
                                          createName = "" },
                                Cmd.none)
    
    GetSingle qName         -> ({ model | editing = qName, feedback = "" }, getSingle qName)
    
    GotSingle (Err err)     -> ({ model | feedback = errorToString err}, Cmd.none)
    GotSingle (Ok text)     -> let updatedModel = updateQuizByText text model
                               in ({ updatedModel | displayState = Editing ContentsE }, Cmd.none)

    SetTeamsInQuiz s text   -> let tu = processTeamUpdate s text model
                                   newQuiz = Quiz.adjustTo tu.teams model.currentQuiz
                               in ({ model | teamsInQuiz = tu.teams,
                                             currentQuiz = newQuiz,
                                             feedback = tu.response }, Cmd.none)
    UpdatePoints r g ps     -> let (np, response) =
                                    case run float ps of
                                     Ok p -> 
                                      let maxPs = (Quiz.getRound r model.currentQuiz).maxPoints
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
                               in ({ model | currentQuiz = Quiz.update r g np model.currentQuiz,
                                             feedback = response },
                                   Cmd.none)
    AddRound                -> let newQuiz = Quiz.addRound (Round.emptyOfSize model.teamsInQuiz) 
                                                           model.currentQuiz
                               in ({ model | currentQuiz = newQuiz }, Cmd.none)
    SetMaxPoints rd ps      -> let newModel =
                                    case run float ps of
                                      Ok p -> 
                                        let newQuiz = Quiz.updateMax rd p model.currentQuiz
                                        in { model | currentQuiz = newQuiz }
                                      Err _ -> { model | feedback = "Not a decimal point number."}
                               in (newModel, Cmd.none)
    SetPoints header points -> (updateQuizByText (String.join "\n" [ header, points ]) model, 
                                Cmd.none)
    PostUpdate qName points -> (model, postUpdate model.user model.oneWayHash qName points)
    Updated (Err err)       -> ({ model | feedback = errorToString err }, Cmd.none)
    Updated (Ok _)          -> ({ model | feedback = "Update successful"}, Cmd.none)
    
    AcknowledgeLock         -> ({ model | displayState = ConfirmingLock }, Cmd.none)
    Lock qName              -> (model, postLock model.user model.oneWayHash qName)
    
    Locked (Err err)        -> ({ model | feedback = errorToString err}, Cmd.none)
    Locked (Ok ok)          -> ({ model | feedback = String.concat ["Locked ", model.editing] }, 
                                getAll)

    Login                   -> ({ model | displayState = Authenticating }, 
                                login model.user model.password)
    Logged (Err err)        -> ({ model | feedback = errorToString err}, Cmd.none)
    Logged (Ok text)        -> ({ model | feedback = "", oneWayHash = text }, getAll)

    StartCreatingQuiz       -> ({ model | displayState = CreatingQuiz }, Cmd.none)
    SetNewQuizName name     -> let feedback = 
                                    if isValidInternalQuizName name then "" 
                                    else String.join " " ["Internal name contains invalid symbols:",
                                                          "only characters, numbers",
                                                          "_ and - are allowed."]
                               in ({ model | createName = name, feedback = feedback }, Cmd.none)
    SetRoundsNumber rs      -> let newModel =
                                    case run int rs of
                                     Ok r -> { model | numberOfRounds = r, feedback = "" }
                                     Err _ -> { model | feedback = "Not a valid number of teams." }
                               in (newModel, Cmd.none)
    CreateQuiz              -> if String.isEmpty (model.createName) 
                                then ({ model | feedback = "Empty quiz name" }, Cmd.none)
                                else (model, 
                                      createNewQuiz model.numberOfRounds
                                                    model.teamsInQuiz
                                                    model.user 
                                                    model.oneWayHash 
                                                    model.createName 
                                                    model.labels)
    Created (Err err)       -> ({ model | feedback = errorToString err }, Cmd.none)
    Created (Ok ok)         -> ({ model | editing = model.createName,
                                          labels = Labels.default }, 
                                getSingle model.createName)

    
    StartCreatingUser       -> ({ model | newUser = NewUser.emptyUser, displayState = CreatingUser},
                                Cmd.none)
    SetNewUserParam fld txt -> let nu = NewUser.update fld txt model.newUser
                               in ({ model | newUser =  nu }, 
                                Cmd.none)

    CreateUser              -> (model, createNewUser model.user model.oneWayHash model.newUser)
    CreatedUser (Ok ok)     -> ({ model | newUser = NewUser.emptyUser, 
                                          feedback = String.join " " [ "Created user", 
                                                                       model.newUser.user ] 
                                }, getAll)
    CreatedUser (Err err)   -> ({ model | feedback = errorToString err }, Cmd.none)

    LabelsUpdate fld text   -> let lbls = updateLabelsByField fld text model.labels
                               in ({ model | labels = lbls }, Cmd.none)
    SetTeamName i teamName  -> let newQuiz = Quiz.updateTeamName i teamName model.currentQuiz
                               in ({ model | currentQuiz = newQuiz }, Cmd.none)
    GetLabels               -> (model, getQuizLabels model.editing)
    GotLabels (Ok lbls)     -> let (labels, feedback) = 
                                    case Labels.parseLabels lbls of
                                      Just ls -> (ls, "")
                                      _       -> (Labels.empty, 
                                                  "Cannot parse server response, using empty labels.")
                               in ({ model | feedback = feedback, 
                                             labels = labels, 
                                             displayState = Editing LabelsE }, Cmd.none)
    GotLabels (Err err)     -> ({ model | feedback = errorToString err }, Cmd.none)
    PostLabelUpdate q lbls  -> (model, updateLabels model.user model.oneWayHash q lbls)

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

login : User -> Password -> Cmd Msg
login user password = Http.post {
        url = loginApi,
        expect = Http.expectString Logged,
        body = encodeBody (mkParams [ (userParam, user), (passwordParam, password) ])
    }

getAll : Cmd Msg
getAll = Http.get { 
    url = allApi, 
    expect = Http.expectString GotAll 
  }

type QuizPart = DataPart | LabelsPart

getMsg : QuizPart -> String -> Cmd Msg
getMsg part quizName =
  let (path, action) = 
        case part of
          DataPart -> ("getQuizData", GotSingle)
          LabelsPart -> ("getQuizLabels", GotLabels)
  in Http.get { 
        url = Url.Builder.relative [ quizApi, path ] [ string quizParam quizName ],
        expect = Http.expectString action
    }

getSingle : String -> Cmd Msg
getSingle = getMsg DataPart

getQuizLabels : String -> Cmd Msg
getQuizLabels = getMsg LabelsPart

postUpdate : User -> SessionKey -> QuizName -> String -> Cmd Msg
postUpdate u sk quizName points = 
    let params = mkParamsWithSignature u sk [(quizParam, quizName), 
                                             (roundsParam, points)]
    in Http.post {
        url = updateApi,
        body = encodeBody params,
        expect = Http.expectWhatever Updated
    }

postLock : User -> SessionKey -> QuizName -> Cmd Msg
postLock u sk quizName = 
    let params = mkParamsWithSignature u sk [(quizParam, quizName), (actionParam, lockQuiz)]
    in Http.post {
        url = lockApi,
        body = encodeBody params,
        expect = Http.expectWhatever Locked
    }

createNewQuiz : Int -> Int -> User -> SessionKey -> QuizName -> Labels -> Cmd Msg
createNewQuiz rs gs u sk quizName labels = 
    let params = mkWithSignature u sk [(quizParam, quizName), (actionParam, createQuiz)]
    in Http.post {
        url = newApi,
        body = encodeBody (mkParams ((roundsNumberParam, String.fromInt rs) :: 
                                     (numberOfTeamsParam, String.fromInt gs) :: 
                                     List.concat [params, Labels.toParams labels])),
        expect = Http.expectWhatever Created
    }

createNewUser : User -> SessionKey -> NewUser -> Cmd Msg
createNewUser u sk newUser = 
    let params = mkParamsWithSignature u sk [(newUserParam, newUser.user), 
                                             (passwordParam, newUser.password1)]
    in Http.post {
        url = newUserApi,
        body = encodeBody params,
        expect = Http.expectWhatever CreatedUser
    }

updateLabels : User -> SessionKey -> QuizName -> Labels -> Cmd Msg
updateLabels u sk quizName labels = 
    let params = mkWithSignature u sk [(quizParam, quizName), (actionParam, labelUpdate)]
    in Http.post {
        url = updateLabelsApi,
        body = encodeBody (mkParams (List.concat [params, Labels.toParams labels])),
        expect = Http.expectWhatever Updated
    }

updateLabelsByField : LabelsField -> String -> Labels -> Labels
updateLabelsByField field text lbls = 
    case field of
        RoundField -> { lbls | roundLabel = text }
        TeamField -> { lbls | teamLabel = text }
        OwnPointsField -> { lbls | ownPointsLabel = text }
        MaxReachedField -> { lbls | maxReachedLabel = text }
        MaxReachableField -> { lbls | maxReachableLabel = text }
        BackField -> { lbls | backToChartView = text }
        MainField -> { lbls | mainLabel = text }
        OwnPageField -> { lbls | ownPageLabel = text }
        ViewQuizzesField -> { lbls | viewQuizzesLabel = text }
        CumulativeField -> { lbls | cumulativeLabel = text }
        IndividualField -> { lbls | individualLabel = text }
        ProgressionField -> { lbls | progressionLabel = text }
        PlacementField -> { lbls | placementLabel = text }
        PlaceField -> { lbls | placeLabel = text }
        PointsField -> { lbls | pointsLabel = text }
        RoundWinnerField -> { lbls | roundWinnerLabel = text }
    
updateQuizByText : String -> Model -> Model
updateQuizByText text model = 
    case Quiz.parseQuiz text of
        Ok quiz -> let guess = Quiz.numberOfTeams quiz
                       actual = if guess == 0 then model.teamsInQuiz else guess
                   in { model | currentQuiz = quiz, 
                                teamsInQuiz = actual,
                                isValidQuizUpdate = True, 
                                feedback = ""
                   }
        Err des -> { model | isValidQuizUpdate = False, 
                             feedback = "Parsing error"
                   }

type alias RestParam = String
type alias RestValue = String
type alias RestKey = String

mkParamPure : RestKey -> RestValue -> RestParam
mkParamPure key value = String.join "=" [key, value]

mkParamsPure : List (RestKey, RestValue) -> RestParam
mkParamsPure kvs = String.join "&" (List.map (\(k, v) -> mkParamPure k v) kvs)

mkParams : List (RestKey, RestValue) -> RestParam
mkParams kvs = 
    let done = Url.Builder.relative [] (List.map (\(k, v) -> string k v) kvs)
    in String.dropLeft 1 done

mkWithSignature : User -> SessionKey -> List (RestKey, RestValue) -> List (RestKey, RestValue)
mkWithSignature u key kvs = 
    let allParams = (userParam, u) :: kvs
        sig = sha512 (String.concat [key, mkParamsPure allParams])
    in (signatureParam, sig) :: allParams

mkParamsWithSignature : User -> SessionKey -> List (RestKey, RestValue) -> RestParam
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
                        let maxTeams = Quiz.maxNumberOfTeams model.currentQuiz
                        in if n <= maxTeams then (n, "")
                           else (maxTeams, 
                                 String.join " " ["Quiz supports only", 
                                                  String.fromInt maxTeams, 
                                                  "teams."])
                  Err _ -> (0, "Invalid team number. Substituting 0.")
  in { teams = ts, response = r }
                                    