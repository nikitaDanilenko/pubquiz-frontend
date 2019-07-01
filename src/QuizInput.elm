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
import Labels exposing          ( Labels, defaultLabels )
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
                                          groupsInQuiz = initialModel.groupsInQuiz }, 
                                 getAll)

    GotAll (Err err)        -> ({ model | feedback = errorToString err}, Cmd.none)
    GotAll (Ok text)        -> ({ model | quizzes = String.lines text, 
                                          displayState = Selecting,
                                          feedback = "" }, 
                                Cmd.none)
    
    GetSingle qName         -> ({ model | editing = qName, feedback = "" }, getSingle qName)
    
    GotSingle (Err err)     -> ({ model | feedback = errorToString err}, Cmd.none)
    GotSingle (Ok text)     -> let updatedModel = updateQuizByText text model
                               in ({ updatedModel | displayState = Editing }, Cmd.none)

    SetGroupsInQuiz text    -> let (groups, response) = 
                                    case run int text of
                                     Ok n -> (n, "")
                                     Err _ -> (0, "Invalid group number. Substituting 0.")
                                   newQuiz = Quiz.adjustTo groups model.currentQuiz
                               in ({ model | groupsInQuiz = groups,
                                             currentQuiz = newQuiz,
                                             feedback = response }, Cmd.none)
    UpdatePoints r g ps     -> let (np, response) =
                                    case run float ps of
                                     Ok p -> (p, "")
                                     Err _ -> (0, 
                                               String.join " " 
                                                           ["Invalid decimal point number",
                                                            "at round =", 
                                                            String.fromInt (1 + r),
                                                            "and group =",
                                                            String.concat [String.fromInt (1 + g), 
                                                                           "."],
                                                            "Substituting 0."])
                               in ({ model | currentQuiz = Quiz.update r g np model.currentQuiz,
                                             feedback = response },
                                   Cmd.none)
    AddRound                -> let newQuiz = Quiz.addRound (Round.emptyOfSize model.groupsInQuiz) 
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
                                     Err _ -> { model | feedback = "Not a valid number of groups." }
                               in (newModel, Cmd.none)
    CreateQuiz              -> if String.isEmpty (model.createName) 
                                then ({ model | feedback = "Empty quiz name" }, Cmd.none)
                                else (model, 
                                      createNewQuiz model.numberOfRounds
                                                    model.groupsInQuiz
                                                    model.user 
                                                    model.oneWayHash 
                                                    model.createName 
                                                    model.labels)
    Created (Err err)       -> ({ model | feedback = errorToString err }, Cmd.none)
    Created (Ok ok)         -> ({ model | editing = model.createName,
                                          labels = defaultLabels }, 
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

    LabelsUpdate fld text   -> let lbls = updateLabels fld text model.labels
                               in ({ model | labels = lbls }, Cmd.none)
    _                       -> (model, Cmd.none)

view : Model -> Html Msg
view model = 
    let currentView = case model.displayState of
            Authenticating -> authenticationView
            Initial -> authenticationView
            Editing -> editingView
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

getSingle : String -> Cmd Msg
getSingle quizName = Http.get { 
        url = Url.Builder.relative [ quizApi, "getQuizData"] [ string quizParam quizName ],
        expect = Http.expectString GotSingle
    }

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
                                     (numberOfGroupsParam, String.fromInt gs) :: 
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

updateLabels : LabelsField -> String -> Labels -> Labels
updateLabels field text lbls = 
    case field of
        RoundField -> { lbls | roundLabel = text }
        GroupField -> { lbls | groupLabel = text }
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
    
updateQuizByText : String -> Model -> Model
updateQuizByText text model = 
    case Quiz.parseQuiz text of
        Ok quiz -> let guess = Quiz.numberOfGroups quiz
                       actual = if guess == 0 then model.groupsInQuiz else guess
                   in { model | currentQuiz = quiz, 
                             groupsInQuiz = actual,
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