module QuizInput exposing ( main )

import Browser
import Html exposing            ( Html, div, node )
import Html.Attributes exposing ( rel, type_, href, id )
import Http
import Http exposing            ( get, emptyBody, post )
import Json.Encode as Encode
-- todo Write all out.
import Constants exposing       ( .. )
import Model exposing           ( .. )
import Views exposing           ( .. )

main : Program () Model Msg
main =
  Browser.document
    { init          = initialModel
    , view          = \model -> { title = "Quiz Interface", body = [ view model ] }
    , update        = update
    , subscriptions = \_ -> Sub.none
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
    SetUser u               -> ({ model | user = u }, Cmd.none)
    SetPassword p           -> ({ model | password = p}, Cmd.none)
    GetAll                  -> ({ model | displayState = Authenticating,
                                          currentPoints = "",
                                          editing = "" }, 
                                 getAll)

    GotAll (Err err)        -> ({ model | feedback = errorToString err}, Cmd.none)
    GotAll (Ok text)        -> ({ model | quizzes = String.lines text, 
                                          displayState = Selecting }, 
                                Cmd.none)
    
    GetSingle qName         -> ({ model | editing = qName, feedback = "" }, getSingle qName)
    
    GotSingle (Err err)     -> ({ model | feedback = errorToString err}, Cmd.none)
    GotSingle (Ok text)     -> ({ model | currentPoints = text,
                                          displayState = Editing,
                                          feedback = "" }, Cmd.none)
    
    SetPoints points        -> ({ model | currentPoints = points}, Cmd.none)
    PostUpdate qName points -> (model, postUpdate qName points)
    Updated (Err err)       -> ({ model | feedback = errorToString err }, Cmd.none)
    Updated (Ok _)          -> ({ model | feedback = "Update successful"}, Cmd.none)
    
    AcknowledgeLock         -> ({ model | displayState = ConfirmingLock }, Cmd.none)
    Lock qName              -> (model, postLock qName)
    
    Locked (Err err)        -> ({ model | feedback = errorToString err}, Cmd.none)
    Locked (Ok ok)          -> ({ model | feedback = String.concat ["Locked ", model.editing] }, 
                                getAll)

    Login                   -> ({ model | displayState = Authenticating }, 
                                login model.user model.password)
    Logged (Err err)        -> ({ model | feedback = errorToString err}, Cmd.none)
    Logged (Ok text)        -> ({ model | feedback = "" }, getAll)

    StartCreating           -> ({ model | displayState = Creating }, Cmd.none)
    SetNewQuizName name     -> ({ model | createName = name }, Cmd.none)
    Create name             -> if String.isEmpty (model.createName) 
                                then ({ model | feedback = "Empty quiz name" }, Cmd.none)
                                else (model, createNew model.createName)
    Created (Err err)       -> ({ model | feedback = errorToString err }, Cmd.none)
    Created (Ok ok)         -> ({ model | editing = model.createName }, getSingle model.createName)
    _                       -> (model, Cmd.none)

view : Model -> Html Msg
view model = 
    let currentView = case model.displayState of
            Authenticating -> authenticationView
            Initial -> authenticationView
            Editing -> editingView
            Selecting -> selectionView
            ConfirmingLock -> confirmView
            Creating -> creatingView
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
        url = mkPath [quizApi, (String.concat [ "getQuizData?", mkParam quizParam quizName ])],
        expect = Http.expectString GotSingle
    }

postUpdate : QuizName -> String -> Cmd Msg
postUpdate quizName points = 
    Http.post {
        url = updateApi,
        body = encodeBody (mkParams [(quizParam, quizName), 
                                     (roundsParam, String.replace "\n" "%0A" points)]),
        expect = Http.expectWhatever Updated
    }
postLock : QuizName -> Cmd Msg
postLock quizName = Http.post {
    url = lockApi,
    body = encodeBody (mkParam quizParam quizName),
    expect = Http.expectWhatever Locked
  }

createNew : QuizName -> Cmd Msg
createNew quizName = Http.post {
    url = newApi,
    body = encodeBody (mkParam quizParam quizName),
    expect = Http.expectWhatever Created
  }

type alias RestParam = String
type alias RestValue = String
type alias RestKey = String

mkParam : RestKey -> RestValue -> RestParam
mkParam key value = String.join "=" [key, value]

mkParams : List (RestKey, RestValue) -> RestParam
mkParams kvs = String.join "&" (List.map (\(k, v) -> mkParam k v) kvs)

encodeBody : String -> Http.Body
encodeBody = Http.stringBody "application/x-www-form-urlencoded"