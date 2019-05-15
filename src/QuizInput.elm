module QuizInput exposing ( main )

import Browser

import Html exposing         ( Html )
import Http
import Http exposing         ( get, emptyBody, post )
import Json.Encode as Encode

import Constants exposing    ( serverLocation )
import Model exposing        ( .. )
import Views exposing        ( .. )

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

    -- todo: Pretty-print the actual error.
    GotAll (Err err)        -> ({ model | errorMsg = "An error occurred"}, Cmd.none)
    GotAll (Ok text)        -> ({ model | quizzes = String.lines text, displayState = Selecting }, 
                                Cmd.none)
    
    GetSingle qName         -> ({ model | editing = qName }, getSingle qName)
    -- todo: Pretty-print the actual error.
    GotSingle (Err err)     -> ({ model | errorMsg = "An error occurred"}, Cmd.none)
    GotSingle (Ok text)     -> ({ model | currentPoints = text,
                                          displayState = Editing }, Cmd.none)
    
    SetPoints points        -> ({ model | currentPoints = points}, Cmd.none)
    PostUpdate qName points -> (model, postUpdate qName points)
    
    AcknowledgeLock         -> ({ model | displayState = ConfirmingLock}, Cmd.none)
    Lock qName              -> (model, postLock qName)
    -- todo: Pretty-print the actual error.
    Locked (Err err)        -> ({ model | errorMsg = "An error occurred"}, Cmd.none)
    Locked (Ok ok)          -> (model, getAll)
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
     in currentView model

mkFullPath : String -> String
mkFullPath str = String.concat [serverLocation, str]

getAll : Cmd Msg
getAll = Http.get { 
    url = mkFullPath "all", 
    expect = Http.expectString GotAll 
  }

getSingle : String -> Cmd Msg
getSingle quizName = Http.get { 
        url = mkFullPath (String.concat [ "getQuizData?quiz=", quizName ]),
        expect = Http.expectString GotSingle
    }

postUpdate : QuizName -> String -> Cmd Msg
postUpdate quizName points = 
    Http.post {
        url = mkFullPath "update",
        body = encodeBody (String.concat [
                                "quiz=", quizName, "&rounds=", (String.replace "\n" "%0A" points)
                                ]),
        expect = Http.expectWhatever Updated
    }
postLock : QuizName -> Cmd Msg
postLock quizName = Http.post {
    url = mkFullPath "lock",
    body = encodeBody (String.concat ["quiz=", quizName]),
    expect = Http.expectWhatever Locked
  }

encodeBody : String -> Http.Body
encodeBody = Http.stringBody "application/x-www-form-urlencoded"
