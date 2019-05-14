module QuizInput exposing ( .. )

import Browser
import Html exposing              ( Html, div, text, input, button, textarea )
import Html.Attributes exposing   ( id, autocomplete, class )            
import Html.Events exposing       ( onInput, onClick )
import Html.Events.Extra exposing ( onEnter )
import Http
import Http exposing              ( Error, get, emptyBody, post )
import Json.Encode as Encode

import Constants exposing         ( serverLocation )

main : Program () Model Msg
main =
  Browser.document
    { init          = initialModel
    , view          = \model -> { title = "Quiz Interface", body = [ view model ] }
    , update        = update
    , subscriptions = \_ -> Sub.none
    }

type alias Model = 
    {
        user : User,
        password : Password,
        quizzes : List QuizName,
        editing : QuizName,
        currentPoints : String,
        displayState : DisplayState,
        errorMsg : String
    }

initialModel : () -> (Model, Cmd Msg)
initialModel () = ({ user = "",
                     password = "", 
                     quizzes = [], 
                     editing = "",
                     currentPoints = "",
                     displayState = Initial, 
                     errorMsg = "" 
                     }, Cmd.none)

type Msg = GetAll 
         | GotAll (Result Http.Error String)
         | GetSingle QuizName
         | GotSingle (Result Http.Error String)
         | PostUpdate QuizName String
         | AcknowledgeLock
         | Lock QuizName
         | SetUser User
         | SetPassword Password
         | SetPoints String
         | LocationChange
         | Updated (Result Http.Error ())

type alias User = String 
type alias Password = String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
    SetUser u               -> ({ model | user = u }, Cmd.none)
    SetPassword p           -> ({ model | password = p}, Cmd.none)
    GetAll                  -> ({ model | displayState = Authenticating }, getAll)
    -- todo: Pretty-print the actual error.
    GotAll (Err err)        -> ({ model | errorMsg = "An error occurred"}, Cmd.none)
    GotAll (Ok text)        -> ({ model | quizzes = String.lines text, displayState = Selecting }, 
                                Cmd.none)
    GetSingle qName         -> ({ model | editing = qName }, 
                                 getSingle qName)
    -- todo: Pretty-print the actual error.
    GotSingle (Err err)     -> ({ model | errorMsg = "An error occurred"}, Cmd.none)
    GotSingle (Ok text)     -> ({ model | currentPoints = text,
                                          displayState = Editing }, Cmd.none)
    AcknowledgeLock         -> ({ model | displayState = ConfirmingLock}, Cmd.none)
    SetPoints points        -> ({ model | currentPoints = points}, Cmd.none)
    PostUpdate qName points -> (model, postUpdate qName points)
    _                       -> (model, Cmd.none)

view : Model -> Html Msg
view model = 
    let authenticationView : Model -> Html Msg
        authenticationView md = 
            div [ id "initialMain"]
                [ div [ id "userField" ] 
                      [   text "User name"
                        , input [ autocomplete True, onInput SetUser, onEnter GetAll ] []
                      ],
                  div [ id "passwordField" ]
                      [
                          text "Password"
                        , input [ autocomplete True, onInput SetPassword, onEnter GetAll ] []
                      ],
                  div [ id "fetchButton" ]
                      [ button [ class "button", onClick GetAll ] [ text "Login" ] ],
                  div [ id "errorLabel" ] [ text md.errorMsg ]
                ]

        selectionView : Model -> Html Msg
        selectionView md =
            let mkButton : QuizName -> Html Msg
                mkButton name = button [ class "quizButton", 
                                         onClick (GetSingle name) ] 
                                       [ text name ]
            in div [ id "allQuizzesMain" ]
                   (List.map mkButton (List.filter (\q -> not (String.isEmpty q)) md.quizzes))

        editingView : Model -> Html Msg
        editingView md =
            div [ id "singleQuiz" ]
                [ textarea [ id "singleQuizArea", onInput SetPoints ] [ text md.currentPoints ],
                  button [ class "button", onClick GetAll ] [ text "Back" ],
                  button [ class "button", onClick AcknowledgeLock ] [ text "Lock" ],
                  button [ class "button", onClick (PostUpdate md.editing md.currentPoints) ]
                         [ text "Update" ]
                          ]

        confirmView : Model -> Html Msg
        confirmView md =
            div [ id "confirmView" ]
                [ text (String.concat ["You are about to lock ", md.editing, ". ",
                                       "This cannot be undone. Please confirm. "]),
                  button [ class "button", onClick (GetSingle md.editing) ]
                         [ text "Abort" ],
                  button [ class "button", onClick (Lock md.editing) ]
                         [ text "Yes, lock" ]
                ]

        currentView = case model.displayState of
            Authenticating -> authenticationView
            Initial -> authenticationView
            Editing -> editingView
            Selecting -> selectionView
            ConfirmingLock -> confirmView
     in currentView model

mkFullPath : String -> String
mkFullPath str = String.concat [serverLocation, str]

getAll : Cmd Msg
getAll = Http.get { url = mkFullPath "all", expect = Http.expectString GotAll }

getSingle : String -> Cmd Msg
getSingle quizName = Http.get { 
        url = mkFullPath (String.concat [ "getQuizData?quiz=", quizName ]),
        expect = Http.expectString GotSingle
    }

postUpdate : QuizName -> String -> Cmd Msg
postUpdate quizName points = 
    Http.post {
        url = mkFullPath "update",
        body = Http.stringBody "application/x-www-form-urlencoded"  
                               (String.concat [
                                "quiz=", quizName, "&rounds=", (String.replace "\n" "%0A" points)
                                ]),
        expect = Http.expectWhatever Updated
    }

type alias QuizName = String

type alias Quiz = 
    {
        name : String,
        rounds : List Round
    }


type alias Round = 
    {
        maxPoints : Float,
        teamPoints : List Float
    }

{- The different types of possible states the page can transition. -}
type DisplayState = Initial -- The state at the beginning of the application.
                  | Editing -- The view where one sees the current quiz values and can update those.
                  | Authenticating -- The view presented for the authentication of a user.
                  | Selecting -- In this view you see all available quizzes.
                  | ConfirmingLock