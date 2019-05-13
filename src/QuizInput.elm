module QuizInput exposing ( .. )

import Browser
import Html exposing              ( Html, div, text, input, button )
import Html.Attributes exposing   ( id, autocomplete, class )            
import Html.Events exposing       ( onInput, onClick )
import Html.Events.Extra exposing ( onEnter )
import Http
import Http exposing              ( Error, get, emptyBody, post )

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
        displayState : DisplayState,
        errorMsg : String
    }

initialModel : () -> (Model, Cmd Msg)
initialModel () = ({ user = "",
                     password = "", 
                     quizzes = [], 
                     displayState = Initial, 
                     errorMsg = "" 
                     }, Cmd.none)

type Msg = GetAll 
         | GetSingle QuizName 
         | GotAll (Result Http.Error String)
         | PostUpdate Quiz 
         | Lock Quiz 
         | SetUser User
         | SetPassword Password
         | LocationChange

type alias User = String 
type alias Password = String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
    SetUser u        -> ({ model | user = u }, Cmd.none)
    SetPassword p    -> ({ model | password = p}, Cmd.none)
    GetAll           -> ({ model | displayState = Authenticating }, getAll)
    -- todo: Pretty-print the actual error.
    GotAll (Err err) -> ({ model | errorMsg = "An error occurred"}, Cmd.none)
    GotAll (Ok text) -> ({ model | quizzes = String.lines text, displayState = Selecting }, 
                         Cmd.none)
    _                -> (model, Cmd.none)

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
                mkButton name = button [ class "quizButton", onClick (GetSingle (mkFullPath name)) ] 
                                       [ text name ]
            in div [ id "allQuizzesMain" ]
                   (List.map mkButton md.quizzes)

        currentView = case model.displayState of
            Authenticating -> authenticationView
            Initial -> authenticationView
            Editing -> authenticationView
            Selecting -> selectionView
     in currentView model

mkFullPath : String -> String
mkFullPath str = String.concat [serverLocation, str]

getAll : Cmd Msg
getAll = Http.get { url = mkFullPath "all", expect = Http.expectString GotAll }

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