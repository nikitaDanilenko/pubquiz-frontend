module QuizInput exposing ( .. )

import Browser
import Html exposing              ( Html, div, text, input )
import Html.Attributes exposing   ( id, autocomplete )            
import Html.Events exposing       ( onInput )
import Html.Events.Extra exposing ( onEnter )
import Http exposing              ( Error, get, emptyBody, post )

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
        user : String,
        quizzes : List QuizName,
        displayState : DisplayState
    }

initialModel : () -> (Model, Cmd Msg)
initialModel () = ({ user = "", quizzes = [], displayState = Initial }, Cmd.none)

type Msg = GetAll 
         | GetSingle QuizName 
         | PostUpdate Quiz 
         | Lock Quiz 
         | SetUser User
         | SetPassword Password
         | LocationChange

type alias User = String 
type alias Password = String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Html Msg
view model = 
    let initialView = 
            div [ id "initialMain"]
                [ div [ id "userField" ] 
                      [   text "User name"
                        , input [ autocomplete True, onInput SetUser, onEnter GetAll ] []
                      ],
                  div [ id "passwordField" ]
                      [
                          text "Password"
                        , input [ autocomplete True, onInput SetPassword, onEnter GetAll ] []
                      ]
                ]
    in case model.displayState of
        Initial -> initialView
        Editing -> initialView


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

