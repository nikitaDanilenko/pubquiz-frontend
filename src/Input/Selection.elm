module Input.Selection exposing (..)

import Common.Types exposing (DbQuizId, QuizInfo, QuizRatings)
import Common.Util exposing (ErrorOr, getAllWith)
import Common.WireUtil exposing (addFeedbackLabel, errorToString)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


type alias Model =
    { quizzes : List QuizInfo
    , feedback : String
    }


updateFeedback : Model -> String -> Model
updateFeedback model feedback =
    { model | feedback = feedback }


updateQuizzes : Model -> List QuizInfo -> Model
updateQuizzes model quizzes =
    { model | quizzes = quizzes }


type Msg
    = StartPointInput QuizInfo
    | StartCreatingQuiz
    | StartCreatingUser
    | GotAll (ErrorOr (List QuizInfo))


init : ( Model, Cmd Msg )
init =
    ( { quizzes = []
      , feedback = ""
      }
    , getAllWith GotAll
    )


view : Model -> Html Msg
view md =
    let
        mkButton : QuizInfo -> Html Msg
        mkButton quizInfo =
            button
                [ class "quizButton"
                , onClick (StartPointInput quizInfo)
                ]
                [ text quizInfo.quizIdentifier.name ]
    in
    div [ id "quizSelectionMain" ]
        [ div [ id "selectExistingQuizzesMain" ]
            (List.map mkButton (List.filter (\q -> not (String.isEmpty q.quizIdentifier.name)) md.quizzes))
        , div [ id "createNewArea" ]
            [ button [ class "newQuizButton", onClick StartCreatingQuiz ] [ text "New quiz" ]
            , button [ class "newUserButton", onClick StartCreatingUser ] [ text "New user" ]
            ]
        , addFeedbackLabel md.feedback
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAll quizzesCandidate ->
            case quizzesCandidate of
                Ok quizzes ->
                    ( updateQuizzes model quizzes, Cmd.none )

                Err error ->
                    ( updateFeedback model (errorToString error), Cmd.none )

        _ ->
            ( model, Cmd.none )
