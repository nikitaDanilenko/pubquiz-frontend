module Input.Selection exposing (..)

import Common.WireUtil exposing (addFeedbackLabel, errorToString)
import Common.Constants exposing (getQuizRatingsApi)
import Common.Types exposing (DbQuizId, QuizInfo, QuizRatings, jsonDecQuizRatings)
import Common.Util exposing (getAllWith, getMsg)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Input.Model exposing (ErrorOr)


type alias Model =
    { quizzes : List QuizInfo
    , feedback : String
    }


type Msg
    = GetQuizRatings DbQuizId
    | GotQuizRatings (ErrorOr QuizRatings)
    | StartCreatingQuiz
    | StartCreatingUser
    | GotAll (ErrorOr (List QuizInfo))


init : (Model, Cmd Msg)
init  =
    ({ quizzes = []
    , feedback = ""
    }, getAllWith GotAll)


view : Model -> Html Msg
view md =
    let
        mkButton : QuizInfo -> Html Msg
        mkButton qi =
            button
                [ class "quizButton"
                , onClick (GetQuizRatings qi.quizId)
                ]
                [ text qi.quizIdentifier.name ]
    in
    div [ id "quizSelectionMain" ]
        [ div [ id "selectExistingQuizzesMain" ]
            (List.map mkButton (List.filter (\q -> not (String.isEmpty q.quizIdentifier.name)) md.quizzes))
        , div [ id "createNewQuiz" ]
            [ button [ class "newQuizButton", onClick StartCreatingQuiz ] [ text "New quiz" ] ]
        , div [ id "createNewUser" ]
            [ button [ class "newUserButton", onClick StartCreatingUser ] [ text "New user" ] ]
        , addFeedbackLabel md.feedback
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuizRatings qid ->
            ( { model | feedback = "" }, getQuizRatings qid )

        GotAll candidate ->
            case candidate of
                Ok quizInfos ->
                    ( { model | quizzes = quizInfos }, Cmd.none )

                Err error ->
                    ( { model | feedback = errorToString error }, Cmd.none )

        _ ->
            ( model, Cmd.none )


getQuizRatings : DbQuizId -> Cmd Msg
getQuizRatings =
    getMsg getQuizRatingsApi GotQuizRatings jsonDecQuizRatings
