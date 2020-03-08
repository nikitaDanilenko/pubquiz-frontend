module Output.All exposing (..)

import Common.Constants exposing (quizIdParam)
import Common.Types exposing (QuizInfo, TeamQuery)
import Common.Util as Util exposing (getAllWith)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, id)
import Input.Model exposing (ErrorOr)
import Output.OutputUtil exposing (mkFullQuizName)
import Url.Builder


type alias Model =
    { teamQueryCandidate : Maybe TeamQuery
    , quizInfos : List QuizInfo
    }


type Msg
    = GotAllQuizzes (ErrorOr (List QuizInfo))


init : ( Model, Cmd Msg )
init =
    ( { teamQueryCandidate = Nothing, quizInfos = [] }, getAllQuizzes )


view : Model -> Html Msg
view model =
    div [ id "allQuizzes" ]
        (List.map mkQuizInfoButton model.quizInfos)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllQuizzes quizInfosCandidate ->
            ( Util.foldResult model (\qs -> { model | quizInfos = qs }) quizInfosCandidate, Cmd.none )


mkQuizInfoButton : QuizInfo -> Html Msg
mkQuizInfoButton quizInfo =
    div []
        [ a
            [ class "quizInfoButton"
            , href (Url.Builder.absolute [ quizIdParam, String.fromInt quizInfo.quizId ] [])
            ]
            [ text (mkFullQuizName quizInfo.quizIdentifier) ]
        ]


getAllQuizzes : Cmd Msg
getAllQuizzes =
    getAllWith GotAllQuizzes
