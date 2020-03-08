module Output.All exposing (..)

import Common.Constants exposing (quizIdParam)
import Common.Types exposing (Labels, QuizInfo, TeamQuery)
import Common.Util as Util exposing (getAllWith)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, id)
import Input.Model exposing (ErrorOr)
import Output.OutputUtil exposing (fragmentUrl, mkFullQuizName)


type alias Model =
    { teamQueryCandidate : Maybe TeamQuery
    , labelsCandidate : Maybe Labels
    , quizInfos : List QuizInfo
    }


updateQuizInfos : Model -> List QuizInfo -> Model
updateQuizInfos model quizInfos =
    { model | quizInfos = quizInfos }


type Msg
    = GotAllQuizzes (ErrorOr (List QuizInfo))


init : Maybe Labels -> Maybe TeamQuery -> ( Model, Cmd Msg )
init mLabels mTeamQuery =
    ( { teamQueryCandidate = mTeamQuery, labelsCandidate = mLabels, quizInfos = [] }, getAllQuizzes )


view : Model -> Html Msg
view model =
    div [ id "allQuizzes" ]
        (List.map mkQuizInfoButton model.quizInfos)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllQuizzes quizInfosCandidate ->
            ( Util.foldResult model (updateQuizInfos model) quizInfosCandidate, Cmd.none )


mkQuizInfoButton : QuizInfo -> Html Msg
mkQuizInfoButton quizInfo =
    div []
        [ a
            [ class "quizInfoButton"
            , href (fragmentUrl [ quizIdParam, String.fromInt quizInfo.quizId ])
            ]
            [ text (mkFullQuizName quizInfo.quizIdentifier) ]
        ]


getAllQuizzes : Cmd Msg
getAllQuizzes =
    getAllWith GotAllQuizzes
