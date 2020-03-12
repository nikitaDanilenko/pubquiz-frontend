module Output.QuizOutput exposing (..)

import Basics.Extra exposing (flip)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Common.Constants exposing (quizIdParam)
import Common.Types exposing (DbQuizId, QuizInfo, TeamQuery)
import Common.Util as Util
import Html exposing (Html, div)
import Output.All as All
import Output.OutputUtil exposing (mkFullQuizName)
import Output.Quiz as Quiz
import Output.Table as Table exposing (Msg)
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = \model -> { title = titleFor model, body = [ view model ] }
        }


type alias Model =
    { key : Nav.Key
    , page : Page
    }


titleFor : Model -> String
titleFor model =
    case model.page of
        Table tableModel ->
            String.join " - " [ mkFullQuizName tableModel.quizInfo.quizIdentifier, tableModel.labels.ownPointsLabel ]

        Quiz quizModel ->
            String.join " - " [ mkFullQuizName quizModel.quizInfo.quizIdentifier, quizModel.labels.backToChartView ]

        All all ->
            -- Entering the all view does not provide a sensible way of fetching a label for the title.
            Util.foldMaybe "" .viewPrevious all.labelsCandidate

        Void ->
            "Welcome to Janet's void! How did you get here?"


type Page
    = Table Table.Model
    | Quiz Quiz.Model
    | All All.Model
    | Void


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
    | TableMsg Table.Msg
    | QuizMsg Quiz.Msg
    | AllMsg All.Msg


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepTo url { key = key, page = Void }


view : Model -> Html Msg
view model =
    case model.page of
        Table table ->
            Html.map TableMsg (Table.view table)

        Quiz quiz ->
            Html.map QuizMsg (Quiz.view quiz)

        All all ->
            Html.map AllMsg (All.view all)

        Void ->
            div [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External _ ->
                    ( model, Cmd.none )

        ChangedUrl url ->
            stepTo url model

        AllMsg allMsg ->
            case model.page of
                All all ->
                    stepAll model (All.update allMsg all)

                _ ->
                    ( model, Cmd.none )

        QuizMsg quizMsg ->
            case model.page of
                Quiz quiz ->
                    stepQuiz model (Quiz.update quizMsg quiz)

                _ ->
                    ( model, Cmd.none )

        TableMsg tableMsg ->
            case model.page of
                Table table ->
                    stepTable model (Table.update tableMsg table)

                _ ->
                    ( model, Cmd.none )


stepTo : Url -> Model -> ( Model, Cmd Msg )
stepTo url model =
    case Parser.parse (parser model) (fragmentToPath url) of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = Void }, Cmd.none )


stepAll : Model -> ( All.Model, Cmd All.Msg ) -> ( Model, Cmd Msg )
stepAll model ( all, cmd ) =
    ( { model | page = All all }, Cmd.map AllMsg cmd )


stepQuiz : Model -> ( Quiz.Model, Cmd Quiz.Msg ) -> ( Model, Cmd Msg )
stepQuiz model ( quiz, cmd ) =
    ( { model | page = Quiz quiz }, Cmd.map QuizMsg cmd )


stepTable : Model -> ( Table.Model, Cmd Table.Msg ) -> ( Model, Cmd Msg )
stepTable model ( table, cmd ) =
    ( { model | page = Table table }, Cmd.map TableMsg cmd )


parser : Model -> Parser (( Model, Cmd Msg ) -> c) c
parser model =
    let
        quizIdParser =
            s quizIdParam </> Parser.int

        teamParser =
            quizIdParser </> s "teamNumber" </> Parser.int </> s "teamCode" </> Parser.string

        ( currentLabels, currentQuizInfo ) =
            case model.page of
                Quiz quiz ->
                    ( Just quiz.labels, Just quiz.quizInfo )

                Table table ->
                    ( Just table.labels, Just table.quizInfo )

                All all ->
                    ( all.labelsCandidate, Nothing )

                _ ->
                    ( Nothing, Nothing )

        currentTeamQuery =
            case model.page of
                Table table ->
                    Just table.teamQuery

                Quiz quiz ->
                    quiz.teamQueryCandidate

                All all ->
                    all.teamQueryCandidate

                _ ->
                    Nothing
    in
    oneOf
        [ route Parser.top (stepAll model (All.init currentLabels currentTeamQuery))
        , route quizIdParser (Quiz.init currentLabels currentTeamQuery >> stepQuiz model)
        , route teamParser (\qid tn tc -> TeamQuery qid tn tc |> Table.init currentLabels currentQuizInfo |> stepTable model)
        ]


route : Parser a b -> a -> Parser (b -> c) c
route =
    flip Parser.map


fragmentToPath : Url -> Url
fragmentToPath url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
