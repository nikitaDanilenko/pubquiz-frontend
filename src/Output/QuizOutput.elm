module Output.QuizOutput exposing (..)

import Basics.Extra exposing (flip)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi, quizIdParam, teamQueryParam, teamTableApi)
import Common.Types exposing (DbQuizId, QuizInfo, TeamQuery, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings, jsonDecTeamTableInfo, jsonEncTeamQuery)
import Common.Util as Util exposing (getAllWith, getMsg, getMsgWith, uncurry3)
import Html exposing (Html, div)
import Input.Model exposing (ErrorOr)
import Output.All as All
import Output.OutputUtil exposing (mkFullQuizName)
import Output.Quiz as Quiz
import Output.Table as Table
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Url.Parser.Query as Query


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

        All _ ->
            -- todo: A title would be nice, but how to get one?
            ""

        Void ->
            "Welcome to Janet's void!"


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


stepTo : Url -> Model -> ( Model, Cmd Msg )
stepTo url model =
  case Parser.parse (parser model) (fragmentToPath url) of
    Just answer -> answer
    Nothing -> ({model | page = Void}, Cmd.none)


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
    in
    oneOf
        [ route Parser.top (stepAll model All.init)
        , route quizIdParser (Quiz.init >> stepQuiz model)
        , route teamParser (\qid tn tc -> TeamQuery qid tn tc |> Table.init |> stepTable model)
        ]


route : Parser a b -> a -> Parser (b -> c) c
route =
    flip Parser.map

fragmentToPath : Url -> Url
fragmentToPath url = {url | path = Maybe.withDefault "" url.fragment, fragment = Nothing}

testUrl : Url
testUrl =
    { protocol = Http
    , host = "danilenko.io"
    , port_ = Just 9000
    , path = "/quizzes"
    , query = Nothing
    , fragment = Just "quizId/23/teamNumber/1/teamCode/baad12"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case Debug.log "request" urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External _ ->
                    ( model, Cmd.none )

        ChangedUrl url -> stepTo url model

        AllMsg allMsg ->
          case model.page of
            All all -> stepAll model (All.update allMsg all)
            _ -> (model, Cmd.none)
        --( GetQuizRatings quizInfo, _ ) ->
        --    ( model, getQuizRatings quizInfo )
        --
        --( GotQuizRatings quizInfo qrCandidate, _ ) ->
        --    ( updateSubModel (Result.map (\quizRatings -> QuizModel quizRatings quizInfo) qrCandidate) model, Cmd.none )
        --
        --( GetTeamTable, QuizModel _ tq _ _ ) ->
        --    ( model, Util.foldMaybe Cmd.none getTeamTable tq )
        --
        --( GotTeamTable teamTableCandidate, QuizModel labels _ _ quizInfo ) ->
        --    ( updateSubModel (Result.map (\table -> TableModel labels table quizInfo) teamTableCandidate) model, Cmd.none )
        --
        --( GetAllQuizzes, QuizModel _ _ _ _) ->
        --    ( model, getAllQuizzes )
        --
        --( GotAllQuizzes quizzesCandidate, QuizModel _ mtq _ _) ->
        --    ( updateSubModel (Result.map (AllModel mtq) quizzesCandidate) model, Cmd.none )
        --
        --( GetLabels qid, _) ->
        --  (model, getLabels qid)
        --
        --( GotLabels qid labelsCandidate, _ ) ->
        --  (Result.withDefault model (Result.map (updateLabels model) labelsCandidate), getQuizInfo qid)
        --( GotQuizInfo quizInfoCandidate, _) ->
        _ ->
            ( model, Cmd.none )



-- todo: check
--updateSubModel : ErrorOr Model -> Model -> Model
--updateSubModel newModel model = Result.withDefault model newModel
--getQuizRatings : QuizInfo -> Cmd Msg
--getQuizRatings quizInfo =
--    getMsg getQuizRatingsApi (GotQuizRatings quizInfo) jsonDecQuizRatings quizInfo.quizId
-- todo: check
--getTeamTable : TeamQuery -> Cmd Msg
--getTeamTable =
--    getMsgWith jsonEncTeamQuery teamQueryParam teamTableApi GotTeamTable jsonDecTeamTableInfo
