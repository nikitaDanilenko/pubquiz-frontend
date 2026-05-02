module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, h1, li, nav, p, section, text, ul)
import Html.Attributes exposing (class, href)
import Monocle.Lens exposing (Lens)
import Pages.Public.Quiz.Handler
import Pages.Public.Quiz.Page
import Pages.Public.Quiz.View
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type alias Flags =
    { apiBase : String
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Nav.Key
    , page : Page
    , apiBase : String
    }


lenses :
    { page : Lens Model Page
    , apiBase : Lens Model String
    }
lenses =
    { page = Lens .page (\b a -> { a | page = b })
    , apiBase = Lens .apiBase (\b a -> { a | apiBase = b })
    }


type Page
    = Landing
    | Quiz Pages.Public.Quiz.Page.Model
    | NotFound


type Route
    = LandingRoute
    | QuizRoute Int


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map LandingRoute Parser.top
        , Parser.map QuizRoute (Parser.s "quiz" </> Parser.int)
        ]


parseUrl : Url -> Maybe Route
parseUrl url =
    Parser.parse routeParser url


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , page = NotFound
            , apiBase = flags.apiBase
            }
    in
    navigateTo (parseUrl url) model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | QuizMsg Pages.Public.Quiz.Page.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            navigateTo (parseUrl url) model

        ( QuizMsg quizMsg, Quiz quizModel ) ->
            let
                ( newQuizModel, quizCmd ) =
                    Pages.Public.Quiz.Handler.update quizMsg quizModel
            in
            ( { model | page = Quiz newQuizModel }
            , Cmd.map QuizMsg quizCmd
            )

        _ ->
            ( model, Cmd.none )


navigateTo : Maybe Route -> Model -> ( Model, Cmd Msg )
navigateTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just LandingRoute ->
            ( { model | page = Landing }, Cmd.none )

        Just (QuizRoute quizId) ->
            let
                ( quizModel, quizCmd ) =
                    Pages.Public.Quiz.Handler.init
                        { quizId = quizId
                        , apiBase = model.apiBase
                        }
            in
            ( { model | page = Quiz quizModel }
            , Cmd.map QuizMsg quizCmd
            )


view : Model -> Browser.Document Msg
view model =
    { title = pageTitle model.page
    , body = [ viewPage model.page ]
    }


pageTitle : Page -> String
pageTitle page =
    case page of
        Landing ->
            "Pubquiz"

        Quiz _ ->
            "Quiz - Pubquiz"

        NotFound ->
            "Not Found - Pubquiz"


viewPage : Page -> Html Msg
viewPage page =
    case page of
        Landing ->
            viewLanding

        Quiz quizModel ->
            Html.map QuizMsg (Pages.Public.Quiz.View.view quizModel)

        NotFound ->
            viewNotFound


viewLanding : Html msg
viewLanding =
    section [ class "landing" ]
        [ h1 [] [ text "Pubquiz" ]
        , p [] [ text "Select a quiz to view results." ]
        , nav []
            [ ul []
                [ li [] [ a [ href "/quiz/1" ] [ text "Quiz 1" ] ]
                ]
            ]
        ]


viewNotFound : Html msg
viewNotFound =
    section [ class "not-found" ]
        [ h1 [] [ text "Page Not Found" ]
        , p [] [ text "The page you're looking for doesn't exist." ]
        , a [ href "/" ] [ text "Go to home" ]
        ]
