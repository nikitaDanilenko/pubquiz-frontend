port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h1, p, section, text)
import Html.Attributes exposing (attribute, class, href)
import Html.Events exposing (onClick)
import Pages.Public.Overview.Handler
import Pages.Public.Overview.Page
import Pages.Public.Overview.View
import Pages.Public.Quiz.Handler
import Pages.Public.Quiz.Page
import Pages.Public.Quiz.View
import Pages.Public.Team.Handler
import Pages.Public.Team.Page
import Pages.Public.Team.View
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)
import Util.Theme as Theme exposing (Theme)


port saveTheme : String -> Cmd msg


type alias Flags =
    { apiBase : String
    , theme : String
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
    , theme : Theme
    }


type Page
    = Landing
    | Overview Pages.Public.Overview.Page.Model
    | Quiz Pages.Public.Quiz.Page.Model
    | Team Pages.Public.Team.Page.Model
    | NotFound


type Route
    = LandingRoute
    | OverviewRoute
    | QuizRoute Int
    | TeamRoute Int Int


routeParser : Parser (Route -> a) a
routeParser =
    let
        quizParser =
            -- The 'quizId' route is a legacy route that is supported so that old links are easy to redirect.
            Parser.oneOf
                [ Parser.map QuizRoute (Parser.s "quizId" </> Parser.int) -- legacy route
                , Parser.map QuizRoute (Parser.s "quizzes" </> Parser.int)
                ]

        teamParser =
            -- The 'quizId/teamNumber' route is a legacy route that is supported so that old links are easy to redirect.
            Parser.oneOf
                [ Parser.map (\quizId teamNumber _ -> TeamRoute quizId teamNumber) (Parser.s "quizId" </> Parser.int </> Parser.s "teamNumber" </> Parser.int </> Parser.s "teamCode" </> Parser.string)
                , Parser.map TeamRoute (Parser.s "quizzes" </> Parser.int </> Parser.s "teams" </> Parser.int)
                ]
    in
    Parser.oneOf
        [ Parser.map LandingRoute Parser.top
        , Parser.map OverviewRoute (Parser.s "quizzes")
        , teamParser
        , quizParser
        ]


parseUrl : Url -> Maybe Route
parseUrl url =
    Parser.parse routeParser url


parseTheme : String -> Theme
parseTheme str =
    if str == "dark" then
        Theme.Dark

    else
        Theme.Light


themeToString : Theme -> String
themeToString theme =
    case theme of
        Theme.Light ->
            "light"

        Theme.Dark ->
            "dark"


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , page = NotFound
            , apiBase = flags.apiBase
            , theme = parseTheme flags.theme
            }
    in
    navigateTo (parseUrl url) model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OverviewMsg Pages.Public.Overview.Page.Msg
    | QuizMsg Pages.Public.Quiz.Page.Msg
    | TeamMsg Pages.Public.Team.Page.Msg
    | ToggleTheme


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

        ( OverviewMsg overviewMsg, Overview overviewModel ) ->
            let
                ( newOverviewModel, overviewCmd ) =
                    Pages.Public.Overview.Handler.update overviewMsg overviewModel
            in
            ( { model | page = Overview newOverviewModel }
            , Cmd.map OverviewMsg overviewCmd
            )

        ( QuizMsg quizMsg, Quiz quizModel ) ->
            let
                ( newQuizModel, quizCmd ) =
                    Pages.Public.Quiz.Handler.update quizMsg quizModel
            in
            ( { model | page = Quiz newQuizModel }
            , Cmd.map QuizMsg quizCmd
            )

        ( TeamMsg teamMsg, Team teamModel ) ->
            let
                ( newTeamModel, teamCmd ) =
                    Pages.Public.Team.Handler.update teamMsg teamModel
            in
            ( { model | page = Team newTeamModel }
            , Cmd.map TeamMsg teamCmd
            )

        ( ToggleTheme, _ ) ->
            let
                newTheme =
                    case model.theme of
                        Theme.Light ->
                            Theme.Dark

                        Theme.Dark ->
                            Theme.Light
            in
            ( { model | theme = newTheme }
            , saveTheme (themeToString newTheme)
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

        Just OverviewRoute ->
            let
                ( overviewModel, overviewCmd ) =
                    Pages.Public.Overview.Handler.init
                        { apiBase = model.apiBase
                        }
            in
            ( { model | page = Overview overviewModel }
            , Cmd.map OverviewMsg overviewCmd
            )

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

        Just (TeamRoute quizId teamNumber) ->
            initTeamPage model quizId teamNumber


initTeamPage : Model -> Int -> Int -> ( Model, Cmd Msg )
initTeamPage model quizId teamNumber =
    let
        ( teamModel, teamCmd ) =
            Pages.Public.Team.Handler.init
                { quizId = quizId
                , teamNumber = teamNumber
                , apiBase = model.apiBase
                }
    in
    ( { model | page = Team teamModel }
    , Cmd.map TeamMsg teamCmd
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Quizzes"
    , body =
        [ div [ attribute "data-theme" (themeToString model.theme), class "app" ]
            [ viewThemeToggle model.theme
            , viewPage model.theme model.page
            ]
        ]
    }


viewThemeToggle : Theme -> Html Msg
viewThemeToggle theme =
    let
        icon =
            case theme of
                Theme.Light ->
                    "💡"

                Theme.Dark ->
                    "💡"
    in
    button [ class "theme-toggle", onClick ToggleTheme ]
        [ text icon
        ]


viewPage : Theme -> Page -> Html Msg
viewPage theme page =
    case page of
        Landing ->
            viewLanding

        Overview overviewModel ->
            Html.map OverviewMsg (Pages.Public.Overview.View.view overviewModel)

        Quiz quizModel ->
            Html.map QuizMsg (Pages.Public.Quiz.View.view theme quizModel)

        Team teamModel ->
            Html.map TeamMsg (Pages.Public.Team.View.view teamModel)

        NotFound ->
            viewNotFound


viewLanding : Html msg
viewLanding =
    section [ class "landing" ]
        [ h1 [] [ text "Pubquiz" ]
        , a [ href "/quizzes", class "browse-link" ] [ text "Browse Quizzes" ]
        ]


viewNotFound : Html msg
viewNotFound =
    section [ class "not-found" ]
        [ h1 [] [ text "Page Not Found" ]
        , p [] [ text "The page you're looking for doesn't exist." ]
        , a [ href "/" ] [ text "Go to home" ]
        ]
