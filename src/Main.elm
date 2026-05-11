port module Main exposing (main)

import Api.Api
import Api.Types
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h1, p, section, text)
import Html.Attributes exposing (attribute, class, href)
import Html.Events exposing (onClick)
import OpenApi.Common
import Pages.BackOffice.CreateQuiz.Handler
import Pages.BackOffice.CreateQuiz.Page
import Pages.BackOffice.CreateQuiz.View
import Pages.BackOffice.Login.Handler
import Pages.BackOffice.Login.Page
import Pages.BackOffice.Login.View
import Pages.BackOffice.Overview.Handler
import Pages.BackOffice.Overview.Page
import Pages.BackOffice.Overview.View
import Pages.BackOffice.QuizEdit.Handler
import Pages.BackOffice.QuizEdit.Page
import Pages.BackOffice.QuizEdit.View
import Pages.BackOffice.QuizSettings.Handler
import Pages.BackOffice.QuizSettings.Page
import Pages.BackOffice.QuizSettings.View
import Pages.Public.QuizSheets.Handler
import Pages.Public.QuizSheets.Page
import Pages.Public.QuizSheets.View
import Pages.Public.Overview.Handler
import Pages.Public.Overview.Page
import Pages.Public.Overview.View
import Pages.Public.Quiz.Handler
import Pages.Public.Quiz.Page
import Pages.Public.Quiz.View
import Pages.Public.Team.Handler
import Pages.Public.Team.Page
import Pages.Public.Team.View
import Maybe.Extra
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)
import Util.Theme as Theme exposing (Theme)


port saveTheme : String -> Cmd msg


type alias Flags =
    { theme : String
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
    , theme : Theme
    , authenticatedUser : Maybe Api.Types.AuthenticatedUser
    , pendingRoute : Maybe Route
    }


type Page
    = Landing
    | Overview Pages.Public.Overview.Page.Model
    | Quiz Pages.Public.Quiz.Page.Model
    | Team Pages.Public.Team.Page.Model
    | Login Pages.BackOffice.Login.Page.Model
    | BackOfficeOverview Pages.BackOffice.Overview.Page.Model
    | CreateQuiz Pages.BackOffice.CreateQuiz.Page.Model
    | QuizEdit Pages.BackOffice.QuizEdit.Page.Model
    | QuizSettings Pages.BackOffice.QuizSettings.Page.Model
    | QuizSheets Pages.Public.QuizSheets.Page.Model
    | NotFound


type Route
    = LandingRoute
    | OverviewRoute
    | QuizRoute Int
    | TeamRoute Int Int
    | LoginRoute
    | BackOfficeOverviewRoute
    | CreateQuizRoute
    | QuizEditRoute Int
    | QuizSettingsRoute Int
    | QuizSheetsRoute Int


routeParser : Parser (Route -> a) a
routeParser =
    let
        public =
            "quizzes"

        backOffice =
            "backoffice"

        quizParser =
            -- The 'quizId' route is a legacy route that is supported so that old links are easy to redirect.
            Parser.oneOf
                [ Parser.map QuizRoute (Parser.s "quizId" </> Parser.int) -- legacy route
                , Parser.map QuizRoute (Parser.s public </> Parser.int)
                ]

        teamParser =
            -- The 'quizId/teamNumber' route is a legacy route that is supported so that old links are easy to redirect.
            Parser.oneOf
                [ Parser.map (\quizId teamNumber _ -> TeamRoute quizId teamNumber) (Parser.s "quizId" </> Parser.int </> Parser.s "teamNumber" </> Parser.int </> Parser.s "teamCode" </> Parser.string)
                , Parser.map TeamRoute (Parser.s public </> Parser.int </> Parser.s "teams" </> Parser.int)
                ]
    in
    Parser.oneOf
        [ Parser.map LandingRoute Parser.top
        , Parser.map OverviewRoute (Parser.s public)
        , Parser.map LoginRoute (Parser.s backOffice </> Parser.s "login")
        , Parser.map CreateQuizRoute (Parser.s backOffice </> Parser.s "create")
        , Parser.map QuizSettingsRoute (Parser.s backOffice </> Parser.int </> Parser.s "settings")
        , Parser.map QuizEditRoute (Parser.s backOffice </> Parser.int)
        , Parser.map BackOfficeOverviewRoute (Parser.s backOffice)
        , Parser.map QuizSheetsRoute (Parser.s public </> Parser.int </> Parser.s "sheets")
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
            , theme = parseTheme flags.theme
            , authenticatedUser = Nothing
            , pendingRoute = parseUrl url
            }
    in
    ( model
    , Api.Api.backofficeWhoami { toMsg = GotWhoami }
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotWhoami (Result (OpenApi.Common.Error Never String) Api.Types.AuthenticatedUser)
    | OverviewMsg Pages.Public.Overview.Page.Msg
    | QuizMsg Pages.Public.Quiz.Page.Msg
    | TeamMsg Pages.Public.Team.Page.Msg
    | LoginMsg Pages.BackOffice.Login.Page.Msg
    | BackOfficeOverviewMsg Pages.BackOffice.Overview.Page.Msg
    | CreateQuizMsg Pages.BackOffice.CreateQuiz.Page.Msg
    | QuizEditMsg Pages.BackOffice.QuizEdit.Page.Msg
    | QuizSettingsMsg Pages.BackOffice.QuizSettings.Page.Msg
    | QuizSheetsMsg Pages.Public.QuizSheets.Page.Msg
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

        ( GotWhoami result, _ ) ->
            let
                newModel =
                    case result of
                        Ok user ->
                            { model | authenticatedUser = Just user }

                        Err _ ->
                            { model | authenticatedUser = Nothing }
            in
            navigateTo newModel.pendingRoute newModel

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

        ( LoginMsg loginMsg, Login loginModel ) ->
            let
                ( newLoginModel, loginCmd, loginSuccess ) =
                    Pages.BackOffice.Login.Handler.update loginMsg loginModel
            in
            if loginSuccess then
                ( { model | pendingRoute = Just BackOfficeOverviewRoute }
                , Api.Api.backofficeWhoami { toMsg = GotWhoami }
                )

            else
                ( { model | page = Login newLoginModel }
                , Cmd.map LoginMsg loginCmd
                )

        ( BackOfficeOverviewMsg backOfficeOverviewMsg, BackOfficeOverview backOfficeOverviewModel ) ->
            let
                ( newModel, cmd ) =
                    Pages.BackOffice.Overview.Handler.update backOfficeOverviewMsg backOfficeOverviewModel
            in
            ( { model | page = BackOfficeOverview newModel }
            , Cmd.map BackOfficeOverviewMsg cmd
            )

        ( CreateQuizMsg createQuizMsg, CreateQuiz createQuizModel ) ->
            let
                ( newModel, cmd, maybeQuizId ) =
                    Pages.BackOffice.CreateQuiz.Handler.update createQuizMsg createQuizModel
            in
            case maybeQuizId of
                Just quizId ->
                    ( model
                    , Nav.pushUrl model.key (String.concat [ "/backoffice/", String.fromInt quizId ])
                    )

                Nothing ->
                    ( { model | page = CreateQuiz newModel }
                    , Cmd.map CreateQuizMsg cmd
                    )

        ( QuizEditMsg quizEditMsg, QuizEdit quizEditModel ) ->
            let
                ( newModel, cmd ) =
                    Pages.BackOffice.QuizEdit.Handler.update quizEditMsg quizEditModel
            in
            ( { model | page = QuizEdit newModel }
            , Cmd.map QuizEditMsg cmd
            )

        ( QuizSettingsMsg quizSettingsMsg, QuizSettings quizSettingsModel ) ->
            let
                ( newModel, cmd ) =
                    Pages.BackOffice.QuizSettings.Handler.update quizSettingsMsg quizSettingsModel
            in
            ( { model | page = QuizSettings newModel }
            , Cmd.map QuizSettingsMsg cmd
            )

        ( QuizSheetsMsg quizSheetsMsg, QuizSheets quizSheetsModel ) ->
            let
                ( newModel, cmd ) =
                    Pages.Public.QuizSheets.Handler.update quizSheetsMsg quizSheetsModel
            in
            ( { model | page = QuizSheets newModel }
            , Cmd.map QuizSheetsMsg cmd
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
            ( { model | page = NotFound, pendingRoute = Nothing }, Cmd.none )

        Just LandingRoute ->
            ( { model | page = Landing, pendingRoute = Nothing }, Cmd.none )

        Just OverviewRoute ->
            let
                ( overviewModel, overviewCmd ) =
                    Pages.Public.Overview.Handler.init
            in
            ( { model | page = Overview overviewModel, pendingRoute = Nothing }
            , Cmd.map OverviewMsg overviewCmd
            )

        Just (QuizRoute quizId) ->
            let
                ( quizModel, quizCmd ) =
                    Pages.Public.Quiz.Handler.init
                        { quizId = quizId
                        }
            in
            ( { model | page = Quiz quizModel, pendingRoute = Nothing }
            , Cmd.map QuizMsg quizCmd
            )

        Just (TeamRoute quizId teamNumber) ->
            initTeamPage model quizId teamNumber

        Just LoginRoute ->
            let
                ( loginModel, loginCmd ) =
                    Pages.BackOffice.Login.Handler.init
            in
            ( { model | page = Login loginModel, pendingRoute = Nothing }
            , Cmd.map LoginMsg loginCmd
            )

        Just BackOfficeOverviewRoute ->
            if model.authenticatedUser /= Nothing then
                let
                    ( backOfficeOverviewModel, backOfficeOverviewCmd ) =
                        Pages.BackOffice.Overview.Handler.init
                in
                ( { model | page = BackOfficeOverview backOfficeOverviewModel, pendingRoute = Nothing }
                , Cmd.map BackOfficeOverviewMsg backOfficeOverviewCmd
                )

            else
                ( model, Nav.pushUrl model.key "/backoffice/login" )

        Just CreateQuizRoute ->
            if model.authenticatedUser /= Nothing then
                let
                    ( createQuizModel, createQuizCmd ) =
                        Pages.BackOffice.CreateQuiz.Handler.init
                in
                ( { model | page = CreateQuiz createQuizModel, pendingRoute = Nothing }
                , Cmd.map CreateQuizMsg createQuizCmd
                )

            else
                ( model, Nav.pushUrl model.key "/backoffice/login" )

        Just (QuizEditRoute quizId) ->
            if model.authenticatedUser /= Nothing then
                let
                    ( quizEditModel, quizEditCmd ) =
                        Pages.BackOffice.QuizEdit.Handler.init { quizId = quizId }
                in
                ( { model | page = QuizEdit quizEditModel, pendingRoute = Nothing }
                , Cmd.map QuizEditMsg quizEditCmd
                )

            else
                ( model, Nav.pushUrl model.key "/backoffice/login" )

        Just (QuizSettingsRoute quizId) ->
            if model.authenticatedUser /= Nothing then
                let
                    isAdmin =
                        model.authenticatedUser
                            |> Maybe.Extra.unwrap False .isAdmin

                    ( quizSettingsModel, quizSettingsCmd ) =
                        Pages.BackOffice.QuizSettings.Handler.init { quizId = quizId, isAdmin = isAdmin }
                in
                ( { model | page = QuizSettings quizSettingsModel, pendingRoute = Nothing }
                , Cmd.map QuizSettingsMsg quizSettingsCmd
                )

            else
                ( model, Nav.pushUrl model.key "/backoffice/login" )

        Just (QuizSheetsRoute quizId) ->
            let
                ( quizSheetsModel, quizSheetsCmd ) =
                    Pages.Public.QuizSheets.Handler.init { quizId = quizId }
            in
            ( { model | page = QuizSheets quizSheetsModel, pendingRoute = Nothing }
            , Cmd.map QuizSheetsMsg quizSheetsCmd
            )


initTeamPage : Model -> Int -> Int -> ( Model, Cmd Msg )
initTeamPage model quizId teamNumber =
    let
        ( teamModel, teamCmd ) =
            Pages.Public.Team.Handler.init
                { quizId = quizId
                , teamNumber = teamNumber
                }
    in
    ( { model | page = Team teamModel, pendingRoute = Nothing }
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

        Login loginModel ->
            Html.map LoginMsg (Pages.BackOffice.Login.View.view loginModel)

        BackOfficeOverview backOfficeOverviewModel ->
            Html.map BackOfficeOverviewMsg (Pages.BackOffice.Overview.View.view backOfficeOverviewModel)

        CreateQuiz createQuizModel ->
            Html.map CreateQuizMsg (Pages.BackOffice.CreateQuiz.View.view createQuizModel)

        QuizEdit quizEditModel ->
            Html.map QuizEditMsg (Pages.BackOffice.QuizEdit.View.view quizEditModel)

        QuizSettings quizSettingsModel ->
            Html.map QuizSettingsMsg (Pages.BackOffice.QuizSettings.View.view quizSettingsModel)

        QuizSheets quizSheetsModel ->
            Html.map QuizSheetsMsg (Pages.Public.QuizSheets.View.view quizSheetsModel)

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
