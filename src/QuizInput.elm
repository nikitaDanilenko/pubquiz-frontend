module QuizInput exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Common.Authentication as Authentication exposing (Authentication)
import Common.WireUtil exposing (errorToString)
import Common.Types exposing (Action(..), Credentials, DbQuizId, Labels, Password, QuizIdentifier, QuizName, QuizRatings, QuizSettings, UserHash, UserName)
import Html exposing (Html, div, node)
import Html.Attributes exposing (href, id, rel, type_)
import Input.ConfirmLock as ConfirmLock exposing (Msg(..))
import Input.CreateUser as CreateUser exposing (Msg(..))
import Input.Login as Login exposing (Msg(..))
import Input.PointInput as PointInput exposing (Msg(..))
import Input.QuizValues as QuizValues
import Input.Selection as Selection exposing (Msg(..))
import Input.SetQuizSettings as SetQuizSettings


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Quiz Interface", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { page : Page
    , authentication : Authentication
    , feedback : String
    }


updatePage : Model -> Page -> Model
updatePage model page =
    { model | page = page }


updateAuthentication : Model -> Authentication -> Model
updateAuthentication model authentication =
    { model | authentication = authentication }


updateFeedback : Model -> String -> Model
updateFeedback model feedback =
    { model | feedback = feedback }


type Page
    = Login Login.Model
    | CreateUser CreateUser.Model
    | Selection Selection.Model
    | ConfirmLock ConfirmLock.Model
    | PointInput PointInput.Model
    | CreateQuiz SetQuizSettings.CreateModel
    | UpdateQuiz SetQuizSettings.UpdateModel


type Msg
    = LoginMsg Login.Msg
    | CreateUserMsg CreateUser.Msg
    | SelectionMsg Selection.Msg
    | ConfirmLockMsg ConfirmLock.Msg
    | PointInputMsg PointInput.Msg
    | CreateQuizMsg SetQuizSettings.Msg
    | UpdateQuizMsg SetQuizSettings.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Login Login.init
      , authentication = Authentication.empty
      , feedback = ""
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        currentView =
            case model.page of
                Login login ->
                    Html.map LoginMsg (Login.view login)

                CreateUser createUser ->
                    Html.map CreateUserMsg (CreateUser.view createUser)

                Selection selection ->
                    Html.map SelectionMsg (Selection.view selection)

                ConfirmLock confirmLock ->
                    Html.map ConfirmLockMsg (ConfirmLock.view confirmLock)

                PointInput pointInput ->
                    Html.map PointInputMsg (PointInput.view pointInput)

                CreateQuiz createQuiz ->
                    Html.map CreateQuizMsg (SetQuizSettings.viewCreate createQuiz)

                UpdateQuiz updateQuiz ->
                    Html.map UpdateQuizMsg (SetQuizSettings.viewUpdate updateQuiz)
    in
    wrapView currentView


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginMsg loginMsg ->
            case model.page of
                Login login ->
                    case loginMsg of
                        LoggedIn response ->
                            case response of
                                Ok sessionKey ->
                                    let
                                        authenticated =
                                            Authentication.updateSessionKey model.authentication sessionKey
                                                |> flip Authentication.updateUserName login.user
                                                |> updateAuthentication model
                                    in
                                    stepSelection authenticated Selection.init

                                Err error ->
                                    ( updateFeedback model (errorToString error), Cmd.none )

                        other ->
                            stepLogin model (Login.update other login)

                _ ->
                    ( model, Cmd.none )

        CreateUserMsg createUserMsg ->
            case model.page of
                CreateUser createUser ->
                    case createUserMsg of
                        CreateUser.Back ->
                            stepSelection model Selection.init

                        other ->
                            stepCreateUser model (CreateUser.update other createUser)

                _ ->
                    ( model, Cmd.none )

        SelectionMsg selectionMsg ->
            case model.page of
                Selection selection ->
                    case selectionMsg of
                        StartCreatingQuiz ->
                            stepCreateQuiz model (SetQuizSettings.initCreate model.authentication)

                        StartCreatingUser ->
                            stepCreateUser model (CreateUser.init model.authentication)

                        otherMsg ->
                            stepSelection model (Selection.update otherMsg selection)

                _ ->
                    ( model, Cmd.none )

        ConfirmLockMsg confirmLockMsg ->
            case model.page of
                ConfirmLock confirmLock ->
                    case confirmLockMsg of
                        ConfirmLock.Back ->
                            stepPointInput model (PointInput.init model.authentication confirmLock.quizInfo)

                        Locked response ->
                            case response of
                                Ok _ ->
                                    stepSelection model Selection.init

                                Err error ->
                                    stepConfirmLock model ( ConfirmLock.updateFeedback confirmLock (errorToString error), Cmd.none )

                        otherConfirmLockMsg ->
                            stepConfirmLock model (ConfirmLock.update otherConfirmLockMsg confirmLock)

                _ ->
                    ( model, Cmd.none )

        PointInputMsg pointInputMsg ->
            case model.page of
                PointInput pointInput ->
                    case pointInputMsg of
                        PointInput.Back ->
                            stepSelection model Selection.init

                        EditSettings ->
                            let
                                quizSettings =
                                    { -- todo: This is not right - it should be the current value. But where to get it from?
                                      rounds = QuizValues.defaultRounds
                                    , numberOfTeams = pointInput.numberOfTeams
                                    , labels = pointInput.labels
                                    }
                            in
                            stepUpdateQuiz model (SetQuizSettings.initUpdate model.authentication pointInput.quizInfo quizSettings)

                        AcknowledgeLock ->
                            stepConfirmLock model (ConfirmLock.init pointInput.quizInfo model.authentication)

                        otherPointInputMsg ->
                            stepPointInput model (PointInput.update otherPointInputMsg pointInput)

                _ ->
                    ( model, Cmd.none )

        CreateQuizMsg createQuizMsg ->
            case model.page of
                CreateQuiz createQuiz ->
                    case createQuizMsg of
                        SetQuizSettings.Back ->
                            stepSelection model Selection.init

                        SetQuizSettings.Created quizInfoCandidate ->
                            case quizInfoCandidate of
                                Ok quizInfo ->
                                    stepPointInput model (PointInput.init model.authentication quizInfo)

                                Err error ->
                                    stepCreateQuiz model
                                        ( SetQuizSettings.updateFeedback createQuiz.base (errorToString error)
                                            |> SetQuizSettings.updateCreateBase createQuiz
                                        , Cmd.none
                                        )

                        otherCreateQuizMsg ->
                            stepCreateQuiz model (SetQuizSettings.updateCreate otherCreateQuizMsg createQuiz)

                _ ->
                    ( model, Cmd.none )

        UpdateQuizMsg updateQuizMsg ->
            case model.page of
                UpdateQuiz updateQuiz ->
                    case updateQuizMsg of
                        SetQuizSettings.Back ->
                            stepPointInput model (PointInput.init model.authentication updateQuiz.quizInfo)

                        otherUpdateQuizMsg ->
                            stepUpdateQuiz model (SetQuizSettings.updateUpdate otherUpdateQuizMsg updateQuiz)

                _ ->
                    ( model, Cmd.none )


stepLogin : Model -> ( Login.Model, Cmd Login.Msg ) -> ( Model, Cmd Msg )
stepLogin model ( login, cmd ) =
    ( updatePage model (Login login), Cmd.map LoginMsg cmd )


stepSelection : Model -> ( Selection.Model, Cmd Selection.Msg ) -> ( Model, Cmd Msg )
stepSelection model ( selection, cmd ) =
    ( updatePage model (Selection selection), Cmd.map SelectionMsg cmd )


stepCreateUser : Model -> ( CreateUser.Model, Cmd CreateUser.Msg ) -> ( Model, Cmd Msg )
stepCreateUser model ( createUser, cmd ) =
    ( updatePage model (CreateUser createUser), Cmd.map CreateUserMsg cmd )


stepConfirmLock : Model -> ( ConfirmLock.Model, Cmd ConfirmLock.Msg ) -> ( Model, Cmd Msg )
stepConfirmLock model ( confirmLock, cmd ) =
    ( updatePage model (ConfirmLock confirmLock), Cmd.map ConfirmLockMsg cmd )


stepPointInput : Model -> ( PointInput.Model, Cmd PointInput.Msg ) -> ( Model, Cmd Msg )
stepPointInput model ( pointInput, cmd ) =
    ( updatePage model (PointInput pointInput), Cmd.map PointInputMsg cmd )


stepCreateQuiz : Model -> ( SetQuizSettings.CreateModel, Cmd SetQuizSettings.Msg ) -> ( Model, Cmd Msg )
stepCreateQuiz model ( createQuiz, cmd ) =
    ( updatePage model (CreateQuiz createQuiz), Cmd.map CreateQuizMsg cmd )


stepUpdateQuiz : Model -> ( SetQuizSettings.UpdateModel, Cmd SetQuizSettings.Msg ) -> ( Model, Cmd Msg )
stepUpdateQuiz model ( updateQuiz, cmd ) =
    ( updatePage model (UpdateQuiz updateQuiz), Cmd.map UpdateQuizMsg cmd )


wrapView : Html Msg -> Html Msg
wrapView innerView =
    div [ id "mainPage" ]
        [ node "link" [ rel "stylesheet", type_ "text/css", href "style.css" ] []
        , innerView
        ]
