module QuizInput exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Common.Authentication as Authentication exposing (Authentication)
import Common.Base exposing (SessionKey)
import Common.ConnectionUtil exposing (errorToString)
import Common.Constants exposing (actionParam, getLabelsApi, getQuizRatingsApi, loginApi, newApi, passwordParam, quizIdParam, quizIdentifierParam, quizSettingsParam, updateQuizSettingsApi, userParam)
import Common.Copy exposing (updateLabelsByField, updateQuizIdentifierDate, updateQuizIdentifierName, updateQuizIdentifierPlace, updateQuizInfoQuizId, updateQuizInfoQuizIdentifier, updateQuizSettingsLabels, updateQuizSettingsNumberOfTeams, updateQuizSettingsRounds)
import Common.QuizRatings as QuizRatings
import Common.Types exposing (Action(..), Credentials, DbQuizId, Labels, Password, QuizIdentifier, QuizName, QuizRatings, QuizSettings, UserHash, UserName, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings, jsonDecUserHash, jsonEncAction, jsonEncDbQuizId, jsonEncPassword, jsonEncQuizIdentifier, jsonEncQuizSettings, jsonEncUserName)
import Common.Util as Util exposing (adjustToSizeWith, getAllWith, getMsg, isValidQuizName, updateIndex)
import Date
import Html exposing (Html)
import Http exposing (Error)
import Input.ConfirmLock as ConfirmLock exposing (Msg(..))
import Input.CreateUser as CreateUser exposing (Msg(..))
import Input.Login as Login exposing (Msg(..))
import Input.NewUser as NewUser exposing (NewUser)
import Input.PointInput as PointInput exposing (Msg(..))
import Input.RequestUtils exposing (RestKey, RestParam, RestValue, encodeWithSignature, mkJSONParams)
import Input.Selection as Selection exposing (Msg(..))
import Input.SetQuizSettings as SetQuizSettings exposing (Usage(..))
import Input.Validity as Validity


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Quiz Interface", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type alias Model = {
  page: Page,
  authentication: Authentication,
  feedback : String
 }

updatePage : Model -> Page -> Model
updatePage model page = {model | page = page}

updateAuthentication : Model -> Authentication -> Model
updateAuthentication model authentication = {model | authentication = authentication}

updateFeedback : Model -> String -> Model
updateFeedback model feedback = { model | feedback = feedback }

type Page =
  Login Login.Model
  | CreateUser CreateUser.Model
  | Selection Selection.Model
  | ConfirmLock ConfirmLock.Model
  | PointInput PointInput.Model
  | CreateQuiz SetQuizSettings.Model
  | UpdateQuiz SetQuizSettings.Model

type Msg =
  LoginMsg Login.Msg
  | CreateUserMsg CreateUser.Msg
  | SelectionMsg Selection.Msg
  | ConfirmLockMsg ConfirmLock.Msg
  | PointInputMsg PointInput.Msg
  | CreateQuizMsg SetQuizSettings.Msg
  | UpdateQuizMsg SetQuizSettings.Msg

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
                    let authenticated =
                            Authentication.updateSessionKey model.authentication sessionKey
                              |> flip Authentication.updateUserName login.user
                              |> updateAuthentication model
                    in stepSelection authenticated Selection.init
                  Err error ->
                    (updateFeedback model (errorToString error), Cmd.none)
              other -> stepLogin model (Login.update other login)
          _ -> (model, Cmd.none)

      CreateUserMsg createUserMsg ->
        case model.page of
          CreateUser createUser ->
            case createUserMsg of
              CreateUser.Back -> stepSelection model Selection.init
              other -> stepCreateUser model (CreateUser.update other createUser)
          _ -> (model, Cmd.none)

      SelectionMsg selectionMsg ->
        case model.page of
          Selection selection ->
            case selectionMsg of
              StartCreatingQuiz ->
                stepCreateQuiz model (SetQuizSettings.init model.authentication Create)
              StartCreatingUser ->
                stepCreateUser model (CreateUser.init model.authentication)
              otherMsg -> stepSelection model (Selection.update otherMsg selection)
          _ -> (model, Cmd.none)

      ConfirmLockMsg confirmLockMsg ->
        case model.page of
          ConfirmLock confirmLock ->
            case confirmLockMsg of
              ConfirmLock.Back -> stepPointInput model (PointInput.init model.authentication confirmLock.quizInfo)
              Locked response ->
                          case response of
                              Ok _ ->
                                  stepSelection model Selection.init

                              Err error ->
                                  stepConfirmLock model (ConfirmLock.updateFeedback confirmLock (errorToString error), Cmd.none)
              otherConfirmLockMsg -> stepConfirmLock model (ConfirmLock.update otherConfirmLockMsg confirmLock)
          _ -> (model, Cmd.none)

      PointInputMsg pointInputMsg ->
        case model.page of
          PointInput pointInput ->
            case pointInputMsg of
              PointInput.Back -> stepSelection model Selection.init
              AcknowledgeLock -> stepConfirmLock model (ConfirmLock.init pointInput.quizInfo model.authentication)
              otherPointInputMsg -> stepPointInput model (PointInput.update otherPointInputMsg pointInput)

          _ -> (model, Cmd.none)


      CreateQuizMsg msg ->



      UpdateQuizMsg msg ->



view : Model -> Html Msg
view model =
    let
        currentView =
            case model.page of
              Login login -> Login.view login

              CreateUser createUser -> CreateUser.view createUser


              Selection selection -> Selection.view selection


              ConfirmLock confirmLock -> ConfirmLock.view confirmLock


              PointInput pointInput -> PointInput.view pointInput


              CreateQuiz setValues -> SetQuizSettings.view setValues


              UpdateQuiz setValues -> SetQuizSettings.view setValues

    in
    wrapView currentView model


stepUpdateQuiz : Model -> (Login.Model, Cmd Login.Msg) -> (Model, Cmd Msg)
stepLogin model (login, cmd) =
  ({model | page = Login login}, Cmd.map LoginMsg cmd)

stepSelection : Model -> (Selection.Model, Cmd Selection.Msg) -> (Model, Cmd Msg)
stepSelection model (selection, cmd) =
  ({model | page = Selection selection}, Cmd.map SelectionMsg cmd)

stepCreateUser : Model -> (CreateUser.Model, Cmd CreateUser.Msg) -> (Model, Cmd Msg)
stepCreateUser model (createUser, cmd) =
  ({model | page = CreateUser createUser}, Cmd.map CreateUserMsg cmd)

stepConfirmLock : Model -> (ConfirmLock.Model, Cmd ConfirmLock.Msg) -> (Model, Cmd Msg)
stepConfirmLock model (confirmLock, cmd) =
  ({model | page = ConfirmLock confirmLock}, Cmd.map ConfirmLockMsg cmd)

stepPointInput : Model -> (PointInput.Model, Cmd PointInput.Msg) -> (Model, Cmd Msg)
stepPointInput model (pointInput, cmd) =
  ({model | page = PointInput pointInput}, Cmd.map PointInputMsg cmd)

stepCreateQuiz : Model -> (SetQuizSettings.Model, Cmd SetQuizSettings.Msg) -> (Model, Cmd Msg)
stepCreateQuiz model (createQuiz, cmd) =
  ({model | page = CreateQuiz createQuiz}, Cmd.map CreateQuizMsg cmd)

stepUpdateQuiz : Model -> (SetQuizSettings.Model, Cmd SetQuizSettings.Msg) -> (Model, Cmd Msg)
stepUpdateQuiz model (updateQuiz, cmd) =
  ({model | page = UpdateQuiz updateQuiz}, Cmd.map UpdateQuizMsg cmd)

getAll : Cmd Msg
getAll = getAllWith (GotAll >> ResponseF)









createNewQuiz : UserName -> UserHash -> QuizIdentifier -> QuizSettings -> Cmd Msg
createNewQuiz u sk idf s =
    Http.post
        { url = newApi
        , body =
            encodeBody
                (encodeWithSignature u
                    sk
                    [ ( quizIdentifierParam, jsonEncQuizIdentifier idf )
                    , ( quizSettingsParam, jsonEncQuizSettings s )
                    , ( actionParam, jsonEncAction CreateQuizA )
                    ]
                )
        , expect = Http.expectJson (CreatedQuiz >> ResponseF) jsonDecQuizInfo
        }




updateQuizSettings : UserName -> SessionKey -> DbQuizId -> QuizSettings -> Cmd Msg
updateQuizSettings u sk qid settings =
    let
        params =
            encodeWithSignature u
                sk
                [ ( quizIdParam, jsonEncDbQuizId qid )
                , ( quizSettingsParam, jsonEncQuizSettings settings )
                , ( actionParam, jsonEncAction UpdateSettingsA )
                ]
    in
    Http.post
        { url = updateQuizSettingsApi
        , body = encodeBody params
        , expect = Http.expectWhatever (ResponseP Updated)
        }


updateQuizByQuizRatings : ErrorOr QuizRatings -> Model -> Model
updateQuizByQuizRatings eQuizRatings model =
    case eQuizRatings of
        Ok quizRatings ->
            let
                guess =
                    QuizRatings.numberOfTeams quizRatings

                actual =
                    if guess == 0 then
                        model.currentQuizSettings.numberOfTeams

                    else
                        guess

                pointsValid =
                    QuizRatings.arePointsValid quizRatings

                validity =
                    { pointsValid = pointsValid
                    , serverTextOK = True
                    , teamNamesValid = True
                    }
            in
            { model
                | currentQuizRatings = quizRatings
                , currentQuizSettings = updateQuizSettingsNumberOfTeams model.currentQuizSettings actual
                , currentQuizInfo = Maybe.withDefault Model.defaultQuizInfo (Util.find (\qi -> qi.quizId == model.currentQuizInfo.quizId) model.quizzes)
                , isValidQuizUpdate = validity
                , feedback = ""
                , displayState = Editing ContentsE
            }

        Err _ ->
            { model
                | isValidQuizUpdate =
                    Validity.updateServerText False model.isValidQuizUpdate
                , feedback = "Parsing error: Could not read quiz ratings from server"
                , displayState = Selecting
            }


wrapView : (model -> Html Msg) -> model -> Html Msg
wrapView viewOf model =
    div [ id "mainPage" ]
        [ node "link" [ rel "stylesheet", type_ "text/css", href "style.css" ] []
        , viewOf model
        ]






