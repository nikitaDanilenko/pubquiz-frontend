module Pages.BackOffice.QuizSettings.Handler exposing (init, update)

import Api.Api
import Api.Types exposing (Quiz, Round, Team)
import Date
import Dict exposing (Dict)
import Pages.BackOffice.QuizSettings.Page as Page


init : { quizId : Int, isAdmin : Bool } -> ( Page.Model, Cmd Page.Msg )
init params =
    ( { quizId = params.quizId
      , quiz = Nothing
      , name = ""
      , date = ""
      , place = ""
      , teamNames = Dict.empty
      , questionsPerRound = Dict.empty
      , isAdmin = params.isAdmin
      , isLoading = True
      , isSaving = False
      , isLocked = False
      , error = Nothing
      , successMessage = Nothing
      }
    , Api.Api.publicQuizId
        { params = { quizId = params.quizId }
        , toMsg = Page.GotQuiz
        }
    )


update : Page.Msg -> Page.Model -> ( Page.Model, Cmd Page.Msg )
update msg model =
    case msg of
        Page.GotQuiz result ->
            case result of
                Ok quiz ->
                    ( { model
                        | quiz = Just quiz
                        , name = quiz.summary.identifier.name
                        , date = Date.toIsoString quiz.summary.identifier.date
                        , place = quiz.summary.identifier.place
                        , teamNames = initTeamNames quiz.scoreBoard.teams
                        , questionsPerRound = initQuestionsPerRound quiz
                        , isLoading = False
                        , isLocked = not quiz.summary.active
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to load quiz"
                        , isLoading = False
                      }
                    , Cmd.none
                    )

        Page.SetName name ->
            ( { model | name = name, successMessage = Nothing }
            , Cmd.none
            )

        Page.SetDate date ->
            ( { model | date = date, successMessage = Nothing }
            , Cmd.none
            )

        Page.SetPlace place ->
            ( { model | place = place, successMessage = Nothing }
            , Cmd.none
            )

        Page.SaveIdentifier ->
            case ( Date.fromIsoString model.date, model.quiz ) of
                ( Ok date, Just quiz ) ->
                    ( { model | isSaving = True, error = Nothing, successMessage = Nothing }
                    , Api.Api.backofficeQuizIdChangeSettings
                        { params = { quizId = model.quizId }
                        , toMsg = Page.GotSaveIdentifierResponse
                        , body =
                            { identifier = { name = model.name, date = date, place = model.place }
                            , settings =
                                { numberOfTeams = List.length quiz.scoreBoard.teams
                                , questionsPerRound =
                                    quiz.rounds
                                        |> List.sortBy .number
                                        |> List.map (\r -> Dict.get r.number model.questionsPerRound |> Maybe.withDefault r.numberOfQuestions)
                                }
                            }
                        }
                    )

                ( Err _, _ ) ->
                    ( { model | error = Just "Invalid date format" }
                    , Cmd.none
                    )

                ( _, Nothing ) ->
                    ( { model | error = Just "Quiz not loaded" }
                    , Cmd.none
                    )

        Page.GotSaveIdentifierResponse result ->
            case result of
                Ok _ ->
                    ( { model
                        | isSaving = False
                        , successMessage = Just "Settings saved"
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | isSaving = False
                        , error = Just "Failed to save settings"
                      }
                    , Cmd.none
                    )

        Page.SetTeamName teamNumber name ->
            ( { model
                | teamNames = Dict.insert teamNumber name model.teamNames
                , successMessage = Nothing
              }
            , Cmd.none
            )

        Page.SaveTeamName teamNumber ->
            case Dict.get teamNumber model.teamNames of
                Just name ->
                    ( { model | isSaving = True, error = Nothing, successMessage = Nothing }
                    , Api.Api.backofficeQuizIdRenameTeam
                        { params = { quizId = model.quizId }
                        , toMsg = Page.GotSaveTeamNameResponse teamNumber
                        , body = { teamNumber = teamNumber, newName = name }
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )

        Page.GotSaveTeamNameResponse teamNumber result ->
            case result of
                Ok _ ->
                    let
                        updatedQuiz =
                            model.quiz
                                |> Maybe.map (updateTeamName teamNumber (Dict.get teamNumber model.teamNames |> Maybe.withDefault ""))
                    in
                    ( { model
                        | isSaving = False
                        , quiz = updatedQuiz
                        , successMessage = Just "Team name saved"
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | isSaving = False
                        , error = Just "Failed to save team name"
                      }
                    , Cmd.none
                    )

        Page.ToggleTeamActive teamNumber active ->
            ( { model | isSaving = True, error = Nothing, successMessage = Nothing }
            , Api.Api.backofficeQuizIdSetTeamActive
                { params = { quizId = model.quizId }
                , toMsg = Page.GotToggleTeamActiveResponse teamNumber active
                , body = { teamNumber = teamNumber, active = active }
                }
            )

        Page.GotToggleTeamActiveResponse teamNumber active result ->
            case result of
                Ok _ ->
                    let
                        updatedQuiz =
                            model.quiz
                                |> Maybe.map (updateTeamActive teamNumber active)
                    in
                    ( { model
                        | isSaving = False
                        , quiz = updatedQuiz
                        , successMessage = Just "Team status updated"
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | isSaving = False
                        , error = Just "Failed to update team status"
                      }
                    , Cmd.none
                    )

        Page.SetQuestionsForRound roundNumber input ->
            let
                value =
                    String.toInt input |> Maybe.withDefault 0 |> max 0
            in
            ( { model | questionsPerRound = Dict.insert roundNumber value model.questionsPerRound }
            , Cmd.none
            )

        Page.LockQuiz ->
            ( { model | isSaving = True, error = Nothing, successMessage = Nothing }
            , Api.Api.backofficeQuizIdLock
                { params = { quizId = model.quizId }
                , toMsg = Page.GotLockResponse
                }
            )

        Page.GotLockResponse result ->
            case result of
                Ok _ ->
                    ( { model
                        | isSaving = False
                        , isLocked = True
                        , successMessage = Just "Quiz locked"
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | isSaving = False
                        , error = Just "Failed to lock quiz"
                      }
                    , Cmd.none
                    )

        Page.UnlockQuiz ->
            ( { model | isSaving = True, error = Nothing, successMessage = Nothing }
            , Api.Api.backofficeQuizIdUnlock
                { params = { quizId = model.quizId }
                , toMsg = Page.GotUnlockResponse
                }
            )

        Page.GotUnlockResponse result ->
            case result of
                Ok _ ->
                    ( { model
                        | isSaving = False
                        , isLocked = False
                        , successMessage = Just "Quiz unlocked"
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | isSaving = False
                        , error = Just "Failed to unlock quiz"
                      }
                    , Cmd.none
                    )

        Page.GoToPointEntry ->
            ( model, Cmd.none )


initQuestionsPerRound : Quiz -> Dict Int Int
initQuestionsPerRound quiz =
    quiz.rounds
        |> List.map (\r -> ( r.number, r.numberOfQuestions ))
        |> Dict.fromList


initTeamNames : List Team -> Dict Int String
initTeamNames teams =
    teams
        |> List.map (\t -> ( t.number, t.name ))
        |> Dict.fromList


updateTeamName : Int -> String -> Quiz -> Quiz
updateTeamName teamNumber newName quiz =
    let
        updatedTeams =
            quiz.scoreBoard.teams
                |> List.map
                    (\t ->
                        if t.number == teamNumber then
                            { t | name = newName }

                        else
                            t
                    )
    in
    { quiz | scoreBoard = { scores = quiz.scoreBoard.scores, teams = updatedTeams } }


updateTeamActive : Int -> Bool -> Quiz -> Quiz
updateTeamActive teamNumber active quiz =
    let
        updatedTeams =
            quiz.scoreBoard.teams
                |> List.map
                    (\t ->
                        if t.number == teamNumber then
                            { t | active = active }

                        else
                            t
                    )
    in
    { quiz | scoreBoard = { scores = quiz.scoreBoard.scores, teams = updatedTeams } }
