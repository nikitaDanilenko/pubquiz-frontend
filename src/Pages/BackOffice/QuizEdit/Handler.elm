module Pages.BackOffice.QuizEdit.Handler exposing (init, update)

import Api.Api
import Api.Types exposing (Quiz, RecordRoundScoresCommand, TeamScore)
import Dict exposing (Dict)
import Pages.BackOffice.QuizEdit.Page as Page
import Set exposing (Set)


init : { quizId : Int } -> ( Page.Model, Cmd Page.Msg )
init params =
    ( { quizId = params.quizId
      , quiz = Nothing
      , expandedRound = Just 1
      , roundInputs = Dict.empty
      , completedRounds = Set.empty
      , editingRound = Nothing
      , isSubmitting = False
      , error = Nothing
      , isLoading = True
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
                    let
                        roundInputs =
                            initRoundInputs quiz

                        completedRounds =
                            detectCompletedRounds quiz

                        expandedRound =
                            findFirstIncompleteRound quiz completedRounds
                    in
                    ( { model
                        | quiz = Just quiz
                        , roundInputs = roundInputs
                        , completedRounds = completedRounds
                        , expandedRound = expandedRound
                        , isLoading = False
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

        Page.ExpandRound roundNumber ->
            ( { model | expandedRound = Just roundNumber }
            , Cmd.none
            )

        Page.CollapseRound ->
            ( { model | expandedRound = Nothing }
            , Cmd.none
            )

        Page.SetScore roundNumber teamNumber value ->
            ( updateScoreInput roundNumber teamNumber value model
            , Cmd.none
            )

        Page.IncrementScore roundNumber teamNumber delta ->
            ( incrementScore roundNumber teamNumber delta model
            , Cmd.none
            )

        Page.MarkRoundComplete roundNumber ->
            ( { model | completedRounds = Set.insert roundNumber model.completedRounds }
            , Cmd.none
            )

        Page.EditCompletedRound roundNumber ->
            ( { model | editingRound = Just roundNumber }
            , Cmd.none
            )

        Page.CancelEdit ->
            ( { model | editingRound = Nothing }
            , Cmd.none
            )

        Page.SubmitRound roundNumber ->
            let
                cmd =
                    buildSubmitCommand model roundNumber
            in
            ( { model | isSubmitting = True, error = Nothing }
            , cmd
            )

        Page.GotSubmitResponse roundNumber result ->
            case result of
                Ok _ ->
                    ( { model
                        | isSubmitting = False
                        , editingRound = Nothing
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | isSubmitting = False
                        , error = Just (String.concat [ "Failed to submit round ", String.fromInt roundNumber ])
                      }
                    , Cmd.none
                    )

        Page.GotCorrectResponse result ->
            case result of
                Ok _ ->
                    ( { model | isSubmitting = False }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | isSubmitting = False
                        , error = Just "Failed to correct score"
                      }
                    , Cmd.none
                    )

        Page.AddRound ->
            ( addNewRound model
            , Cmd.none
            )

        Page.GoToSettings ->
            ( model
            , Cmd.none
            )


initRoundInputs : Quiz -> Dict Int Page.RoundInput
initRoundInputs quiz =
    let
        teamNumbers =
            List.map .number quiz.scoreBoard.teams

        roundNumbers =
            List.map .number quiz.rounds

        initRound roundNumber =
            let
                scores =
                    teamNumbers
                        |> List.map
                            (\teamNumber ->
                                ( teamNumber
                                , getExistingScore quiz roundNumber teamNumber
                                    |> Maybe.map String.fromFloat
                                    |> Maybe.withDefault ""
                                )
                            )
                        |> Dict.fromList
            in
            ( roundNumber, { scores = scores } )
    in
    List.map initRound roundNumbers
        |> Dict.fromList


getExistingScore : Quiz -> Int -> Int -> Maybe Float
getExistingScore quiz roundNumber teamNumber =
    quiz.scoreBoard.scores
        |> List.filter (\s -> s.roundNumber == roundNumber && s.teamNumber == teamNumber)
        |> List.head
        |> Maybe.map .points


detectCompletedRounds : Quiz -> Set Int
detectCompletedRounds quiz =
    let
        teamCount =
            List.length quiz.scoreBoard.teams

        scoreCountPerRound =
            quiz.scoreBoard.scores
                |> List.foldl
                    (\score acc ->
                        Dict.update score.roundNumber
                            (\maybeCount -> Just (Maybe.withDefault 0 maybeCount + 1))
                            acc
                    )
                    Dict.empty
    in
    quiz.rounds
        |> List.filterMap
            (\round ->
                let
                    count =
                        Dict.get round.number scoreCountPerRound
                            |> Maybe.withDefault 0
                in
                if count == teamCount then
                    Just round.number

                else
                    Nothing
            )
        |> Set.fromList


findFirstIncompleteRound : Quiz -> Set Int -> Maybe Int
findFirstIncompleteRound quiz completedRounds =
    let
        incompleteRound =
            quiz.rounds
                |> List.map .number
                |> List.filter (\n -> not (Set.member n completedRounds))
                |> List.head

        firstRound =
            quiz.rounds
                |> List.map .number
                |> List.head
    in
    case incompleteRound of
        Just n ->
            Just n

        Nothing ->
            firstRound


updateScoreInput : Int -> Int -> String -> Page.Model -> Page.Model
updateScoreInput roundNumber teamNumber value model =
    let
        updateRound maybeRoundInput =
            case maybeRoundInput of
                Just roundInput ->
                    Just { roundInput | scores = Dict.insert teamNumber value roundInput.scores }

                Nothing ->
                    Just { scores = Dict.singleton teamNumber value }
    in
    { model | roundInputs = Dict.update roundNumber updateRound model.roundInputs }


incrementScore : Int -> Int -> Float -> Page.Model -> Page.Model
incrementScore roundNumber teamNumber delta model =
    let
        currentValue =
            Dict.get roundNumber model.roundInputs
                |> Maybe.andThen (\ri -> Dict.get teamNumber ri.scores)
                |> Maybe.andThen String.toFloat
                |> Maybe.withDefault 0

        newValue =
            max 0 (currentValue + delta)

        newValueStr =
            formatScore newValue
    in
    updateScoreInput roundNumber teamNumber newValueStr model


formatScore : Float -> String
formatScore score =
    if toFloat (round score) == score then
        String.fromInt (round score)

    else
        String.fromFloat score


buildSubmitCommand : Page.Model -> Int -> Cmd Page.Msg
buildSubmitCommand model roundNumber =
    case Dict.get roundNumber model.roundInputs of
        Just roundInput ->
            let
                scores =
                    roundInput.scores
                        |> Dict.toList
                        |> List.filterMap
                            (\( teamNumber, valueStr ) ->
                                String.toFloat valueStr
                                    |> Maybe.map (\points -> { teamNumber = teamNumber, points = points })
                            )
            in
            Api.Api.backofficeQuizIdRecordRoundScores
                { params = { quizId = model.quizId }
                , toMsg = Page.GotSubmitResponse roundNumber
                , body = { roundNumber = roundNumber, scores = scores }
                }

        Nothing ->
            Cmd.none


addNewRound : Page.Model -> Page.Model
addNewRound model =
    case model.quiz of
        Just quiz ->
            let
                maxRoundNumber =
                    quiz.rounds
                        |> List.map .number
                        |> List.maximum
                        |> Maybe.withDefault 0

                newRoundNumber =
                    maxRoundNumber + 1

                teamNumbers =
                    List.map .number quiz.scoreBoard.teams

                newRoundInput =
                    { scores = teamNumbers |> List.map (\t -> ( t, "" )) |> Dict.fromList }

                newRound =
                    { number = newRoundNumber
                    , displayMaxPoints = 10
                    , numberOfQuestions = 10
                    }

                updatedQuiz =
                    { quiz | rounds = List.concat [ quiz.rounds, [ newRound ] ] }
            in
            { model
                | quiz = Just updatedQuiz
                , roundInputs = Dict.insert newRoundNumber newRoundInput model.roundInputs
                , expandedRound = Just newRoundNumber
            }

        Nothing ->
            model
