module Pages.BackOffice.QuizEdit.Page exposing
    ( Model
    , Msg(..)
    , RoundInput
    , RoundState(..)
    , lenses
    )

import Api.Types exposing (BackofficeQuizIdCorrectScore_Error, BackofficeQuizIdRecordRoundScores_Error, Quiz)
import Dict exposing (Dict)
import Monocle.Lens exposing (Lens)
import OpenApi.Common
import Set exposing (Set)


type alias Model =
    { quizId : Int
    , quiz : Maybe Quiz
    , expandedRound : Maybe Int
    , roundInputs : Dict Int RoundInput
    , completedRounds : Set Int
    , editingRound : Maybe Int
    , isSubmitting : Bool
    , isLocked : Bool
    , error : Maybe String
    , isLoading : Bool
    }


type alias RoundInput =
    { scores : Dict Int String
    }


type RoundState
    = Draft
    | Complete
    | Editing


lenses :
    { quizId : Lens Model Int
    , quiz : Lens Model (Maybe Quiz)
    , expandedRound : Lens Model (Maybe Int)
    , roundInputs : Lens Model (Dict Int RoundInput)
    , completedRounds : Lens Model (Set Int)
    , editingRound : Lens Model (Maybe Int)
    , isSubmitting : Lens Model Bool
    , isLocked : Lens Model Bool
    , error : Lens Model (Maybe String)
    , isLoading : Lens Model Bool
    }
lenses =
    { quizId = Lens .quizId (\b a -> { a | quizId = b })
    , quiz = Lens .quiz (\b a -> { a | quiz = b })
    , expandedRound = Lens .expandedRound (\b a -> { a | expandedRound = b })
    , roundInputs = Lens .roundInputs (\b a -> { a | roundInputs = b })
    , completedRounds = Lens .completedRounds (\b a -> { a | completedRounds = b })
    , editingRound = Lens .editingRound (\b a -> { a | editingRound = b })
    , isSubmitting = Lens .isSubmitting (\b a -> { a | isSubmitting = b })
    , isLocked = Lens .isLocked (\b a -> { a | isLocked = b })
    , error = Lens .error (\b a -> { a | error = b })
    , isLoading = Lens .isLoading (\b a -> { a | isLoading = b })
    }


type Msg
    = GotQuiz (Result (OpenApi.Common.Error () String) Quiz)
    | ExpandRound Int
    | CollapseRound
    | SetScore Int Int String
    | IncrementScore Int Int Float
    | MarkRoundComplete Int
    | EditCompletedRound Int
    | CancelEdit
    | SubmitRound Int
    | GotSubmitResponse Int (Result (OpenApi.Common.Error BackofficeQuizIdRecordRoundScores_Error String) ())
    | GotCorrectResponse (Result (OpenApi.Common.Error BackofficeQuizIdCorrectScore_Error String) ())
    | AddRound
    | GoToSettings
