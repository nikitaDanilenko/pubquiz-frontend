module Pages.BackOffice.QuizSettings.Page exposing
    ( Model
    , Msg(..)
    , lenses
    )

import Api.Types
    exposing
        ( BackofficeQuizIdAddTeams_Error
        , BackofficeQuizIdChangeSettings_Error
        , BackofficeQuizIdRenameTeam_Error
        , BackofficeQuizIdSetTeamActive_Error
        , Quiz
        )
import Dict exposing (Dict)
import Monocle.Lens exposing (Lens)
import OpenApi.Common


type alias Model =
    { quizId : Int
    , quiz : Maybe Quiz
    , name : String
    , date : String
    , place : String
    , teamNames : Dict Int String
    , additionalTeams : Int
    , isLoading : Bool
    , isSaving : Bool
    , error : Maybe String
    , successMessage : Maybe String
    }


lenses :
    { quizId : Lens Model Int
    , quiz : Lens Model (Maybe Quiz)
    , name : Lens Model String
    , date : Lens Model String
    , place : Lens Model String
    , teamNames : Lens Model (Dict Int String)
    , additionalTeams : Lens Model Int
    , isLoading : Lens Model Bool
    , isSaving : Lens Model Bool
    , error : Lens Model (Maybe String)
    , successMessage : Lens Model (Maybe String)
    }
lenses =
    { quizId = Lens .quizId (\b a -> { a | quizId = b })
    , quiz = Lens .quiz (\b a -> { a | quiz = b })
    , name = Lens .name (\b a -> { a | name = b })
    , date = Lens .date (\b a -> { a | date = b })
    , place = Lens .place (\b a -> { a | place = b })
    , teamNames = Lens .teamNames (\b a -> { a | teamNames = b })
    , additionalTeams = Lens .additionalTeams (\b a -> { a | additionalTeams = b })
    , isLoading = Lens .isLoading (\b a -> { a | isLoading = b })
    , isSaving = Lens .isSaving (\b a -> { a | isSaving = b })
    , error = Lens .error (\b a -> { a | error = b })
    , successMessage = Lens .successMessage (\b a -> { a | successMessage = b })
    }


type Msg
    = GotQuiz (Result (OpenApi.Common.Error () String) Quiz)
    | SetName String
    | SetDate String
    | SetPlace String
    | SaveIdentifier
    | GotSaveIdentifierResponse (Result (OpenApi.Common.Error BackofficeQuizIdChangeSettings_Error String) ())
    | SetTeamName Int String
    | SaveTeamName Int
    | GotSaveTeamNameResponse Int (Result (OpenApi.Common.Error BackofficeQuizIdRenameTeam_Error String) ())
    | ToggleTeamActive Int Bool
    | GotToggleTeamActiveResponse Int Bool (Result (OpenApi.Common.Error BackofficeQuizIdSetTeamActive_Error String) ())
    | SetAdditionalTeams String
    | AddTeams
    | GotAddTeamsResponse (Result (OpenApi.Common.Error BackofficeQuizIdAddTeams_Error String) ())
    | GoToPointEntry
