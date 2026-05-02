module Pages.Public.Overview.Page exposing
    ( Flags
    , Model
    , Msg(..)
    , SortBy(..)
    , SortDirection(..)
    , lenses
    )

{-| Quiz overview page state.
-}

import Api.Types exposing (QuizSummary)
import Http
import Monocle.Lens exposing (Lens)
import Util.Tristate exposing (Tristate)


type alias Model =
    { quizzes : Tristate Http.Error (List QuizSummary)
    , searchText : String
    , sortBy : SortBy
    , sortDirection : SortDirection
    }


lenses :
    { quizzes : Lens Model (Tristate Http.Error (List QuizSummary))
    , searchText : Lens Model String
    , sortBy : Lens Model SortBy
    , sortDirection : Lens Model SortDirection
    }
lenses =
    { quizzes = Lens .quizzes (\b a -> { a | quizzes = b })
    , searchText = Lens .searchText (\b a -> { a | searchText = b })
    , sortBy = Lens .sortBy (\b a -> { a | sortBy = b })
    , sortDirection = Lens .sortDirection (\b a -> { a | sortDirection = b })
    }


type SortBy
    = ByName
    | ByDate


type SortDirection
    = Ascending
    | Descending


type Msg
    = GotQuizzes (Result Http.Error (List QuizSummary))
    | SetSearchText String
    | SetSortBy SortBy
    | SetSortDirection SortDirection


type alias Flags =
    { apiBase : String
    }
