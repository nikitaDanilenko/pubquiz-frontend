module Pages.Public.Overview.Page exposing
    ( Model
    , Msg(..)
    , SortBy(..)
    , SortDirection(..)
    , lenses
    )

{-| Quiz overview page state.
-}

import Api.Types exposing (QuizSummary)
import Monocle.Lens exposing (Lens)
import OpenApi.Common
import Util.Tristate exposing (Tristate)


type alias Model =
    { quizzes : Tristate (OpenApi.Common.Error () String) (List QuizSummary)
    , searchText : String
    , sortBy : SortBy
    , sortDirection : SortDirection
    }


lenses :
    { quizzes : Lens Model (Tristate (OpenApi.Common.Error () String) (List QuizSummary))
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
    = GotQuizzes (Result (OpenApi.Common.Error () String) (List QuizSummary))
    | SetSearchText String
    | SetSortBy SortBy
    | SetSortDirection SortDirection
