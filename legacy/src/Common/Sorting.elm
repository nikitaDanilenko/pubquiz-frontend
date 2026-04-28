module Common.Sorting exposing (..)

import Common.Types exposing (QuizInfo)
import Output.OutputUtil as OutputUtil


type alias Sorting =
    { searchText : String
    , sortBy : SortBy
    , sortType : SortType
    }


updateSearchText : Sorting -> String -> Sorting
updateSearchText sorting searchText =
    { sorting | searchText = searchText }


updateSortBy : Sorting -> SortBy -> Sorting
updateSortBy sorting sortBy =
    { sorting | sortBy = sortBy }


updateSortType : Sorting -> SortType -> Sorting
updateSortType sorting sortType =
    { sorting | sortType = sortType }


default : Sorting
default =
    { searchText = ""
    , sortBy = Date
    , sortType = Descending
    }


type SortBy
    = Name
    | Date


type SortType
    = Ascending
    | Descending


selectAndSort : Sorting -> List QuizInfo -> List QuizInfo
selectAndSort sorting =
    let
        sorter =
            case sorting.sortBy of
                Name ->
                    List.sortBy (.quizIdentifier >> .name)

                Date ->
                    List.sortBy (.quizIdentifier >> .date)

        orderer =
            case sorting.sortType of
                Ascending ->
                    identity

                Descending ->
                    List.reverse

        selector =
            List.filter
                (.quizIdentifier
                    >> OutputUtil.mkFullQuizName
                    >> String.toLower
                    >> String.contains (String.toLower sorting.searchText)
                )
    in
    selector >> sorter >> orderer
