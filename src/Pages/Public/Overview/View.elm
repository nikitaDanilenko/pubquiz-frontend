module Pages.Public.Overview.View exposing (view)

import Api.Types exposing (QuizIdentifier, QuizSummary)
import Date
import Html exposing (Html, a, button, h1, input, li, nav, p, section, text, ul)
import Html.Attributes exposing (class, href, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Pages.Public.Overview.Page as Page
import Util.Tristate as Tristate


view : Page.Model -> Html Page.Msg
view model =
    Tristate.fold
        { onInitial = viewLoading
        , onReady = viewOverview model
        , onFailed = viewError
        }
        model.quizzes


viewLoading : Html msg
viewLoading =
    section [ class "loading" ]
        [ p [] [ text "Loading quizzes..." ]
        ]


viewError : a -> Html msg
viewError _ =
    section [ class "error" ]
        [ h1 [] [ text "Error" ]
        , p [] [ text "Failed to load quizzes." ]
        ]


viewOverview : Page.Model -> List QuizSummary -> Html Page.Msg
viewOverview model quizzes =
    let
        filteredQuizzes =
            quizzes
                |> filterBySearch model.searchText
                |> sortQuizzes model.sortBy model.sortDirection
    in
    section [ class "overview" ]
        [ h1 [] [ text "Quizzes" ]
        , viewControls model
        , viewQuizList filteredQuizzes
        ]


viewControls : Page.Model -> Html Page.Msg
viewControls model =
    nav [ class "overview-controls" ]
        [ viewSortButtons model
        , viewSearchInput model.searchText
        ]


viewSortButtons : Page.Model -> Html Page.Msg
viewSortButtons model =
    let
        sortByButton sortBy label =
            button
                [ class "sort-button"
                , class
                    (if model.sortBy == sortBy then
                        "active"

                     else
                        ""
                    )
                , onClick (Page.SetSortBy sortBy)
                ]
                [ text label ]

        directionButton direction label =
            button
                [ class "sort-button"
                , class
                    (if model.sortDirection == direction then
                        "active"

                     else
                        ""
                    )
                , onClick (Page.SetSortDirection direction)
                ]
                [ text label ]
    in
    nav [ class "sort-controls" ]
        [ sortByButton Page.ByName "Name"
        , sortByButton Page.ByDate "Date"
        , directionButton Page.Ascending "↑"
        , directionButton Page.Descending "↓"
        ]


viewSearchInput : String -> Html Page.Msg
viewSearchInput searchText =
    input
        [ type_ "search"
        , class "search-input"
        , placeholder "Search..."
        , value searchText
        , onInput Page.SetSearchText
        ]
        []


viewQuizList : List QuizSummary -> Html msg
viewQuizList quizzes =
    if List.isEmpty quizzes then
        p [ class "no-results" ] [ text "No quizzes found." ]

    else
        ul [ class "quiz-list" ]
            (List.map viewQuizItem quizzes)


viewQuizItem : QuizSummary -> Html msg
viewQuizItem quiz =
    li [ class "quiz-item" ]
        [ a [ href (String.concat [ "/quizzes/", String.fromInt quiz.quizId ]) ]
            [ text (formatQuizName quiz.identifier) ]
        ]


formatQuizName : QuizIdentifier -> String
formatQuizName identifier =
    let
        dateStr =
            Date.format "d MMM y" identifier.date

        parts =
            List.concat
                [ [ dateStr ]
                , [ identifier.name ]
                , if String.isEmpty identifier.place then
                    []

                  else
                    [ String.concat [ "(", identifier.place, ")" ] ]
                ]
    in
    String.join " " parts


filterBySearch : String -> List QuizSummary -> List QuizSummary
filterBySearch searchText quizzes =
    if String.isEmpty searchText then
        quizzes

    else
        let
            needle =
                String.toLower searchText

            matches quiz =
                String.toLower (formatQuizName quiz.identifier)
                    |> String.contains needle
        in
        List.filter matches quizzes


sortQuizzes : Page.SortBy -> Page.SortDirection -> List QuizSummary -> List QuizSummary
sortQuizzes sortBy direction quizzes =
    let
        sorted =
            case sortBy of
                Page.ByName ->
                    List.sortBy (.identifier >> .name >> String.toLower) quizzes

                Page.ByDate ->
                    List.sortBy (.identifier >> .date >> Date.toRataDie) quizzes

        ordered =
            case direction of
                Page.Ascending ->
                    sorted

                Page.Descending ->
                    List.reverse sorted
    in
    ordered
