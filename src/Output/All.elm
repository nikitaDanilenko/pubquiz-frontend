module Output.All exposing (..)

import Common.Constants exposing (allApi, quizIdParam)
import Common.Sorting as Sorting exposing (SortBy(..), SortType(..), Sorting, selectAndSort)
import Common.Types exposing (Labels, QuizInfo, TeamQuery)
import Common.Util as Util exposing (ErrorOr, getAllWith)
import Common.WireUtil exposing (linkButton)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, disabled, id, value)
import Html.Events exposing (onClick, onInput)
import Output.OutputUtil exposing (fragmentUrl, mkFullQuizName)


type alias Model =
    { teamQueryCandidate : Maybe TeamQuery
    , labelsCandidate : Maybe Labels
    , quizInfos : List QuizInfo
    , sorting : Sorting
    }


updateQuizInfos : Model -> List QuizInfo -> Model
updateQuizInfos model quizInfos =
    { model | quizInfos = quizInfos }


updateSorting : Model -> Sorting -> Model
updateSorting model sorting =
    { model | sorting = sorting }


type Msg
    = GotAllQuizzes (ErrorOr (List QuizInfo))
    | SetSortType SortType
    | SetSortBy SortBy
    | SetSearchText String


init : Maybe Labels -> Maybe TeamQuery -> ( Model, Cmd Msg )
init mLabels mTeamQuery =
    ( { teamQueryCandidate = mTeamQuery, labelsCandidate = mLabels, quizInfos = [], sorting = Sorting.default }, getAllQuizzes )


view : Model -> Html Msg
view model =
    let
        byName =
            SetSortBy Name

        byDate =
            SetSortBy Date

        ascending =
            SetSortType Ascending

        descending =
            SetSortType Descending

        mkSortingButton action disabledWhen symbol =
            button [ class "sortingButton", onClick action, disabled disabledWhen ] [ text (special symbol) ]
    in
    div [ id "allQuizzesVies" ]
        [ div [ id "sortingArea" ]
            [ div [ id "sortingType" ]
                [ mkSortingButton byName (model.sorting.sortBy == Name) 9872
                , mkSortingButton byDate (model.sorting.sortBy == Date) 128197
                ]
            , div [ id "sortingDirection" ]
                [ mkSortingButton ascending (model.sorting.sortType == Ascending) 8593
                , mkSortingButton descending (model.sorting.sortType == Descending) 8595
                ]
            , div [ id "searchField" ]
                [ label [] [ text (special 128269) ]
                , input [ onInput SetSearchText ] []
                ]
            ]
        , div [ id "allQuizzes" ]
            (List.map mkQuizInfoButton (selectAndSort model.sorting model.quizInfos))
        ]


special : Int -> String
special =
    Char.fromCode >> String.fromChar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllQuizzes quizInfosCandidate ->
            ( Util.foldResult model (updateQuizInfos model) quizInfosCandidate, Cmd.none )

        SetSortType sortType ->
            ( Sorting.updateSortType model.sorting sortType |> updateSorting model, Cmd.none )

        SetSortBy sortBy ->
            ( Sorting.updateSortBy model.sorting sortBy |> updateSorting model, Cmd.none )

        SetSearchText searchText ->
            ( Sorting.updateSearchText model.sorting searchText |> updateSorting model, Cmd.none )


mkQuizInfoButton : QuizInfo -> Html Msg
mkQuizInfoButton quizInfo =
    div []
        [ linkButton
            (fragmentUrl [ quizIdParam, String.fromInt quizInfo.quizId ])
            [ class "quizInfoButton", value (mkFullQuizName quizInfo.quizIdentifier) ]
            []
        ]


getAllQuizzes : Cmd Msg
getAllQuizzes =
    getAllWith allApi GotAllQuizzes
