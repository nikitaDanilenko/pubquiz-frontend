module Output.Views exposing (..)

import Common.Types exposing (Labels, TeamTable)
import Html exposing (Html, div)
import Html.Attributes exposing (attribute)
import Output.Model exposing (Model(..), Msg)


view : Model -> Html Msg
view model =
    case model of
        TableModel teamTable labels _ ->
            tableView teamTable

        QuizModel quizRatings labels _ ->
            quizView model

        AllModel quizInfos _ ->
            allView model


encoding : Html Msg
encoding = attribute "meta charset" "\"UTF-8\""

tableView : TeamTable -> Labels -> Html Msg
tableView table =

    div [] []


quizView : Model -> Html Msg
quizView model =
    div [] []


allView : Model -> Html Msg
allView model =
    div [] []
