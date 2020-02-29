module Output.Views exposing (..)

import Html exposing (Html, div)
import Output.Model exposing (Model, Msg, State(..))


view : Model -> Html Msg
view model = case model.state of
  Table -> tableView model
  Quiz -> quizView model
  All -> allView model

tableView : Model -> Html Msg
tableView model =
    div [] []

quizView : Model -> Html Msg
quizView model = div [] []

allView : Model -> Html Msg
allView model = div [] []