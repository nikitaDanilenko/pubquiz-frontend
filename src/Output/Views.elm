module Output.Views exposing (..)

--import Chartjs.Chart exposing (chart)
--import Color.Convert
--import Common.Ranking exposing (RoundRankings, rankingToPlacement, ratingsToRankings, roundRankingsToRoundWinners)
--import Common.Types exposing (DbQuizId, Header, Labels, QuizInfo, QuizRatings, TeamLine, TeamQuery, TeamTable, TeamTableInfo)
--import Common.Util as Util
--import Html exposing (Html, button, div, h1, table, td, text, th, tr)
--import Html.Attributes exposing (class, id, style)
--import Html.Events exposing (onClick)
--import List.Extra exposing (maximumBy)
--import Output.Charts as Charts
--import Output.Colors exposing (mkColors)
--import Output.Model exposing (Model(..), Msg(..), mkFullQuizName)
--
--
--view : Model -> Html Msg
--view model =
--    case model of
--        TableModel labels _ teamTableInfo quizInfo ->
--            tableView teamTableInfo quizInfo labels
--
--        QuizModel labels mTeamQuery quizRatings quizInfo ->
--            quizView quizRatings
--                (Util.foldMaybe Impossible (\tq -> if tq.teamQueryQuizId == quizInfo.quizId then Possible else Impossible) mTeamQuery)
--                labels
--
--        AllModel _ quizInfos ->
--            allView quizInfos


x : Int
x = 5