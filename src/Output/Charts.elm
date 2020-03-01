module Output.Charts exposing (..)

import Chartjs.Chart exposing (Chart, Type(..))
import Chartjs.Data exposing (DataSet(..))
import Chartjs.DataSets.Bar as Bar
import Chartjs.Options exposing (defaultOptions)
import Common.Types exposing (Header, Labels, QuizRatings, RoundNumber, TeamInfo, TeamName, TeamNumber, TeamRating)
import Common.Util as Util
import List.Extra exposing (transpose)

perRoundChart : QuizRatings -> Labels -> Chart
perRoundChart quizRatings labels =
  let sortedRatings = List.sortBy Tuple.first quizRatings.ratings
      sortedHeader = List.sortBy .teamInfoNumber quizRatings.header
      data = {
       labels = List.map (\(n, _) -> String.join " " [labels.roundLabel, String.fromInt n]) sortedRatings,
       datasets = mkPerRoundDataSets sortedHeader quizRatings
       }
  in {
       chartType = Bar,
       data = data,
       options = defaultOptions
       }

mkDataSet : TeamName -> List (RoundNumber, TeamRating) -> Bar.DataSet
mkDataSet tn xs =
  let base = Bar.defaultFromLabel tn
      data = List.map (\x -> x |> Tuple.second |> .rating) xs
  in { base | data = data}

mkPerRoundDataSets : Header -> QuizRatings -> List DataSet
mkPerRoundDataSets header quizRatings =
  let sorted = transpose (List.map (\(rn, rat) -> rat.points |> List.sortBy .teamNumber |> List.map (\x -> (rn, x))) quizRatings.ratings)
  in List.map2 (\ti x -> (ti.teamInfoName, x) |> Util.uncurry mkDataSet |> BarDataSet) header sorted