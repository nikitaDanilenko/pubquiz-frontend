module Output.Charts exposing (..)

import Basics.Extra exposing (uncurry)
import Chartjs.Chart exposing (Chart, Type(..))
import Chartjs.Data exposing (DataSet(..))
import Chartjs.DataSets.Bar as Bar
import Chartjs.Options exposing (defaultOptions)
import Chartjs.Options.Scales exposing (Axis, defaultAxis, defaultScales, defaultTicks)
import Chartjs.Options.Title exposing (defaultTitle)
import Common.Types exposing (Header, Labels, QuizRatings, Ratings, RoundNumber, TeamInfo, TeamName, TeamNumber, TeamRating)
import List.Extra exposing (scanl)


perRoundChart : Header -> List (List ( RoundNumber, TeamRating )) -> List String -> Labels -> Chart
perRoundChart sortedHeader arranged roundLabels labels =
    let
        data =
            { labels = roundLabels
            , datasets = mkPerRoundDataSets sortedHeader arranged
            }
    in
    { chartType = Bar
    , data = data
    , options = chartOptionsWithTitle labels.individualRoundsLabel
    }


cumulativeChart : Header -> List (List ( RoundNumber, TeamRating )) -> List String -> Labels -> Chart
cumulativeChart sortedHeader arranged roundLabels labels =
    let
        data =
            { labels = roundLabels
            , datasets = mkCumulativeDataSets sortedHeader arranged
            }
    in
    { chartType = Bar
    , data = data
    , options = chartOptionsWithTitle labels.cumulativeLabel
    }


mkPerRoundDataSet : TeamName -> List Float -> Bar.DataSet
mkPerRoundDataSet tn xs =
    let
        base =
            Bar.defaultFromLabel tn
    in
    { base | data = xs }


chartYAxis : Axis
chartYAxis =
    { defaultAxis
        | stacked = Just False
        , ticks = Just { defaultTicks | beginAtZero = Just True }
    }


chartScales : Chartjs.Options.Scales.Scales
chartScales =
    { defaultScales | yAxes = [ chartYAxis ] }


chartOptionsWithTitle : String -> Chartjs.Options.Options
chartOptionsWithTitle title =
    { defaultOptions | scales = Just chartScales, title = Just { defaultTitle | text = Just title, display = Just True } }


mkPerRoundDataSets : Header -> List (List ( RoundNumber, TeamRating )) -> List DataSet
mkPerRoundDataSets header rearranged =
    List.map2 (\ti x -> ( ti.teamInfoName, List.map (\a -> a |> Tuple.second |> .rating) x ) |> uncurry mkPerRoundDataSet |> BarDataSet) header rearranged


mkCumulativeDataSets : Header -> List (List ( RoundNumber, TeamRating )) -> List DataSet
mkCumulativeDataSets header rearranged =
    let
        cumulative =
            List.map (\xs -> xs |> scanl (\( _, nextTr ) current -> current + nextTr.rating) 0 |> List.drop 1) rearranged
    in
    List.map2 (\ti x -> ( ti.teamInfoName, x ) |> uncurry mkPerRoundDataSet |> BarDataSet) header cumulative
