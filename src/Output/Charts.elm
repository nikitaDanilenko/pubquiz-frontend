module Output.Charts exposing (cumulativeChart, perRoundChart, progressionChart)

import Chartjs.Chart exposing (Chart, Type(..))
import Chartjs.Common exposing (PointProperty(..))
import Chartjs.Data exposing (DataSet(..))
import Chartjs.DataSets.Bar as Bar
import Chartjs.DataSets.Line as Line
import Chartjs.Options exposing (defaultOptions)
import Chartjs.Options.Elements exposing (LineFill(..), defaultElements)
import Chartjs.Options.Scales exposing (Axis, defaultAxis, defaultScales, defaultTicks)
import Chartjs.Options.Title exposing (defaultTitle)
import Color exposing (Color)
import Common.Types exposing (Header, Labels, QuizRatings, Ratings, RoundNumber, TeamInfo, TeamName, TeamNumber, TeamRating)
import Common.Util exposing (uncurry3)


perRoundChart : Header -> List Color -> List (List Float) -> List String -> String -> Chart
perRoundChart =
    mkChartWith mkPerRoundDataSets Bar


cumulativeChart : Header -> List Color -> List (List Float) -> List String -> String -> Chart
cumulativeChart =
    mkChartWith mkCumulativeDataSets Bar


progressionChart : Header -> List Color -> List (List Float) -> List String -> String -> Chart
progressionChart =
    mkChartWith mkProgressionDataSets Line


mkChartWith : (Header -> List Color -> List (List Float) -> List DataSet) -> Type -> Header -> List Color -> List (List Float) -> List String -> String -> Chart
mkChartWith mkDataSets chartType header colors ratings roundLabels chartTitle =
    { chartType = chartType
    , data = { labels = roundLabels, datasets = mkDataSets header colors ratings }
    , options = chartOptionsWithTitle chartTitle
    }


mkPerRoundBarDataSet : TeamName -> Color -> List Float -> Bar.DataSet
mkPerRoundBarDataSet tn color xs =
    let
        base =
            Bar.defaultFromLabel tn
    in
    { base
        | data = xs
        , backgroundColor = mkColorProperty color
        , borderColor = mkColorProperty color
    }


mkColorProperty : Color -> Maybe (PointProperty Color)
mkColorProperty =
    All >> Just


mkPerRoundLineDataSet : TeamName -> Color -> List Float -> Line.DataSet
mkPerRoundLineDataSet tn color xs =
    let
        base =
            Line.defaultFromLabel tn
    in
    { base
        | data = xs
        , backgroundColor = mkColorProperty color
        , borderColor = mkColorProperty color
    }


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
    { defaultOptions
        | scales = Just chartScales
        , title = Just { defaultTitle | text = Just title, display = Just True }
        , elements = Just { defaultElements | line = Just { defaultLine | tension = Just 0, fill = Just NoFill } }
    }


defaultLine : Chartjs.Options.Elements.Line
defaultLine =
    { tension = Nothing
    , backgroundColor = Nothing
    , borderWidth = Nothing
    , borderColor = Nothing
    , borderCapStyle = Nothing
    , borderDash = Nothing
    , borderDashOffset = Nothing
    , borderJoinStyle = Nothing
    , capBezierPoints = Nothing
    , fill = Nothing
    , stepped = Nothing
    }


mkPerRoundDataSets : Header -> List Color -> List (List Float) -> List DataSet
mkPerRoundDataSets =
    List.map3 (\ti c x -> ( ti.teamInfoName, c, x ) |> uncurry3 mkPerRoundBarDataSet |> BarDataSet)


mkCumulativeDataSets : Header -> List Color -> List (List Float) -> List DataSet
mkCumulativeDataSets =
    processCumulativeWith mkPerRoundBarDataSet BarDataSet


mkProgressionDataSets : Header -> List Color -> List (List Float) -> List DataSet
mkProgressionDataSets =
    processCumulativeWith mkPerRoundLineDataSet LineDataSet


processCumulativeWith : (TeamName -> Color -> List Float -> ds) -> (ds -> DataSet) -> Header -> List Color -> List (List Float) -> List DataSet
processCumulativeWith mkSpecialDataSet mkDataSet =
    List.map3 (\ti c x -> ( ti.teamInfoName, c, x ) |> uncurry3 mkSpecialDataSet |> mkDataSet)
