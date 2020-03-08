module Output.Charts exposing (cumulativeChart, perRoundChart, progressionChart)

import Basics.Extra exposing (uncurry)
import Chartjs.Chart exposing (Chart, Type(..))
import Chartjs.Common exposing (PointProperty(..))
import Chartjs.Data exposing (DataSet(..))
import Chartjs.DataSets.Bar as Bar
import Chartjs.DataSets.Line as Line
import Chartjs.Options exposing (defaultOptions)
import Chartjs.Options.Elements exposing (LineFill(..), defaultElements)
import Chartjs.Options.Legend exposing (defaultLabels, defaultLegend)
import Chartjs.Options.Scales exposing (Axis, defaultAxis, defaultScales, defaultTicks)
import Chartjs.Options.Title exposing (defaultTitle)
import Color exposing (Color)
import Common.Ranking exposing (RoundRankingPerTeam, RoundRankings, roundRankingPerTeamToPointsPerTeam)


perRoundChart : List Color -> RoundRankings -> List String -> String -> Chart
perRoundChart =
    mkChartWith mkPerRoundDataSets Bar


cumulativeChart : List Color -> RoundRankings -> List String -> String -> Chart
cumulativeChart =
    mkChartWith mkCumulativeDataSets Bar


progressionChart : List Color -> RoundRankings -> List String -> String -> Chart
progressionChart =
    mkChartWith mkProgressionDataSets Line


mkChartWith : (List Color -> RoundRankings -> List DataSet) -> Type -> List Color -> RoundRankings -> List String -> String -> Chart
mkChartWith mkDataSets chartType colors rankings roundLabels chartTitle =
    { chartType = chartType
    , data = { labels = roundLabels, datasets = mkDataSets colors rankings }
    , options = chartOptionsWithTitle chartTitle
    }


mkPerRoundBarDataSet : Color -> RoundRankingPerTeam -> Bar.DataSet
mkPerRoundBarDataSet color roundRanking =
    let
        base =
            Bar.defaultFromLabel roundRanking.teamName
    in
    { base
        | data = roundRankingPerTeamToPointsPerTeam roundRanking
        , backgroundColor = mkColorProperty color
        , borderColor = mkColorProperty color
    }


mkColorProperty : Color -> Maybe (PointProperty Color)
mkColorProperty =
    All >> Just


mkPerRoundLineDataSet : Color -> RoundRankingPerTeam -> Line.DataSet
mkPerRoundLineDataSet color roundRanking =
    let
        base =
            Line.defaultFromLabel roundRanking.teamName
    in
    { base
        | data = roundRankingPerTeamToPointsPerTeam roundRanking
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
        , title = Just { defaultTitle | text = Just title, display = Just True, fontSize = Just titleFontSize }
        , elements = Just { defaultElements | line = Just { defaultLine | tension = Just 0, fill = Just NoFill }}
        , legend = Just { defaultLegend | labels = Just { defaultLabels | fontSize = Just legendFontSize}}
        }

titleFontSize : Int
titleFontSize = 30

legendFontSize : Int
legendFontSize = 24

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


mkPerRoundDataSets : List Color -> RoundRankings -> List DataSet
mkPerRoundDataSets =
    List.map2 (\c x -> (c, x) |> uncurry mkPerRoundBarDataSet |> BarDataSet)


mkCumulativeDataSets : List Color -> RoundRankings -> List DataSet
mkCumulativeDataSets =
    processCumulativeWith mkPerRoundBarDataSet BarDataSet


mkProgressionDataSets : List Color -> RoundRankings -> List DataSet
mkProgressionDataSets =
    processCumulativeWith mkPerRoundLineDataSet LineDataSet


processCumulativeWith : (Color -> RoundRankingPerTeam -> ds) -> (ds -> DataSet) -> List Color -> RoundRankings -> List DataSet
processCumulativeWith mkSpecialDataSet mkDataSet =
    List.map2 (\c x -> (c, x ) |> uncurry mkSpecialDataSet |> mkDataSet)
