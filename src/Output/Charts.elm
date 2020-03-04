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
import Common.Ranking exposing (RoundRanking, RoundRankings, roundRankingToPointsByTeam)
import Common.Types exposing (Header, Labels, QuizRatings, Ratings, RoundNumber, TeamInfo, TeamName, TeamNumber, TeamRating)
import Common.Util exposing (uncurry3)


perRoundChart : Header -> List Color -> RoundRankings -> List String -> String -> Chart
perRoundChart =
    mkChartWith mkPerRoundDataSets Bar


cumulativeChart : Header -> List Color -> RoundRankings -> List String -> String -> Chart
cumulativeChart =
    mkChartWith mkCumulativeDataSets Bar


progressionChart : Header -> List Color -> RoundRankings -> List String -> String -> Chart
progressionChart =
    mkChartWith mkProgressionDataSets Line


mkChartWith : (Header -> List Color -> RoundRankings -> List DataSet) -> Type -> Header -> List Color -> RoundRankings -> List String -> String -> Chart
mkChartWith mkDataSets chartType header colors rankings roundLabels chartTitle =
    { chartType = chartType
    , data = { labels = roundLabels, datasets = mkDataSets header colors rankings }
    , options = chartOptionsWithTitle chartTitle
    }


mkPerRoundBarDataSet : TeamName -> Color -> RoundRanking -> Bar.DataSet
mkPerRoundBarDataSet tn color roundRanking =
    let
        base =
            Bar.defaultFromLabel tn
    in
    { base
        | data = roundRankingToPointsByTeam roundRanking
        , backgroundColor = mkColorProperty color
        , borderColor = mkColorProperty color
    }


mkColorProperty : Color -> Maybe (PointProperty Color)
mkColorProperty =
    All >> Just


mkPerRoundLineDataSet : TeamName -> Color -> RoundRanking -> Line.DataSet
mkPerRoundLineDataSet tn color roundRanking =
    let
        base =
            Line.defaultFromLabel tn
    in
    { base
        | data = roundRankingToPointsByTeam roundRanking
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


mkPerRoundDataSets : Header -> List Color -> RoundRankings -> List DataSet
mkPerRoundDataSets =
    List.map3 (\ti c x -> ( ti.teamInfoName, c, x ) |> uncurry3 mkPerRoundBarDataSet |> BarDataSet)


mkCumulativeDataSets : Header -> List Color -> RoundRankings -> List DataSet
mkCumulativeDataSets =
    processCumulativeWith mkPerRoundBarDataSet BarDataSet


mkProgressionDataSets : Header -> List Color -> RoundRankings -> List DataSet
mkProgressionDataSets =
    processCumulativeWith mkPerRoundLineDataSet LineDataSet


processCumulativeWith : (TeamName -> Color -> RoundRanking -> ds) -> (ds -> DataSet) -> Header -> List Color -> RoundRankings -> List DataSet
processCumulativeWith mkSpecialDataSet mkDataSet =
    List.map3 (\ti c x -> ( ti.teamInfoName, c, x ) |> uncurry3 mkSpecialDataSet |> mkDataSet)
