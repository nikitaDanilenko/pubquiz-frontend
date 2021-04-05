module Output.Charts exposing (cumulativeChart, perRoundChart, progressionChart, roundEvaluationChart)

import Basics.Extra exposing (uncurry)
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
import Common.Ranking exposing (RoundRankingPerTeam, RoundRankings, roundRankingPerTeamToPointsPerTeam)
import Common.Types exposing (QuizRatings, RoundRating)
import Stat


perRoundChart : List Color -> RoundRankings -> List String -> String -> Chart
perRoundChart =
    mkChartWith mkPerRoundDataSets Bar


cumulativeChart : List Color -> RoundRankings -> List String -> String -> Chart
cumulativeChart =
    mkChartWith mkCumulativeDataSets Bar


progressionChart : List Color -> RoundRankings -> List String -> String -> Chart
progressionChart =
    mkChartWith mkProgressionDataSets Line


roundEvaluationChart : List Color -> QuizRatings -> List String -> String -> Chart
roundEvaluationChart cs qrs roundLabels t =
    { chartType = Bar
    , data = { labels = roundLabels, datasets = mkEvaluationDataSets cs qrs }
    , options = chartOptionsWithTitle t
    }

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


mkPerRoundDataSets : List Color -> RoundRankings -> List DataSet
mkPerRoundDataSets =
    List.map2 (\c x -> ( c, x ) |> uncurry mkPerRoundBarDataSet |> BarDataSet)


mkCumulativeDataSets : List Color -> RoundRankings -> List DataSet
mkCumulativeDataSets =
    processCumulativeWith mkPerRoundBarDataSet BarDataSet


mkProgressionDataSets : List Color -> RoundRankings -> List DataSet
mkProgressionDataSets =
    processCumulativeWith mkPerRoundLineDataSet LineDataSet


processCumulativeWith : (Color -> RoundRankingPerTeam -> ds) -> (ds -> DataSet) -> List Color -> RoundRankings -> List DataSet
processCumulativeWith mkSpecialDataSet mkDataSet =
    List.map2 (\c x -> ( c, x ) |> uncurry mkSpecialDataSet |> mkDataSet)


type alias RoundEvaluation =
    { min : Float
    , max : Float
    , mean : Float
    , median : Float
    }


mkRoundEvaluation : RoundRating -> RoundEvaluation
mkRoundEvaluation r =
    let
        ps =
            List.map .rating r.points

        orZero =
            Maybe.withDefault 0
    in
    { min = orZero (List.minimum ps)
    , max = orZero (List.maximum ps)
    , mean = orZero (Stat.mean ps)
    , median = orZero (Stat.median ps)
    }


mkEvaluationDataSet : Color -> String -> (RoundEvaluation -> Float) -> List RoundEvaluation -> Bar.DataSet
mkEvaluationDataSet color desc selector evs =
    let
        base =
            Bar.defaultFromLabel desc
    in
    { base
        | data = List.map selector evs
        , backgroundColor = mkColorProperty color
        , borderColor = mkColorProperty color
    }


mkEvaluationDataSets : List Color -> QuizRatings -> List DataSet
mkEvaluationDataSets cs qrs =
    let
        roundEvaluations =
            List.map (Tuple.second >> mkRoundEvaluation) qrs.ratings
    in
      List.map2 (\c (n, f) -> mkEvaluationDataSet c n f roundEvaluations |> BarDataSet)
                cs
                [("Min", .min), ("Max", .max), ("Ø", .mean), ("Med", .median)]
