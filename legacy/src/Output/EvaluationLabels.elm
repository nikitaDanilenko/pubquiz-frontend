module Output.EvaluationLabels exposing (..)


type alias EvaluationLabels =
    { min : String
    , max : String
    , mean : String
    , median : String
    }


default : EvaluationLabels
default =
    { min = "Min"
    , max = "Max"
    , mean = "Avg"
    , median = "Med"
    }
