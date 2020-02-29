module Output.Model exposing (..)

import Common.Types exposing (Code, DbQuizId, TeamNumber, TeamQuery)


type alias Model =
    { quizId : DbQuizId
    , teamAndCode : Maybe ( TeamNumber, Code )
    , state : State
    }


type Msg
    = Any


type State
    = Table
    | Quiz
    | All


initialModelFunction : () -> ( Model, Cmd Msg )
initialModelFunction _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { quizId = -1
    , teamAndCode = Nothing
    , state = All
    }
