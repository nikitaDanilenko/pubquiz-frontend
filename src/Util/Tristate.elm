module Util.Tristate exposing
    ( Tristate(..)
    , failed
    , fold
    , initial
    , ready
    , withDefault
    )


type Tristate error value
    = Initial
    | Ready value
    | Failed error


initial : Tristate error value
initial =
    Initial


ready : value -> Tristate error value
ready =
    Ready


failed : error -> Tristate error value
failed =
    Failed


fold :
    { onInitial : a
    , onReady : value -> a
    , onFailed : error -> a
    }
    -> Tristate error value
    -> a
fold handlers tristate =
    case tristate of
        Initial ->
            handlers.onInitial

        Ready value ->
            handlers.onReady value

        Failed error ->
            handlers.onFailed error


withDefault : value -> Tristate error value -> value
withDefault default tristate =
    case tristate of
        Ready value ->
            value

        _ ->
            default
