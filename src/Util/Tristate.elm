module Util.Tristate exposing
    ( Tristate(..)
    , failed
    , fold
    , initial
    , isFailed
    , isInitial
    , isReady
    , mapReady
    , ready
    , toMaybe
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


mapReady : (a -> b) -> Tristate error a -> Tristate error b
mapReady f tristate =
    case tristate of
        Initial ->
            Initial

        Ready value ->
            Ready (f value)

        Failed error ->
            Failed error


withDefault : value -> Tristate error value -> value
withDefault default tristate =
    case tristate of
        Ready value ->
            value

        _ ->
            default


isInitial : Tristate error value -> Bool
isInitial tristate =
    case tristate of
        Initial ->
            True

        _ ->
            False


isReady : Tristate error value -> Bool
isReady tristate =
    case tristate of
        Ready _ ->
            True

        _ ->
            False


isFailed : Tristate error value -> Bool
isFailed tristate =
    case tristate of
        Failed _ ->
            True

        _ ->
            False


toMaybe : Tristate error value -> Maybe value
toMaybe tristate =
    case tristate of
        Ready value ->
            Just value

        _ ->
            Nothing
