module Common.FromInput exposing (..)

import Basics.Extra exposing (flip)


type alias FromInput a =
    { value : a
    , ifEmptyValue : a
    , text : String
    , parse : String -> Result String a
    , partial : String -> Bool
    }


updateText : FromInput a -> String -> FromInput a
updateText intFromInput text =
    { intFromInput | text = text }


updateValue : FromInput a -> a -> FromInput a
updateValue intFromInput value =
    { intFromInput | value = value }


emptyText :
    { ifEmptyValue : a
    , value : a
    , parse : String -> Result String a
    , isPartial : String -> Bool
    }
    -> FromInput a
emptyText params =
    { value = params.value
    , ifEmptyValue = params.ifEmptyValue
    , text = ""
    , parse = params.parse
    , partial = params.isPartial
    }


isValid : FromInput a -> Bool
isValid fromInput =
    case fromInput.parse fromInput.text of
        Ok value ->
            value == fromInput.value

        Err _ ->
            False


lift : (model -> FromInput a -> model) -> FromInput a -> String -> model -> model
lift ui fromInput text model =
    let
        possiblyValid =
            if String.isEmpty text || fromInput.partial text then
                fromInput
                    |> flip updateValue fromInput.ifEmptyValue
                    |> flip updateText text

            else
                fromInput
    in
    case fromInput.parse text of
        Ok value ->
            possiblyValid
                |> flip updateValue value
                |> ui model

        Err _ ->
            ui model possiblyValid
