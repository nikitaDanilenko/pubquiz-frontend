module Common.FromInput exposing (..)

import Basics.Extra exposing (flip)


type alias FromInput a =
    { value : a
    , ifEmptyValue : a
    , text : String
    , parse : String -> Result String a
    , partial : String -> Bool
    , check : a -> Bool
    }


updateText : FromInput a -> String -> FromInput a
updateText fromInput text =
    { fromInput | text = text }


updateValue : FromInput a -> a -> FromInput a
updateValue fromInput value =
    { fromInput | value = value }


updateCheck : FromInput a -> (a -> Bool) -> FromInput a
updateCheck fromInput check =
    { fromInput | check = check }


emptyText :
    { ifEmptyValue : a
    , value : a
    , parse : String -> Result String a
    , isPartial : String -> Bool
    , check : a -> Bool
    }
    -> FromInput a
emptyText params =
    { value = params.value
    , ifEmptyValue = params.ifEmptyValue
    , text = ""
    , parse = params.parse
    , partial = params.isPartial
    , check = params.check
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
        invalidCase = ui model possiblyValid
    in
    case fromInput.parse text of
        Ok value ->
            if fromInput.check value then
                possiblyValid
                    |> flip updateValue value
                    |> ui model

            else
                model

        Err _ ->
            invalidCase
