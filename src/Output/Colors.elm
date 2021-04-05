module Output.Colors exposing (..)

import Color exposing (Color, rgb255)
import Color.Interpolate as Interpolate exposing (Space(..))
import List.Extra


fixedPalette : List Color
fixedPalette =
    List.map (\( x, y, z ) -> rgb255 x y z)
        [ ( 255, 99, 132 )
        , ( 255, 159, 64 )
        , ( 255, 205, 86 )
        , ( 75, 192, 150 )
        , ( 54, 162, 235 )
        , ( 73, 91, 191 )
        , ( 153, 102, 255 )
        ]


lastColor : Color
lastColor =
    Maybe.withDefault (rgb255 153 102 255) (List.Extra.last fixedPalette)


type alias Indexed a =
    { lower : Int
    , upper : Int
    , value : a
    }


indexedPalette : List (Indexed ( Color, Color ))
indexedPalette =
    List.indexedMap (\i cp -> { lower = i, upper = 1 + i, value = cp })
        (List.map2 Tuple.pair fixedPalette (List.drop 1 fixedPalette))


numberOfFixedColors : Int
numberOfFixedColors =
    List.length fixedPalette


type alias Rational =
    { numerator : Int
    , denominator : Int
    }


rationalToFloat : Rational -> Float
rationalToFloat r =
    toFloat r.numerator / toFloat r.denominator


position : Int -> Int -> Rational
position total i =
    { numerator = i * (numberOfFixedColors - 1), denominator = total - 1 }


lowerUpper : Rational -> ( Int, Int )
lowerUpper rat =
    ( rat.numerator // rat.denominator, rat.numerator // rat.denominator + 1 )


mkIndexed : Rational -> Indexed Rational
mkIndexed r =
    let
        ( lower, upper ) =
            lowerUpper r
    in
    { lower = lower, upper = upper, value = r }


mergeAndInterpolate : List (Indexed Rational) -> List (Indexed ( Color, Color )) -> List Color
mergeAndInterpolate irs ics =
    case ( irs, ics ) of
        ( [], _ ) ->
            []

        ( rest, [] ) ->
            List.repeat (List.length rest) lastColor

        ( ir :: irsTail, icp :: icsTail ) ->
            if ir.lower == icp.lower && ir.upper == icp.upper then
                Interpolate.interpolate RGB (Tuple.first icp.value) (Tuple.second icp.value) (rationalToFloat ir.value - toFloat ir.lower) :: mergeAndInterpolate irsTail ics

            else
                mergeAndInterpolate irs icsTail


mkColors : Int -> List Color
mkColors total =
    if total <= numberOfFixedColors then
        List.take total fixedPalette

    else
        mergeAndInterpolate (List.map (position total >> mkIndexed) (List.range 0 (total - 1))) indexedPalette


evaluationColors : List Color
evaluationColors =
    [ -- tangerine tango
      rgb255 221 65 36
    , -- emerald
      rgb255 0 155 119
    , -- mimosa
      rgb255 239 192 80
    , -- ultra violet
      rgb255 107 91 149
    ]
