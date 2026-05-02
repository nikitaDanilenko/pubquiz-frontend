module Util.Colors exposing (interpolateColor, statisticsColors, teamColors, toHex)

import Color exposing (Color, rgb255)
import Color.Convert
import Color.Interpolate as Interpolate


teamColors : Int -> List String
teamColors count =
    mkColors count |> List.map toHex


statisticsColors :
    { min : String
    , average : String
    , median : String
    , max : String
    }
statisticsColors =
    { min = "#dd4124" -- tangerine tango
    , average = "#009b77" -- emerald
    , median = "#efc050" -- mimosa
    , max = "#6b5b95" -- ultra violet
    }


toHex : Color -> String
toHex =
    Color.Convert.colorToHex


fixedPalette : List Color
fixedPalette =
    [ rgb255 255 99 132
    , rgb255 255 159 64
    , rgb255 255 205 86
    , rgb255 75 192 150
    , rgb255 54 162 235
    , rgb255 73 91 191
    , rgb255 153 102 255
    ]


lastColor : Color
lastColor =
    rgb255 153 102 255


numberOfFixedColors : Int
numberOfFixedColors =
    List.length fixedPalette


mkColors : Int -> List Color
mkColors total =
    List.range 0 (total - 1)
        |> List.map (colorForIndex total)


colorForIndex : Int -> Int -> Color
colorForIndex total index =
    if total <= numberOfFixedColors then
        colorAt index

    else
        let
            position =
                toFloat index * toFloat (numberOfFixedColors - 1) / toFloat (total - 1)

            lowerIndex =
                floor position

            upperIndex =
                min (lowerIndex + 1) (numberOfFixedColors - 1)

            fraction =
                position - toFloat lowerIndex

            lowerColor =
                colorAt lowerIndex

            upperColor =
                colorAt upperIndex
        in
        Interpolate.interpolate Interpolate.RGB lowerColor upperColor fraction


interpolateColor : Int -> Int -> Color
interpolateColor =
    colorForIndex


colorAt : Int -> Color
colorAt index =
    List.drop index fixedPalette |> List.head |> Maybe.withDefault lastColor
