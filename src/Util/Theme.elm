module Util.Theme exposing (Theme(..), labelColor)


type Theme
    = Light
    | Dark


labelColor : Theme -> String
labelColor theme =
    case theme of
        Light ->
            "#808BAB"

        Dark ->
            "rgb(150, 150, 150)"
