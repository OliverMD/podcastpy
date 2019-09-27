module Colours exposing (colorA, colorB, colorC, colorD, colorE, colorToString)

import Element exposing (Color, rgba255, toRgb)


colorA =
    rgba255 0 0 0 1


colorB =
    rgba255 20 126 251 1


colorC =
    rgba255 204 204 204 1


colorD =
    rgba255 51 51 51 1


colorE =
    rgba255 102 102 102 1


colorToString : Color -> String
colorToString color =
    let
        c =
            toRgb color
    in
    "rgba("
        ++ String.fromFloat (c.red * 255)
        ++ ", "
        ++ String.fromFloat (c.green * 255)
        ++ ", "
        ++ String.fromFloat (c.blue * 255)
        ++ ", "
        ++ String.fromFloat c.alpha
        ++ ")"
