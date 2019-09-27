module Utils exposing (floatFlip, microTickLenMs, scaled, urlRoot)

import Element exposing (Color, modular, toRgb)


floatFlip : (Float -> Float -> Float) -> Float -> (Float -> Float)
floatFlip function den =
    \x -> function x den


scaled : Int -> Int
scaled =
    round << modular 8 1.25


urlRoot =
    ""


microTickLenMs =
    500
