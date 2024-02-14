module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.OpaqueType
import Review.Rule exposing (Rule)
import NoMissingTypeExpose


config : List Rule
config =
    [ Review.OpaqueType.forbid

    -- so that your exposed type aliases don't reference hidden types
    , NoMissingTypeExpose.rule
    ]
