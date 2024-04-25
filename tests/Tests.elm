module Tests exposing (tests)

import Elm.Project
import Json.Decode
import Review.OpaqueType
import Review.Project
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test)


tests : Test
tests =
    Test.describe "elm-review-opaque-type"
        [ Test.test "does not report when the choice type exposes its variants"
            (\() ->
                """module A exposing (A(..))

type A
    = A
"""
                    |> Review.Test.run
                        Review.OpaqueType.forbid
                    |> Review.Test.expectNoErrors
            )
        , Test.test "does not report when the type alias uses types from dependencies"
            (\() ->
                """module A exposing (A)

type alias A =
    Sub Int
"""
                    |> Review.Test.run
                        Review.OpaqueType.forbid
                    |> Review.Test.expectNoErrors
            )
        , Test.test "reports direct opaque type"
            (\() ->
                """module A exposing (OpaqueType)

type OpaqueType
    = HiddenVariant
"""
                    |> Review.Test.run Review.OpaqueType.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "variants not exposed"
                            , details =
                                [ """This is often called "opaque type" because only the module this type is defined in can create and match on its variants."""
                                , """These "opaque" types don't give you the usual rewards of type-safety and allow a bit more room for mistakes."""
                                , """If you see no problem with exposing the variants, I suggest applying the automatic fix,
otherwise I suggest looking at these alternatives: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-opaque-type/latest#but-what-are-the-alternatives"""
                                ]
                            , under = "OpaqueType"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 6 }, end = { row = 3, column = 16 } }
                            |> Review.Test.whenFixed
                                """module A exposing (OpaqueType(..))

type OpaqueType
    = HiddenVariant
"""
                        ]
            )
        , Test.test "only reports direct opaque type in application"
            (\() ->
                [ """module A exposing (OpaqueType)
import B

type alias OpaqueType =
    B.OpaqueType
"""
                , """module B exposing (OpaqueType)

type OpaqueType
    = HiddenVariant
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData sampleApplicationProject Review.OpaqueType.forbid
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ Review.Test.error
                                { message = "variants not exposed"
                                , details =
                                    [ """This is often called "opaque type" because only the module this type is defined in can create and match on its variants."""
                                    , """These "opaque" types don't give you the usual rewards of type-safety and allow a bit more room for mistakes."""
                                    , """If you see no problem with exposing the variants, I suggest applying the automatic fix,
otherwise I suggest looking at these alternatives: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-opaque-type/latest#but-what-are-the-alternatives"""
                                    ]
                                , under = "OpaqueType"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 3, column = 6 }, end = { row = 3, column = 16 } }
                                |> Review.Test.whenFixed
                                    """module B exposing (OpaqueType(..))

type OpaqueType
    = HiddenVariant
"""
                            ]
                          )
                        ]
            )
        , Test.test "reports alias to opaque type in non-exposed module in package"
            (\() ->
                [ """module A exposing (OpaqueType)
import B

type alias OpaqueType =
    B.PackageOpaqueType
"""
                , """module B exposing (PackageOpaqueType(..))

type PackageOpaqueType
    = HiddenVariant
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData samplePackageProjectExposingModuleA Review.OpaqueType.forbid
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "full type definition not exposed"
                                , details =
                                    [ """Only the package knows how this type is defined. This way, it's hidden from the users how to construct values of this type (through variants etc.)."""
                                    , """These "opaque" types don't give you the usual rewards of type-safety and allow a bit more room for mistakes."""
                                    , """I suggest looking at these alternatives: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-opaque-type/latest#but-what-are-the-alternatives"""
                                    ]
                                , under = "B.PackageOpaqueType"
                                }
                            ]
                          )
                        ]
            )
        ]


elmJsonDataFromRawSource : String -> { path : String, raw : String, project : Elm.Project.Project }
elmJsonDataFromRawSource =
    \rawSource ->
        { path = "/"
        , raw = rawSource
        , project =
            case rawSource |> Json.Decode.decodeString Elm.Project.decoder of
                Ok ok ->
                    ok

                Err error ->
                    Debug.todo (error |> Json.Decode.errorToString)
        }


sampleApplicationProject : Review.Project.Project
sampleApplicationProject =
    Review.Test.Dependencies.projectWithElmCore
        |> Review.Project.addElmJson
            ("""{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5"
        },
        "indirect": {
            "elm/json": "1.1.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""
                |> elmJsonDataFromRawSource
            )


samplePackageProjectExposingModuleA : Review.Project.Project
samplePackageProjectExposingModuleA =
    Review.Test.Dependencies.projectWithElmCore
        |> Review.Project.addElmJson
            ("""{
    "type": "package",
    "name": "lue-bird/elm-review-opaque-type-test-package",
    "summary": "example",
    "license": "MIT",
    "version": "1.0.0",
    "exposed-modules": [
        "A"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.5 <= v < 2.0.0"
    },
    "test-dependencies": {
    }
}
"""
                |> elmJsonDataFromRawSource
            )
