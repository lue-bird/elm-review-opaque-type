module Review.OpaqueType exposing (forbid)

{-| Reports choice `type` parameters that aren't used in the definition (often called "opaque types").

    import NoUnused.CustomTypeConstructorArgs
    import NoUnused.CustomTypeConstructors
    import Review.OpaqueType

    config =
        [ Review.OpaqueType.forbid

        -- to catch variables in unused parts of the type
        , NoUnused.CustomTypeConstructors.rule []
        , NoUnused.CustomTypeConstructorArgs.rule
        ]

  - 🧩 [`NoUnused.CustomTypeConstructorArgs`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-unused/latest/NoUnused-CustomTypeConstructorArgs)
  - 🧩 [`NoUnused.CustomTypeConstructors`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-unused/latest/NoUnused-CustomTypeConstructors)

@docs forbid

-}

import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation
import FastDict exposing (Dict)
import Review.Fix
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule exposing (Rule)
import Set exposing (Set)


type alias ProjectContext =
    { exposedModules : ExposedModules }


{-|

  - `AllModulesExposed` means: The reviewed project is an application and therefore every module is considered "exposed"
  - `SomeModulesExposed` means: The reviewed project is an application and therefore the elm.json has an explicit list of exposed modules

-}
type ExposedModules
    = AllModulesExposed
    | SomeModulesExposed (Set Elm.Syntax.ModuleName.ModuleName)


type alias ModuleContext =
    { moduleNameLookup : ModuleNameLookupTable
    , typesExposedWithoutVariants : Dict String Range
    , exposedModules : ExposedModules
    }


{-| [`Rule`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-Rule#Rule) to report opaque types.

More on the why and the alternatives in the [readme](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-opaque-type/latest/)

-}
forbid : Rule
forbid =
    Review.Rule.newProjectRuleSchema "Review.OpaqueType.forbid" initialProjectContext
        |> Review.Rule.providesFixesForProjectRule
        |> Review.Rule.withElmJsonProjectVisitor
            (\maybeElmJson projectContext ->
                ( []
                , case maybeElmJson of
                    Nothing ->
                        { projectContext | exposedModules = AllModulesExposed }

                    Just elmJson ->
                        case elmJson.project of
                            Elm.Project.Application _ ->
                                { projectContext | exposedModules = AllModulesExposed }

                            Elm.Project.Package package ->
                                let
                                    names : List Elm.Module.Name
                                    names =
                                        case package.exposed of
                                            Elm.Project.ExposedList namesList ->
                                                namesList

                                            Elm.Project.ExposedDict headersAndNames ->
                                                headersAndNames
                                                    |> List.concatMap (\( _, nameList ) -> nameList)
                                in
                                { projectContext
                                    | exposedModules =
                                        SomeModulesExposed
                                            (names
                                                |> List.map (\name -> name |> Elm.Module.toString |> String.split ".")
                                                |> Set.fromList
                                            )
                                }
                )
            )
        |> Review.Rule.withModuleVisitor
            (\moduleRuleSchema ->
                moduleRuleSchema
                    |> Review.Rule.withModuleDefinitionVisitor
                        (\(Node _ moduleHeader) context ->
                            ( []
                            , { context
                                | typesExposedWithoutVariants = moduleHeader |> moduleHeaderTypesExposedWithoutVariants
                              }
                            )
                        )
                    |> Review.Rule.withDeclarationEnterVisitor
                        (\(Node _ declaration) context -> ( declarationVisitor declaration context, context ))
            )
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = projectToModuleContextCreator
            , fromModuleToProject = moduleToProjectContextCreator
            , foldProjectContexts = projectContextsMerge
            }
        |> Review.Rule.fromProjectRuleSchema


initialProjectContext : ProjectContext
initialProjectContext =
    { exposedModules = AllModulesExposed -- dummy
    }


projectToModuleContextCreator : Review.Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContextCreator =
    Review.Rule.initContextCreator
        (\moduleNameLookup projectContext ->
            { moduleNameLookup = moduleNameLookup
            , typesExposedWithoutVariants = FastDict.empty
            , exposedModules = projectContext.exposedModules
            }
        )
        |> Review.Rule.withModuleNameLookupTable


projectContextsMerge : ProjectContext -> ProjectContext -> ProjectContext
projectContextsMerge _ previous =
    { exposedModules = previous.exposedModules }


moduleToProjectContextCreator : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContextCreator =
    Review.Rule.initContextCreator
        (\moduleContext ->
            { exposedModules = moduleContext.exposedModules }
        )


declarationVisitor : Declaration -> ModuleContext -> List (Review.Rule.Error {})
declarationVisitor declaration context =
    case declaration of
        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
            case typeAliasDeclaration.typeAnnotation of
                Node _ (Elm.Syntax.TypeAnnotation.Typed (Node nameRange _) _) ->
                    case Review.ModuleNameLookupTable.moduleNameAt context.moduleNameLookup nameRange of
                        Nothing ->
                            []

                        -- defined in the same module
                        Just [] ->
                            []

                        -- defined in separate module
                        Just (moduleNamePart0 :: moduleNamePart1Up) ->
                            let
                                isFromExposedModule : Bool
                                isFromExposedModule =
                                    case context.exposedModules of
                                        AllModulesExposed ->
                                            True

                                        SomeModulesExposed exposedModuleSet ->
                                            exposedModuleSet |> Set.member (moduleNamePart0 :: moduleNamePart1Up)
                            in
                            if isFromExposedModule then
                                []

                            else
                                [ Review.Rule.error
                                    { message = "full type definition not exposed"
                                    , details =
                                        [ """Only the package knows how this type is defined. This way, it's hidden from the users how to construct values of this type (through variants etc.)."""
                                        , """These "opaque" types don't give you the usual rewards of type-safety and allow a bit more room for mistakes."""
                                        , """I suggest looking at these alternatives: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-opaque-type/latest#but-what-are-the-alternatives"""
                                        ]
                                    }
                                    nameRange
                                ]

                _ ->
                    []

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            let
                (Node choiceTypeNameRange choiceTypeName) =
                    choiceTypeDeclaration.name
            in
            case context.typesExposedWithoutVariants |> FastDict.get choiceTypeName of
                Just exposeRange ->
                    [ Review.Rule.errorWithFix
                        { message = "variants not exposed"
                        , details =
                            [ """This is often called "opaque type" because only the module this type is defined in can create and match on its variants."""
                            , """These "opaque" types don't give you the usual rewards of type-safety and allow a bit more room for mistakes."""
                            , """If you see no problem with exposing the variants, I suggest applying the automatic fix,
otherwise I suggest looking at these alternatives: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-opaque-type/latest#but-what-are-the-alternatives"""
                            ]
                        }
                        choiceTypeNameRange
                        [ Review.Fix.insertAt exposeRange.end "(..)" ]
                    ]

                Nothing ->
                    []

        _ ->
            []


moduleHeaderTypesExposedWithoutVariants : Elm.Syntax.Module.Module -> Dict String Range
moduleHeaderTypesExposedWithoutVariants =
    \moduleHeader ->
        case moduleHeader of
            Elm.Syntax.Module.NormalModule defaultModuleHeader ->
                defaultModuleHeader |> defaultModuleHeaderTypesExposedWithoutVariants

            Elm.Syntax.Module.PortModule defaultModuleHeader ->
                defaultModuleHeader |> defaultModuleHeaderTypesExposedWithoutVariants

            Elm.Syntax.Module.EffectModule _ ->
                -- not supported
                FastDict.empty


defaultModuleHeaderTypesExposedWithoutVariants : Elm.Syntax.Module.DefaultModuleData -> Dict String Range
defaultModuleHeaderTypesExposedWithoutVariants =
    \defaultModuleHeader ->
        case defaultModuleHeader.exposingList of
            Node _ (Elm.Syntax.Exposing.All _) ->
                FastDict.empty

            Node _ (Elm.Syntax.Exposing.Explicit exposes) ->
                exposes
                    |> List.filterMap
                        (\(Node exposeRange expose) ->
                            expose
                                |> exposeToTypeExposedWithoutVariants
                                |> Maybe.map (\exposeName -> ( exposeName, exposeRange ))
                        )
                    |> FastDict.fromList


exposeToTypeExposedWithoutVariants : Elm.Syntax.Exposing.TopLevelExpose -> Maybe String
exposeToTypeExposedWithoutVariants =
    \expose ->
        case expose of
            Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                name |> Just

            Elm.Syntax.Exposing.TypeExpose typeExpose ->
                case typeExpose.open of
                    Just _ ->
                        Nothing

                    Nothing ->
                        typeExpose.name |> Just

            Elm.Syntax.Exposing.FunctionExpose _ ->
                Nothing

            Elm.Syntax.Exposing.InfixExpose _ ->
                Nothing
