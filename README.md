> ⚠️ This rule is pretty idealistic, especially for packages, so decide carefully before committing to using it

# elm-review-opaque-type

[(🔧) `Review.OpaqueType.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-opaque-type/1.0.0/Review-OpaqueType#forbid "provides fixes only for applications")
reports types that are exposed without their variants.

If you want to learn more about opaque types first:
  - [🕮 article "Use opaque types in Elm!" by Héctor Ramón](https://dev.to/hecrj/use-opaque-types-in-elm-3oal)
  - [🗎 design guideline "Keep tags and record constructors secret" by Evan Czaplicki](https://package.elm-lang.org/help/design-guidelines#keep-tags-and-record-constructors-secret)
  - 🎙 any [elm radio](https://elm-radio.com/) episode
  - [🕮 article "Types of Types in Elm" by Mike Knepper](https://8thlight.com/insights/types-of-types-in-elm)

```elm
import Review.Rule
import Review.OpaqueType
import NoMissingTypeExpose

config : List Review.Rule.Rule
config =
    [ Review.OpaqueType.forbid

    -- so that your exposed type aliases don't reference hidden types
    , NoMissingTypeExpose.rule
    ]
```
  - 🧩 [`NoMissingTypeExpose`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-common/latest/NoMissingTypeExpose)

## why?

Claim: "opaque types give you neither convenience, confidence, nor the rewards".

  - the stored value does not know as much as your type suggests.
    ```elm
    type Email = Email String
    domain = \(Email email) -> ??
    ```
    compare with e.g.
    ```elm
    type Email
        = Email { local : Local, domain : Domain }
    domain = \(Email email) -> email.domain
    ```
    why not take this free gift from storing the parsed data,
    even if you don't need it right now?
    
    When using opaque types, you still have to validate broad values. I know you have this regex around that "should work". The code will barely grow in complexity if you make it ["parse, not validate"](https://elm-radio.com/episode/parse-dont-validate/) instead, if at all.

    And once you've tightly defined the type, your job is done forever since there's no way to construct invalid values, even internally. If it makes sense, maybe publish it and let everyone profit

  - no module has "authority" over a piece of data. This is effectively an argument against encapsulation, where certain data can only be read and edited by certain privileged functions.
    When your type makes it impossible to construct values that don't make sense, there's no need to hide access away.

    For example, [`elm/html`](https://dark.elm.dmy.fr/packages/elm/html/latest/) does not expose the [`Html`](https://dark.elm.dmy.fr/packages/elm/html/latest/Html#Html) type and the only way to "use" it is by passing it to the runtime in `view`. But what if you wanted to convert the html to a `String`, have a global sanitizing function, encode it or easily test for specific properties in pure elm?

    Most ui libraries have the same problem where the only way to use their ui types is by converting them to the opaque `Html` type.
    A better alternative would be a type like [`Html.Parser.Node`](https://dark.elm.dmy.fr/packages/hecrj/html-parser/latest/Html-Parser#Node) or [`Web.DomNode`](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/Web#DomNode)

    Another problematic area is transporting elm `type` values through another format.
    Some packages provide `encode` and `decode` if they're generous, others don't.
    What if you wanted xml, bytes or yaml instead?
    [lamdera](https://www.lamdera.com/) for example will simply not wire opaque types between frontend and backend – and that makes sense.
    What if a new patch version of the package stores the `type`'s data differently?
    What if someone altered the wired bytes so that the opaque data wouldn't even pass validation?
    ...
  
    Limiting access to values might not be a great idea because you can't and shouldn't really account for all possible use-cases when writing the `type`.
    
  - usually there's no safe way to construct them, which
    can make benchmarking and testing the insides of modules with opaque types in an application harder. Generally, it's encouraged to only test a module from the outside but sometimes you might want to check if some implementation detail specifically is working
    ```elm
    -- module PersonalNumberUk exposing (PersonalNumberUk)

    type PersonalNumberUk
        = PersonalNumberUk
            { prefix : Prefix, digits : Vector6 Digit, finalLetter : FinalLetter }
    
    -- we want to test this
    finalLetterFromChar : Char -> Maybe FinalLetter
    ```
    try for example
    ```elm
    -- module PersonalNumberUk exposing (PersonalNumberUk)
    import PersonalNumberUk.FinalLetter exposing (FinalLetter)

    type PersonalNumberUk
        = PersonalNumberUk
            { prefix : Prefix, digits : Vector6 Digit, finalLetter : FinalLetter }
    
    -- module PersonalNumberUk.FinalLetter exposing 
    fromChar : Char -> Maybe FinalLetter

    -- module PersonalNumberUk.Test exposing (tests)
    tests : Test
    tests =
        Test.test "final letter parses a|A as A"
            (\() ->
                'a'
                  |> PersonalNumberUk.FinalLetter.fromChar
                  |> Expect.equal
                      -- now we can directly check for the value
                      PersonalNumberUk.FinalLetter.A
            )
    ```
    that way, it's not part of the API of `PersonalNumberUk` but still accessible from tests and the main module.
    (Btw, if you have a better example for this, [tell me](https://github.com/lue-bird/elm-morph/issues/new))
    

## but what are the alternatives?

from stupidly obvious to powerful

  - Did you hide the variants because constructing a value of that type is useless/impossible? Like
    ```elm
    type YourTypeOnlyTag = YourTypeOnlyTag Never
    ```
    There's no harm in exposing those variants. Add the `Never` to be extra sure nobody gets the idea to construct it.
  
  - Do you lose guarantees if you expose this `type`'s variants?
    ```elm
    -- module UsMoney exposing (UsMoney, cents, dollars)
    type UsMoney
        = InCents Int
    
    cents : Int -> UsMoney
    cents =
        InCents

    dollars : Int -> UsMoney
    dollars = \dollarAmount ->
        (dollarAmount * 100) |> cents
    ```
    you lose nothing by exposing the variant `UsMoney.InCents`
    ```elm
    module UsMoney exposing (UsMoney(..), cents, dollars)
    ..same as before..
    ```
    As an added benefit you allow pattern matching.

    If you take away one thing from this package,
    it's to use descriptive wrapper types with just one variant often, even if you don't plan on hiding that variant. That alone will prevent most accidents and make things more clear.
  
  - Did you hide the variants because your `type` has phantom type parameters?
    → ["phantom types - but what are the alternatives?"](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-phantom-type/latest#but-what-are-the-alternatives-)

  - Did you hide the variants because you want to internally preserve certain properties that a user could bypass? Try modeling it using choice `type`s
    
    TODO

  - Do you hide the variants because if you moved the `type` into the exposed modules there would be import cycles?
    ```elm
    -- module Expression exposing (Expression)
    type alias Expression =
        Expression.Internal.Expression
    
    -- module Expression.Internal exposing (Expression(..), LetIn(..))
    type Expression
        = ...
        | LetIn Expression.LetIn.LetIn
    
    type LetIn
        = ...
        | LetDestructuring { ..., destructured : Expression }
    
    -- module Expression.LetIn exposing (LetIn)
    type alias LetIn =
        Expression.Internal.LetIn
    ```
    Why not move the necessary `types` together into one module?
    ```elm
    -- module Expression exposing (Expression(..), LetIn(..))
    type Expression
        = ...
        | LetIn Expression.LetIn.LetIn
    
    type LetIn
        = ...
        | LetDestructuring { ..., destructured : Expression }
    
    -- module Expression.LetIn exposing (..., ...)
    {-| Helpers for [`Expression.LetIn`](Expression#LetIn)
    -}
    import Expression exposing (LetIn)
    ```
    you can of course add an alias back into `Expression.LetIn` but just linking to it seems enough.

    Here's another approach for module structures like
    ```elm
    -- module Decimal exposing (Decimal, round)
    import Integer.Internal exposing (Integer)

    type alias Decimal = Decimal.Internal.Decimal
    round : Decimal -> Integer
    ```
    ```elm
    -- module Integer exposing (Integer, divideBy)
    import Decimal.Internal exposing (Decimal)

    type alias Integer = Integer.Internal.Integer
    divideBy : Integer -> (Integer -> Decimal)
    ```
    ```elm
    -- module Integer.Internal exposing (Integer(..))
    type Integer = Integer ...
    ```
    ```elm
    -- module Decimal.Internal exposing (Decimal(..))
    type Decimal = Decimal ...
    ```
    I would strongly suggest re-organizing the modules so that for example `Decimal` gets all the functions that return a `Decimal` (here some form of the `divideBy` function)
    but if this is not viable or pretty, here's a trick:
    Wrapping the data into a record instead of variant
    ```elm
    -- module Decimal exposing (Decimal, round)
    import Integer.Internal exposing (Integer)
    type alias Decimal = { decimal : ... }
    type alias Integer = { integer : ... }
    round : Decimal -> Integer
    ```
    ```elm
    -- module Integer exposing (Integer, divideBy)
    import Decimal.Internal exposing (Decimal)
    type alias Decimal = { decimal : ... }
    type alias Integer = { integer : ... }
    divideBy : Integer -> (Integer -> Decimal)
    ```
    This way, you can define the type in multiple modules to break the cycle. As a bonus, you'll get an easy way to deconstruct using `.integer` & `.decimal`.
    Make sure to keep these definitions in sync. Maybe even write tests like
    ```elm
    ... |> Integer.divideBy ... |> Decimal.round
    ```

    Yet another technique is "duplicating internal API to the outside".
    This can mean 1:1 copy or a "user-facing view" of some aspect.
    ```elm
    -- module A exposing (A(..), doSomething)
    type A
        = X X
        | Y Y
    aToInternal : A -> A.Internal.A
    aFromInternal : A.Internal.A -> A

    doSomething : A -> A
    doSomething = \a ->
        a |> aToInternal |> A.Internal.doSomething |> aFromInternal
    ```
    ```elm
    -- module A.Internal exposing (A(..), doSomething)
    type A
        = X X
        | Y Y
    doSomething : A -> A
    ```
    I've written it this abstractly because this is as boilerplate-y as it looks and I've yet to see a package where earlier techniques didn't work. Maybe yours?

Mostly for packages:

  - Do you use opaque types to allow adding configuration in a future version without it counting as a major version bump?
    I feel your pain. I also dream for the day where adding variants as input or fields as output only requires a minor version bump.
    I don't think cases like this will be super frequent, though, so clearly telling your users your new version won't break their code is pretty good already.
  
  - Did you hide the variants because you want to be able to change details about the type (not what it represents but how it's stored) in the future without forcing a major version bump?
    I find cases like that to be really rare in practice, with type definitions only changing with a change of context. I think the best you can do is telling users that the upgrading to the version
    won't mean any breaking changes if they didn't access the safe internals.

## not convinced?

I'm super interested in what you're brewing!
Do you use them to get better performance, cash some data or because there doesn't seem to be another way to ensure certain properties (like sorting in a `Dict`)?
If you want to, text me @lue on slack as these are problems I like finding nicer fixes for.

## thanks
- [miniBill for elm-fast-dict](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest)
