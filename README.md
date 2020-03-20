# :diamond_shape_with_a_dot_inside: character-cases :diamond_shape_with_a_dot_inside:

A Haskell library for subspecies types of Char, and naming cases.

- Subspecies types (Char like)
    - lower `[a-z]`
    - Upper `[A-Z]`
    - numbers `[0-9]`
    - or these combinations. e.g. `[a-zA-Z]`, `[a-zA-Z0-9]`.

- Naming cases (String like)
    - PascalCase `[A-Z][A-Za-z0-9]*`
    - camelCase `[a-zA-Z][a-zA-Z0-9]*`
    - sneak_case `[a-zA-Z_][a-zA-Z0-9_]*`

## Why we use this?

I'm developping a software, that is a program compiler.

I really want to separate Char cases, and naming cases.
But the way was not prepared.

Now, we can use a lot of Char cases and naming cases, by this library.

## Usage

### `QuasiQuoter`s

That ADT can be used directly, but `QuasiQuoter`s is more useful.

Characters

```haskell
>>> [upperCharQ|X|]
X

>>> [lowerCharQ|x|]
X_

>>> [digitCharQ|0|]
D0

>>> [alphaNumCharQ|X|]
AlphaNumAlpha (AlphaUpper X)

>>> [alphaNumCharQ|x|]
AlphaNumAlpha (AlphaLower X_)

>>> [alphaNumCharQ|0|]
AlphaNumDigit D0

>>> [sneakCharQ|_|]
SneakUnderscore
```

Naming cases

```haskell
>>> [pascalQ|Pascal|]
Pascal P [AlphaNumAlpha (AlphaLower A_),AlphaNumAlpha (AlphaLower S_),AlphaNumAlpha (AlphaLower C_),AlphaNumAlpha (AlphaLower A_),AlphaNumAlpha (AlphaLower L_)]

>>> [nonEmptyQ|x|]
NonEmpty 'x' ""
>>> [nonEmptyQ|foo|]
NonEmpty 'f' "oo"
>>> [nonEmptyQ|Bar|]
NonEmpty 'B' "ar"

>>> [camelQ|camel|]
"camel"
>>> [camelQ|Pascal|]
"Pascal"

>>> [sneakQ|foo_bar|]
Sneak (SneakHeadAlpha (AlphaLower F_)) [SneakAlphaNum (AlphaNumAlpha (AlphaLower O_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower O_)),SneakUnderscore,SneakAlphaNum (AlphaNumAlpha (AlphaLower B_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower A_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower R_))]
>>> [sneakQ|__constructor|]
Sneak SneakHeadUnderscore [SneakUnderscore,SneakAlphaNum (AlphaNumAlpha (AlphaLower C_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower O_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower N_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower S_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower T_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower R_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower U_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower C_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower T_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower O_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower R_))]
>>> [sneakQ|FOO_MEE_9|]
Sneak (SneakHeadAlpha (AlphaUpper F)) [SneakAlphaNum (AlphaNumAlpha (AlphaUpper O)),SneakAlphaNum (AlphaNumAlpha (AlphaUpper O)),SneakUnderscore,SneakAlphaNum (AlphaNumAlpha (AlphaUpper M)),SneakAlphaNum (AlphaNumAlpha (AlphaUpper E)),SneakAlphaNum (AlphaNumAlpha (AlphaUpper E)),SneakUnderscore,SneakAlphaNum (AlphaNumDigit D9)]

>>> [lowerStringQ|imavimmer|]
LowerString I_ [M_,A_,V_,I_,M_,M_,E_,R_]
```

Wrong cases will be rejected on the compile time!

```haskell
>>> [upperCharQ|x|]
error:
    • ''x'' is not an UpperChar.
    • In the quasi-quotation: [upperCharQ|x|]

>>> [lowerCharQ|X|]
error:
    • ''X'' is not a LowerChar.
    • In the quasi-quotation: [lowerCharQ|X|]

>>> [alphaNumCharQ|one or more characters|]
error:
    • alphaNumCharQ required a Char, but a String is specified: one or more characters
    • In the quasi-quotation: [alphaNumCharQ|one or more characters|]
```

```haskell
>>> [pascalQ|camel|]
error:
    • ''c'' is not an UpperChar.
    • In the quasi-quotation: [pascalQ|camel|]

>>> [nonEmptyQ||]
error:
    • nonEmptyQ required a non empty string, but the empty string is specified.
    • In the quasi-quotation: [nonEmptyQ||]

>>> [camelQ|sneak_case|]
error:
    • ''_'' is not an AlphaNumChar.
    • In the quasi-quotation: [camelQ|sneak_case|]

>>> [lowerStringQ|Upper|]
error:
    • ''U'' is not a LowerChar.
    • In the quasi-quotation: [lowerStringQ|Upper|]
```

Please also see doctests of modules.

- [Data.Char.Cases](https://github.com/aiya000/hs-character-cases/blob/master/src/Data/Char/Cases.hs)
- [Data.String.Cases](https://github.com/aiya000/hs-character-cases/blob/master/src/Data/String/Cases.hs)

### Parser combinators

And we can use it as parser combinators.

Please see modules and documents.

- [character-cases - Hackage](https://hackage.haskell.org/package/character-cases)

- - - - -

Thanks.
