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
    - snake_case `[a-zA-Z_][a-zA-Z0-9_]*`

```haskell
data UpperChar = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Show, Eq, Ord)

data LowerChar = A_ | B_ | C_ | D_ | E_ | F_ | G_ | H_ | I_ | J_ | K_ | L_ | M_ | N_ | O_ | P_ | Q_ | R_ | S_ | T_ | U_ | V_ | W_ | X_ | Y_ | Z_
  deriving (Show, Eq, Ord)

data DigitChar = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Show, Eq, Ord)

data AlphaChar = AlphaLower LowerChar
               | AlphaUpper UpperChar
  deriving (Show, Eq, Ord)

data AlphaNumChar = AlphaNumAlpha AlphaChar
                  | AlphaNumDigit DigitChar
  deriving (Show, Eq, Ord)
```

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

>>> [snakeCharQ|_|]
SnakeUnderscore
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

>>> [snakeQ|foo_bar|]
Snake (SnakeHeadAlpha (AlphaLower F_)) [SnakeAlphaNum (AlphaNumAlpha (AlphaLower O_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower O_)),SnakeUnderscore,SnakeAlphaNum (AlphaNumAlpha (AlphaLower B_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower A_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower R_))]
>>> [snakeQ|__constructor|]
Snake SnakeHeadUnderscore [SnakeUnderscore,SnakeAlphaNum (AlphaNumAlpha (AlphaLower C_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower O_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower N_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower S_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower T_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower R_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower U_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower C_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower T_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower O_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower R_))]
>>> [snakeQ|FOO_MEE_9|]
Snake (SnakeHeadAlpha (AlphaUpper F)) [SnakeAlphaNum (AlphaNumAlpha (AlphaUpper O)),SnakeAlphaNum (AlphaNumAlpha (AlphaUpper O)),SnakeUnderscore,SnakeAlphaNum (AlphaNumAlpha (AlphaUpper M)),SnakeAlphaNum (AlphaNumAlpha (AlphaUpper E)),SnakeAlphaNum (AlphaNumAlpha (AlphaUpper E)),SnakeUnderscore,SnakeAlphaNum (AlphaNumDigit D9)]

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

>>> [camelQ|snake_case|]
error:
    • ''_'' is not an AlphaNumChar.
    • In the quasi-quotation: [camelQ|snake_case|]

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
