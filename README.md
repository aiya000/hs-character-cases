# :diamond_shape_with_a_dot_inside: character-cases :diamond_shape_with_a_dot_inside:

[![](https://img.shields.io/hackage/v/character-cases)](https://hackage.haskell.org/package/character-cases)

A Haskell library for subspecies types of Char, and naming cases.

- Naming cases (String like)

```haskell
-- | [A-Z][a-zA-Z0-9]*
data Pascal = Pascal UpperChar [AlphaNumChar]
  deriving (Show, Eq)

-- | [a-z][a-zA-Z0-9]*
data Camel = Camel AlphaChar [AlphaNumChar]
  deriving (Eq)

-- | [a-zA-Z_][a-zA-Z0-9_]*
data Snake = Snake SnakeHeadChar [SnakeChar]
  deriving (Show, Eq)

-- | [A-Z_][A-Z0-9_]*
data UpperSnake = UpperSnake UpperSnakeHeadChar [UpperSnakeChar]
  deriving (Show, Eq)

-- | [a-z]+
data LowerString = LowerString LowerChar [LowerChar]
  deriving (Show, Eq)

-- | .+
data NonEmpty = NonEmpty Char String
  deriving (Show, Eq)
```

- Subspecies types (Char like)

```haskell
-- | [A-Z]
data UpperChar = ...
  deriving (Show, Eq, Ord)

-- | [a-z]
data LowerChar = ...
  deriving (Show, Eq, Ord)

-- | [0-9]
data DigitChar = ...
  deriving (Show, Eq, Ord)

-- | [A-Za-z]
data AlphaChar = ...
  deriving (Show, Eq, Ord)

-- | [A-Za-z0-9]
data AlphaNumChar = ...
  deriving (Show, Eq, Ord)
```

## Why we use this?

**Make impossible things to impossible** to strict `data` driven programming.

For example, in the first place,

- `List.NonEmpty a` wont be the empty list `[]`

also

- `Pascal` wont be other than `[A-Z][a-zA-Z0-9]*`
- `Camel` wont be other than `[a-z][a-zA-Z0-9]*`
- ...

These are useful in

- Abstract Syntax Tree (Identifiers)
- ...

**Make impossible things to impossible.**

## Usage

- [character-cases - Hackage](https://hackage.haskell.org/package/character-cases)

- [Data.Char.Cases](https://github.com/aiya000/hs-character-cases/blob/master/src/Data/Char/Cases.hs)
- [Data.String.Cases](https://github.com/aiya000/hs-character-cases/blob/master/src/Data/String/Cases.hs)

### `QuasiQuoter`s

That ADT can be used directly, but `QuasiQuoter`s is more useful.

- Naming cases

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

- Characters

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

Wrong cases will be rejected on **the compile time**!

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
