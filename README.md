# :diamond_shape_with_a_dot_inside: hs-character-cases :diamond_shape_with_a_dot_inside:

A Haskell library for subspecies types of Char, and naming cases.

- Subspecies types (Char like)
    - lower `[a-z]`
    - `[A-Z]`
    - `[0-9]`
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

That ADT can be used directly, but `QuasiQuoter`s is more useful.

```haskell
TODO
```

Wrong cases will be rejected on the compile time!

```haskell
TODO
```

Also we can use it as parser combinators.

```haskell
TODO
```

- - - - -

Thanks.
