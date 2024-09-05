# FP Study

## Prerequisites

1. Install GHCup from [official Haskell page](https://www.haskell.org/ghcup/)
   - Installation guide is in the page, including uninstallation guide.
   - Download GHC, Cabal, Haskell Language Server and Stack; Download everything you can.
2. Download Haskell package in VSCode.
3. Run following command in cmd, powershell, or whatever: `cabal install --lib random` to download `random` package.
4. Run following command: `ghci` to run GHCi: The commandline haskell interpreter.
5. Play with GHCi. To be more specific, try running the following commands.
   - `4 + 6`
   - `take 5 [1..]`
   - `map (+3) [1, 2, 3]`
   - `:t 5` -- :t is the command which prints the type of given expression.
   - `:t (+)`
   - `:t (+3)`
   - `:i Int` -- :i is the command which prints the overall information of given *element*: It can be the expression, type name, or whatever.
   - `:i String`
   - `:i (+)`
   - `:q`

## Index

- 0: What is Functional Programming?

  - What is the difference between statement and expression?
  - What is the meaning of first-class datatype?
  - What is lazy evaluation?
  - What is referential transparency?
  - About recursion
  - What is syntatic sugar?

- 1: About data structure

  - Basic enums
  - Discriminated union
  - Polymorphic types
    - Error detection with Maybe, Either
    - Why use monad? (briefly)
  - Recursively defined types
  - What is typeclass?

- 2: About type system

  - Haskell's type system
    - What is kind?
    - What is type wrapper?
    - What is the difference between type constructor and data constructor?
  - What is functor?
  - What is applicative?

- 3: About Monad

  - What is Monad?
  - Brief introduction about Maybe monad, Either monad, and List monad

- 4: More about Monad
  - 4-0
    - State monad
    - Why use monad?
  - 4-1
    - Haskell's program structure
    - IO monad
