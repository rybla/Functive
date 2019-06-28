# Functive

A simple modular extendible statically-typed symbolic programming language with dependent types.
Implemented in Haskell.

## concepts

__simple__:
- clean and simple "low-level" functional programming language
- explicit abstract syntax i.e. parentheses are required
- inspired  Gallina and the ML family (but without many extras)
- no annoying "syntactical-sugar" constructions built-in; all syntax is consistent and modular

__extendible__:
- interface for easy implementations of macros and language constructs into pre-processor
- inspired by Racket and Coq's `Notation`

__modular__:
- straighforward module system
- doesn't depend of file structure i.e. compilation/interpretation is done after concatenating all sourcecode together

__statically-typed__:
- complete compile-time type-checking enforcement
- (named) typed holes in the form of `Assumption`s

__symbolic__:
- symbolic representations of values
- if `f` is not defined and `(f x)` is evaluated, `f` is assumed to have the correct type and the value is the symbolic expression `(f x)`
- if two functions `f` and `g` are assumed this way and then compared (e.g. `f x = g x` is written) they are treated as distinct since their names are distinct

__dependent types__:
- type constructors e.g. `list : type -> type`
- value-dependent types e.g. `finite : nat -> type`
- type variables e.g. `mul_identity : forall n:nat, n * 1 = n`
- explicit polymorphism e.g. `empty_list : forall a:type, list a`
- infinite type hierarchy e.g. `(type = type0) : type1 : type2 : type3 : ...`


## organization

| directory   | description |
| ---         | --- |
| `src/`      | haskell source-code. |
| `app/`      | haskell executable source-code. |
| `test/`     | haskell unit tests. |
| `doc/`      | haskell documentation. |
| `design/`   | reader-friendly design details (LaTex/PDF) |
| `examples/` | example _functive_ prorams. |

## building

Using [stack](https://docs.haskellstack.org/en/stable/README/).

```sh
  stack build
```

## executing

Run unit tests with

```sh
  stack test
```

Run executable (`app/Main.hs`) with

```sh
  stack exec functive-hs-exe
```

## tutorial

TODO
