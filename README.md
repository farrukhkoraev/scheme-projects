# Scheme/Racket Playground

A collection of small projects exploring Scheme and Racket concepts.

## Overview

This repository serves as a learning environment for experimenting with functional programming patterns, language features, and practical implementations in Scheme/Racket.

## Current Projects

### Parser Combinators (`parser.rkt`)

A monadic parser combinator library built from scratch, demonstrating:
- Monadic composition: `fmap`, `ap`, `bind`, `either`, `sequence`
- Practical application with a complete JSON parser
- Functional approach to building complex parsers from simple primitives

**Usage:**
```bash
racket parser.rkt
```

### Monads (`monads.rkt`)

Monad implementations and do-notation in Racket, exploring:
- Maybe monad for handling optional values
- List monad for non-deterministic computation
- Custom `do` macro for monadic composition
- Functor, Applicative, and Monad interfaces

**Usage:**
```bash
racket monads.rkt
```

## Planned Explorations

- Continuations and control flow
- Macro systems
- Type systems and interpreters
- Concurrency patterns
- More...
