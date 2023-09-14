+++
title = "Implementing a Small Interpreter (and Parser) in Haskell"
author = ["Sophie"]
tags = ["functional", "programming", "haskell"]
draft = true
+++

One of my favourite assignments in my advanced functional programming course,
was implementing a parser and interpreter for a subset of Python, called Boa, in
Haskell.

We'd just started getting a grasp of what monads are and how they work, and this
project is what really solidified it for me. Here's how we approached it and
some of my reflections when looking back at it.

I'll be focusing on the interpreter, because we implemented the monad for the
interpreter by hand, and I feel it's easier to see how
monads work when seeing the implementation. The parser, on the other hand, uses
the monad combinator library Parsec, which is both more complicated and more
well-documented elsewhere.

The code is available in its entirety on my [GitHub](https://github.com/SophieBosio/boa), and I would encourage you to
follow along!


## The Boa Language {#the-boa-language}

The Boa language is a small subset of Python. It looks pretty familiar to anyone
who has programmed in Python before, and if you're interested in a more thorough
explanation of the language, you can check out the `./docs` folder in the repo.

The most important aspects of the language, for our interpreter, is that it has
global state. In our purely functional implementation language, computations are
normally _not_ stateful. So, we have to make use of a monad to model the program
environment and interpret terms with respect to the existing bindings.

Thankfully, Boa computations can make do with read-only access to the
environment. Therefore, we can use a neat and (as far as monads go) pretty
intuitive monad, namely the [Reader monad](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html). We'll implement it ourselves, but you
could also just import the above package in your project.


### Spoiler Alert! Syntax File {#spoiler-alert-syntax-file}

Below, you can find the finished `Syntax.hs` file. If you want to implement the
language yourself, you might want to try your hand at writing this file, too,
before seeing this suggestion.

<details>
<summary><code>Syntax.hs</code></summary>
<div class="details">

```haskell
module Syntax where

data Value =
    None
  | Boolean Bool
  | Number  Integer
  | Text    String
  | List    [Value]
  deriving (Eq, Show, Read)

data Expression =
    Constant  Value
  | Variable  VariableName
  | Operation OperationSymbol Expression Expression
  | Not Expression
  | Call FunctionName FunctionInput
  | ListExpression    [Expression]
  | ListComprehension Expression [Clause]
  deriving (Eq, Show, Read)

type VariableName      = String
type FunctionName      = String
type FunctionInput     = [Expression]
type FunctionArguments = [Value]

data OperationSymbol =
    Plus
  | Minus
  | Times
  | Div
  | Mod
  | Eq
  | Less
  | Greater
  | In
  deriving (Eq, Show, Read)

data Clause =
    For VariableName Expression
  | If               Expression
  deriving (Eq, Show, Read)

type Program = [Statement]

data Statement =
    Define  VariableName Expression
  | Execute              Expression
  deriving (Eq, Show, Read)
```
</div>
</details>


## Boa Monad {#boa-monad}

Executing a Boa program means evaluating all the program statements, starting
from the empty environment and populating it as we go. If we encounter any
runtime errors underway, we'll stop execution and print an error message. If all
is well, we'll print the output of the program.

Let's use the following abbreviations.

```haskell
type Output      = [String]
type ErrorMessage = String
data RuntimeError =
    UnboundVariable VariableName
  | BadFunction     FunctionName
  | BadArgument     ErrorMessage
  deriving (Eq, Show)

type Environment = [(VariableName, Value)]
type Runtime a   = Environment -> (Either RuntimeError a, Output)
```


### Monad Operations {#monad-operations}


## Monads {#monads}
