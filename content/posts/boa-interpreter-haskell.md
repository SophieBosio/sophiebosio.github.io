+++
title = "Implementing a Small Interpreter in Haskell"
author = ["Sophie"]
date = 2023-09-14T10:24:00+02:00
tags = ["functional", "programming", "haskell"]
draft = false
+++

One of my favourite assignments in my advanced functional programming course,
was implementing a parser and interpreter for a subset of Python, called Boa, in
Haskell.

We'd just started getting a grasp of what monads are and how they work, and this
project is what really solidified it for me. Here's how we approached it and
some of my reflections when looking back at it.

I'll be focusing on the interpreter, because we implemented the monad for the
interpreter by hand, and I feel it's easier to see how monads work when seeing
the implementation. The parser, on the other hand, uses the monad combinator
library Parsec, which is both more complicated and more well-documented elsewhere.

I'll assume you're familiar with monads and their operations, but this is also a
great project to try your hand at if you're still getting the hang of using them.

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


## Monad Implementation {#monad-implementation}

Executing a Boa program means evaluating all the program statements, starting
from the empty environment and populating it as we go. If we encounter any
runtime errors underway, we'll stop execution and print an error message. If all
is well, we'll continue until there are no more statements, and then print the output of the program.

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

An environment is a mapping from variable names to values. What we want is a
monad that allows us to keep track of the variable bindings in the environment.
When we want to perform a computation, we need a runtime where we can read from
the environment and either raise a runtime error or return a value, and append
the output from the computation so we can print it later.
This is essentially what we would want to use the Reader monad for! Let's
implement a simple version.


### The Boa Monad {#the-boa-monad}

Every monad is an applicative functor and every applicative functor is a
functor. Those are interesting to read about in their own right, but for now, we
just need to know that if we implement the monad correctly, we get these two for
free. For these freebies, we need to import `Control.Monad`. We'll also need a
newtype `Boa a`, whose "run" action will have the type `Runtime a`.

```haskell
import Control.Monad

newtype Boa a = Boa {run :: Runtime a}

instance Functor Boa where
  fmap = liftM

instace Applicative Boa where
  pure  = return
  (<$>) = ap
```

Nice! Now, we need to define two functions for our monad, namely
`return` and `>>=`, and we have a fully fledged monad instance!

For a quick refresher, let's look at their type signatures.

```haskell
return :: Monad m => a -> m a
(>>=)  :: Monad m => m a -> (a -> m b) -> m b
```

In the case of `return`, we just want to take a value and put it inside the monad.
What's the simplest way we can do that? Well, our monad lets us read from the
environment and we're supposed to
return either an error message or a value `a`, and then the output we want to
print after the computation is done. The simplest way, then, is to not read
anything from the environment, just put in the value, and not add anything to the
output.

```haskell
instance Monad Boa where
  return a = Boa $ const (Right a, [])
  ba >>= f = undefined
```

Essentially, this says "I don't care about the environment, just give me the
tuple with `a` and no output in it, then put it all inside the Boa monad for me".
The following notation is semantically equivalent, but the syntax may be easier
to read depending on what you're used to.

```haskell
return a = Boa (\_ -> (Right a, []))
```

Then what do we want to do for `>>=`, is define a way to take a value out of the monad,
apply a function to it, and then put the result back into the monad. To unwrap
the value `a` from `ba`, we can simply use our monad's `run` function. We have to take
in an environment so that we can run the
monad in the environment and look at  the result. If it's an error, then we just
return the error. But if we get a value `a` out and some output, then we want to run
the function `f` on `a`. What we're gonna return, is the result of `f a` and the
output of that computation appended to all the previous output. Finally, we put
it all back into the Boa monad.

```haskell
instance Monad Boa where
  return a = Boa $ const (Right a, [])
  ba >>= f = Boa $ \env ->
    case run ba env of
      (Left re, out) -> (Left re, out)      -- just return the error
      (Right a, out) ->
        let (result, out') = run (f a) env  -- run f a in the environment
        in  (result, out ++ out')           -- append the new output to the old
```

Sweet! Now let's define some operations that let us interact with the monad in a
more ergonomic way.


### Monad Operations {#monad-operations}

Now we have the monad itself, but it's nice to abstract away some details and
instead work with more intuitive function names. Let's write some functions with
more easily understood names.

```haskell
abort  :: RuntimeError -> Boa a
look   :: VariableName -> Boa Value
bind   :: VariableName -> Value -> (Boa a -> Boa a)
output :: String       -> Boa ()
```

`abort` should just accept a runtime error and put it into the Boa monad.

```haskell
abort :: RuntimeError -> Boa a
abort re = Boa $ const (Left re, [])
-- or equivalently,
abort re = Boa (\_ -> (Left re, []))
```

`look` should accept a variable name and look to see if it is in the environment.
if it is, return the value inside the monad. If it isn't, return an "unbound
variable" runtime error inside the monad. In either case, no output is
necessary.

```haskell
look :: VariableName -> Boa Value
look x = Boa $ \env ->
  case lookup x env of
    Just a  -> (Right a, [])
    Nothing -> (Left (UnboundVariable x), [])
```

`bind` takes a variable name and a value.  What it should do, is run the monad
with that name-value binding _prepended_ to the old environment, and then put the
value of that computation back into the monad. It needs to be
prepended and not appended because when we perform a lookup, we want to find the
most recent bindings first.

```haskell
bind :: VariableName -> Value -> (Boa a -> Boa a)
bind x v ba = Boa $ \env -> run ba $ (x,v) : env
-- or equivalently,
bind x v ba = Boa (\env -> run ba ( [(x,v)] ++ env ))
```

`output` should take an string and put it into the monad as output.

```haskell
output :: String -> Boa ()
output s = Boa $ const (Right (), [s])
```


## Interpreter Functions {#interpreter-functions}

Finally, we need the main operations of the interpreter. To execute a Boa
program, we want to take a program and return a tuple with the output of the
program and possibly a runtime error.

```haskell
execute :: Program -> (Output, Maybe Runtime Error)
execute p = undefined
```

Since a `Program` is made up of `Define` and `Execute` statements, we can write a
helper function `exec` that pattern matches on the type of statement.

```haskell
exec :: Program -> Boa ()
exec []                 = undefined
exec ((Define x e) : s) = undefined
exec ((Execute  e) : s) = undefined
```

And finally, we'll need a helper function `eval` that's responsible for evaluating
a single expression and putting the resulting value into the Boa monad for us.
This is what we'll focus on for now.

```haskell
eval :: Expression -> Boa Value
```

Since they're not the most interesting part of the interpreter, I'm omitting quite a few other helper functions, notably `operate ::
OperationSymbol -> Value -> Value -> Either ErrorMessage Value` and `apply ::
FunctionName -> FunctionArguments -> Boa Value`. They're of course in the repo,
and I'd encourage you to try implementing them yourself.

`eval` needs to pattern match on each expression. There are the following types of
`Expression`:

-   `Constant`
-   `Variable VariableName`
-   `Not Expression`
-   `Operation OperationSymbol Expression Expression`
-   `Call FunctionName FunctionInput`
-   `ListExpression [Expression]`
-   `ListComprehension Expression [Clause]`


### Constant {#constant}

Let's start with `Constant`.

```haskell
eval (Constant v) = return v
```

It's already a value, so we can just take it and put it directly in the
monad. Easy!


### Variable {#variable}

What about a variable? Well, either it's bound in the environment or it's not
and should return an error. `look` seems like a perfect fit for the job. It even
returns an error message for us if `x` is not present in the environment.

```haskell
eval (Variable x) = look x
```


### Not {#not}

If we meet a `Not` expression, we should evaluate the sub-expression to a Boolean
value, then take the opposite value, cast it as a `Boolean` and put it in the Boa
monad. Actually, we can do all of this by `fmap`-ing `(Boolean . not . truthy)` onto
the evaluated expression. That unwraps the expression, applies the three
functions above, and then wraps it back up in the monad.

Note that `<$>` is just an infix version of `fmap`, and `truthy` is a helper function that evaluates a `Value` to a Boolean value.

```haskell
eval (Not e) = Boolean . not . truthy <$> eval e
```


### Operation {#operation}

An `Operation` is performed by evaluating each of its arguments,
extracting the `Value` from each, using `operate` on them to get the result `Value`,
and wrapping that result back in the monad. In this case, `do`-notation makes our code much more readable.

```haskell
eval (Operation o e1 e2) =
  do v1 <- eval e1
     v2 <- eval e2
     case operate o v1 v2 of
       Right v -> return v
       Left re -> abort $ BadArgument re
```


### Call {#call}

Recall that `Call` takes a function name `f` and a list of expressions `es`, which is the
function input. What we want to do, is to evaluate all the function arguments,
and then send all those values to our helper function `apply`, which will apply `f`
to the evaluated `es` and put the result into the Boa monad.

With the help of `mapM`, we can write the function for `Call` expressions very
succinctly. Its type signature is

`mapM :: (Traversable t, Monad m) ==> (a -> m b) -> t a -> m (t b)`.

In our case, the traversable structure is a list and the
monad is the Boa monad. In our case, we can rewrite it as

`mapM :: (Expression -> Boa Value) -> [Expression] -> Boa ([Value])`.

`eval`, of course has the type `Expression -> Boa Value` and the function input has type `[Expression].`

Great! So if we do `mapM eval es`, we'll get a `Boa [Value]`, where each `Value` is an
evaluated function argument. Then we just want to take this list of values out
of the monad and send it to the `apply f` function.

```haskell
eval (Call f es) = mapM eval es >>= apply f
```


### List Expression {#list-expression}

We can do something similar for `ListExpression`, except this case is much easier!
We want to evaluate all the expressions, as above, but then all we need is to
put the results in a `List` and wrap it in the monad. Then we can just `fmap` `List`
onto the evaluated expressions.

```haskell
eval (ListExpression es) = List <$> mapM eval es
```


### List Comprehension {#list-comprehension}

Now, the `ListComprehension` is probably the trickiest case to write. Once way is to write
two mutually recursive functions, the `eval` case for `ListComprehension` and a
helper function `comprehension`.

The `eval` case for list comprehension, does more or less the same as the
`ListExpression` does: It takes the result of evaluating the contents, and puts
the resulting `List` into the Boa monad. Then, `comprehension` is the function that
actually computes the result.

```haskell
eval (ListComprehension e1 c1) = List <$> comprehension c1
  where
    comprehension :: [Clause] -> Boa [Value]
    comprehension = undefined
```

`comprehension` takes in a list of clauses, and a `Clause` is either an if-statement
on the form `If Expression` or a for-statement
on the form `For VariableName Expression=`.

Therefore, `comprehension` pattern matches on three cases: The empty list, a
for-statement and more clauses, or an if-statement and more clauses.

```haskell
eval (ListComprehension e1 c1) = List <$> comprehension c1
  where
    comprehension :: [Clause] -> Boa [Value]
    comprehension []              = undefined
    comprehension (If    e2 : c2) = undefined
    comprehension (For x e2 : c2) = undefined
```

In the case of the empty list, we have a list comprehension that is really just
a list expression, on the form `ListComprehension Expression []`. So in that case,
`comprehension` can just evaluate `e1` and return the list containing that one
value, inside the `Boa` monad.

Once again, there's a very handy function we can use, to write this code in a single, short
line, namely `<&>`. It's implemented as `flip fmap` and has the type signature

`(<&>) :: Functor f ==> f a -> (a -> b) -> f b.`

In our case, we're applying it like this,

`(<&>) :: Boa Value -> (Value -> [Value]) -> Boa [Value]`.

The `return` we're using here is the `return` of the list monad. Basically, we're
saying "evaluate `e1`, which gives us a `Boa Value`. Then, take that value out of
the `Boa` monad, put it into the list monad with `return`, and finally put that list
with the value back in the `Boa` monad".

```haskell
eval (ListComprehension e1 c1) = List <$> comprehension c1
  where
    comprehension :: [Clause] -> Boa [Value]
    comprehension []              = eval e1 <&> return
    comprehension (If    e2 : c2) = undefined
    comprehension (For x e2 : c2) = undefined
```

> Recall that the result of evaluating a list comprehension should be on the form
> `Boa (List [Value])`, which (because `List [Value]` is itself a `Value`) is just a `Boa
> Value`. When we return from `comprehension` to `eval`, we can `fmap` the constructor `List` onto
> the `Boa [Value]`, which accomplishes exactly this: It turns the result `Boa [Value]` into
> `Boa (List [Value])`.

When we encounter an `If`-expression, the syntax corresponds to `e1 if e2` . So we
know that the expression `e2` should be a Boolean value. Luckily, in Boa as in
Python, non-Boolean values correspond to a Boolean value. E.g., `1==True` is true
and `0==True` is false.The first thing we can do, then, is to evaluate `e2` and find
the corresponding Boolean value with `truthy`.

By `fmap`-ing `truthy` onto the evaluated expression, we get a `Boa Bool`, i.e., a
normal Haskell Boolean inside the Boa monad. By using `do`-notation, then, we can
give `b` the unwrapped Boolean and we can use it directly in an if-statement. If the condition `b` holds,
then evaluate the rest of the list comprehension. Else, there's no more to evaluate.

```haskell
eval (ListComprehension e1 c1) = List <$> comprehension c1
  where
    comprehension :: [Clause] -> Boa [Value]
    comprehension []              = eval e1 <&> return
    comprehension (If e2 : c2) =
      do b <- truthy <$> eval e2
         if b
           then comprehension c2
           else return []
    comprehension (For x e2 : c2) = undefined
```

The case for `For` statements is probably the hardest to read, but it's not that
bad now that we're more familiar with `<$>` and `mapM`.

The corresponding syntax is `e1 for x in e2` so we know that `e2` is the list where
we should bind the variable name `x` to an element, for each iteration.

We evaluate `e2` to `v2`. If `v2` is not iterable, then it's
not possible to use it in a for-statement and we should throw an error. If it's
a list, then we want to
bind each value in `xs` to `x` inside the rest of the comprehension, `c2`.

Recall the following function types.

`comprehension :: [Clause] -> Boa [Value]`
`bind :: VariableName -> Value -> (Boa a -> Boa a)`
`mapM :: (a -> m b) -> [a] -> m ([b])`

We want to do `bind x v (comprehension c2)`, where `v` is each of the values from `xs`. We
already have the variable name `x` and the comprehension `c2`, but `bind` needs a
single value `v`, whereas `xs` is a _list_ of values. Time for some `mapM` magic.

Since `xs` has the type `[Value]`, we can rewrite `mapM` as

`mapM :: (Value -> Boa Value) -> [Value] -> Boa [Value]`

Then `xs` can be the second argument! In order for `bind` to be the first argument,
we can write an anonymous function `(\v -> bind x v (comprehension c2)) :: Value
-> Boa Value`.

Now `mapM (\v -> bind x v (comprehension c2)) xs` does exactly what we wanted it
to do! Except that we're mapping a function that returns a list, over a list,
so the result is a `Boa [[Value]]`. To fix this, we can simply `fmap` the function
`concat` over the result to turn it into a single, flat list and put it inside
the Boa monad. And **then**, we're all done with `eval`!

```haskell
eval (ListComprehension e1 c1) = List <$> comprehension c1
  where
    comprehension :: [Clause] -> Boa [Value]
    comprehension []              = eval e1 <&> return
    comprehension (If e2 : c2) =
      do b <- truthy <$> eval e2
         if b
           then comprehension c2
           else return []
    comprehension (For x e2 : c2) =
      do v2 <- eval e2
         case v2 of
           (List xs) -> concat <$> mapM (\v -> bind x v (comprehension c2)) xs
           _         -> abort $ BadArgument $
                        "Argument " ++ showValue v2 ++ " is not iterable"
```

> If `eval` and `comprehension` look completely crazy to you - especially if you're
> wondering how we can use `e1` inside of `comprehension` - I'd recommend reading
> about _closures_ and _mutually recursive_ functions. It's really powerful stuff, but
> I was definitely confused the first time I saw it!


## Putting It All Together {#putting-it-all-together}

Now we have the following `eval` function.

```haskell
eval :: Expression -> Boa Value
eval (Constant v)              = return v
eval (Variable x)              = look x
eval (Operation o e1 e2)       =
  do v1 <- eval e1
     v2 <- eval e2
     case operate o v1 v2 of
       Right v -> return v
       Left re -> abort $ BadArgument re
eval (Not e)                   = Boolean . not . truthy <$> eval e
eval (Call f es)               = mapM eval es >>= apply f
eval (ListExpression es)       = List <$> mapM eval es
eval (ListComprehension e1 c1) = List <$> comprehension c1
  where
    comprehension :: [Clause] -> Boa [Value]
    comprehension []              = eval e1 <&> return
    comprehension (For x e2 : c2) =
      do v2 <- eval e2
         case v2 of
           (List xs) -> concat <$> mapM (\v -> bind x v (comprehension c2)) xs
           _         -> abort $ BadArgument $
                        "Argument " ++ showValue v2 ++ " is not iterable"
    comprehension (If e2 : c2) =
      do b <- truthy <$> eval e2
         if b
           then comprehension c2
           else return []
```

At this point, all we need is to write `exec` and `execute`!

`exec` should just perform the computations in order and collect the output. It
doesn't need to keep track of which value was just computed, so the return type
of `exec` is just `Boa ()`.

We decided earlier that it should pattern match on
`Define` and `Execute` statements. If it's a definition, then we should evaluate the
expression and bind the variable name to the result in the environment.

```haskell
exec :: Program -> Boa ()
exec []                 = return ()
exec ((Define x e) : s) =
  do v <- eval e
     bind x v (exec s)
exec ((Execute  e) : s) = undefined
```

If it's an execution statement, then we should just evaluate the expression and
keep executing the rest of the program.

```haskell
exec :: Program -> Boa ()
exec []                 = return ()
exec ((Define x e) : s) =
  do v <- eval e
     bind x v (exec s)
exec ((Execute  e) : s) = eval e >> exec s
```

And... That's it! All our hard work when writing `eval` and its helper functions
is finally paying off. This code it short, easy to read, and (relatively) easy
to understand.

Then `execute` is just the function that runs `exec program` in the empty
environment.

```haskell
execute :: Program -> (Output, Maybe RuntimeError)
execute p =
  case run (exec p) [] of
    (Right _, out) -> (out, Nothing)
    (Left re, out) -> (out, Just re)
```

Then we're all done! After writing a driver program (like `Main.hs` in the repo),
you can install this `boa` executable by typing `stack install` in the directory.
Now you can interpret Boa ASTs!


## Contact {#contact}

I appreciate any feedback, comments, corrections, etc. you may have. If that's
the case, you can reach me via my
[GitHub](https://github.com/SophieBosio) or my email at [sophie.bosio@outlook.com](mailto:sophie.bosio@outlook.com). Happy coding!
