+++
title = "Monads in Haskell"
author = ["Sophie"]
date = 2023-09-15T21:20:00+02:00
tags = ["functional", "programming", "haskell", "monads"]
draft = false
+++

Monads are extremely useful and notoriously hard to wrap your head around the
first time around. The formal definition, "a monoid in the category of
endofunctors",  doesn't really help the average person much.

A monad is essentially a way to perform a computation with some added
_context_ in a functional way. Personally, I had the most luck starting with _how_
to use monads in functional programming, and as I got better at using them, I
would re-read the definitions and slowly feel like I started to understand them.

First, I'll give a brief introduction to monads, using the `List` monad as an example.

Then, to explain why we might want to use monads, I'll reproduce a famous example:
Using the Writer monad to perform logging. I'll be borrowing heavily
[this intro](https://www.youtube.com/watch?v=C2w45qRc3aU) to monads by Studying With Alex on YouTube.

And finally, to cover more of the theory behind monads, we'll have a look at the `Maybe`
monad in Haskell.


## Wait, List is a Monad? {#wait-list-is-a-monad}

Yes! Remember that a monad is a structure that lets us perform a computation with some added
_context_. For a list, that context is what _elements_ are in the list (including
duplicates) and their _order_.

Let's say you want to map a function `f` over a list `xs`. All well and great, but
you can't just do it any way you like: You have to map the function over _all_ the
elements and preserve their order! So somehow, the context of the list carries
over into the next computation you want to do on it.

In general, I find it useful to think of monads as "containers" or boxes
around values. In this case, the list `[...]` is a container and the elements are
the values.

If you take a normal integer, that value isn't a (singleton) list by itself.
E.g., wrapping `5` isn't a list, but `[5]` is. To take an integer like `5` and make it
into a singleton list, we have to "wrap it" in square brackets. In general this
function is called _return_.

The containers, like `[]`, also prevent us from accessing the values directly, so
we have to "unwrap" them somehow. E.g., I can't do
`[5] + 3` because I'm trying to add a list of integers to an integer, but I
_can_ do `map (+ 3) [5]`, because now I'm adding an integer to another integer. This
function is called _bind_ and is written in Haskell with the symbol `>>=`.

Note that when we are "extracting" a value in this way, we _must_ pass it to a
function that promises to put it back in to the monad. There are other ways to
just take out the value and use it normally, but then we also lose the context.
I.e., if I do `([5, 6, 7] !! 0) + 3`, I can extract `5` without any additional
context. Then I can add it to `3` and get `8` and that's fine, but now the rest of
the list is "gone" and I have no way of putting `8` into the list instead of `5`.

So, monads allow us to perform computations with context in a functional way.
What's so special about that? Well, the number of elements and ordering of the list, depends on the
list itself! So, it's almost like the list has a _state_. But we normally can't do _stateful_ computations in a _functional_ language, right?
And yet, it's still functional, because all the computations on lists return the same
results each time they are called _in the same context_, i.e., on the same list.

> For later, remember that a monad needs a way to put a normal value _into_ the
> monad, and a way to extract a normal value _out of_ the monad to pipe it to
> another function, that must eventually put it back into the monad.

Normal values can be "put inside" a monad by wrapping them with some extra
context. In a way, then, `return` is the function that takes a normal value `a` and
returns that value with some extra context `m` and gives us the new value `(a, m)`.
Now we can't just access `a` directly anymore, we need to first take it out of
this context wrapper and then _bind_ it to a function that puts it back.

{{< figure src="/img/monad-1.png" link="/img/monad-1.png" >}}


## Writer Monad to the Rescue! {#writer-monad-to-the-rescue}

Now we'll have a look at the [Writer](https://hackage.haskell.org/package/mtl/docs/Control-Monad-Writer.html) monad, a commonly used monad
that will hopefully illustrate the concepts more clearly.

**Please note:** This example is common, but my particular implementation borrows heavily from
[this intro](https://www.youtube.com/watch?v=C2w45qRc3aU) to monads by Studying With Alex on YouTube, and I would recommend
that video!

The **Writer** monad is often used for logging.

Let's say you have a function that adds two ints and a function that squares
ints. Then you want to add a log statement for each computation, so that you can
review the log later.

In an imperative language, you might add a global
variable, but then all your functions depend on this one variable. You could
pass the list as argument and return it as output, but then your `add` function
suddenly also takes a weird, extra log argument. Instead, you
could return a tuple, containing first the result of the computation and then
the log so far. We could do that in Haskell as well. Let's define a new type
`LoggedInt` that contains an integer and a list of strings, which will be our
log. Then `add` and `square` can return this type!

```haskell
type LoggedInt = (Int, [String])

add :: Int -> Int -> LoggedInt
add n m = (n + m, ["Added " ++ show n ++ " to " ++ show m])

square :: Int -> LoggedInt
square n = (n^2, ["Squared " ++ show n])
```

But now, you can't really chain these together, because both functions take
normal ints and return these weird `LoggedInt` values.

```shell
ghci> square(add 2 4)

<interactive>:11:8: error:
    • Couldn't match type ‘(Int, [String])’ with ‘Int’
      Expected: Int
        Actual: LoggedInt
    • In the first argument of ‘square’, namely ‘(add 2 4)’
      In the expression: square (add 2 4)
      In an equation for ‘it’: it = square (add 2 4)
```

Also, there is no way to access and extend the previous log, since each
computation just returns the singleton list with their log statement.

```haskell
ghci> add 2 4
(6,["Added 2 to 4"])

ghci> square(2)
(4,["Squared 2"])
```

Of course, we could make both the functions take `LoggedInt` values. But that's
pretty much equivalent to passing the list as argument and returning it as output.

```haskell
type LoggedInt = (Int, [String])

add :: LoggedInt -> LoggedInt -> LoggedInt
add (n, log1) (m, log2) =
  (n + m, log1 ++ log2 ++ ["Added " ++ show n ++ " to " ++ show m])

square :: LoggedInt -> LoggedInt
square (n, log) =
  (n^2, log ++ ["Squared " ++ show n])
```

What to do? Well, if we squint, we can see that we have another context (the
previously logged material) that we want to use in our computation, and we want
to return a result that has the updated context (the old logs plus the new log
statement)!

This is exactly the kind of thing monads are built for! Let's implement the
Writer monad from scratch together.

The type of monad we want, is basically an instance of the `Writer` monad in
Haskell. Let's call ours `LogWriter`.

The results of our computations are going to be inside the `LogWriter` monad from now on.
Right off the bat, we want a way to take a computation wrapped in the monad and
_unwrap_ it. That's usually called _running_ the monad, so let's define
`LogWriter` with a function called `run` that just unwraps the computation and
gives us the result and the log as a pair.

```haskell
newtype LogWriter l a = LogWriter { run :: (a, l) }
```

`l` is the type of the logs and `a` is the type of the result of the
computation. Since we want to be able to concatenate (possibly empty) logs, `l`
must be a member of the `Monoid` type class.

Let's take the old versions of our functions, that take normal integers and
return integers with logs, and rewrite them so return values inside the `LogWriter` monad. The
`LogWriter` contains the list of strings (logs) and an integer (result).

```haskell
newtype LogWriter l a = LogWriter { run :: (a, l) }

add :: Int -> Int -> LogWriter [String] Int
add n m = undefined

square :: Int -> LogWriter [String] Int
square n = undefined
```

Like we saw earlier, a monad needs a way of putting a normal value (here, an int) into the
container (here, an int with a log). In Haskell, this is called `return`. We want to find
the "simplest", most straightforward way of taking a normal value and putting
into the monad.

In our case, that would be returning the value and the empty
list. But since we specified that `l` should be a monoid, and not a list in
particular, we can't use `[]` for the empty list. Instead, we can use `mempty`
which corresponds to the empty list, but works for all monoids!

```haskell
instance (Monoid l) => Monad (LogWriter l) where
  return a = LogWriter (a, mempty)
```

Then we need a way to extract a value _out of_ the monad, so that we can use it to
perform computations (such as squaring it) and then putting it _back into_ the
monad. Unwrapping, computing, and wrapping! This is called the "bind" operator
and in Haskell, it is written as `>>=`.

> Note that it takes a wrapped value `m a` and a function that takes a _normal_
> value and returns a _wrapped_ value again `(a -> m b)`. So we can't just take
> any normal function `a -> b` and bring it into monad-land using the bind
> operator - but we'll see later that there is a function that does exactly that,
> called `fmap`!

Of course, what we want to do is to apply `f` to the value `a` inside the monad,
and return the `result` of that computation. Additionally, we want to take any
new output and append it to the existing logs, and return that as well.

```haskell
instance (Monoid l) => Monad (LogWriter l) where
  return a = LogWriter (a, mempty)
  (LogWriter (a, logs)) >>= f =
    let (LogWriter (result, output)) = f a
    in   LogWriter (result, logs <> output)
```

Finally, every monad is an applicative functor, and in turn a normal functor.
Therefore, we need to make our monad an instance of those typeclasses as well.
We'll cover how this works in the section about `Maybe`, but for now, you can
copy these instance declarations to make GHC happy and accept our `LogWriter`
into the monad family.

```haskell
import Control.Monad

instance (Monoid l) => Applicative (LogWriter l) where
  pure  = return
  (<*>) = ap

instance (Monoid l) => Functor (LogWriter l) where
  fmap  = liftM
```

Now we can rewrite `add` and `square` to return values inside the `LogWriter`
monad!

```haskell
import Control.Monad

newtype LogWriter l a = LogWriter { run :: (a, l) }

instance (Monoid l) => Monad (LogWriter l) where
  return a = LogWriter (a, mempty)
  (LogWriter (a, logs)) >>= f =
    let (LogWriter (result, output)) = f a
    in   LogWriter (result, logs <> output)

instance (Monoid l) => Applicative (LogWriter l) where
  pure  = return
  (<*>) = ap

instance (Monoid l) => Functor (LogWriter l) where
  fmap = liftM

add :: Int -> Int -> LogWriter [String] Int
add n m = LogWriter (n + m, ["Added " ++ show n ++ " to " ++ show m])

square :: Int -> LogWriter [String] Int
square n = LogWriter (n^2, ["Squared " ++ show n])
```

And finally, perform monadic operation that keep track of the log the whole way!
Since the result of `add` and `square` is now values within the `LogWriter`, we
have to remember to `run` the monad so we get a nice, printable result. `run` gives
us the final output _with_ the context, after all the computations are done.

```shell
ghci> add 5 3   # Gives us values INSIDE the monad!

<interactive>:126:1: error:
    • No instance for (Show (LogWriter [String] Int))
        arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it

ghci> run (add 5 3)
(8,["Added 5 to 3"])

ghci> run (square 2)
(4,["Squared 2"])
```

We can even chain computations using the `>>` operator, which lets us perform an
operation, ignore the result, and perform the next operation. In these two examples, we see the
result of the last computation in the chain, `add 4 5`, and the total log.

```shell
ghci> run $ add 6 3 >> square 2 >> add 4 5
(9,["Added 6 to 3","Squared 2","Added 4 to 5"])

ghci> run $ add 5 10 >> square (fst $ run $ add 3 2) >> square 2 >> add 4 5
(9,["Added 5 to 10","Squared 5","Squared 2","Added 4 to 5"])
```

Now, we have a normal function like `+` between normal values and
a "stateful" function like `add` from normal values to wrapped values. As an
example, we can picture `run $ add 4 5 >>​= square` like this:

{{< figure src="/img/monad-2.png" link="/img/monad-2.png" >}}


## `Maybe` We'll Get A Value {#maybe-we-ll-get-a-value}

Time for some theory! Hopefully, with the context of the previous examples, this
will feel mostly familiar.

As mentioned, we'll be looking at _applicatives_ and _functors_. Let's look at some definitions and introduce some terminology:

> Every **monad** is an **applicative functor**, and every applicative functor is a
> normal **functor**.

So, just like a square is a special rectangle, a monad is a special applicative,
which in turn is a special functor.

Let's start at the lowest level, **functors**, and work our way up.


### Functors {#functors}

Formally, a functor is a transformation that maps all the objects (values) in a category
to objects in another, and all the morphisms (functions) in a category to
morphisms in another.

For example, the `Maybe` functor applied the type `Int` maps regular integers like `5`
to `Just 5` or `Nothing`, and maps functions that work on `Int` s to functions that
work on `Maybe Int` s.

In Haskell, I usually think of values as _wrapped_ or _unwrapped_ values.

An _unwrapped_ value, is just the value as we typically think of them. E.g., `5`.
A _wrapped_ value, has a "container" that prevents us from using them as normal.
E.g., `Just 5`.

Then we need to do something special to use the functions on the wrapped values.
For functors, this is `fmap`. It "lifts" a normal function up to functor territory
so we can apply them to values. Or, if you like, it unwraps the value, applies
the function, and wraps it back up.

Here's how a functor is defined in Haskell.

```haskell
class Functor f where
	fmap :: (a -> b) -> f a -> f b
```

So all we need to do to make something an instance of the Functor typeclass, is
define a way to lift a function `f` to apply it to the wrapped values!

Here's the `Maybe` functor instance in Haskell:

```haskell
instance Functor Maybe where
	fmap f Nothing  = Nothing
	fmap f (Just x) = Just (f x)
```

I.e., applying a function `f` to a `Nothing` value, will always give you `Nothing`.
Applying a function `f` to `Just x`, is the same as applying `f` to `x`, and then
putting the result back into `Just`.

Trying to use a normal function directly on a wrapped value, results in an
error. But it works just as you might expect when you `fmap` the function onto the value.

```shell
ghci> (+2) (Just 5)

<interactive>:1:1: error:
    • Non type-variable argument in the constraint: Num (Maybe a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall {a}. (Num a, Num (Maybe a)) => Maybe a


ghci> fmap (+2) (Just 5)
Just 7
```

These are the two functor laws, which hopefully make sense. They state that
`fmap`-ing a the identity function is just the identity function, and that
`fmap`-ing two composed functions is the same as `fmap`-ing each function and then
composing them.

```haskell
fmap id      = id                -- Identity
fmap (g . f) = fmap g . fmap f   -- Composition
```

> Note that you can use `fmap` in its infix form, which is `<$>`.
>
> `(+2) <$> (Just 5)   -- ==> (Just 7)`

Since it's a mapping from objects to objects and functions to functions, all
instance of the `Functor` typeclass in Haskell are data structures that can be
_mapped over_, such as lists, trees, `Maybe` and `Either`. Some of them you can find below:

<details>
<summary>More functors for the interested!</summary>
<div class="details">

Lists are probably the most commonly used Functor instance.

```haskell
instance Functor [] where
	fmap = map
```

Trees can be mapped over by applying `f` to each node recursively.

```haskell
instance Functor Tree where
	fmap f Leaf = Leaf
	fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
```

`Either` is defined in a curried way, because it takes two arguments.

```haskell
instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left  x) = Left x
```

The `IO` monad, since it's also a functor, has an `fmap` defined this way.

```haskell
instance Functor IO where
	fmap f action = do
		result <- action    -- perform an IO action and save the result
		return (f result)   -- apply f to the result of the IO action,
		--                     which is itself an IO action
```

And functions are actually also functors! `fmap` on the function arrow `->` looks a
little strange, but it does all make sense.

```haskell
instance Functor ((->) r) where
	fmap f g = (\x -> f (g x))
```

But notice how the type of this function, if you replace all the `f` s in the type
of `fmap` with `r ->`, gives us `fmap :: (a -> b) -> (r -> a) -> (r -> b)`.Notice also
how we now have a function `r -> a` and a function `a -> b` and we end up with a
function `r -> b`. So this is actually just function composition!

```haskell
instance Functor ((->) r) where
	fmap = (.)
```
</div>
</details>


### Applicatives {#applicatives}

An applicative functor has more structure than a regular functor, but less than
a monad.

It lets us unwrap both a function _and_ a parameter. For normal functors, we could
only unwrap the parameter, but needed a normal function.

In Haskell, the Applicative typeclass is defined like this.

```haskell
class (Functor f) => Applicative f where
	pure  :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
```

`pure` is similar to the `return` function: It takes a value and wraps it in the
applicative functor.

The `<*>` ("application") operator allows us to unwrap both the function `a -> b` and the parameter
`a`, and finally wrap the result up to get `f b`.

Basically, an Applicative functor allows us to take in a function and a
parameter in a given context (`f`) and perform the function application while
_preserving_ that context (`f b`).

Note that the following three lines are equivalent.

````haskell
```haskell
pure f   <*> x <*> y <*> ...
fmap f x <*> y <*> ...
f  <$> x <*> y <*> ...
````

These are the laws for applicative functors.

````haskell
pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
````

Lists, `Maybe`, `IO`, functions, and a bunch of other functors are also applicative
functors.

<details>
<summary>More applicative functors.</summary>
<div class="details">

```haskell
instance Applicative Maybe where
	pure                   = Just
	Nothing <*> _          = Nothing
	(Just f) <*> something = fmap f something


instance Applicative [] where
	pure x    = [x]
	fs <*> xs = [f x | f <- fs, x <- xs]


instance Applicative IO where
	pure    = return
	a <*> b = do
		f <- a
		x <- b
		return (f x)


instance Applicative ((->) r) where
	pure x  = (\_ -> x)
	f <*> g = \x -> f x (g x)


instance Applicative ZipList where
	pure x                    = ZipList (repeat x)
	ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```
</div>
</details>


### Monads {#monads}

Finally, we get to monads! At this point, remember that you can use all the same
functions on monads as you could on functors and applicative functions.

Functors allow us to _lift_ functions from one context to another, and
applicative functors allow us to lift both functions and an argument while
_preserving_ a context. But the functions were always normal functions without any
context. Monads also allow us to preserve context, but also allow
us to use functions that take a value _without_ context and return a value _with_
context.

A monad has the following operations. A minimal complete definition requires us
to write return and bind ourselves.

```haskell
class Monad m where
	return :: a -> m a                    -- return, or unit
	(>>=)  :: m a -> (a -> m b) -> m b    -- bind
	(>>)   :: m a -> m b -> m b
	x >> y =  x >>= \_ -> y
	fail   :: String -> m a
	fail msg =  error msg
```

The most interesting function we need for a monad is the bind operator, `>>=`.

Compare this to `fmap` and `<*>`:

```haskell
fmap  :: (Functor     f) => (a -> b) -> f a -> f b
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
(>>=) :: (Monad       m) => m a -> (a -> m b) -> m b
```

Notice that the second argument to the bind operator, is a function
from a normal value `a` to a value with context `m b`.

I think the relationship between the different functions is especially clear when we look at the flipped version of the bind operator, `=<<`.

```haskell
(<$>) :: Functor     f =>   (a ->   b) -> f a -> f b
(<*>) :: Applicative f => f (a ->   b) -> f a -> f b
(=<<) :: Monad       m =>   (a -> m b) -> m a -> m b
```

To see how we might use the bind operator, though, we can write a function that
works on `Maybe Int` values, which halves the integer value inside it.

```haskell
halve :: Int -> Maybe Int
halve x = if evel x
  then Just (x `div` 2)
  else Nothing
```

Then we can pass it an `Int` value, but if we want to pass it a `Maybe Int` we
have to use the bind operator.

```shell
ghci> halve 4
Just 2

ghci> halve (Just 4)

<interactive>:1:1: error:
    • Non type-variable argument in the constraint: Integral (Maybe a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall {a}. (Integral (Maybe a), Num a) => Maybe (Maybe a)

ghci> (Just 4) >>= halve
Just 2
```

My thesis supervisor taught me this great trick for remembering how the bind
operator works. It's like a toilet plunger (and it looks a little like it, too)! It extracts the value out of the
monad, but then the function you pass it to has to be responsible for putting
it back into the monad.

There are a bunch of functions from the `Functor` and `Applicative` typeclasses that
have equivalent `Monad` functions.

```haskell
pure   :: (Applicative f) => a -> f a
return :: (Monad       m) => a -> m a

(<*>)  :: (Applicative f) => f (a -> b) -> f a -> f b
ap     :: (Monad       m) => m (a -> b) -> m a -> m b

fmap   :: (Functor     f) => (a -> b) -> f a -> f b
liftM  :: (Monad       m) => (a -> b) -> m a -> m b
```

And many more! But what's nice about this, is that if we import `Control.Monad` in
our file, we can define a `Monad` instance quite easily by setting the `Applicative`
and `Functor` functions to be equal to these monadic functions. For example, we
can implement the list monad like this.

```haskell
import Control.Monad

instance Monad [] where
  return a = [a]
  xs >>= f = concat $ map f xs

instance Applicative [] where
  pure  = return
  (<*>) = ap

instance Functor [] where
  fmap = liftM
```

Finally, monads also have to obey the monad laws.

```haskell
(return x) >>= f == f x                     -- 'return' is left-identity  wrt. '>>='
m >>= return     == m                       -- 'return' is right-identity wrt. '>>='
(m >>= f) >>= g  == m >>= (\x -> f x >>= g) -- '>>=' is associative
```


## Picturing A Monad {#picturing-a-monad}


## Conclusion &amp; Further Reading {#conclusion-and-further-reading}

That's it for the crash course in monads!

It's also really interesting to read about the `Monoidal` typeclass, and how you
can implement a monad using either `(>=>)` or `join` instead of the bind operator.
Bartosz Milewski has a really great [lecture on YouTube about monads](https://youtu.be/gHiyzctYqZ0?si=Pt9eCaT7Z4WJiJnX) where he explains this.

There are several cool functions on monads and friends, that let you chain
together computations in a really neat, clean way. For example, my life got
easier when I started using `<&>`, `<$`, `<*` and [similar functions](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Monad.html). Not to mention
do-notation!

And finally, all this learning and reading about monads really becomes useful
when you use them in projects. To get started, check out the [Reader](https://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader.html), [Writer](https://hackage.haskell.org/package/mtl/docs/Control-Monad-Writer-Strict.html) and
[Reader Writer State](https://hackage.haskell.org/package/mtl/docs/Control-Monad-RWS-Strict.html) monads.

I've had a lot of fun learning how to use monads to write parsers and
interpreters.

> I'd appreciate any feedback, comments, corrections, etc. that you might have!
> Happy coding!
