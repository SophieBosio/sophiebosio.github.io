+++
title = "Monads in Haskell"
author = ["Sophie"]
date = 2023-09-15T21:20:00+02:00
tags = ["functional", "programming", "haskell", "monads"]
draft = false
+++

Monads are a special kind of mathematical structure that make our lives as
functional programmers much easier. They allow us to perform _stateful_ computations
in a _functional_ way. In a way, they preserve _context_.

You're bound to have seen a monad if you've ever used Haskell, since the main
function usually uses the `IO` monad. And so does every other function
you'll use that just prints or reads something to/from the console.

But what exactly is a monad? That question has been posed to Google by many
a beginner to functional programming. Usually, the results will include the
formal definition: "A monoid in the category of endofunctors." Which doesn't
really help the average person much.

Personally, I had the most luck starting with _how_ to use monads in functional
programming, and as I got better at using them, I would re-read the definitions
and slowly feel like I started to understand them.

We'll look at a common monad that you might already be familiar with: The `Maybe` monad.

To have a something to re-read and hopefully understand later, let's look at some definitions and introduce some terminology:

> Every **monad** is an **applicative functor**, and every applicative functor is a
> normal **functor**.

So, just like a square is a special rectangle, a monad is a special applicative,
which in turn is a special functor.

Let's start at the lowest level, **functors**, and work our way up.


## Functors {#functors}

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


## Applicatives {#applicatives}

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


## Monads {#monads}

Finally, we get to monads! At this point, remember that you can use all the same
functions on monads as you could on functors and applicative functions.

Functors allow us to _lift_ functions from one context to another, and
applicative functors allow us to lift both functions and an argument while
_preserving_ a context. But the functions were always normal functions without any
context. Monads also allow us to preserve context, but also allow
us to use functions that take a value _without_ context and return a value _with_
context.

A monad has the following operations.

```haskell
class Monad m where
	return :: a -> m a                    -- return, or unit
	(>>=)  :: m a -> (a -> m b) -> m b    -- bind
	(>>)   :: m a -> m b -> m b
	x >> y =  x >>= \_ -> y
	fail   :: String -> m a
	fail msg =  error msg
```

The most interesting function we need for a monad is the "bind" operator, `>>=`.

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

Finally, monads also have to obey some laws.

```haskell
(return x) >>= f == f x                     -- 'return' is left-identity  wrt. '>>='
m >>= return     == m                       -- 'return' is right-identity wrt. '>>='
(m >>= f) >>= g  == m >>= (\x -> f x >>= g) -- '>>=' is associative
```


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
