+++
title = "Monads in Haskell"
date = 2023-06-08T12:56:00+02:00
tags = ["blog"]
categories = ["tech"]
draft = false
+++

**Please note:** This is a test "post" to see how different formatting in
Org+Hugo works, it's not a proper or finished post :~)

Monads are a special kind of mathematical structure that make our lives as
functional programmers much easier. They allow us to do _stateful_ computations
in a _functional_ way.

You're bound to have seen a monad if you've ever used Haskell, since the main
function usually has the signature `IO ()`. And so does every other function
you'll use that just prints or reads something to/from the console.

But what exactly is a monad? That question has been posed to Google by many
a beginner to functional programming. Usually, the results will include the
formal definition: "A monoid in the category of endofunctors." Which doesn't
really help the average person much.

Personally, I had the most luck starting with _how_ to use monads in functional
programming, and as I got better at using them, I would re-read the definitions
and slowly feel like I started to understand them.

To have a something to re-read and hopefully understand later, let's look at some definitions and introduce some terminology:

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">

Every **monad** is an **applicative functor**, and every applicative functor is a
normal **functor**.

</div>

Let's start at the lowest level, **functors**, and work our way up.


## Functors {#functors}

To understand functors, it's useful to have a look at a simple function in Haskell. How about
addition?

The built-in function is the infix `+` operator, but to be clear, let's define
our own `add` function:

```haskell
add :: Num a => a -> a -> a
add x y = x + y
```

Now I think we're ready to look at our first functor, `Maybe`. If you've worked with
Haskell before, you might already be familiar with how it works.

Essentially, it allows you to return `Nothing` for a computation that for some
reason has no return, and otherwise wrap a real result in `Just`. This is
really useful if we want to deal with functions that _may_ fail. In that case,
we can return `Nothing` when it fails and `Just result` if it succeeds.

`Maybe` is a _type constructor_, meaning it's not a type in and of itself, but
you can pass it a real, concrete type to construct a new type. If we try to
check the type of `Maybe`, we therefore get an error:

```haskell
ghci> :t Maybe

<interactive>:1:1: error:
    * Illegal term-level use of the type constructor `Maybe'
        imported from `Prelude' (and originally defined in `GHC.Maybe')
    * In the expression: Maybe
```

On the other hand, we can check its _kind_:

```haskell
ghci> :k Maybe
Maybe :: * -> *
```

This indicates how we can use it. It takes exactly one concrete type, like
`String` or `Int`, and returns a concrete type, like `Maybe String` or `Maybe
Int`.

An instance of the type `Maybe Int` could be for example `Just 3` or `Nothing`.
Notice that in `Just 3`, the normal integer `3` is "wrapped" inside of the
`Just`. What this means, is that we can't use `Just 3` exactly like we would
use `3`.

Let's define our `add` function and try to use it on normal ints, and then on
some `Maybe` ints.

```haskell
ghci> add x y = x + y
ghci> add 3 5
8
ghci> add 5 (Just 3)

<interactive>:5:1: error:
    * No instance for (Num (Maybe Integer)) arising from a use of `it'
    * In the first argument of `print', namely `it'
      In a stmt of an interactive GHCi command: print it
```

To use a value that has been wrapped, or "lifted", by a functor, we need to use
a special function called `fmap`. This is how the Functor typeclass is defined
in Haskell:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Basically, it says "give me a normal function and a wrapped value, and I'll
unwrap the value, apply the function, and wrap up the result again".

What should happen with our `Maybe Int` then, is that we can give it a normal
function (like `add 5`) and a wrapped value (like `Just 3`) and it should
perform the computation as we would expect (`add 5 3`) and wrap up the result
for us (`Just 8`).

```haskell
ghci> fmap (add 5) (Just 3)
Just 8
```

Wow, great! Now we know how to apply a function to a wrapped value.

<details>
<summary>A note on lifting functions</summary>
<div class="details">

You can think of this as described above, that `fmap` brings the wrapped values "down" to unwrapped values to perform
computations and then bring the result back "up".

But what it actually does, is "lifting" the normal function "up" to the wrapped
values so that it can perform computations on the wrapped values directly.

I wanted to mention it, but please think of it in the way that makes most sense to you.
</div>
</details>

Now that we have seen `fmap` in action, I think we're ready to see how the `Maybe`-instance of `Functor` is defined:

```haskell
instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

This means that if we try to apply the normal function `f` to `Nothing`, then
the result is always just `Nothing`. I.e., if we try to add `5` to `Nothing`, we
actually get `Nothing` back. That's because `Nothing` is not equivalent to 0, but rather
to some computation having failed earlier. For example, if we tried to divide
by zero earlier and got `Nothing` as a result, it doesn't really make sense to
add 5 to it and expect an actual result.

But if we try to apply the normal function `f` to `Just x` where `f` is defined
for the type of `x`, then we should apply `f` to `x` and wrap up the result in
a `Just`. I.e., if we try to add `5` to
