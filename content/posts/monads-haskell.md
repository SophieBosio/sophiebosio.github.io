+++
title = "Monads in Haskell"
date = 2023-06-08T12:56:00+02:00
tags = ["blog"]
categories = ["tech"]
draft = false
+++

**Please note:** This is a test "post" to see how different formatting in
Org+Hugo works, it's not a proper or finished post :~)

Monads are a special kind of mathematical structure. Technically (category
theoretically) speaking, they can be defined quite succintly as "a monoid in
the category of endofunctors".

But what exactly does that mean? That question has been posed to Google by many
a beginner to functional programming. And that in turn, is because monads are a
very useful structure that actually makes our lives as functional programmer
significantly easier! Once you get past the definition of a monad and you're ready to actually use them, that is.

Personally, I had the most luck starting with _how_ to use monads in functional
programming, and as I got better at using them, I would re-read the definitions
and slowly feel like I started to understand them.

I think it helps to have a pretty clear understanding of what functions _are_,
in the functional programming sense.

<details>
<summary>Functions in Haskell</summary>
<div class="details">

A function in Haskell has an optional signature, with its input and output types.

```haskell
f :: a -> a
f x = x
```
</div>
</details>
