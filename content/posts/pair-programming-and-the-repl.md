+++
title = "Interactive Development: Pair Programming and the REPL"
author = ["Sophie Adeline Solheim Bosio"]
date = 2025-09-23T23:57:00+02:00
draft = false
+++

There is no best way to learn programming, but if I were going to pick a favourite, I'd probably choose pair programming.

In my team at work, we do a fair bit of it. Pairing is different with every partner, so it's difficult to define it or really talk about specifics, and I've had a hard time formulating what I like about it so much.

After thinking about it for a while, I realised that it might be the interactivity of it.

That sounds kind of obvious, but consider the Clojure REPL. If you've ever spoken to a Clojurian (Clojurist?) about their favourite language, the interactive REPL has definitely come up. What's so nice about it?

Well, [a lot of things](https://www.youtube.com/watch?app=desktop&v=i_dUvhEIGBQ), but crucially, it shortens your feedback loop dramatically. No shutting down your program, recompiling, setting up. Just evaluate your code and go. Your program is a running process that you can interact with through the REPL and you can modify it as it's running to immediately see the effects of your change.

But beyond the waiting time, I think having a REPL like Clojure's invites the programmer to prototype, iterate, and _play_. Go ahead, pick some data from your test DB or make a network call. Thread that data into your first function and inspect the result. Realise your approach isn't going to work and use a different function instead. Validate the result and keep building on it.

Hardly anything is mysterious, off-limits, or hard to access - they're all your tools, neatly arranged in your toolbox and ready for you to use. Answer a question, test a hypothesis, or produce an output. You ask your machine to do something and it answers you with data, like a weird but lovely conversation.

Writing software in this way is extremely fun and it has taught me tons. It let's me write my code in small steps where each intermediate result is available for inspection at the press of a keybinding, gaining confidence in the correctness of my approach and letting me quickly pivot when something goes awry. It helps that Clojure as a language is so concise that it never feels like a hassle to type out some extra definitions or function calls just because I'm curious what would happen. And your computer will tell you exactly the result of your idea!

Pair programming to me is kind of like that, but on a higher level.

Instead of talking to a computer which will tell me the result of evaluating and executing some code, I'm talking to another person who will tell me what they think about some piece of a program. That might be architecturally, aesthetically, conceptually or downright concretely. (With some people, you feel like you might as well skip the computer and just get all your answers from them, but that's neither here nor there.)

A pair programming partner isn't really like a REPL (a computer deals with much more concrete problems and has more concrete answers), but the interactive process has a similar feeling. It's rewarding, insightful, and dynamic. And sometimes tough, when you realise some shortcoming in your thinking process.

In a pair programming session, you're almost bound to learn something. Even if you're the more experienced party, being "forced" (in a nice way!) to explain in detail your motivations and reasoning to someone else is a great way of surfacing and making explicit some of that [tacit knowledge](https://elementsofclojure.com/) you're sitting on. Only by teaching and learning can we propagate good practices underpinned by real understanding.

It's well and good for someone to tell you "you should always do A and never do B", but a completely different matter to understand _why_ that's good advice. It's usually only by exploring alternative solutions that you realise what you gain from it and what pain you can avoid by doing it a certain way. And if you really understand it, you'll gain the muscle memory to self-correct and you'll be able to teach others in turn.

For the less experienced party, it is _golden_ to learn these problem solving skills, good habits, and stylistic conventions through practice and in the context of solving a concrete problem. Getting the solution to a math problem and trying to intuit the relationship between the problem and the solution, doesn't help you learn half as much as working on the problem yourself and getting guidance when stuck. In the same way, reading really great code and trying to guess at how one might write such good code is much less useful than writing some bad (or even wrong) code and getting help to make it good. The process of trial and guidance itself is how we learn to solve the math problem or write the good code. It's how we build skill.

They (Navy SEALs, I think?) say "slow is smooth, smooth is fast". Pairing is that!

You don't have to wait for a PR review or an answer to your Slack message. You may discover possibilities you hadn't even considered, or realise that you set out to build the wrong thing. And you'll learn the mechanics of writing high-quality software. You're also collaborating, which makes you stop to consider things more deeply, ask about or explain details you'd otherwise gloss over, challenge assumptions, and be explicit about all the intermediate steps of solving the problem and translating it into code.

Like evaluating code in the REPL and seeing the output, you're asking your pair programming partner a question or suggesting something and getting agreement, pushback, alternatives, or counter-questions.

I fear I've stretched the REPL metaphor a little too far, as a pairing session obviously goes both ways, while my REPL never asks me to evaluate any code on its behalf. Nonetheless, I hope this post conveyed the joy I get from writing programs in an interactive environment!
