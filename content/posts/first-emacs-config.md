+++
title = "Building Your First Emacs Config"
author = ["Sophie"]
date = 2023-12-21T23:29:00+01:00
tags = ["emacs"]
draft = false
+++

[Emacs](https://www.gnu.org/software/emacs/) is a wonderful piece of software. It's easily my favourite program of all
time. It can also be a little overwhelming to configure for the first time.

One of the many things that make Emacs great, though, is its dedicated and
helpful user base. I hope to add a small contribution to that community with
this post, going over what I would have told my past self as I was just starting
out using Emacs and trying to write my first Emacs configuration file.

I'll assume you know the basics of editing with Emacs and won't cover general
concepts like major modes or how to save files in Emacs. I'll primarily be
focusing on how you can get started personalising it.

Before we get into it, I'd like to cover three things: (1) Emacs is a Lisp
interpreter at heart, (2) you should try out pieces of other people's configs,
and (3) there are great places to get help.


## Preamble {#preamble}


### A Primer in Emacs and Elisp {#a-primer-in-emacs-and-elisp}

Emacs is often thought of as a text editor. While it is built around text
editing and has many great packages to do so, I'd argue it's more useful to
think of it as an (Emacs) Lisp interpreter.

Your configuration file is a program of Emacs Lisp (or "elisp" for short) code
that modifies the behaviour of Emacs itself. Most of the behaviours of vanilla
Emacs are [written
in Elisp](https://en.wikipedia.org/wiki/Emacs_Lisp#:~:text=Emacs%20Lisp%20is%20a%20dialect,as%20is%20the%20Lisp%20interpreter.), and the rest in C. Therefore, almost everything that Emacs does, you
can change! You can, of course, add and create new functionality as well.

The power to build your ideal editor is right at your fingertips! As long as
you're willing to learn a little bit of a Lisp dialect.

Thankfully, Elisp is a pretty readable language and as long as you watch your
parentheses, you'll be fine.


### Config Inspo {#config-inspo}

This brings me to my first and (I think) most helpful tip: Get inspired by other
people's configurations.

I'll talk a little later about the basics of using Elisp for your config file,
and that might be useful to help you read and understand the configs, but I
think it's good to have a starting point and to start experimenting early.

If you're only just starting out, I invite you to check out [my config](https://github.com/SophieBosio/.emacs.d) and send
me any questions you might have by [email](mailto:sophie.bosio@outlook.com). (If you're experienced, I invite you to send me
any corrections or improvements!)

Here are some of the configurations I have stolen a lot of my configuration from and
can heartily recommend checking out. I'd also like to recommend Jake Boxerman's
[YouTube channel](https://www.youtube.com/@JakeBox0), covering concise and useful Emacs tips.

-   [Lars Tveito](https://github.com/larstvei/dot-emacs)
-   [Jake Boxerman](https://github.com/jakebox/jake-emacs)
-   [Protesilaos Stavrou](https://github.com/protesilaos/dotfiles)
-   [David Wilson (System Crafters)](https://github.com/daviwil/dotfiles)
-   [Nicolas Rougier](https://github.com/rougier/dotemacs)

Check out the configs piece by piece by copying something into your own config
and testing it out for a while. This is probably what has been the most helpful
to me.

You could also get a lot of useful tips and tricks from checking out some
curated, "sane defaults" builds of Emacs. These are designed to be minimal and
extensible, just configuring some settings to make the initial switch to Emacs a
little smoother and give you a solid foundation for writing your own config.
Most of them don't include too many extra packages, either.
Some of these include:

-   [Prelude](https://github.com/bbatsov/prelude)
-   [Crafted Emacs](https://github.com/SystemCrafters/crafted-emacs)
-   [Purcell's Emacs config](https://github.com/purcell/emacs.d)
-   [Nano Emacs](https://github.com/rougier/nano-emacs)


### Get Help {#get-help}

As mentioned, the Emacs community is very friendly and are often keep to help
out. I hang out a lot on the [Emacs
subreddit](https://www.reddit.com/r/emacs/) and have gotten tons of tips and troubleshooting help from there.

Emacs also has a pretty good documentation and help system. You can take the
Emacs tutorial using `C-h t`. Ask Emacs for help using:

-   `C-h v` to see the documentation for a variable
-   `C-h f` to see the documentation for a function
-   `C-h k` to see what a keybinding does
-   `C-h m` to show help for the current major and minor modes

Or read on the [Emacs Wiki](https://www.emacswiki.org/).


## Basics: Setting Variables &amp; Installing Packages {#basics-setting-variables-and-installing-packages}

Your configuration file is an Emacs Lisp file called `init.el`, located in your
Emacs home directory. Usually, this is at `~/.emacs.d/init.el`. This is where
you'll add your code, and Emacs will load this file at startup.

Emacs can be configured either by modifying default behaviour or by installing
and configuring community packages. You'll probably want to do both.

There are quite a few blog posts and configurations that cover how to set "sane
defaults" --- i.e., modify the default behaviour of Emacs in a way most people
would agree with. Check out the "early init" files or startup sections of the
configs above.

In general, you set the value of a variable using `setq` and that code will often have
this form, where `t`, `nil` `1` and `-1` are common values for activating/deactivating
an option or a mode.

```emacs-lisp
(setq enabled-option  t
	  disabled-option nil

	  enabled-mode     1
	  disabled-mode   -1)
```

After you're done tweaking Emacs' default behaviours, you'll probably need some packages. Many Emacs packages are
available at [MELPA](https://melpa.org/), a package archive with generally high-quality packages. You
might also want to add some other package sources. Here's an example from my
config, where I add quite a few sources and tell Emacs which places to check
first.

```emacs-lisp
(setq package-archives
	  '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
	  package-archive-priorities
	  '(("GNU ELPA"     . 20)
        ("MELPA"        . 15)
        ("ORG"          . 10)
        ("MELPA Stable" . 5)
        ("nongnu"       . 0)))

(package-initialize)
```

You can also point Emacs to look for `.el` files that you have downloaded manually
and saved on your computer. It's practical to put all these files in the same
place and add all of them to your "load path" so Emacs knows where to look for
them.

```text
(defvar local-extensions "~/.emacs.d/local-lisp/")
(add-to-list 'load-path  local-lisp)
(let ((default-directory local-lisp))
  (normal-top-level-add-subdirs-to-load-path))
```

You can install a package from a package source or load an installed package in
your load path using the `require` keyword.

```emacs-lisp
(require 'package-name)
```

This is the easiest way to get started. Add the `require`-line to your `init.el`
file and use `setq` to configure the package.

In fact, at the top of your `init.el` file, you should `(require 'package)`.
`package.el` is the default package manager for Emacs.

<details>
<summary>A note on package management</summary>
<div class="details">

Using `require` and setting all your variables with `setq` statements can lead to a
long and hard-to-read config when you add many packages.

That's why some people prefer using other systems or wrappers around `package.el`,
for example `use-package`, `quelpa`, or `straight`. You'll see this a lot in the
installation instructions for packages. Personally, I use `use-package` because it's so
prevalent and because it's quite convenient.

But don't get too caught up in this, you don't
_need_ to use any of these, and it's not very hard to refactor your code incrementally if you
decide you want to try later.
</div>
</details>

Here's a (simple) example of how to remove some visual elements from vanilla Emacs:

```emacs-lisp
(setq tool-bar-mode     0    ;; Remove toolbar
      scroll-bar-mode   0    ;; Remove scollbars
      menu-bar-mode     0    ;; Remove menu bar
      blink-cursor-mode 0)   ;; Solid cursor, not blinking
```

And here's an example of setting up the [Olivetti](https://github.com/rnkn/olivetti) package:

```emacs-lisp
(require 'olivetti)
(setq olivetti-style t)
```


## Some Package Suggestions {#some-package-suggestions}

I still think you should use bits and pieces of other people's configurations to
figure out what look and feel you prefer for your Emacs experience, but below I
have gathered some packages I like and some alternatives to them.


### Completion Systems {#completion-systems}

The really big one is your **completion system**. Many people use a system to act as
their main entry point to running commands in Emacs, providing auto-completions,
search tools, suggestions, prettification, etc. For example, they can auto-fill
directory and file names when opening a file with `C-x C-f`. In reality, these packages all work quite
differently and are usually combined with other supplementary packages, but
there are a few common ones used as the cores in these systems.
They include:

-   [Vertico](https://github.com/minad/vertico)
-   [Helm](https://github.com/emacs-helm/helm)
-   [Ido](https://www.gnu.org/software/emacs/manual/html_mono/ido.html)

As mentioned, there are many, _many_ packages people pair with these systems. Ido
comes built-in with Emacs, so you might want to start there. I personally use
Vertico and have been very happy with that. I tried Helm, but found it a little
overwhelming and felt I wasn't using it to its full potential. People do love it
though. Play around with them a little, and if you like one, I'd say stick to
it. Jumping around with these is kind of confusing, so I would only consider
switching if I had a problem with the one I was using, or if another one had
some very attractive feature.


### Terminal Emulators {#terminal-emulators}

You can emulate a terminal inside Emacs. I.e., you never have to leave Emacs!

There's a built-in terminal emulator, but it's not particularly nice. Instead, I
would suggest checking out either
[vterm](https://github.com/akermu/emacs-libvterm) or [Eat](https://codeberg.org/akib/emacs-eat).


### Text Editing {#text-editing}

Some packages just make text editing easier.

Take for example auto-completion packages such as the built-in [dabbrev-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html), or
[corfu](https://github.com/minad/corfu) and [company-mode](https://company-mode.github.io/).

Depending on your use, you might find
[multiple-cursors](https://github.com/magnars/multiple-cursors.el) useful.


### De Facto Standards {#de-facto-standards}

[Magit](https://magit.vc/) is the best Git interface I've ever used.

Spell- and syntax-checking is usually done with
[Flyspell](https://www.emacswiki.org/emacs/FlySpell) and [Flycheck](https://github.com/flycheck/flycheck), respectively. These are pretty great.

[Helpful](https://github.com/Wilfred/helpful) gives you better help buffers.

When you start a keybinding combination, [which-key](https://github.com/justbur/emacs-which-key) shows a popup with
suggestions for possible continuations of the sequence.


### More Packages {#more-packages}

For suggestions on more packages to check out,
[awesome-emacs](https://github.com/emacs-tw/awesome-emacs) is a curated and oft-updated package list. I also love browsing
the top posts on the Emacs subreddit.


## Aesthetics: Fonts &amp; Themes {#aesthetics-fonts-and-themes}

The quickest way to personalise!

There are many different ways to set fonts in Emacs. You can also set different
fonts for the Emacs UI, for programming (monospaced or "fixed pitch" fonts) and
for prose editing (regular or "variable pitch" fonts).

Here's how I set my fonts. I use Roboto Mono for the UI and for programming, and
I use Source Sans Pro for my other documents. I check that the relevant font is
installed, and then set their size using the `:height` property.

```emacs-lisp
(when (member "Roboto Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Roboto Mono" :height 108)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono"))

(when (member "Source Sans Pro" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.18))
```

Note that I don't set the height of the programming (fixed pitch) font. It
simply uses the same height as the default font. I do set the size of the prose
(variable pitch) font, though, and I do that by giving a _relative_ size -- it's
relative to the size of the default font.

When it comes to themes, I'd recommend starting with a pack so you can explore
many different options quickly, without needing to install a bunch of theme
packages by hand. A great place to get started, is Doom Emacs' theme pack called
[doom-themes](https://github.com/doomemacs/themes). I particularly like the `doom-nord` theme. You can install and enable
it like this:

```emacs-lisp
(require' doom-themes)
(load-theme 'doom-nord t)
```

I also really like the [Modus themes](https://protesilaos.com/emacs/modus-themes), the
[Ef themes](https://protesilaos.com/emacs/ef-themes) and the [Nano themes](https://github.com/rougier/nano-emacs). You can find many more on [emacsthemes.com](https://emacsthemes.com/).


## Programming {#programming}

[Eglot](https://github.com/joaotavora/eglot) (Emacs polyGLOT) is an LSP server client for Emacs that comes pre-installed from Emacs 29.
You can install it manually in Emacs &gt;26.3.

You can also build Emacs with
[tree-sitter](https://emacs-tree-sitter.github.io/) support. It's available for Emacs &gt;25.1.

Besides this, there are tons of major modes for various languages, usually
called `<language-name>-mode`. These will often give you basic syntax highlighting
and commands.

For example, here's a
simple way to get basic Python 3.11 support:

```emacs-lisp
(require 'python-mode)
(setq python-shell-interpreter "python3.11")
```

Here's my current setup for Haskell (except I've written it using `require`
instead of `use-package`) using Stack:

```emacs-lisp
(require 'haskell-mode)
(add-hook 'haskell-mode 'haskell-doc-mode)
(setq haskell-hoogle-command                  "hoogle"
		haskell-compile-stack-build-command     "stack build"
		haskell-compile-stack-build-alt-command "stack build --pedantic")
(define-key haskell-mode-map (kbd "C-c h")   'haskell-hoogle)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
```


## Hooks {#hooks}

In the Haskell example above, we briefly saw how to add a hook. A hook lets you
run a function each time a specific thing happens. Most of the time, this is
used to execute some function or activate/deactivate a mode when a certain mode
is activated.

For example, in my Haskell config, I want to activate `haskell-doc-mode` each time
I activate `haskell-mode`, so that I always have documentation on hand while I'm
programming in Haskell.

Maybe you want to activate line numbers each time you start programming. To do
that, you can add a hook to `prog-mode` like so:

```emacs-lisp
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
```

You can do so many more things with hooks and get really creative! I mostly use
them to set up my modes the way I like them. E.g., I always center my text
buffers (Org mode, LaTeX, etc.) with Olivetti and I hide the line numbers.


## Binding Keys {#binding-keys}

We can get pretty deep on the subject of keybindings, so I'll try to keep it
brief.

Most major modes with have a dedicated "mode map" of all the keybindings you can
use while in that mode. Remember that you can always check out the currently
available keybindings with `C-h m`, which describes the current major and minor
modes.

This separation is very useful, as it allows you to bind the same keys to
similar commands in different settings. E.g., you can bind `C-c C-c` to the
relevant compilation command in all your programming languages if you like, even
if the actual compilation command is different for each. You can also define
your own custom keymaps and dictate when they should be used (another thing you
can use hooks for!)

In general, you can bind a key globally, locally, or in a specific keymap like
so:

```emacs-lisp
(global-set-key KEY COMMAND)
(local-set-key  KEY COMMAND)
(define-key     KEYMAP KEY COMMAND)
```

Note that the `KEY` must be given in a way Emacs understands. You can use the
`kbd` macro and then provide your keystrokes as a string, e.g., `(kbd "C-c C-c")`.
Navigation keys and the function keys must be surrounded by `<>`, e.g., `<tab>` and
`<F1>`.

Again from the Haskell config, I set keybindings for `haskell-compile` and
`haskell-hoogle` commands in the `haskell-mode-map`.

```emacs-lisp
(define-key haskell-mode-map (kbd "C-c h")   'haskell-hoogle)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
```

You can define keybindings for any command in Emacs. People even build
personalised mnemonic systems. If you want to dive deep into keybindings, I
would recommend checking out the package [Hydra](https://github.com/abo-abo/hydra).


## Conclusion {#conclusion}

Tinkering with my Emacs config is probably my favourite past-time, but even I
will admit it was a bit of a headache in the very, very beginning. I hope this
post has put a small dent in that frustration for you, or maybe given you some
pointers to where you might want to take your WIP config.

I welcome corrections, feedback, and questions by [email](mailto:sophie.bosio@outlook.com). Happy hacking!
