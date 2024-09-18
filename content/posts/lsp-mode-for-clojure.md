+++
title = "Setting Up Emacs LSP-Mode For Clojure"
author = ["Sophie Adeline Solheim Bosio"]
date = 2024-09-17T22:42:00+02:00
tags = ["emacs"]
draft = false
+++

I use [Clojure](https://clojure.org/) on the backend [where I work](https://www.ardoq.com/). I think it's really fun, especially in combination with Emacs, since Emacs is built with Emacs Lisp and Clojure is a Lisp dialect. So many of Emacs' default keybindings started clicking for me once I started working with a language in the same tradition, editing structurally and by semantic units rather than line-by-line or character-by-character. I'm not alone in loving both and there's clearly a lot of overlap between the two communities, which is evident from the number of Clojure packages for Emacs.

The probably most famous and popular among these is the excellent package [CIDER](https://cider.mx/), which provides a full-fledged development environment for Emacs with support for interactive development. CIDER has a ton of features for evaluating, debugging, and testing your code. However, you might want to supplement CIDER with other packages for things such as peeking function argument lists, navigating your code, and finding references/implementations. Emacs has some built-in documentation (`eldoc`), project management (`project`) and reference-finding capabilities (`xref`). But when you need more, [clojure-lsp](https://clojure-lsp.io/) is your friend and the LSP client for Emacs [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) integrates pretty well with the aforementioned built-ins.


## Basic Setup {#basic-setup}

With `clojure-lsp` installed, you can use your preferred package manager to get the Emacs package `lsp-mode`. I use `use-package`. `lsp-mode` is pretty good at integrating with the built-in Emacs features, so I'll let it autoconfigure, which it does by default. I'll defer the package loading to shave off some precious milliseconds at Emacs' startup.

```emacs-lisp
(use-package lsp-mode
  :defer t)
```

Let's add some hooks to start up `lsp-mode` when I'm programming in Clojure. I'll also enable the [which-key](https://github.com/justbur/emacs-which-key) integration.

```emacs-lisp
(use-package lsp-mode
  :defer t
  :hook ((clojure-mode  . lsp)
         (clojurec-mode . lsp)
         (lsp-mode      . lsp-enable-which-key-integration)))
```


## Custom Bindings {#custom-bindings}

Next, I'll bind some keys. The LSP server can suggest code actions and to execute these quickly, I'll bind the command `lsp-execute-code-action` to `M-<return>`. `xref` works pretty well in Clojure and lets you jump to the definition of a symbol with `M-.` (and jump back by popping the ref off the stack with `M-,`), so to find references instead of the implementation, I'll bind `lsp-find-references` to `C-M-.`.

```emacs-lisp
(use-package lsp-mode
  :defer t
  :hook ((clojure-mode  . lsp)
         (clojurec-mode . lsp)
         (lsp-mode      . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("C-M-."      . lsp-find-references)
              ("C-c r"      . lsp-rename)
              ("M-<return>" . lsp-execute-code-action)))
```

Then, `C-M-.` pops up an `xref` buffer where I can see all references to a function, in which I can select an entry and jump to the call site.

{{< figure src="/img/lsp-xref.png" link="/img/lsp-xref.png" >}}

I can also press `C-c r` to rename a symbol as you'd expect.

{{< figure src="/img/lsp-rename.png" link="/img/lsp-rename.png" >}}

`M-<return>` gives me a minibuffer with available code actions.

{{< figure src="/img/lsp-code-actions.png" link="/img/lsp-code-actions.png" >}}

> As a quick note, the code actions show up in a regular minibuffer. I just use [vertico](https://github.com/minad/vertico) with [vertico-posframe](https://github.com/tumashu/vertico-posframe), which gives me a nice childframe for mine.
>
> In case you're interested, you can get rounded corners (on Mac OS) and fringes (Mac OS &amp; Linux) by setting the variable `vertico-posframe-paramters` with the following settings:
>
> ```emacs-lisp
> (setq vertico-posframe-parameters '((left-fringe  . 12)
>                                     (right-fringe . 12)
>                                     (undecorated  . nil)))
> ```


## Diagnostics {#diagnostics}

LSP mode automatically tries to figure out which diagnostics tool you're using, but I ran into some strange problem with the `:auto` setting when using [Flycheck](https://www.flycheck.org/en/latest/), so I set it to use Flycheck manually.

````emacs-lisp
(use-package lsp-mode
  :defer t
  :hook ((clojure-mode  . lsp)
         (clojurec-mode . lsp)
         (lsp-mode      . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("C-M-."      . lsp-find-references)
              ("C-c r"      . lsp-rename)
              ("M-<return>" . lsp-execute-code-action))
  :config
  (setq lsp-diagnostics-provider :flycheck))
````

This gives me the regular Flycheck indicators (the arrows) that something's wrong, but also information in the echo area and in a popup childframe on mouse hover.

{{< figure src="/img/lsp-unused-var.png" link="/img/lsp-unused-var.png" >}}


## Customisation {#customisation}

From this point onwards, the LSP server works really well on my machine for Clojure. However, `lsp-mode` comes with a few more bells and whistles than I'd like, so I'll finish by deactivating some features. A tour of on-by-default features and guide on how to disable them can be found [here](https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/).

````emacs-lisp
(use-package lsp-mode
  :defer t
  :hook ((clojure-mode  . lsp)
         (clojurec-mode . lsp)
         (lsp-mode      . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("C-M-."      . lsp-find-references)
              ("C-c r"      . lsp-rename)
              ("M-<return>" . lsp-execute-code-action))
  :config
  (setq lsp-diagnostics-provider :flycheck)
        ;; Disable visual features
  (setq lsp-headerline-breadcrumb-enable nil   ;; No breadcrumbs
        lsp-ui-sideline-enable           nil   ;; No sideline
        lsp-lens-enable                  nil   ;; No lenses

        ;; Disable all mode line features, since I use a custom mode line
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable  nil

        ;; Limit raising of the echo area to show docs
        lsp-signature-doc-lines 3))
````

With all this, I get a lot of nice LSP features. In conjunction with CIDER, this is starting to feel really smooth.

For example, the function's docstring in the echo area when it's at point.

{{< figure src="/img/lsp-lookup-signature.png" link="/img/lsp-lookup-signature.png" >}}

LSP-mode also plays nicely with my completion system, here using [corfu](https://github.com/minad/corfu).

{{< figure src="/img/lsp-corfu.png" link="/img/lsp-corfu.png" >}}


## Performance Improvements {#performance-improvements}

And finally, LSP-mode works a little bit faster if we set it to use plists for serialisation. We can set this in the `:init`-block of the `use-package` declaration.

````emacs-lisp
:init (setq lsp-use-plists t)
````

Note that if you want to do this, you also need to add `(setenv "LSP_USE_PLISTS" "true")` to your `early-init.el`. See [the performance part](https://emacs-lsp.github.io/lsp-mode/page/performance/) of the `lsp-mode` documentation for more information.


## Final Configuration &amp; Further Reading {#final-configuration-and-further-reading}

And here's the final configuration that I'm using for Clojure as of September 2024.

````emacs-lisp
(use-package lsp-mode
  :defer t
  :init (setq lsp-use-plists t)
  :hook ((clojure-mode       . lsp)
         (clojurec-mode      . lsp)
         (lsp-mode           . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("C-M-."      . lsp-find-references)
              ("C-c r"      . lsp-rename)
              ("M-<return>" . lsp-execute-code-action))
  :config
  (setq lsp-diagnostics-provider :flycheck)
        ;; Disable visual features
  (setq lsp-headerline-breadcrumb-enable nil   ;; No breadcrumbs
        lsp-ui-sideline-enable           nil   ;; No sideline
        lsp-lens-enable                  nil   ;; No lenses

        ;; Disable all mode line features, since I use a custom mode line
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable  nil

        ;; Limit raising of the echo area to show docs
        lsp-signature-doc-lines 3))
````

The next thing I would consider to enhance my Clojure coding experience even further, is incorporating structural editing. Clojure, being a LISP dialect and therefore pretty close to an abstract syntax tree, is especially well-suited to structural editing. [paredit](https://paredit.org/) is the classic and what I've used previously. Now, I use a combination of paredit and [lispy](https://github.com/abo-abo/lispy), which has many more functions than paredit, with paredit-style keybindings. [Smartparens](https://github.com/Fuco1/smartparens) is a paredit-alternative. [puni](https://github.com/AmaiKinono/puni) is the most language-agnostic alternative, as far as I can tell.

Thanks for reading! I welcome feedback, comments, or questions. You can reach me via email or GitHub.
