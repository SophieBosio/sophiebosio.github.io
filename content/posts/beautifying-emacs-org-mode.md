+++
title = "Beautifying Emacs Org Mode"
author = ["Sophie"]
date = 2023-08-10T22:19:00+02:00
tags = ["emacs"]
categories = ["tech"]
draft = false
+++

In my opinion, the power of Emacs is that you're able to program your own
working environment. Other editors may be just as, or possibly more, efficient
to use, but I haven't come across any that give you control over as many aspects
of your editor as Emacs does. Customising and tweaking your configuration may be an
endless endeavour, but it's also a highly rewarding one.

All that being said, I was inspired to tweak the look of my Org Mode setup,
especially after reading [this post](https://zzamboni.org/post/beautifying-org-mode-in-emacs/). Below you can find screenshots and code. I
hope there's something in here that you might want to steal for your own config!
In case I change these settings later, my most recently updated config is always
available on my [GitHub](https://github.com/SophieBosio/.emacs.d).

{{< figure src="/img/init-org-screenshot.png" link="/img/init-org-screenshot.png" >}}


## Theme {#theme}

The easiest and quickest way to change the look of vanilla Emacs is to load a
colour theme. I'm using the `doom-nord` theme, which is part of the [`doom-themes`](https://github.com/doomemacs/themes)
package. I find both it and several of the other themes from that pack to be
excellent.

You can find an updated list of themes on [emacsthemes.com](https://emacsthemes.com/) where they
have screenshots. From spending time on the Emacs subreddit, I also know that
people are very fond of Prot's [Modus](https://github.com/protesilaos/modus-themes) and [Ef themes](https://github.com/protesilaos/ef-themes), as well as the built-in
`leuven` theme. Feel free to play around!


## Fonts {#fonts}

Next up is setting up variable-pitch and fixed-pitch fonts. I love Roboto Mono
and I use a ligaturised version of it for programming, from the
[a-better-ligaturizer project](https://github.com/lemeb/a-better-ligaturizer). Here, I'll add that a package such as `ligature.el`
is required to display the ligatures.

For variable-pitch (regular) text, I want to use [Source Sans Pro](https://fonts.adobe.com/fonts/source-sans).

In my Emacs config, I have set these fonts outside the Org section, under
"Interaction, Look &amp; Feel".

```emacs-lisp
(when (member "Roboto Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Roboto Mono" :height 108)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono"))

(when (member "Source Sans Pro" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.18))
```

Then, back in the Org-specific part of the config, I resize the Org headings and choose Source Sans
Pro to be the header font.

```emacs-lisp
;; Resize Org headings
(dolist (face '((org-level-1 . 1.35)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'bold :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Source Sans Pro" :weight
'bold :height 1.8)
```

In order to avoid line spacing issues when a line of text contains both
variable- and fixed-pitch text, we need to make sure that the `org-indent` face
inherits from `fixed-pitch`.

```emacs-lisp
(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
```

And then, we want to make sure that some parts of the Org document
always use fixed-pitch even when `variable-pitch-mode` is on.

```emacs-lisp
(set-face-attribute 'org-block nil            :foreground nil :inherit
'fixed-pitch :height 0.85)
(set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 0.85)
(set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face
fixed-pitch))
(set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)
```

For this all to come together, we need to make sure that  `variable-pitch-mode` is always active in Org buffers.

```emacs-lisp
(add-hook 'org-mode-hook 'variable-pitch-mode)
```

Also, if you're having troubles with the size of LaTeX-previews like I did, you
can increase the size like so.

```emacs-lisp
(plist-put org-format-latex-options :scale 2)
```


## Decluttering &amp; Text Prettification {#decluttering-and-text-prettification}

We'll declutter by hiding leading starts in headings and emphasis markers (e.g.,
the slashes in  `/.../` ). We'll also use ["pretty entities"](https://orgmode.org/manual/Special-Symbols.html), which allow us to
insert special characters LaTeX-style by using a leading backslash (e.g., `\alpha` to
write the greek letter alpha). `org-ellipsis` is the symbol displayed after an
Org-heading that is collapsed - I prefer a simple dot.

```emacs-lisp
(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
	  org-ellipsis "  ·")
```

For source code blocks specifically, I want Org to display the contents using
the major mode of the relevant language. I also want TAB to behave inside the
source code block like it normally would when writing code in that language.

```emacs-lisp
(setq org-src-fontify-natively t
	  org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)
```


## Centring &amp; Line Breaks {#centring-and-line-breaks}

I want the text to fill the screen adaptively, so that long lines of text adapt
to the size of the window. It also breaks lines instead of truncating them.

```emacs-lisp
(add-hook 'org-mode-hook 'visual-line-mode)
```

I prefer having my Org buffer centred. I think it looks prettier when I only
have one buffer open, and it's barely noticeable when several are open because
the width of the margins adapt. For this, I use [Olivetti](https://github.com/rnkn/olivetti), which I think is a
great package for this purpose.

```emacs-lisp
(add-hook 'org-mode-hook 'olivetti-mode)
```

As you can see in the below screenshot, the Org document fills up the left side
of the screen comfortably even when `olivetti-mode` is on.

{{< figure src="/img/split-screen.png" link="/img/split-screen.png" >}}


## Task &amp; Time Tracking {#task-and-time-tracking}

Org mode is also a really powerful tool for tracking tasks and time usage.
However, the default colours don't go too well with our new look.

Of course, you
should change the keywords and the number of priorities to suit your tastes. I
have lifted my colours straight from the official [Nord theme pallette](https://www.nordtheme.com/docs/colors-and-palettes) so that
they go well with my preferred theme.

Let's set the number of task priorities and specify the colour for each
priority.

```emacs-lisp
(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-priority-faces
      '((65 . "#BF616A")
        (66 . "#EBCB8B")
        (67 . "#B48EAD")
        (68 . "#81A1C1")
        (69 . "#5E81AC")
        (70 . "#4C566A")))
```

And then the keywords and their colours.

```emacs-lisp
(setq org-todo-keywords
      '((sequence
		 "TODO" "PROJ" "READ" "CHECK" "IDEA" ; Needs further action
		 "|"
		 "DONE")))                           ; Needs no action currently

(setq org-todo-keyword-faces
      '(("TODO"      :inherit (org-todo region) :foreground "#A3BE8C" :weight bold)
		("PROJ"      :inherit (org-todo region) :foreground "#88C0D0" :weight bold)
        ("READ"      :inherit (org-todo region) :foreground "#8FBCBB" :weight bold)
		("CHECK"     :inherit (org-todo region) :foreground "#81A1C1" :weight bold)
		("IDEA"      :inherit (org-todo region) :foreground "#EBCB8B" :weight bold)
		("DONE"      :inherit (org-todo region) :foreground "#30343d" :weight bold)))
```

Here, you can see a screenshot of these TODOs in action.

{{< figure src="/img/todos.png" link="/img/todos.png" >}}


## Prettier UI Elements {#prettier-ui-elements}

I use a combination of  [`org-modern`](https://github.com/minad/org-modern) and [`org-superstar`](https://github.com/integral-dw/org-superstar-mode) to style my UI elements.
`org-modern` is what gives me such pretty source code blocks, for example. Here's my
(relatively minimal) setup for it.

```emacs-lisp
(use-package org-modern
  :config
  (setq
   org-auto-align-tags t
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Don't style the following
   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
	 (800 1000 1200 1400 1600 1800 2000)
	 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (global-org-modern-mode))
```

And here is the setup for `org-superstar`.

```emacs-lisp
(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("DONE" . 9744)
                                          ("READ" . 9744)
                                          ("IDEA" . 9744)
                                          ("WAITING" . 9744)
                                          ("CANCELLED" . 9744)
                                          ("PROJECT" . 9744)
                                          ("POSTPONED" . 9744)))
  )
```


## Conclusion {#conclusion}

There it is, that's pretty much all of the visual Org-specific code in my
config. If you're interested in other aspects of my config, you're of course
welcome to [check it out](https://github.com/SophieBosio/.emacs.d). I'm just starting out, so I'd also really appreciate constructive criticism or tips!
