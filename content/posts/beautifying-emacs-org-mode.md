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

{{< figure src="/images/init-org-screenshot.png" link="/images/init-org-screenshot.png" >}}


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

I also think the regular version of Roboto is fine
for normal text. For the headers etc. I want to use [Source Sans Pro](https://fonts.adobe.com/fonts/source-sans).

In the regular part of my Emacs config, I set these fonts.

```emacs-lisp
(when (member "Roboto Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Roboto Mono" :height 108)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono"))

(when (member "Source Sans 3" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.8))
```

Then, in the Org-specific part, I resize the Org headings and choose Source Sans
Pro to be the header font.

```emacs-lisp
;; Resize Org headings
(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.35)
                (org-level-3 . 1.3)
                (org-level-4 . 1.2)
                (org-level-5 . 1.2)
                (org-level-6 . 1.2)
                (org-level-7 . 1.2)
                (org-level-8 . 1.2)))
  (set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'bold :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Source Sans Pro" :weight
'bold :height 1.8)
```

In order to avoid line spacing issues when a line of text contains both
variable- and fixed-pitch text, we need to make sure that the `org-indent` face
inherits from `fixed-pitch`.

```emacs-lisp
(org-indent ((t (:inherit (org-hide fixed-pitch)))))
```

And then, we want to make sure that some parts of the Org document
always use fixed-pitch even when `variable-pitch-mode` is on.

```emacs-lisp
(set-face-attribute 'org-block nil           :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil           :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil         :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil            :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil        :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil       :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil        :inherit 'fixed-pitch)
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


## Decluttering {#decluttering}

We'll declutter by hiding leading starts in headings and emphasis markers (e.g.,
the slashes in  `/.../` ). We'll also use ["pretty entities"](https://orgmode.org/manual/Special-Symbols.html), which allow us to
insert special characters LaTeX-style by using a leading backslash (e.g., `\alpha` to
write the greek letter alpha) and display ellipses in a condensed way.

```emacs-lisp
(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
	  org-ellipsis "…")
```

For source code blocks specifically, I want Org to display the contents using
the major mode of the relevant language. I also want TAB to behave inside the
source code block like it normally would when writing code in that language.

```emacs-lisp
(setq org-src-fontify-natively t
	  org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)
```


## Centring and Line Breaks {#centring-and-line-breaks}

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


## Prettier UI Elements {#prettier-ui-elements}

The packages[`org-bullets`](https://github.com/sabof/org-bullets)and[`org-superstar`](https://github.com/integral-dw/org-superstar-mode) are both great for displaying UTF-8
bullets instead of the normal asterisks of your headings, but I stopped using
them because the package [`org-modern`](https://github.com/minad/org-modern) does that and so much more. For example,
this is the package that gives me such pretty source code blocks. Here's my
(relatively minimal) setup for it.

```emacs-lisp
(use-package org-modern
  :config
  (setq
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

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


## Conclusion {#conclusion}

There it is, that's pretty much all of the visual Org-specific code in my
config. If you're interested in other aspects of my config, you're of course
welcome to [check it out](https://github.com/SophieBosio/.emacs.d). I'm just starting out, so I'd also really appreciate constructive criticism or tips!
