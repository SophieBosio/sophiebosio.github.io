+++
title = "Beautifying Emacs Org Mode"
author = ["Sophie"]
date = 2023-08-10T22:19:00+02:00
tags = ["emacs"]
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


## Decluttering {#decluttering}

We'll use ["pretty entities"](https://orgmode.org/manual/Special-Symbols.html), which allow us to
insert special characters LaTeX-style by using a leading backslash (e.g., `\alpha` to
write the greek letter alpha). `org-ellipsis` is the symbol displayed after an
Org-heading that is collapsed - I prefer a simple dot.

```emacs-lisp
(setq org-adapt-indentation t
      org-hide-leading-stars t
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

It's common to hide emphasis markers (e.g., `/.../` for italics, `*...*` for bold,
etc.) to have a cleaner visual look, but this makes it harder to edit the text.
[org-appear](https://github.com/awth13/org-appear) is the solution to all my troubles. It displays the markers when the
cursor is within them and hides them otherwise, making edits easy while looking
pretty.

```emacs-lisp
(use-package org-appear
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)  ; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t   ; Show links
		org-appear-autosubmarkers t)) ; Show sub- and superscripts
```

And finally, I have some Org options to deal with headers and TODO's nicely.

```emacs-lisp
(setq org-log-done                       t
	  org-auto-align-tags                t
	  org-tags-column                    -80
	  org-fold-catch-invisible-edits     'show-and-error
	  org-special-ctrl-a/e               t
	  org-insert-heading-respect-content t)
```


## LaTeX Previews {#latex-previews}

The LaTeX previews in Org mode are pretty small by default, so I'll increase
their size a little.

```emacs-lisp
(plist-put org-format-latex-options :scale 1.35)
```

[org-fragtog](https://github.com/io12/org-fragtog) works like org-appear, but for LaTeX fragments: It toggles LaTeX
previews on and off automatically, depending on the cursor position. If you move the
cursor to a preview, it's toggled off so you can edit the LaTeX snippet. When
you move the cursor away, the preview is turned on again.

```emacs-lisp
(use-package org-fragtog
  :hook (org-mode-hook . org-fragtog-mode))
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

And then the custom keywords.

```emacs-lisp
(setq org-todo-keywords
      '((sequence
		 "TODO(t)" "WAIT(w)" "READ(r)" "PROG(p)" ; Needs further action
		 "|"
		 "DONE(d)")))                            ; Needs no action currently
```

I don't set the colours of each TODO state individually anymore, but if you
wanted to, you could set the `org-todo-keyword-faces` variable like this:

```emacs-lisp
(setq org-todo-keyword-faces
      '(("TODO(t)"      :inherit (org-todo region) :foreground "#A3BE8C" :weight bold)
		...))
```


## Bullets {#bullets}

 [org-superstar](https://github.com/integral-dw/org-superstar-mode)  styles some of my UI elements, such as bullets and special
checkboxes for TODOs. It can style a lot more, so I recommend checking the
package out!

```emacs-lisp
(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("◉" "○" "⚬" "◈" "◇"))
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO"  . 9744)
                                          ("WAIT"  . 9744)
                                          ("READ"  . 9744)
                                          ("PROG"  . 9744)
										  ("DONE"  . 9745)))
  :hook (org-mode . org-superstar-mode))
```


## SVG Elements {#svg-elements}

I use [svg-tag-mode](https://github.com/rougier/svg-tag-mode) to replace progress bars, task priorities, dates, and citations with nice SVG
graphics. This package can also style many more elements and I'd encourage you
to read the documentation to find other things you might want to style with
this.

```emacs-lisp
(use-package svg-tag-mode
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
	(svg-image (svg-lib-concat
				(svg-lib-progress-bar (/ (string-to-number value) 100.0)
			      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
				(svg-lib-tag (concat value "%")
				  nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
	(let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
	  (svg-image (svg-lib-concat
				  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
				  (svg-lib-tag value nil
					:stroke 0 :margin 0)) :ascent 'center)))
  (setq svg-tag-tags
      `(
        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
          (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
          (svg-progress-count (substring tag 1 -1)))))

        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))


        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (with or without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil
						       :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t
						       :crop-left t :margin 0 :face 'org-date)))))))

(add-hook 'org-mode-hook 'svg-tag-mode)
```


## Prettify Tags &amp; Keywords {#prettify-tags-and-keywords}

I have a custom function to prettify tags and other elements, lifted from [Jake
B's Emacs setup](https://github.com/jakebox/jake-emacs/blob/main/jake-emacs/jib-funcs.el).

```emacs-lisp
(defun my/prettify-symbols-setup ()
  ;; Checkboxes
  (push '("[ ]" . "") prettify-symbols-alist)
  (push '("[X]" . "") prettify-symbols-alist)
  (push '("[-]" . "" ) prettify-symbols-alist)

  ;; org-abel
  (push '("#+BEGIN_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+END_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+begin_src" . ?≫) prettify-symbols-alist)
  (push '("#+end_src" . ?≫) prettify-symbols-alist)

  (push '("#+BEGIN_QUOTE" . ?❝) prettify-symbols-alist)
  (push '("#+END_QUOTE" . ?❞) prettify-symbols-alist)

  ;; Drawers
  (push '(":PROPERTIES:" . "") prettify-symbols-alist)

  ;; Tags
  (push '(":projects:" . "") prettify-symbols-alist)
  (push '(":work:"     . "") prettify-symbols-alist)
  (push '(":inbox:"    . "") prettify-symbols-alist)
  (push '(":task:"     . "") prettify-symbols-alist)
  (push '(":thesis:"   . "") prettify-symbols-alist)
  (push '(":uio:"      . "") prettify-symbols-alist)
  (push '(":emacs:"    . "") prettify-symbols-alist)
  (push '(":learn:"    . "") prettify-symbols-alist)
  (push '(":code:"     . "") prettify-symbols-alist)

  (prettify-symbols-mode))

(add-hook 'org-mode-hook        #'my/prettify-symbols-setup)
(add-hook 'org-agenda-mode-hook #'my/prettify-symbols-setup)
```

After all this prettification, TODOs, code blocks, and lists look like
screenshot below.

{{< figure src="/img/todos.png" link="/img/todos.png" >}}


## Conclusion {#conclusion}

There it is, that's pretty much all of the visual Org-specific code in my
config. If you're interested in other aspects of my config, you're of course
welcome to [check it out](https://github.com/SophieBosio/.emacs.d). I'm just starting out, so I'd also really appreciate constructive criticism or tips!
