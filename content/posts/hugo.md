+++
title = "Static Site Generation with Hugo"
author = ["Sophie"]
date = 2023-06-08T12:18:00+02:00
tags = ["blog"]
categories = ["tech"]
draft = false
+++

This website is built with [Hugo](https://gohugo.io/) and [`ox-hugo`](https://ox-hugo.scripter.co/), an exporting engine that lets me
write my posts in [Org mode](https://orgmode.org/) and then put it online easily.

Org mode is a really clever and powerful plain text format that works great for note
taking, text editing, and TODO lists/agendas. Moreover, there are a lot of
convenient features for us Emacs users when using `ox-hugo` to write our blogs
(besides staying in the comfort of a well-known format and editing flow).

For example, I can use this code snippet from [ox-hugo's blog](https://ox-hugo.scripter.co/doc/org-capture-setup/) as a template to
create a new blog post and bind it to a key combination in my Emacs configuration:

```emacs-lisp
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 (file "~/path/to/your/all-posts.org")
                 (function org-hugo-new-subtree-post-capture-template))))
```
