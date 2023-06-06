# STYLE_GUIDE for MMOSMacs

Much of this Style Guide was taken from
[bbatsov/emacs-lisp-style-guide](https://github.com/bbatsov/emacs-lisp-style-guide).


# General guidelines

- Use spaces for indentation. Do not use tabs anywhere.
- Where feasible, avoid making lines longer than 72 chars.
- Avoid trailing whitespace.


## Organization

Emacs Lisp files should begin with a header. The first line of the header should include the filename, a very brief comment (2-5 words), and the `-*- lexical-binding: t; -*-` thing. The next line should be `;;`, and the next should be `;;; Commentary:`, below which should be a more detailed description of the file and it's purpose, as well as any important notes or warnings.

```elisp
;;; init.el --- Init file for MMOSMacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Main config file for MMOSMacs.
```

All Emacs Lisp files are organized into Sections and Subsections using comment decorations. Section titles begin with `;;;` and their decorations extend up to 72 chars. Subsections begin with `;;` and their decorations extend up to 36 chars. Titles should be underlined with dashes on the proceeding line. Explanatory comments go below the
title underline, and an additional 36-or-72-char line goes at the bottom of the header.

Each subsection should have 2 blank lines between the top line of the header and the bottom line of code from the previous subsection. Only one line goes between the Section header and the first Subsection, as well as between portions of code within the same subsection. 4 lines go between a new Section header and the previous bit of code.

```elisp
;;; Commentary:
;; Main configuration file for MMOSMacs.


;; ---------------------------------------------------------------------
;;; First Section Title
;; --------------------
;; Comments about the purpose of the section go here.
;; It should be concise. Don't go into too much detail.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Subsection Title
;; ----------------
;; Subsection explanatory text goes
;; here. Limit the line length to 36
;; chars to maintain the visual
;; "togetherness" of the header.
;;
;; Notice only 1 space between the
;; Section header and the top of the
;; first Subsection header.
;; ---------------------------------

;; `something` is a package I bet
(use-package something
  :straight t
  :init
  (setq some-crap nil))

;; `something-else` is a little less
;; likely to be a package, but it
;; wouldn't surprise me
(use-package something-else
  :straight t)


;; ---------------------------------
;; Subsection Title
;; ----------------
;; Notice the 2 lines separating
;; each Subsection.
;; ---------------------------------

;; I really don't know what to write
;; in these example comments.
(use-package some-other-thing
  :straight t
  :init
  (other-thing-mode))




;; ---------------------------------------------------------------------
;;; SECOND SECTION TITLE
;; ---------------------
;; Notice the 4 empty lines above this section.
;; This provides visual separation; easier on the eyes.
;; ---------------------------------------------------------------------
```


## Comments & Annotations

- Write heading comments with 3 semicolons, and regular comments with 2.
- Write margin comments with 1 semicolon.
- Put a space between the semicolon and the text for heading and regular comments.
- Do not put a space between the semicolon and the text for margin comments.

```elisp
;;; This is a heading comment
;; This is a regular comment with more words and stuff.
;; Something something something.
(defun foo (bar)
  (something something
             something-else)) ;this is a margin comment
```


## Docstrings

- Begin docstrings with a terse, complete sentence. Use imperative
language (e.g. "Verify" instead of "Verifies")
- When a function takes arguments, mention what the arguments do,
whether they are optional or required, etc. Describe the arguments in
UPPERCASE, and describe them in the order they are used in the function.
- Do not indent subsequent lines of a docstring. It looks nice in source
code but looks bizarre when viewed in Emacs.

```elisp
;; good
(defun goto-line (line &optional buffer)
  "Go to LINE, counting from line 1 at beginning of buffer.
If called interactively, a numeric prefix argument specifies
LINE; without a numeric prefix argument, read LINE from the
minibuffer..."
...)

;; bad
(defun goto-line (line &optional buffer)
  "Go to LINE, counting from line 1 at beginning of buffer.
   If called interactively, a numeric prefix argument specifies
   LINE; without a numeric prefix argument, read LINE from the
   minibuffer..."
...)
```
