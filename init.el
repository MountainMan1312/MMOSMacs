;;; -*- lexical-binding: t; -*-
;;; init.el
;;
;; Main configuration file for MMOSMacs.


;; ---------------------------------------------------------------------
;;; Package & Configuration Management
;; -----------------------------------
;; MMOSMacs uses `straight.el' for package management and `use-package'
;; for configuration management to make things more consistent.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Bootstrap `straight.el'
;; -----------------------
;; This is some pre-written magic
;; provided by `straight.el'. Dont
;; ask me what it does.
;; ---------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; ---------------------------------
;; `use-package'
;; -------------
;; This makes sure `use-package' is
;; installed and loaded.
;; ---------------------------------

(straight-use-package 'use-package)


;; ---------------------------------
;; Don't load outdated code
;; ------------------------
;; MMOSMacs is under highly active
;; development and undergoes
;; freuqent changes. The newest code
;; should always be loaded
;; ---------------------------------

;; if a `.el' file is newer than its corresponding `.elc', load the `.el'
(setq load-prefer-newer t)




;; ---------------------------------------------------------------------
;;; Text Editor
;; ------------
;; These tweaks are meant to make the text editor more convenient.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Scrolling
;; ---------
;; This makes the scrolling behavior
;; smoother and easier to follow.
;; ---------------------------------

;; Small scroll margin makes it
;; easier to see when to stop when
;; scrolling fast.
(setq scroll-margin 2)

;; Make mouse scroll smoother
(setq mouse-wheel-scroll-amount '(2)
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Prevent jumpy scrolling
(setq scroll-conservatively 10
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Move point to beginning/end of
;; buffer if attempting to scroll
;; when already at the beginning/end
;; of buffer
(setq scroll-error-top-bottom t)


;; ---------------------------------
;; Whitespace
;; ----------
;; Spaces, tabs, indentation,
;; newlines, and text-wrapping.
;; ---------------------------------

;; Show stray whitespace
(setq-default show-trailing-whitespace t
	            indicate-empty-lines t
	            indicate-buffer-boundaries 'left)

;; Make files end with newline
(setq-default require-final-newline t)

;; Sentences end with ONE space
(setq-default sentence-end-double-space nil)

;; Use spaces for indentation
(setq-default indent-tabs-mode nil
	            tab-width 2)

;; Wrap words at buffer edge
(global-visual-line-mode)




;; ---------------------------------------------------------------------
;;; File Management
;; ----------------
;; Everything to do with file or directory management goes here.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Disable backups
;; ---------------
;; By default, Emacs saves backup
;; files, adding a `~' to the name.
;; Stop saving them.
;; ---------------------------------

(setq backup-inhibited t)


;; ---------------------------------
;; Disable auto-saves
;; ------------------
;; Auto-saves are the files with
;; hashes before and after the name.
;; They should not exist.
;; ---------------------------------

(setq auto-save-default nil           ; don't create auto-saves
      auto-save-list-file-prefix nil) ; don't create auto-save-list dir


;; ---------------------------------
;; Disable lockfiles
;; -----------------
;; Sometimes I want to edit a locked
;; file. I can handle myself, let me
;; do what I want.
;; ---------------------------------

(setq create-lockfiles nil)




;; ---------------------------------------------------------------------
;;; Keybinds
;; ---------
;; For now, just a few simple changes.
;; In the future there will be whole custom keybind system.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Fix ESC behavior
;; ----------------
;; The default behavior of the ESC
;; key is atrocious. Why in God's
;; name would I want it to close all
;; my windows when I press it twice?
;;
;; This fixes the behavior to do
;; what ESC should do: "escape" the
;; current thing I'm doing, like the
;; minibuffer or a popup window.
;; ---------------------------------

;; Define fixed version of `keyboard-escape-quit' which does not close
;; windows or change their layout.
(defun mm/keyboard-escape-quit-keep-windows ()
  "Alternative version of `keyboard-escape-quit' that does not change window layout."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
        ((region-active-p)
         (deactivate-mark))
        ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (current-prefix-arg
         nil)
        ((> (recursion-depth) 0)
         (exit-recursive-edit))
        (buffer-quit-function
         (funcall buffer-quit-function))
        ;; The following lines are from `keyboard-escape-quit'.
        ;; They have been commented to disable the unwanted behavior
        ;; ((not (one-window-p t))
        ;;  (delete-other-windows)
        ((string-match "^ \\*" (buffer-name (current-buffer)))
         bury-buffer)))

;; Fix the keybinds for the ESC key
(global-set-key (kbd "<escape>") 'mm/keyboard-escape-quit-keep-windows)
(global-unset-key (kbd "C-x ESC ESC"))


;; ---------------------------------
;; Replace yes/no prompts with y/n
;; -------------------------------
;; It's too much to ask for me to
;; type 3 whole letters to confirm
;; something. I demand to only have
;; to press a single key.
;; ---------------------------------

(defalias 'yes-or-no-p 'y-or-n-p)
