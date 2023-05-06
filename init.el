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
;;; Graphical Environment
;; ----------------------
;; MMOSMacs is intended to be my entire Desktop Environment.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Disable superfluous UI elements
;; -------------------------------
;; These UI elements are not useful
;; for the system I am trying to
;; make MMOSMacs into.
;;
;; Some of these were disabled in
;; Early Init, but for completion's
;; sake I disable them here as well.
;; ---------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode -1)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil
      inhibit-startup-message t
      initial-scratch-message "")


;; ---------------------------------
;; Emacs X Window Manager (EXWM)
;; -----------------------------
;; EXWM is a fully-featured
;; X Window Manager.
;; ---------------------------------

(use-package exwm
  :straight t
  :config
  (require 'exwm-config)
  (setq exwm-workspace-number 1)
  (exwm-enable))


;; ---------------------------------
;; Temporary theme
;; ---------------
;; MMOSMacs has a planned custom
;; theme, but for now I'll use a
;; temporary theme so I can bear to
;; look at Emacs.
;; ---------------------------------

(use-package jazz-theme
  :straight t)

(load-theme 'jazz t)


;; ---------------------------------
;; Fonts
;; -----
;; For now I use Iosevka and Exo
;; ---------------------------------

(set-face-attribute 'default nil :font "Iosevka" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 120)
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 120 :weight 'regular)



;; ---------------------------------------------------------------------
;;; Completion
;; -----------
;; Completion refers to the suggestions you get when you type a few
;; characters in almost any modern IDE. MMOSMacs attempts to have
;; completion in as many places as possible.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Minibuffer completion
;; ---------------------
;; Vertico provides minibuffer when
;; typing commands and stuff like
;; that.
;; ---------------------------------

(use-package vertico
  :straight t
  :config
  (setq vertico-count 5
        vertico-cycle nil)
  (vertico-mode))


;; ---------------------------------
;; Text completion
;; ---------------
;; Corfu provides in-buffer text
;; completion, and is the
;; counterpart to vertico.
;; ---------------------------------

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info corfu-history))
  :config
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.1
        corfu-min-width 40
        corfu-max-width corfu-min-width
        corfu-count 8
        corfu-scroll-margin 3
        corfu-cycle nil
        corfu-popupinfo-delay 0.15
        corfu-quit-at-boundary t
        corfu-separator ?\s
        corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-indexed-mode))


;; ---------------------------------
;; Minibuffer hints
;; ----------------
;; Add descriptions and keybind
;; hints to minibuffer completions.
;; ---------------------------------

(use-package marginalia
  :straight t
  :init (marginalia-mode))


;; ---------------------------------
;; Orderless completion
;; --------------------
;; A different ordering method for
;; completion suggestions
;; ---------------------------------

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))




;; ---------------------------------------------------------------------
;;; Text Editor
;; ------------
;; These tweaks are meant to make the text editor more convenient.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Line and column numbers
;; -----------------------
;; It helps to know where you are
;; ---------------------------------

(column-number-mode)
(global-display-line-numbers-mode t)


;; ---------------------------------
;; Highlight stuff
;; ---------------
;; Sometimes it's hard to find
;; things. Highlighting makes it a
;; little easier.
;; ---------------------------------

;; Highlight current line
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))


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


;; ---------------------------------
;; Delimiters
;; ----------
;; Parentheses, brackets, and other
;; open-close type things
;; ---------------------------------

;; Auto-close delimiters
(electric-pair-mode)


;; ---------------------------------
;; Type over selected text
;; -----------------------
;; Typing over selected text should
;; delete the selected text
;; ---------------------------------

(delete-selection-mode t)




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
;;; Personal Knowledge Management System (PKMS)
;; --------------------------------------------
;; Using `org', `org-roam', and a note-hierarchy I've developed over
;; several years of trial-and-error, I keep all my notes in a logical
;; organized collection.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; `org'
;; -----
;; `org' is a versatile format for
;; note-taking and other stuff. It
;; is the core of my PKMS system.
;; ---------------------------------

;; Setup run every time a  buffer
;; is opened.
(defun mm/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode)
  (visual-line-mode)
  (variable-pitch-mode))


;; Org font stuff
(defun mm/org-font-setup ()
  ;; Replace hyphens in lists with dots
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set font size per heading level
  (dolist (face '((org-level-1 . 1.7)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height (cdr face)))


  ;; Ensure anything that should be
  ;; fixed-pitch actually is.
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))


;; `org' configuration
(use-package org
  :straight t
  :hook (org-mode . mm/org-mode-setup)
  :config
  (setq org-ellipsis " ►"
        org-hide-leading-stars t
        org-adapt-indentation t
        org-support-shift-select 'always)
  (mm/org-font-setup))


;; Make org heading bullets look nicer
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("•")))




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
