;;; init.el --- Init file for MMOSMacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Main configuration file for MMOSMacs.

;;; Code:

;; ---------------------------------------------------------------------
;;; Startup
;; --------
;; These items need to be done first.
;; ---------------------------------------------------------------------

;; --------------------------------
;; Debug on error
;; --------------
;; Can be helpful to diagnose
;; startup issues.
;; ---------------------------------

;; Produce backtraces when errors occur
(setq debug-on-error t)




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
;; Makes sure `use-package' is
;; installed and loaded.
;; ---------------------------------

(straight-use-package 'use-package)


;; ---------------------------------
;; Don't load outdated code
;; ------------------------
;; MMOSMacs is under active
;; development and undergoes
;; freuqent changes. The newest code
;; should always be loaded.
;; ---------------------------------

;; if a `.el' file is newer than its corresponding `.elc', load the `.el'
(setq load-prefer-newer t)


;; ---------------------------------
;; Exclude modes from modeline list
;; --------------------------------
;; The modeline can get rather
;; clogged with all these modes
;; running around. Delight allows
;; you to hide them
;; ---------------------------------

(use-package delight
  :straight t
  :delight
  (buffer-face-mode))




;; ---------------------------------------------------------------------
;; Performance hacks
;; -----------------
;; These hacks subjectively make Emacs perform "better"
;; ---------------------------------------------------------------------

;; ---------------------------------
;; GCMH
;; ----
;; Garbage Collector Magic Hack
;; ---------------------------------

;; Configure GCMH
(use-package gcmh
  :straight t
  :defer t
  :delight
  :init
  (setq gcmh-idle-delay 15
        gcmh-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024))) ;16kb

;; Start GCMH after init is complete
(add-hook 'after-init-hook 'gcmh-mode)


;; ---------------------------------
;; Async
;; -----
;; Asynchronous processing of things
;; ---------------------------------

;; Configure Async package
(use-package async
  :straight t
  :config
  (setq async-bytecomp-package-mode t      ;Enable byte-compiled Emacs Lisp
        warning-suppress-types '((comp)))) ;Don't steal focus while doing Async compilation




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
  (setq exwm-workspace-number 6
        exwm-workspace-index-map (lambda (index)
                                   (let ((named-workspaces ["0:Admin"
                                                            "1:Agenda"
                                                            "2:KB"
                                                            "3:IDE"
                                                            "4:School"
                                                            "5:Reading"]))
                                     (if (< index (length named-workspaces))
                                         (elt named-workspaces index)
                                       (number-to-string index))))
        exwm-input-prefix-keys '(?\C-h
                                 ?\C-u
                                 ?\C-x
                                 ?\M-x
                                 ?\M-`
                                 ?\M-:
                                 ?\M-&)
        exwm-input-global-keys '(;; Reset EXWM to line-mode to regain control
                                 ([?\s-r] . exwm-reset)
                                 ;; Move between windows with Super + ESDF
                                 ([?\s-e] . windmove-up)
                                 ([?\s-d] . windmove-down)
                                 ([?\s-s] . windmove-left)
                                 ([?\s-f] . windmove-right)
                                 ;; Launch applications via shell command with `s-x'
                                 ([?\s-x] . (lambda (command)
                                              (interactive (list (read-shell-command "$ ")))
                                              (start-process-shell-command
                                               command nil command)))
                                 ;; Switch workspace with `s-w'
                                 ([?\s-w] . exwm-workspace-switch)
                                 ([?\s-g] . exwm-floating-toggle-floating)))
  (exwm-enable))

;; Setup multiple monitors & workspace assignments
(require 'exwm-randr)
(exwm-randr-enable)
(start-process-shell-command "xrandr" nil "xrandr --output VGA-1 --off --output DP-1 --off --output HDMI-1 --mode 1920x1080 --pos 0x500 --rotate normal --output DP-2 --off --output HDMI-2 --mode 1920x1080 --pos 1920x0 --rotate left")
(setq exwm-randr-workspace-monitor-plist
      (pcase (system-name)
        ("anu.tgwil.net" '(5 "HDMI-2"))))

;; Make EXWM buffer names useful
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; ---------------------------------
;; Temporary theme
;; ---------------
;; MMOSMacs has a planned custom
;; theme, but for now I'll use a
;; temporary theme so I can bear to
;; look at Emacs.
;; ---------------------------------

;; Jazz Theme is the nicest-looking
;; theme I've found so far. Really
;; easy on the eyes.
(use-package jazz-theme
  :straight t)
(load-theme 'jazz t)

;; Background color
(set-background-color "black")

;; Cursor
(setq cursor-type 'box
      blink-cursor-blinks 0)
(add-to-list 'default-frame-alist '(cursor-color . "orange"))

;; Mouse
(add-to-list 'default-frame-alist '(mouse-color . "orange"))

;; Highlights
(set-face-attribute 'highlight nil :background "#333")

;; Selection
(set-face-attribute 'region nil
                    :background "#110"
                    :foreground nil)

;; Comments
(set-face-attribute 'font-lock-comment-face nil :foreground "#222")

;; Different face for delimiters
(use-package paren-face
  :straight t
  :config
  (set-face-attribute 'parenthesis nil :foreground "#222"))
(add-hook 'after-init-hook 'global-paren-face-mode)

;; Highlight color codes as their respective color
(use-package rainbow-mode
  :straight t
  :delight
  :hook (prog-mode . rainbow-mode))

;; Modeline
(set-face-attribute 'mode-line nil
                    :height 80
                    :box nil
                    :foreground "orange"
                    :background "#222")
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :foreground "#222"
                    :background "#111")
(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "orange")


;; ---------------------------------
;; Fonts
;; -----
;; For now I use JetBrains Mono and
;; Noto Sans.
;; ---------------------------------

;; Fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :height 100)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 100)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 110 :weight 'regular)

;; Syntax highlighting
(use-package lisp-extra-font-lock
  :straight t)
(lisp-extra-font-lock-global-mode)

(custom-set-faces '(font-lock-warning-face
                    ((t :foreground "black"
                        :background "firebrick"
                        :bold nil
                        :underline nil
                        :inherit nil))))
(custom-set-faces '(lisp-extra-font-lock-special-variable-name
                    ((t :foreground "tomato"
                        :inherit nil))))
(custom-set-faces '(font-lock-constant-face
                    ((t :foreground "SaddleBrown"
                        :inherit nil))))

(custom-set-faces '(lisp-extra-font-lock-quoted
                    ((t :foreground "RoyalBlue"
                        :inherit nil))))


;; ---------------------------------
;; Modeline
;; --------
;; I'm losing the will to give a
;; shit about these descriptions.
;; ---------------------------------

;; Display time in modeline
(setq display-time-default-load-average nil)

(defface mm/display-time-face
  '((((type x w32 mac))
     (:foreground "DeepSkyBlue")))
  "Face used to display the time in the modeline.")

(setq display-time-string-forms
      '((propertize (concat
                     "[ "
                     (format-time-string "%Y-%m-%d" (current-time))
                     " ]"
                     "[ " 24-hours ":" minutes " ]")
                    'face 'mm/display-time-face)))

(display-time-mode)

;; Display battery percentage in modeline
(use-package emacs
  :config
  (setq battery-mode-line-format "[ %b%p%% ]"
        battery-load-low 30
        battery-load-critical 15))
(display-battery-mode)

;; ---------------------------------
;; Icons
;; -----
;; Icons make it easy to recongnize
;; things with memorable logos.
;; ---------------------------------

;; All the icons
(use-package all-the-icons
  :straight t)

;; Add icons to dired buffers
(use-package all-the-icons-dired
  :straight t
  :config (setq all-the-icons-dired-monochrome nil)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Add icons to completion candidates
(use-package all-the-icons-completion
  :straight t)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

;; Add icons to ivy menus
(use-package all-the-icons-ivy
  :straight t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))


;; ---------------------------------
;; Highlight stuff
;; ---------------
;; Sometimes it's hard to find
;; things. Highlighting makes it a
;; little easier.
;; ---------------------------------

;; Highlight current line
(use-package hl-line
  :straight (:type built-in)
  :hook (prog-mode . hl-line-mode))
(require 'hl-line)
(set-face-attribute 'hl-line nil :background "#111")


;; ---------------------------------
;; ADHD Accessibility
;; ------------------
;; I have ADHD, real bad. All this
;; *shit* going on in Emacs can be
;; detrimental to my attention.
;;
;; Make it easier to focus...
;; ---------------------------------

(use-package auto-dim-other-buffers
  :straight t
  :config (setq auto-dim-other-buffers-dim-on-focus-out t
                auto-dim-other-buffers-dim-on-switch-to-minibuffer t))
(set-face-attribute 'auto-dim-other-buffers-face nil :background "#000")
(set-face-attribute 'auto-dim-other-buffers-hide-face nil :background "#000" :foreground "#000")
(add-hook 'after-init-hook 'auto-dim-other-buffers-mode)


;; ---------------------------------
;; Hide text for privacy
;; ---------------------
;; Sometimes you just don't want
;; people reading your stuff.
;; ---------------------------------

(use-package redacted
  :straight t
  :config
  (global-set-key (kbd "s-p") 'redacted-mode))
(add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1))))




;; ---------------------------------------------------------------------
;;; Completion, Correction, & Search
;; ---------------------------------
;; Completion and correction are the main source of my typing efficiency
;; if I'm being honest.
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
  :config
  (setq marginalia-field-width 120)
  :bind ("M-a" . marginalia-cycle)
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
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; ---------------------------------
;; Search
;; ------
;; Find stuff.
;; ---------------------------------

(use-package isearch
  :straight (:type built-in)
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format nil           ;don't show match count before search
        lazy-count-suffix-format "    (%s/%S)" ;show match count at end of search
        search-whitespace-regexp ".*"
        isearch-lax-whitespace t               ;match any number of spaces
        isearch-regexp-lax-whitespace t))




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

;; Display line/column numbers
(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Disable line numbers in specific modes
;; (dolist (mode '(org-mode-hook
                ;; org-agenda-mode-hook
                ;; helpful-mode-hook
                ;; term-mode-hook
                ;; vterm-mode-hook))
  ;; (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Customize appearance of line-numbers
(set-face-attribute 'line-number nil
                    :height 80
                    :weight 'ultra-light
                    :foreground "#222"
                    :background "#0a0a0a")
(set-face-attribute 'line-number-current-line nil
                    :height 80
                    :weight 'normal
                    :foreground "orange"
                    :background (face-attribute 'hl-line :background))



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

;; Don't show whitespace in specific places
(dolist (hook '(comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook
                special-mode-hook
                term-mode-hook
                vterm-mode-hook))
  (add-hook hook (lambda () (setq-local show-trailing-whitespace nil))))

;; Make files end with newline
(setq-default require-final-newline t)

;; Sentences end with ONE space
(setq-default sentence-end-double-space nil)

;; Use spaces for indentation
(setq-default indent-tabs-mode nil
	            tab-width 2)

;; Wrap words at buffer edge (and hide modeline entry for WRAP)
(global-visual-line-mode)
(use-package emacs
  :delight (visual-line-mode))


;; ---------------------------------
;; Delimiters
;; ----------
;; Parentheses, brackets, and other
;; open-close type things
;; ---------------------------------

;; Auto-close delimiters
(electric-pair-mode)


;; ---------------------------------
;; Selection
;; -----------------------
;; Efficient selection is one of the
;; 47 totems of efficiency.
;; ---------------------------------

;; Typing over selected text deletes
;; the selected text
(delete-selection-mode t)

;; Expand region by semantic units
(use-package expand-region
  :straight t
  :config
  (setq expand-region-subword-enabled t)
  :bind ("M-=" . er/expand-region))

;; Enable `cua-selection-mode' for
;; rectangular selection.
(cua-selection-mode nil)
(global-set-key (kbd "s-c") 'cua-rectangle-mark-mode)
(set-face-attribute 'cua-rectangle nil
                    :background "#110"
                    :foreground nil)


;; ---------------------------------
;; Multiple cursors
;; ----------------
;; This allows you to do the same
;; thing in multiple places.
;; ---------------------------------

(use-package multiple-cursors
  :straight t
  :bind (("s-m" . mc/mark-more-like-this-extended))
  :config
  (setq mc/cmds-to-run-for-all
        '(electric-pair-delete-pair
          end-of-visual-line
          beginning-of-visual-line
          beginning-of-line-text
          mm/keyboard-escape-quit-keep-windows)))




;; ---------------------------------------------------------------------
;;; Security
;; ---------
;; I'm studying to be a security professional, might as well start
;; being secure about stuff.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; GPG
;; ---
;; Sign and encrypt files
;; ---------------------------------

(setq epa-pinentry-mode 'loopback
      epa-file-name-regexp "\\.\\(gpg\\|\\asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'"
      auth-source-debug t)
(epa-file-name-regexp-update)


;; ---------------------------------------------------------------------
;;; File, project, & repository management
;; ---------------------------------------
;; This section contains everything to do with file, project, & repo
;; management. This includes `magit' and `projectile'.
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


;; --------------------------------
;; Auto-reload changed files
;; -------------------------
;; When files are changed on disk
;; externally, emacs should reload
;; those files.
;; ---------------------------------

(global-auto-revert-mode t)


;; ---------------------------------
;; Delete files
;; ------------
;; Make deleting files a lot more
;; convenient.
;; ---------------------------------

;; It's possible to delete a file
;; and forget about it, then save
;; buffer again. This function kills
;; the buffer after deleting the
;; file.
(defun mm/delete-visited-file (buffer-name)
  "Delete the file visited by BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
                 (file-exists-p filename))
        (delete-file filename))
      (kill-buffer buffer))))


;; ---------------------------------
;; Dired
;; -----
;; Various configurations for dired
;; ---------------------------------

(use-package dired-hacks
  :straight t)


;; ---------------------------------
;; Project management
;; ------------------
;; `Projectile' provides features
;; for operating on a project level.
;; ---------------------------------

(use-package projectile
  :straight t
  :config
  (add-hook 'after-init-hook 'projectile-global-mode)
  (setq projectile-project-search-path '("~/Projects")
        projectile-known-projects-file "~/.emacs.d/projectile-known-projects.eld"
        projectile-cache-file "~/.emacs.d/projectile.cache")
  :bind-keymap ("C-c p" . projectile-command-map))


;; ---------------------------------
;; `Magit' - A git porcelain
;; -------------------------
;; Magit provides a state-of-the-art
;; interface for managing `git'
;; repositories.
;; ---------------------------------

(use-package magit
  :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setq magit-diff-refine-hunk t
        magit-diff-paint-whitespace t
        magit-diff-paint-whitespace-lines t
        magit-diff-highlight-trailing t))


;; ---------------------------------
;; Highlight diff in fringe
;; ------------------------
;; In buffers for git-controlled
;; files, highlight changed lines in
;; the fringe.
;; ---------------------------------

(use-package diff-hl
  :straight t
  :config
  (set-fringe-mode '(10 . 0)) ;fringe width
  :hook
  (prog-mode    . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode))




;; ---------------------------------------------------------------------
;;; Documents
;; ----------
;; Stuff for working with various document formats.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; PDF
;; ---
;; Stuff for working with PDF files
;; ---------------------------------

(defvar mm/pdf-view-midnight-foreground (face-attribute 'default :foreground)
  "FOREGROUND to be used by `pdf-view-midnight-colors'.")

(defvar mm/pdf-view-midnight-background (face-attribute 'default :background)
  "BACKGROUND to be used by `pdf-view-midnight-colors'.")

(use-package pdf-tools
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (setq pdf-view-use-imagemagick t
        pdf-view-use-scaling nil
        doc-view-resolution 300
        pdf-view-midnight-colors
        `(,mm/pdf-view-midnight-foreground . ,mm/pdf-view-midnight-background)
        pdf-occur-search-batch-size 1)
  :hook (pdf-view-mode . pdf-view-themed-minor-mode))
(require 'pdf-tools)
(define-key pdf-view-mode-map (kbd "M-s o") 'pdf-occur)
(define-key pdf-view-mode-map (kbd "o") 'pdf-outline)


;; ---------------------------------
;; EPUB
;; ----
;; E-book file format
;; ---------------------------------

(use-package nov
  :straight t
  :after 'after-init-hook
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-save-place-file "~/kb/bookmarksEPUB"))


;; ---------------------------------
;; Muse
;; ---------------------------------

(use-package muse
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.muse\\'" . muse-mode)))


;; ---------------------------------
;; Bookmarks
;; ---------
;; I almost never remember where I
;; left off reading.
;; ---------------------------------

(setq bookmark-file "~/kb/bookmarks")




;; ---------------------------------------------------------------------
;;; Personal Knowledge Management System (PKMS)
;; --------------------------------------------
;; Using `org', `org-roam', and a note-hierarchy I've developed over
;; several years of trial-and-error, I keep all my notes in a logical
;; organized collection. I feel this knowledgebase system can handle
;; almost any bit of information I can throw at it, and it is flexible
;; enough to adapt when I find new things it can't handle.
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
  (visual-line-mode))

;; Org font stuff
(defun mm/org-font-setup ()
  ;; Replace hyphens in lists with dots
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 ()
                                  (compose-region
                                   (match-beginning 1)
                                   (match-end 1)
                                   "•"))))))
  (set-face-attribute 'org-level-1 nil
                      :height 1.1
                      :foreground "SandyBrown")
  (set-face-attribute 'org-level-2 nil
                      :height 1.1
                      :foreground "SandyBrown")
  (set-face-attribute 'org-level-3 nil
                      :height 1.1
                      :foreground "SandyBrown")
  (set-face-attribute 'org-level-4 nil
                      :height 1.1
                      :foreground "SandyBrown")
  (set-face-attribute 'org-level-5 nil
                      :height 1.1
                      :foreground "SandyBrown")
  (set-face-attribute 'org-level-6 nil
                      :height 1.1
                      :foreground "SandyBrown")
  (set-face-attribute 'org-level-7 nil
                      :height 1.1
                      :foreground "SandyBrown")
  (set-face-attribute 'org-level-8 nil
                      :height 1.1
                      :foreground "SandyBrown"))

;; `org' configuration
(use-package org
  :straight t
  :hook
  (org-mode . mm/org-mode-setup)
  (org-mode . auto-revert-mode)
  :config
  (mm/org-font-setup)
  (setq org-startup-folded t
        org-ellipsis " ►"
        org-hide-leading-stars t
        org-adapt-indentation t
        org-support-shift-select 'always
        org-return-follows-link t
        org-image-actual-width nil
        org-image-max-width 120)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  :bind
  ("C-c n l t" . org-toggle-link-display)
  ("C-c t"     . org-agenda-todo))

;; Set buffer name to #+TITLE
;; Credit to SE user "Tobias"
(defun mm/org-buffer-name-to-title (&optional end)
  "Rename buffer to value of #+TITLE:.
if END if non-nil, search for #+TITLE: at `point' and
delimit it to END. Start an unlimited search at
`point-min' otherwise."
  (interactive)
  (let ((beg (or (and end (point))
                 (point-min))))
    (save-excursion
      (when end
        (goto-char end)
        (setq end (line-end-position)))
      (goto-char beg)
      (when (re-search-forward "^[[:space:]]*#\\+TITLE:[[:space:]]*\\(.*?\\)[[:space:]]*$" end t)
        (rename-buffer (match-string 1)))))
  nil)

(defun mm/org-buffer-name-to-title-config ()
  "Configure Org to rename buffer to value of #+TITLE:."
  (font-lock-add-keywords nil '(mm/org-buffer-name-to-title)))

(add-hook 'org-mode-hook #'mm/org-buffer-name-to-title-config)

;; Make org heading bullets look nicer
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("•")))

;; Auto-show emphasis markers and links on hover
(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

;; Define keybinds to follow org links
(use-package link-hint
  :straight t
  :bind
  ("C-c n l o" . link-hint-open-link))

;; Render animated GIFs
(use-package org-inline-anim
  :straight t
  :hook (org-mode . org-inline-anim-mode))


;; ---------------------------------
;; `org-roam'
;; ----------
;; Org roam adds a ton of PKMS
;; functionality to org.
;; ---------------------------------

;; Define function to insert a link to a node without opening it
(defun mm/org-roam-node-insert-immediate (arg &rest args)
  "Insert `org-roam' node without opening it."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates
         (list (append (car org-roam-capture-templates)
                       '(:immediate-finish)))))
    (apply #'org-roam-node-insert args)))

;; Capture notes quickly into an inbox to be dealt with later
(defun mm/org-roam-capture-inbox ()
  "Capture something to `inbox.org'"
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "\n* [%<%Y-%m-%d %a %H:%M>] %?"
                                   :if-new (file+head "inbox.org"
                                                      "#+TITLE: Inbox\n")))))
(global-set-key (kbd "C-c n c") 'mm/org-roam-capture-inbox)

;; Add Dendron-like note refactoring functionality
;; This was taken from https://github.com/vicrdguez/dendroam
(cl-defmethod mm/org-roam-node-current-file (node)
  (file-name-base (org-roam-node-file node)))

(cl-defmethod mm/org-roam-node-hierarchy-title (node)
  (capitalize (car (last (split-string (org-roam-node-title node) "\\.")))))

(defun mm/org-roam-refactor-file ()
  "Refactor current `org-roam' node/file."
  (interactive)
  (let* ((initial-file (buffer-file-name))
         (initial-slug (file-name-base initial-file))
         (new-slug (read-string "Refactor: " initial-slug))
         (new-file (concat
                    (expand-file-name new-slug org-roam-directory)
                    ".org")))
    (rename-file initial-file new-file)
    (kill-current-buffer)
    (find-file new-file)))

(cl-defmethod mm/org-roam-node-hierarchy (node)
  (funcall 'mm/org-roam-format-hierarchy (org-roam-node-file node)))

(cl-defmethod mm/org-roam-node-current-file (node)
  (file-name-base (buffer-file-name)))

(defun mm/org-roam-get-same-hierarchy-files (hierarchy)
  "Get all the nodes that share the same HIERARCHY totally or partially."
  (let ((files (mapcar #'car (org-roam-db-query [:select [file]
                                                         :from nodes
                                                         :where (like file $r1)]
                                                (concat "%" hierarchy "%"))))) files))

(defun mm/org-roam-refactor-hierarchy (&optional current)
  "Refactor all `org-roam' nodes/files under CURRENT hierarchy."
  (interactive)
  (let*
      ((initial-file (file-name-nondirectory (buffer-file-name)))
       (initial-slug (file-name-base initial-file))
       (new-slug (file-name-base (read-string "Refactor: " initial-slug)))
       (initial-slug-no-title
        (file-name-base initial-slug))
       (files-to-upd (if current `(,initial-file)
                       (mm/org-roam-get-same-hierarchy-files
                        initial-slug-no-title))))
    (dolist (file files-to-upd)
      (let ((new-file
             (replace-regexp-in-string initial-slug-no-title new-slug file)))
        (rename-file file new-file)
        (if (equal buffer-file-name file)
            (progn
              (kill-current-buffer)
              (find-file new-file)))))))


;; `org-roam' configuration
(use-package org-roam
  :straight t
  :custom
  ;; Keep all notes in `~/kb', which should be synced between all computers
  (org-roam-directory (file-truename "~/kb"))
  (make-directory org-roam-directory 'parents)
  ;; Do not sync `org-roam.db', it is ephemeral.
  (org-roam-db-location (concat user-emacs-directory "/org-roam.db"))
  ;; Customize results display when running `org-roam-node-find'
  (org-roam-node-display-template (concat
                                   (propertize "${title:48}    " 'face 'org-document-title)
                                   (propertize "${file:*}" 'face 'org-roam-dim)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :unnarrowed t
      :if-new (file+head "${title}.org"
                         "#+TITLE:\n#+STARTUP: latexpreview inlineimages"))))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "%?"
      :unnarrowed t
      :if-new (file+head "daily.%<%Y.%m.%d>.org"
                         "#+TITLE: Daily Log for %<%Y-%m-%d>\n"))))
  (org-roam-complete-everywhere t)
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  (require 'org-roam-dailies)
  (setq org-roam-dailies-directory "")
  :bind (("C-c n f"   . org-roam-node-find)
         ("C-c n c"   . mm/org-roam-capture-inbox)
         ("C-c n d t" . org-roam-dailies-goto-today)
         ("C-c n d y" . org-roam-dailies-goto-yesterday)
         ("C-c n i"   . mm/org-roam-node-insert-immediate)
         ("C-c n I"   . org-roam-node-insert)
         ("C-c n l s" . org-store-link)
         ("C-c n l i" . org-insert-link)
         ("C-c n u"   . org-roam-update-org-id-locations)
         ("C-c n r h" . mm/org-roam-refactor-hierarchy)
         ("C-c n r f" . mm/org-roam-refactor-file)))

;; Web-served graph UI for org-roam.
(use-package org-roam-ui
  :straight t
  :delight (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))


;; ---------------------------------
;; Task / Time Management
;; ----------------------
;; I track everything in org. It
;; makes it easier to remember
;; things, which is handy because I
;; am quite forgetful.
;; ---------------------------------

;; Agenda configuration
(defun mm/org-agenda-update-agenda-files ()
  "Update the list of `org-agenda-files'."
  (interactive)
  (setq org-agenda-files (append '("~/kb/agenda.org"
                                   "~/kb/self.routine.org"
                                   "~/kb/inbox.org"
                                   "~/kb/shop.org")
                                 (file-expand-wildcards "~/kb/*log.org*")
                                 (file-expand-wildcards "~/kb/*agenda.org*"))))

(use-package org
  :delight
  (org-agenda-mode)
  (org-agenda-log-mode)
  :config
  (mm/org-agenda-update-agenda-files)
  (setq org-agenda-start-with-log-mode t
        org-agenda-log-mode-items '(closed clock state)
        org-todo-keywords
        '((sequence "TODO(t)" "SOMEDAY(s)" "SOON(o)" "NEXT(n)"
                    "IN-PROGRESS(i)" "WAITING(w)" "HOLD(h)" "REVIEW(r)"
                    "|" "DONE(d)" "CANCELED(c)")
          (sequence "EVENT(t)" "EVENT_IN-PROGRESS(i)"
                    "|" "MISSED_EVENT(m)" "CANCELLED_EVENT(c)" "ATTENDED_EVENT(d)")
          (sequence "APPT(t)" "APPT_IN-PROGRESS(i)"
                    "|" "MISSED_APPT(m)" "ATTENDED_APPT(d)" "CANCELLED_APPT(c)")
          (sequence "CLASS(t)" "CLASS_IN-PROGRESS(i)"
                    "|" "ATTENDED_CLASS(d)" "MISSED_CLASS(m)" "CANCELED_CLASS(c)")
          (sequence "EAT(t)" "EATING(i)" "|" "ATE(a)"))
        ;; org-agenda-span 1
        org-agenda-start-day "0d"
        org-agenda-start-on-weekday nil
        org-agenda-use-time-grid t
        org-agenda-time-grid (quote ((daily today remove-match)))
        org-agenda-include-diary t
        org-agenda-show-future-repeats nil
        org-agenda-repeating-timestamp-show-all nil
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-show-done-always-green nil
        org-priority-faces (quote ((65 . "red") (66 . "OrangeRed") (67 . "DarkOrange") (68 . "peru")))
        org-deadline-warning-days 3
        org-agenda-compact-blocks t
        org-log-done 'time
        org-log-into-drawer t)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-habits t
        org-habit-show-habits-only-for-today nil
        org-habit-show-all-today nil
        org-habit-graph-column 0
        org-habit-following-days 0
        org-habit-preceding-days 0)
  :bind (("C-c a"   . org-agenda)
         ("C-c n a" . (lambda () (interactive) (find-file "~/kb/agenda.org")))
         ("C-c n s" . org-save-all-org-buffers)))

;; Super Agenda
(use-package org-super-agenda
  :straight t
  :config
  (org-super-agenda-mode))

;; Custom Agenda views
(use-package org-agenda
  :config
  (setq org-agenda-prefix-format '((agenda . " %-27s %-13:t %-12:c  ") ;" %-11s %-13:t %-12:c  "
                                   (todo . " %-27s %-13:t %-12:c  "))
        org-agenda-custom-commands
        '(("o" "Agenda Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "TODAY"
                                  :discard (:and (:scheduled future :habit t))
                                  :discard (:and (:deadline future :habit t))
                                  :time-grid t
                                  :date today
                                  :scheduled t
                                  :deadline t)
                           (:discard (:anything t))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:discard (:habit))
                            (:name "IN-PROGRESS"
                                   :todo ("IN-PROGRESS" "REVIEW"))
                            ;; (:name "OVERDUE"
                                   ;; :deadline past
                                   ;; :scheduled past)
                            (:name "NEXT ACTIONS"
                                   :todo "NEXT")
                            (:discard (:anything t)))))))))))

;; Update agenda periodically every `mm/refresh-agenda-time-seconds' seconds.
;; This was taken from https://emacs.stackexchange.com/a/68767/38877
;; (defvar mm/refresh-agenda-time-seconds 15)
;; (defvar mm/refresh-agenda-timer nil
  ;; "Timer for `mm/refresh-agenda-timer-function' to reschedule itself, or NIL.")
;; (defun mm/refresh-agenda-timer-function ()
  ;; "If the user types a command while `mm/refresh-agenda-timer' is active, the next time this function is called from it's main idle timer, deactivate `mm/refresh-agenda-timer'."
  ;; (when mm/refresh-agenda-timer
    ;; (cancel-timer mm/refresh-agenda-timer))
  ;; (lambda () (save-window-excursion (org-agenda nil "a")))
  ;; (save-window-excursion (org-agenda nil "a"))
  ;; (setq mm/refresh-agenda-timer
        ;; (run-with-idle-timer
         ;; (time-add (current-idle-time) mm/refresh-agenda-time-seconds)
         ;; nil
         ;; 'mm/refresh-agenda-timer-function)))
;; (run-with-idle-timer mm/refresh-agenda-time-seconds t 'mm/refresh-agenda-timer-function)


;; ---------------------------------
;; Math notes
;; ----------
;; I'm studying math, might as well
;; take some notes
;; ---------------------------------

;; AUCTeX
(use-package tex
  :straight auctex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        org-format-latex-options (plist-put org-format-latex-options
                                            :scale 1.75)))

;; Automatically preview LaTeX in *org-roam* buffers
(add-hook 'org-roam-buffer-postrender-functions
          (lambda () (org--latex-preview-region (point-min) (point-max))))

;; CDLaTeX
(use-package cdlatex
  :straight t
  :delight (org-cdlatex-mode)
  :hook (org-mode . org-cdlatex-mode))

;; Automatically toggle LaTeX previews
;; when cursor enters/exits them.
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode))


;; ---------------------------------
;; Diagrams
;; --------
;; Ditaa, GraphViz, and PlantUML
;; ---------------------------------

;; PlantUML
(use-package plantuml-mode
  :straight t
  :config
  (setq plantuml-executable-path "/usr/bin/plantuml"
        org-plantuml-executable-path "/usr/bin/plantuml"
        plantuml-default-exec-mode 'executable
        org-plantuml-exec-mode 'plantuml))

;; Ditaa
(setq org-ditaa-jar-path "/usr/share/ditaa/lib/ditaa.jar"
      org-ditaa-jar-option "-oS --svg")


;; ---------------------------------
;; Export org to HTML
;; ------------------
;; Sometimes I want to share my notes
;; ---------------------------------

(use-package htmlize
  :straight t
  :config
  (setq org-html-htmlize-output-type 'css))





;; ---------------------------------------------------------------------
;;; Development Environment
;; ------------------------
;; MMOSMacs is intended to serve as a full-featured IDE
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Terminal
;; --------
;; What's Linux without a terminal?
;; ---------------------------------

;; Vterm
(use-package vterm
  :straight t
  :config
  (setq vterm-always-compile-module t)) ;don't ask about compiling module on startup


;; ---------------------------------
;; Syntax checking
;; ---------------
;; I'd like to know when I'm making
;; a mistake.
;; ---------------------------------

;; Eldoc shows function definition hints in the echo area
(use-package eldoc
  :straight (:type built-in)
  :delight)

;; Flycheck is activated by specific language modes.
;; See `:hook's in language modes below to see which ones use it.
(use-package flycheck
  :straight t
  :defer t
  :delight 'flycheck-mode)


;; ---------------------------------
;; Language Server Protocol (LSP)
;; ------------------------------
;; Protocol for interaction between
;; Emacs and language servers.
;; ---------------------------------

(use-package lsp-mode
  :straight t
  :init (setq lsp-keymap-prefix "C-c l"))


;; ---------------------------------
;; Emacs Lisp
;; ----------
;; The language we all know and love
;; ---------------------------------

(use-package elisp-mode
  :straight (:type built-in)
  :commands (emacs-lisp-mode)
  :hook
  (emacs-lisp-mode . flycheck-mode)
  (emacs-lisp-mode . (lambda ()
                       (add-hook 'local-write-file-hooks
                                 'check-parens))))


;; ---------------------------------
;; Common Lisp
;; -----------
;; The programmable language
;; ---------------------------------

;; Sly REPL
(use-package sly
  :straight t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;; Quicklisp integration for Sly
(use-package sly-quicklisp
  :straight t)


;; ---------------------------------
;; Racket + SICP
;; -------------
;; The Wizard Book
;; ---------------------------------

;; Racket
(use-package racket-mode
  :straight t)

(use-package ob-racket
  :straight (:host github :repo "hasu/emacs-ob-racket"))

;; SICP
(use-package sicp
  :straight t)


;; ---------------------------------
;; sh / Bash
;; ----------
;; Scripts for POSIX Shell and Bash.
;;
;; NOTE: The first time you use this
;;       configuration, you must run
;; `M-x lsp-install-server RET bash-ls RET'
;; ---------------------------------

;; Configure sh-mode
(use-package sh-mode
  :hook (sh-mode . flycheck-mode))

;; Configure LSP for sh / Bash
(use-package lsp-mode
  :config
  (setq lsp-bash-highlight-parsing-errors t))


;; ---------------------------------
;; Literate programming
;; --------------------
;; Execute code directly in org-mode
;; via `org-babel'
;; ---------------------------------

(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; Programming languages
   (C          . t)
   (emacs-lisp . t)
   (latex      . t)
   (lisp       . t)
   (makefile   . t)
   (python     . t)
   (racket     . t)
   ;; Diagrams
   (ditaa      . t)
   (dot        . t)
   (plantuml   . t)))

;; Don't ask every time I want to evaluate something
(setq org-confirm-babel-evaluate nil)

;; Redisplay inline images after execute code block
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)


;; ---------------------------------
;; HTTP Server
;; -----------
;; Test some websites why don't you
;; ---------------------------------

(use-package simple-httpd
  :straight t)




;; ---------------------------------------------------------------------
;;; Applications
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Web Browsers
;; ------------
;; Gotta be able to browse the web
;; ---------------------------------

;; Default Browser
(setq browse-url-firefox-program "firefox-bin")
(setq browse-url-browser-function 'browse-url-firefox)


;; ---------------------------------
;; Finance
;; -------
;; Emacs is perfect for plaintext
;; accounting.
;; ---------------------------------

(use-package hledger-mode
  :straight t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :config
  (setq hledger-jfile (expand-file-name "~/kb/self.fin.hledger")
        hledger-currency-string "USD"))

;; Completion for accounts
(defun mm/hledger-completion-accounts ()
  (when-let ((bounds (and (boundp 'hledger-accounts-cache)
                          (bounds-of-thing-at-point 'symbol))))
    (list (car bounds) (point) hledger-accounts-cache)))
(add-hook 'hledger-mode-hook 'mm/hledger-completion-accounts)

(use-package flycheck-hledger
  :straight t
  :after (flycheck
          hledger-mode)
  :demand t
  :config
  (setq flycheck-hledger-strict t)
  (dolist (check
     '("orderless" "payees" "accounts" "ordereddates" "recentassertions" "commodities" "uniqueleafnames"))
     (add-to-list 'flycheck-hledger-checks check)))


;; ---------------------------------------------------------------------
;;; Keybinds
;; ---------
;; For now, just a few simple changes.
;; In the future there will be whole custom keybind system.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Keybind assistance
;; ------------------
;; There's no need to remember every
;; keybinds by heart. Just tell me.
;; ---------------------------------

;; Show keybind hints as they are typed
(use-package which-key
  :straight t
  :delight
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 1.5
        which-key-side-window-max-height 5))

;; Define function and keybind to list all bindings of a certain key
(defun mm/print-all-bindings-for-key (key)
  "Output all bindings for KEY in *MESSAGES*."
  (interactive "kEnter key sequence:  ")
  (mapc (lambda (pair)
          (when (cdr pair)
            (message "%S: %S" (car pair) (cdr pair))))
        (mapcar (lambda (keymap)
                  (cons keymap
                        (let ((binding (lookup-key (eval keymap) key)))
                          (and (not (numberp binding)) binding))))
                (let (atoms)
                  (mapatoms (lambda (a)
                              (when (keymapp (and (boundp a) (eval a)))
                                (push a atoms))))
                  atoms))))
(global-set-key (kbd "C-h C-u k") 'mm/print-all-bindings-for-key)


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


;; ---------------------------------
;; Movement
;; --------
;; MMOSMacs uses s-IJKLUO keybinds
;; for movement.
;;
;; Super + IJKL is equivalent to the
;; arrow keys.
;;
;; Super + U & O are equivalent to
;; Home & End.
;; ---------------------------------

;; Move by character
(global-set-key (kbd "s-i") (kbd "<up>"))
(global-set-key (kbd "s-I") (kbd "<S-up>"))
(global-set-key (kbd "s-k") (kbd "<down>"))
(global-set-key (kbd "s-K") (kbd "<S-down>"))
(global-set-key (kbd "s-j") (kbd "<left>"))
(global-set-key (kbd "s-J") (kbd "<S-left>"))
(global-set-key (kbd "s-l") (kbd "<right>"))
(global-set-key (kbd "s-L") (kbd "<S-right>"))

;; Move left/right by word
(global-set-key (kbd "s-C-j") (kbd "<C-left>"))
(global-set-key (kbd "s-C-S-j") (kbd "<C-S-left>"))
(global-set-key (kbd "s-C-l") (kbd "<C-right>"))
(global-set-key (kbd "s-C-S-l") (kbd "<C-S-right>"))

;; Move up/down by paragraph
(global-set-key (kbd "s-C-i") (kbd "<C-up>"))
(global-set-key (kbd "s-C-S-i") (kbd "<C-S-up>"))
(global-set-key (kbd "s-C-k") (kbd "<C-down>"))
(global-set-key (kbd "s-C-S-k") (kbd "<C-S-down>"))

;; Home
(global-set-key (kbd "<home>") 'beginning-of-visual-line)
(global-set-key (kbd "s-u") (kbd "<home>"))
(global-set-key (kbd "s-U") (kbd "S-<home>"))
(global-set-key (kbd "C-<home>") 'beginning-of-line-text)
(global-set-key (kbd "s-C-u") (kbd "C-<home>"))
(global-set-key (kbd "s-C-S-u") (kbd "C-S-<home>"))

;; End
(global-set-key (kbd "s-o") (kbd "<end>"))
(global-set-key (kbd "s-O") (kbd "S-<end>"))
(global-set-key (kbd "C-<end>") (kbd "<end>"))
(global-set-key (kbd "C-S-<end>") (kbd "S-<end>"))
(global-set-key (kbd "s-C-o") (kbd "C-<end>"))
(global-set-key (kbd "s-C-S-o") (kbd "C-S-<end>"))

;; ;; Backspace / Delete
(global-set-key (kbd "s-SPC") (kbd "<backspace>"))
(global-set-key (kbd "s-C-SPC") (kbd "C-<backspace>"))
(global-set-key (kbd "M-SPC") (kbd "<delete>"))
(global-set-key (kbd "C-M-SPC") (kbd "C-<delete>"))

;; TAB
(global-set-key (kbd "<s-tab>") (kbd "<tab>"))

;; RET
(global-set-key (kbd "s-<return>") (kbd "<return>"))

;; Fix "IK" M-arrow movement for org-mode
(define-key org-mode-map (kbd "s-M-i") (kbd "M-<up>"))
(define-key org-mode-map (kbd "s-M-I") (kbd "M-S-<up>"))
(define-key org-mode-map (kbd "s-M-k") (kbd "M-<down>"))
(define-key org-mode-map (kbd "s-M-K") (kbd "M-S-<down>"))
(define-key org-mode-map (kbd "s-M-j") (kbd "M-<left>"))
(define-key org-mode-map (kbd "s-M-J") (kbd "M-S-<left>"))
(define-key org-mode-map (kbd "s-M-l") (kbd "M-<right>"))
(define-key org-mode-map (kbd "s-M-L") (kbd "M-S-<right>"))

;; Fix movement for corfu
(define-key corfu-map (kbd "s-i") 'corfu-previous)
(define-key corfu-map (kbd "s-k") 'corfu-next)

;; Fix movement for `pdf-view-mode'
(define-key pdf-view-mode-map (kbd "s-C-i") 'pdf-view-previous-page-command)
(define-key pdf-view-mode-map (kbd "s-C-k") 'pdf-view-next-page-command)


;; ---------------------------------
;; Buffer / Window Management
;; --------------------------
;; Make managing buffers and windows
;; more efficient.
;; ---------------------------------

;; Switch buffer
(global-set-key (kbd "s-b") 'switch-to-buffer)


;; ---------------------------------
;; Insert date/time at POINT
;; -------------------------
;; Keybinds to quickly insert
;; date/time at point in the correct
;; format.
;; ---------------------------------

;; Insert active timestamp
(defvar mm/date-time-format-active "<%Y-%m-%d %a %H:%M>"
  "Active date/time format for `mm/insert-date-time-active' function.
See help of `format-time-string' for alternative formats.")
(defun mm/insert-date-time-active ()
  "Insert active timestamp at POINT in the format of `mm/date-time-format-active'."
  (interactive)
  (insert (format-time-string mm/date-time-format-active (current-time))))
(global-set-key (kbd "C-c i D") (lambda () (interactive) (mm/insert-date-time-active)))

;; Insert inactive timestamp
(defvar mm/date-time-format-inactive "[%Y-%m-%d %a %H:%M]"
  "Inactive timestamp format for `mm/insert-date-time-inactive' function.
See help of `format-time-string' for alternative formats.")
(defun mm/insert-date-time-inactive ()
  "Insert inactive timestamp at POINT in the format of `mm/date-time-format-inactive'."
  (interactive)
  (insert (format-time-string mm/date-time-format-inactive (current-time))))
(global-set-key (kbd "C-c i d") (lambda () (interactive) (mm/insert-date-time-inactive)))

;; Insert current date with no formatting
(defvar mm/date-format "%Y-%m-%d"
  "Date format for `mm/insert-date-string'.")
(defun mm/insert-date-string ()
  "Insert date at POINT in the format of `mm/date-format'."
  (interactive)
  (insert (format-time-string mm/date-format (current-time))))
(global-set-key (kbd "C-c i C-u D") (lambda () (interactive) (mm/insert-date-string)))

;; Insert current time with no formatting
(defvar mm/time-format "%H:%M"
  "Time format for `mm/insert-time-string'.")
(defun mm/insert-time-string ()
  "Insert time at POINT in the format of `mm/time-format'."
  (interactive)
  (insert (format-time-string mm/time-format (current-time))))
(global-set-key (kbd "C-c i t") (lambda () (interactive) (mm/insert-time-string)))



;; ---------------------------------
;; Improved Help buffer
;; --------------------
;; Helpful provides a more
;; informative Help buffer.
;; ---------------------------------

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h p" . helpful-at-point)
         ("C-h q" . helpful-kill-buffers)
         ("C-h s" . helpful-symbol)
         ("C-h v" . helpful-variable)
         ("C-h x" . helpful-command)))


;; ---------------------------------
;; Comment / Uncomment region
;; --------------------------
;; Comment / uncomment entire
;; regions instead of having to do
;; each line separately.
;; ---------------------------------

(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)


;; ---------------------------------------------------------------------
;;; End of MMOSMacs configuration
;; ------------------------------

;; ---------------------------------
;; Start Emacs Server
;; ------------------
;; This is the last step of MMOSMacs
;; configuration.
;; ---------------------------------

(add-hook 'after-init-hook (lambda ()
                             (require 'server)
                             (unless (server-running-p)
                               (server-start))))

;;; init.el ends here
