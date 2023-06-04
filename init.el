;;; -*- lexical-binding: t; -*-
;;; init.el
;;
;; Main configuration file for MMOSMacs.


;; ---------------------------------------------------------------------
;;; Startup
;; --------
;; These items need to be done first
;; ---------------------------------------------------------------------

;; --------------------------------
;; Produce backtraces when errors
;; occur. Can be helpful to
;; diagnose startup issues.
;; ---------------------------------

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

(use-package gcmh
  :straight t
  :defer t
  :delight
  :init
  (setq gcmh-idle-delay 15
        gcmh-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024)))             ;16kb
(add-hook 'after-init-hook 'gcmh-mode)


;; ---------------------------------
;; Async
;; -----
;; Asynchronous processing of things
;; ---------------------------------

;; Async package
(use-package async
  :straight t
  :config
  (setq async-bytecomp-package-mode t))




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

;; Jazz Theme
(use-package jazz-theme
  :straight t)
(load-theme 'jazz t)

;; Background color
(set-background-color "black")

;; Cursor
(setq cursor-type 'box)
(set-cursor-color "grey13")

;; Highlights
(set-face-attribute 'highlight nil :background "#333")

;; Selection
(set-face-attribute 'region nil
                    :background "#222"
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


;; ---------------------------------
;; Fonts
;; -----
;; For now I use Iosevka and Exo
;; ---------------------------------

(set-face-attribute 'default nil :font "JetBrains Mono" :height 100)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 100)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 110 :weight 'regular)


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
  :config (set-face-attribute 'hl-line nil :background "#111")
  :hook (prog-mode . hl-line-mode))




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
        lazy-count-prefix-format nil
        lazy-count-suffix-format "    (%s/%S)"
        search-whitespace-regexp ".*"
        isearch-lax-whitespace t
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
(global-display-line-numbers-mode t)

;; Disable line numbers in specific modes
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                helpful-mode-hook
                term-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



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
  (setq mc/cmds
        '(electric-pair-delete-pair
          end-of-visual-line
          beginning-of-visual-line
          beginning-of-line-text
          mm/keyboard-escape-quit-keep-windows)))




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
  (set-fringe-mode '(10 . 0))
  :hook
  (prog-mode    . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode))




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
  (visual-line-mode)
  (variable-pitch-mode))


;; Org font stuff
(defun mm/org-font-setup ()
  ;; Replace hyphens in lists with dots
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Set font sizes
  (dolist (face '((org-level-1 . 1.1)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
                        :font "Noto Sans"
                        :weight 'regular
                        :height (cdr face))
  ;; Ensure anything that should be
  ;; fixed-pitch actually is.
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-time-grid nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-scheduled nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-agenda-structure nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-agenda-date nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-agenda-date-today nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-agenda-current-time nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-agenda-calendar-event nil :inherit 'fixed-pitch)))


;; `org' configuration
(use-package org
  :straight t
  :hook (org-mode . mm/org-mode-setup)
  :config
  (mm/org-font-setup)
  (setq org-startup-folded t
        org-ellipsis " ►"
        org-hide-leading-stars t
        org-adapt-indentation t
        org-support-shift-select 'always
        org-return-follows-link t)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  :bind
  ("C-c n l t" . org-toggle-link-display)
  ("C-c t"     . org-agenda-todo))


;; Make org heading bullets look nicer
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("•")))


;; Auto-show emphasis markers on hover
(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks t
        org-appear-autosubmarkers t))


;; Show link hints to make following links easier
(use-package link-hint
  :straight t
  :bind
  ("C-c n l o" . link-hint-open-link))


;; ---------------------------------
;; `org-roam'
;; ----------
;; Org roam adds a ton of PKMS
;; functionality to org.
;; ---------------------------------

;; Define function to insert a link to a node without opening it
(defun mm/org-roam-node-insert-immediate (arg &rest args)
  "This version of `org-roam-node-insert' inserts a node without opening it."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish)))))
    (apply #'org-roam-node-insert args)))


;; Add Dendron-like note refactoring functionality
;; This was taken from https://github.com/vicrdguez/dendroam
(cl-defmethod mm/org-roam-node-current-file (node)
  (file-name-base (org-roam-node-file node)))

(cl-defmethod mm/org-roam-node-hierarchy-title (node)
  (capitalize (car (last (split-string (org-roam-node-title node)
                                       "\\.")))))

(defun mm/org-roam-refactor-file ()
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
  "Gets all the nodes that share the same HIERARCHY totally or partially"
  (let ((files (mapcar #'car (org-roam-db-query [:select [file]
                                                         :from nodes
                                                         :where (like file $r1)]
                                                (concat "%" hierarchy "%"))))) files))

(defun mm/org-roam-refactor-hierarchy (&optional current)
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
  (org-roam-directory (file-truename "~/kb"))
  (make-directory org-roam-directory 'parents)
  (org-roam-db-location (concat user-emacs-directory "/org-roam.db"))
  (org-roam-node-display-template (concat
                                   (propertize "${title:48}    " 'face 'org-document-title)
                                   (propertize "${file:*}" 'face 'org-roam-dim)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :unnarrowed t
      :if-new (file+head "${title}.org"
                         "#+TITLE:"))))
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
         ("C-c n c"   . org-roam-capture)
         ("C-c n d t" . org-roam-dailies-goto-today)
         ("C-c n d y" . org-roam-dailies-goto-yesterday)
         ("C-c n i"   . mm/org-roam-node-insert-immediate)
         ("C-c n I"   . org-roam-node-insert)
         ("C-c n u"   . org-roam-update-org-id-locations)
         ("C-c n r h" . mm/org-roam-refactor-hierarchy)
         ("C-c n r f" . mm/org-roam-refactor-file)))

;; Graph UI for org-roam.
(use-package org-roam-ui
  :straight t
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
(use-package org
  :delight
  (org-indent-mode)
  :config
  (setq org-agenda-start-with-log-mode t
        org-agenda-files '("~/kb/agenda.org")
        org-todo-keywords'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "HOLD(h)"
                                     "REVIEW(re)" "|" "DONE(d)" "CANCELED(ca)")
                           (sequence "EVENT(e)" "|" "MISSED_EVENT(me)" "ATTENDED_EVENT(ae)")
                           (sequence "APPT(ap)" "|" "MISSED_APPT(ma)" "ATTENDED_APPT(aa)")
                           (sequence "CLASS(cl)" "|" "ATTENDED_CLASS(ac)"
                                     "MISSED_CLASS(mc)" "CANCELED_CLASS(cc)")
                           (sequence "REMINDER(rm)"))
        org-agenda-span 7
        org-agenda-start-day "0d"
        org-agenda-start-on-weekday nil
        org-agenda-use-time-grid t
        org-agenda-time-grid (quote ((daily today remove-match)
                                     (0 100 200 300 400 500 600 700 800 900
                                        1000 1100 1200 1300 1400 1500 1600
                                        1700 1800 1900 2000 2100 2200 2300)
                                     "......." "."))
        org-agenda-include-diary t
        org-agenda-show-future-repeats nil
        org-agenda-repeating-timestamp-show-all nil
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-show-done-always-green nil
        org-agenda-compact-blocks t
        org-log-done 'time
        org-log-into-drawer t)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-habits t
        org-habit-show-habits-only-for-today nil
        org-habit-show-all-today nil
        org-habit-graph-column 60
        org-habit-following-days 1
        org-habit-preceding-days 21)
  :bind (("C-c a"   . org-agenda-list)
         ("C-c n a" . (lambda () (interactive) (find-file "~/kb/agenda.org")))))


;; Update agenda periodically every `mm/refresh-agenda-time-seconds' seconds.
;; This was taken from https://emacs.stackexchange.com/a/68767/38877
(defvar mm/refresh-agenda-time-seconds 15)
(defvar mm/refresh-agenda-timer nil
  "Timer for `mm/refresh-agenda-timer-function' to reschedule itself, or NIL")
(defun mm/refresh-agenda-timer-function ()
  "If the user types a command while `mm/refresh-agenda-timer' is active, the next time this function is called from it's main idle timer, deactivate `mm/refresh-agenda-timer'."
  (when mm/refresh-agenda-timer
    (cancel-timer mm/refresh-agenda-timer))
  ;;(lambda () (save-window-excursion (org-agenda nil "a")))
  (save-window-excursion (org-agenda nil "a"))
  (setq mm/refresh-agenda-timer
        (run-with-idle-timer
         (time-add (current-idle-time) mm/refresh-agenda-time-seconds)
         nil
         'mm/refresh-agenda-timer-function)))
(run-with-idle-timer mm/refresh-agenda-time-seconds t 'mm/refresh-agenda-timer-function)




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
  (setq vterm-always-compile-module t))


;; ---------------------------------
;; Syntax checking
;; ---------------
;; I'd like to know when I'm making
;; a mistake.
;; ---------------------------------

;; Eldoc shows function definition hints in the echo area
(use-package eldoc
  :straight (:type built-in)
  :delight
  :config)

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
  :hook (emacs-lisp-mode . flycheck-mode))


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
;; Improved HOME key behavior
;; --------------------------
;; Something bugs me about HOME
;; moving POINT to the actual
;; beginning of the line. I want it
;; to go to the beginning of the
;; text (i.e. follow indentation)
;; ---------------------------------


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
(global-set-key (kbd "<home>") 'beginning-of-line-text)
(global-set-key (kbd "s-u") (kbd "<home>"))
(global-set-key (kbd "s-U") (kbd "S-<home>"))
(global-set-key (kbd "C-<home>") 'beginning-of-visual-line)
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
(define-key org-mode-map (kbd "s-M-k") (kbd "M-<down>"))


;; ---------------------------------
;; Buffer / Window Management
;; --------------------------
;; Make managing buffers and windows
;; more efficient.
;; ---------------------------------

;; Switch buffers
(global-set-key (kbd "s-b") 'switch-to-buffer)

;; Switch windows
(global-set-key (kbd "s-w") 'other-window)


;; ---------------------------------
;; Insert date/time at POINT
;; -------------------------
;; [YYYY-MM-DD HH:mm]
;; ---------------------------------

(defvar mm/date-time-format "[%Y-%m-%d %H:%M]"
  "Format of the date to insert with `mm/insert-date-time' function. See help of `format-time-string' for alternative formats.")

(defun mm/insert-date-time ()
  "Insert the current date time in the format of `mm/date-time-format' at POINT."
  (interactive)
  (insert (format-time-string mm/date-time-format (current-time))))

(global-set-key (kbd "C-c d") (lambda () (interactive) (mm/insert-date-time)))


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
