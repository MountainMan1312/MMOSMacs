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
  :init
  (setq gcmh-idle-delay 15
        gcmh-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 (* 1024 1024)))  ;16mb
  :hook (after-init-hook . gcmh-mode))


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

(set-background-color "black")


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
                org-agenda-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


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
;; Project management
;; ------------------
;; `Projectile' provides features
;; for operating on a project level.
;; ---------------------------------

(use-package projectile
  :straight t
  :config
  (projectile-mode)
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
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))




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
  ("C-c t l" . org-toggle-link-display))


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
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))


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


;; ---------------------------------
;; Improved HOME key behavior
;; --------------------------
;; Something bugs me about HOME
;; moving POINT to the actual
;; beginning of the line. I want it
;; to go to the beginning of the
;; text (i.e. follow indentation)
;; ---------------------------------

;; Make home key respect indentation
(global-set-key (kbd "<home>") 'beginning-of-line-text)

;; Use Shift + HOME for old behavior
(global-set-key (kbd "C-<home>") 'beginning-of-visual-line)


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
         ("C-h v" . helpful-variable)
         ("C-h x" . helpful-command)))




;; ---------------------------------------------------------------------
;;; Final steps
;; ------------
;; These are the final steps before MMOSMacs is initialized
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Start Emacs server
;; ------------------
;; Fuck it, it's the end of the
;; file, I'm not writing these
;; stupid decriptions anymore!
;; ---------------------------------

(server-start)
