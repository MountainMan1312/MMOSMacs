;;; -*- lexical-binding: t; -*-
;;; init.el
;;
;; Main configuration file for MMOSMacs.


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
