;;; -*- lexical-binding: t; -*-
;;; early-init.el
;;
;; Early Init file for MMOSMacs. This is loaded before the GUI and
;; package system are initialized. This should contain only what
;; absolutely must be in Early Init.


;; ---------------------------------------------------------------------
;;; UI Adjustments
;; ---------------
;; The primary goal of this Section is to prevent Emacs from flashing or
;; stuttering visually on startup.
;;
;; If these customizations were placed in `init.el', as is the norm,
;; Emacs would initialize the UI elements, then make whatever
;; customizations we wrote.
;;
;; By placing the customizations here, they are applied before the UI is
;; initialized, preventing any visual glitches altogether.
;; ---------------------------------------------------------------------

;; ---------------------------------
;; Disable superfluous UI elements
;; -------------------------------
;; These UI elements are useless in
;; a keyboard-centric environment.
;; ---------------------------------

;; Disable the menu bar
(push '(menu-bar-lines . 0) default-frame-alist)

;; Disable the tool-bar
(push '(tool-bar-lines . 0) default-frame-alist)

;; Disable scroll bars
(push '(vertical-scroll-bars . 0) default-frame-alist)
