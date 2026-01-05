;;; hypermodern-windows.el --- window discipline -*- lexical-binding: t; -*-
;;
;; The law: nothing moves your shit around. Ever.
;; No use-package here - init.el owns load order.
;;

(require 'cl-lib)
(require 'winner)

;; Silence byte-compiler warnings for external variables
(defvar shackle-default-rule)
(defvar shackle-rules)
(defvar popper-reference-buffers)
(defvar popper-display-control)

;; Silence byte-compiler warnings for external functions
(declare-function shackle-mode "shackle" (&optional arg))
(declare-function popper-mode "popper" (&optional arg))
(declare-function popper-echo-mode "popper" (&optional arg))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // the law // setup function
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/windows-init ()
  "Set window discipline rules."
  ;; Never resize windows automatically
  (setq even-window-sizes nil)
  (setq window-combination-resize nil)

  ;; Never split windows automatically
  (setq split-height-threshold nil)
  (setq split-width-threshold nil)

  ;; pop-to-buffer: reuse current window, don't split
  (setq pop-up-windows nil)
  (setq pop-up-frames nil)

  ;; switch-to-buffer should never touch other windows
  (setq switch-to-buffer-obey-display-actions nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)

  ;; Compilation doesn't get to resize
  (setq compilation-window-height nil)

  ;; Help doesn't auto-select
  (setq help-window-select nil)

  ;; Winner mode for undo
  (winner-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // shackle // rules
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/shackle-rules
  '(;; Popups: bottom slot, consistent size
    ("\\*Help\\*"              :regexp t :align below :size 0.33 :select t)
    ("\\*helpful.*\\*"         :regexp t :align below :size 0.33 :select t)
    ("\\*info\\*"              :regexp t :align below :size 0.33 :select t)
    ("\\*Info\\*"              :regexp t :align below :size 0.33 :select t)
    ("\\*Man.*\\*"             :regexp t :align below :size 0.33 :select t)
    ("\\*compilation\\*"       :regexp t :align below :size 0.33 :select nil)
    ("\\*Compile-Log\\*"       :regexp t :align below :size 0.25 :select nil)
    ("\\*Warnings\\*"          :regexp t :align below :size 0.20 :select nil)
    ("\\*Backtrace\\*"         :regexp t :align below :size 0.33 :select t)
    ("\\*Messages\\*"          :regexp t :align below :size 0.25 :select nil)
    ("\\*Completions\\*"       :regexp t :align below :size 0.25 :select nil)
    ("\\*rg\\*"                :regexp t :align below :size 0.40 :select t)
    ("\\*grep\\*"              :regexp t :align below :size 0.40 :select t)
    ("\\*deadgrep.*\\*"        :regexp t :align below :size 0.40 :select t)
    ("\\*Occur\\*"             :regexp t :align below :size 0.33 :select t)
    ("\\*xref\\*"              :regexp t :align below :size 0.33 :select t)
    ("\\*eldoc\\*"             :regexp t :align below :size 0.20 :select nil)
    ("\\*Flycheck.*\\*"        :regexp t :align below :size 0.25 :select nil)
    ("\\*Flymake.*\\*"         :regexp t :align below :size 0.25 :select nil)
    ("\\*vc-.*\\*"             :regexp t :align below :size 0.33 :select nil)
    ("\\*Shell Command.*\\*"   :regexp t :align below :size 0.25 :select nil)
    ("\\*Async Shell.*\\*"     :regexp t :align below :size 0.25 :select nil)

    ;; Magit
    ("magit: .*"               :regexp t :same t :select t)
    ("magit-diff:.*"           :regexp t :align below :size 0.50 :select nil)
    ("magit-log:.*"            :regexp t :align below :size 0.50 :select t)
    ("magit-process:.*"        :regexp t :align below :size 0.25 :select nil)
    ("COMMIT_EDITMSG"          :regexp t :same t :select t)

    ;; Terminal: other window if available
    (vterm-mode                :other t :select t)
    (eshell-mode               :other t :select t)
    (shell-mode                :other t :select t)

    ;; Org
    ("\\*Org Agenda\\*"        :regexp t :same t :select t)
    ("\\*Org Select\\*"        :regexp t :align below :size 0.33 :select t))
  "Shackle display rules.")

(defun hypermodern/shackle-setup ()
  "Configure shackle. Call from (use-package shackle :config ...)."
  (setq shackle-default-rule '(:same t))
  (setq shackle-rules hypermodern/shackle-rules)
  (shackle-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // popper // popup tracking
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/popper-reference-buffers
  '("\\*Messages\\*"
    "\\*Warnings\\*"
    "\\*Compile-Log\\*"
    "\\*compilation\\*"
    "\\*Backtrace\\*"
    "\\*Async Shell Command\\*"
    "\\*Shell Command Output\\*"
    "\\*rg\\*"
    "\\*grep\\*"
    "\\*deadgrep.*\\*"
    "\\*helpful.*\\*"
    "\\*Help\\*"
    "\\*info\\*"
    "\\*Info\\*"
    "\\*Man.*\\*"
    "\\*xref\\*"
    "\\*Occur\\*"
    "\\*Flycheck.*\\*"
    "\\*Flymake.*\\*"
    "\\*eldoc\\*"
    "\\*vc-.*\\*"
    "magit-diff:.*"
    "magit-log:.*"
    "magit-process:.*"
    help-mode
    helpful-mode
    compilation-mode
    grep-mode
    occur-mode
    xref--xref-buffer-mode)
  "Buffers/modes that popper tracks as popups.")

(defun hypermodern/popper-setup ()
  "Configure popper. Call from (use-package popper :config ...)."
  (setq popper-reference-buffers hypermodern/popper-reference-buffers)
  (setq popper-display-control nil)
  (popper-mode 1)
  (when (fboundp 'popper-echo-mode)
    (popper-echo-mode 1)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // functions
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/dedicate-window ()
  "Toggle current window dedication."
  (interactive)
  (let ((dedicated (not (window-dedicated-p))))
    (set-window-dedicated-p (selected-window) dedicated)
    (message "Window %s" (if dedicated "dedicated" "undedicated"))))

(defun hypermodern/split-right ()
  "Split right. Show other-buffer in new window."
  (interactive)
  (select-window (split-window-right))
  (switch-to-buffer (other-buffer)))

(defun hypermodern/split-below ()
  "Split below. Show other-buffer in new window."
  (interactive)
  (select-window (split-window-below))
  (switch-to-buffer (other-buffer)))

(defun hypermodern/rotate-windows ()
  "Rotate windows in current frame."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this (selected-window))
             (other (next-window))
             (this-buf (window-buffer this))
             (other-buf (window-buffer other)))
        (set-window-buffer this other-buf)
        (set-window-buffer other this-buf)
        (select-window other))
    (message "Can only rotate 2 windows")))

(provide 'hypermodern-windows)
;;; hypermodern-windows.el ends here
