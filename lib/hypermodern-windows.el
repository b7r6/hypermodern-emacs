;;; hypermodern-windows.el --- window discipline -*- lexical-binding: t; -*-
;;
;; The law: nothing moves your shit around. Ever.
;;
;; - New buffers appear in the current window OR a designated popup zone
;; - Existing windows are NEVER rearranged, resized, or deleted by Emacs
;; - You control splits. You control focus. You control layout.
;;
;; Implementation: shackle for display rules, popper for popup tracking.
;; These libraries have years of edge case handling. We configure them.
;;

(require 'cl-lib)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // the law
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Never resize windows automatically
(setq even-window-sizes nil)
(setq window-combination-resize nil)

;; Never split windows automatically
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; pop-to-buffer: reuse current window, don't split
(setq pop-up-windows nil)

;; No popup frames either
(setq pop-up-frames nil)

;; switch-to-buffer should never touch other windows
(setq switch-to-buffer-obey-display-actions nil)
(setq switch-to-buffer-in-dedicated-window 'pop)

;; Compilation doesn't get to resize
(setq compilation-window-height nil)

;; Help doesn't auto-select
(setq help-window-select nil)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // shackle // display-buffer rules
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Shackle intercepts display-buffer and enforces our rules.
;; Key insight: :same means "reuse current window"
;;              :other means "reuse any other window, don't split"
;;              :popup means "use the popup zone"

(use-package shackle
  :demand t
  :config
  (setq shackle-default-rule '(:same t))  ; default: current window
  
  (setq shackle-rules
        '(;; Popups: bottom slot, consistent size, don't steal focus unless useful
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
          
          ;; Magit: same window for status, popup for diffs/logs
          ("magit: .*"               :regexp t :same t :select t)
          ("magit-diff:.*"           :regexp t :align below :size 0.50 :select nil)
          ("magit-log:.*"            :regexp t :align below :size 0.50 :select t)
          ("magit-process:.*"        :regexp t :align below :size 0.25 :select nil)
          ("COMMIT_EDITMSG"          :regexp t :same t :select t)
          
          ;; Terminal: other window if available, don't split
          (vterm-mode                :other t :select t)
          (eshell-mode               :other t :select t)
          (shell-mode                :other t :select t)
          
          ;; org-mode stuff
          ("\\*Org Agenda\\*"        :regexp t :same t :select t)
          ("\\*Org Select\\*"        :regexp t :align below :size 0.33 :select t)))
  
  (shackle-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // popper // popup buffer tracking
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Popper tracks which buffers are "popups" and gives us toggle/cycle.
;; It delegates display to shackle via popper-display-control nil.

(use-package popper
  :demand t
  :after shackle
  :bind (("C-\\"   . popper-toggle)
         ("M-\\"   . popper-cycle)
         ("C-M-\\" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '(;; By name pattern
          "\\*Messages\\*"
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
          
          ;; By major mode
          help-mode
          helpful-mode
          compilation-mode
          grep-mode
          occur-mode
          xref--xref-buffer-mode))
  
  ;; Let shackle handle placement
  (setq popper-display-control nil)
  
  ;; Show popup info in echo area
  (popper-mode 1)
  (popper-echo-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // dedicated windows
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/dedicate-window ()
  "Toggle current window dedication. Dedicated windows are NEVER touched."
  (interactive)
  (let ((dedicated (not (window-dedicated-p))))
    (set-window-dedicated-p (selected-window) dedicated)
    (message "Window %s" (if dedicated "dedicated" "undedicated"))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // explicit splits only
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

;; Override defaults
(global-set-key (kbd "C-x 2") #'hypermodern/split-below)
(global-set-key (kbd "C-x 3") #'hypermodern/split-right)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // winner mode // undo window changes
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; When something DOES fuck up your layout, winner-undo saves you
(winner-mode 1)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // additional keybindings
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(global-set-key (kbd "C-c w d") #'hypermodern/dedicate-window)
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-c w r") #'winner-redo)

(provide 'hypermodern-windows)
;;; hypermodern-windows.el ends here
