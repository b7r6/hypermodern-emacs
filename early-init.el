;;; early-init.el --- hypermodern visuals bootstrap -*- lexical-binding: t; -*-

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // terminal // truecolor // the hard way
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; The problem: Emacs checks TERM to decide color capability, but:
;; - tmux sets TERM=screen-256color (lies about truecolor)
;; - ssh doesn't forward COLORTERM
;; - TERM=xterm-256color doesn't imply 24-bit
;;
;; The fix: detect truecolor capability and tell Emacs explicitly.

(defun hypermodern/terminal-supports-truecolor-p ()
  "Check if terminal actually supports 24-bit color."
  (or
   ;; COLORTERM is the reliable signal (when present)
   (member (getenv "COLORTERM") '("truecolor" "24bit"))
   ;; Modern terminals set this
   (string-match-p ":\\(truecolor\\|24bit\\|RGB\\)" (or (getenv "TERM_FEATURES") ""))
   ;; Known good terminals (check TERM_PROGRAM first, more reliable)
   (member (getenv "TERM_PROGRAM")
           '("ghostty" "WezTerm" "iTerm.app" "Apple_Terminal" "vscode"))
   ;; Fallback: known good TERM values
   (string-match-p "\\(ghostty\\|kitty\\|alacritty\\|foot\\|wezterm\\)"
                   (or (getenv "TERM") ""))
   ;; tmux with proper config sets this
   (and (getenv "TMUX")
        (member (getenv "COLORTERM") '("truecolor" "24bit")))))

(unless (display-graphic-p)
  (when (hypermodern/terminal-supports-truecolor-p)
    ;; Force COLORTERM for child processes and Emacs internals
    (setenv "COLORTERM" "truecolor")
    
    ;; The actual fix: add a terminfo-like entry for 24-bit color
    ;; This must happen before any frame is created
    (add-to-list 'term-file-aliases '(".*" . "xterm-direct"))
    
    ;; Tell Emacs this terminal can do 24-bit
    (set-terminal-parameter nil 'background-mode 'dark)
    
    ;; Override the color count for the default terminal
    ;; 16777216 = 256^3 = 24-bit RGB
    (tty-set-up-initial-frame-faces)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // startup // performance
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      package-enable-at-startup nil
      frame-inhibit-implied-resize t
      inhibit-compacting-font-caches t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message "")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 256 1024 1024)
                  gc-cons-percentage 0.1)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // diagnostics
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/color-diag ()
  "Print terminal color diagnostic info."
  (interactive)
  (let ((info (list
               (cons "display-graphic-p" (display-graphic-p))
               (cons "TERM" (getenv "TERM"))
               (cons "TERM_PROGRAM" (getenv "TERM_PROGRAM"))
               (cons "COLORTERM" (getenv "COLORTERM"))
               (cons "TMUX" (if (getenv "TMUX") "yes" "no"))
               (cons "tty-color-alist length" (length (tty-color-alist)))
               (cons "display-color-cells" (display-color-cells))
               (cons "truecolor-detected" (hypermodern/terminal-supports-truecolor-p)))))
    (with-current-buffer (get-buffer-create "*color-diag*")
      (erase-buffer)
      (insert "=== hypermodern color diagnostics ===\n\n")
      (dolist (pair info)
        (insert (format "%-25s %s\n" (car pair) (cdr pair))))
      (insert "\n")
      (if (>= (display-color-cells) 16777216)
          (insert "✓ 24-bit color ACTIVE\n")
        (insert "✗ 24-bit color NOT active\n")
        (insert "\nTo fix in tmux, add to ~/.tmux.conf:\n")
        (insert "  set -g default-terminal \"tmux-256color\"\n")
        (insert "  set -sa terminal-features ',xterm*:RGB'\n")
        (insert "  set -ga terminal-overrides ',*:Tc'\n"))
      (pop-to-buffer (current-buffer)))))

(provide 'early-init)
;;; early-init.el ends here
