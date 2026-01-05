;;; hypermodern-terminal.el --- terminal functions -*- lexical-binding: t; -*-
;;
;; The tmux answer: detached.el for persistence, vterm for speed,
;; eat for remote/tramp. No use-package here - init.el owns load order.
;;

(require 'cl-lib)

;; Silence byte-compiler warnings for external variables
(defvar vterm-kill-buffer-on-exit)
(defvar vterm-max-scrollback)
(defvar vterm-always-compile-module)
(defvar vterm-shell)
(defvar eat-term-name)
(defvar eat-enable-shell-prompt-annotation)
(defvar eat-enable-directory-tracking)
(defvar eat-enable-auto-line-mode)
(defvar eat-kill-buffer-on-exit)
(defvar detached-db-directory)
(defvar detached-terminal-data-command)
(defvar detached-notification-function)
(defvar detached-show-output-on-attach)
(defvar detached-compile-session-action)
(defvar server-after-make-frame-hook)
(defvar confirm-kill-processes)

;; Silence byte-compiler warnings for external functions
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function eat "eat" (&optional buffer-name))
(declare-function eat-project "eat" ())
(declare-function server-running-p "server" ())
(declare-function server-start "server" (&optional leave-dead inhibit-prompt))
(declare-function detached-init "detached" ())
(declare-function detached-state-transition-echo-message "detached" (session))

(defgroup hypermodern-terminal nil
  "Terminal configuration."
  :group 'hypermodern)

(defcustom hypermodern/terminal-backend 'vterm
  "Terminal backend: \\='vterm (fast, local) or \\='eat (tramp-compatible)."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat)))

(defvar hypermodern/vterm-height 0.3
  "Height of popup vterm as fraction of frame.")

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // vterm // functions
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/vterm-setup ()
  "Configure vterm. Call from (use-package vterm :config ...)."
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 100000)
  (setq vterm-always-compile-module t)
  (setq vterm-shell (or (getenv "SHELL") "/bin/bash")))

(defun hypermodern/vterm-project ()
  "Open vterm in project root."
  (interactive)
  (require 'project)
  (require 'vterm)
  (let* ((project (project-current t))
         (default-directory (project-root project))
         (buffer-name (format "*vterm-%s*"
                              (file-name-nondirectory
                               (directory-file-name default-directory)))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (vterm buffer-name))))

(defun hypermodern/vterm-named (name)
  "Create or switch to vterm with NAME."
  (interactive "sTerminal name: ")
  (require 'vterm)
  (let ((buffer-name (format "*vterm-%s*" name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (vterm buffer-name))))

(defun hypermodern/vterm-toggle ()
  "Toggle vterm at bottom of frame."
  (interactive)
  (require 'vterm)
  (let ((buffer (get-buffer "*vterm-popup*")))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))
      (let ((window (split-window-below
                     (- (floor (* (frame-height)
                                  (- 1 hypermodern/vterm-height)))))))
        (select-window window)
        (if buffer
            (switch-to-buffer buffer)
          (vterm "*vterm-popup*"))))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // eat // functions
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/eat-setup ()
  "Configure eat. Call from (use-package eat :config ...)."
  (setq eat-term-name "xterm-256color")
  (setq eat-enable-shell-prompt-annotation t)
  (setq eat-enable-directory-tracking t)
  (setq eat-enable-auto-line-mode t)
  (setq eat-kill-buffer-on-exit t))

(defun hypermodern/eat-project ()
  "Open eat in project root."
  (interactive)
  (require 'project)
  (require 'eat)
  (let* ((project (project-current t))
         (default-directory (project-root project)))
    (eat-project)))

(defun hypermodern/eat-remote (host)
  "Open eat on remote HOST via tramp."
  (interactive "sHost: ")
  (require 'eat)
  (let ((default-directory (format "/ssh:%s:~/" host)))
    (eat (format "*eat-%s*" host))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // detached // setup
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/detached-setup ()
  "Configure detached.el. Call from (use-package detached :config ...)."
  (when (fboundp 'detached-init)
    (detached-init))
  
  (setq detached-db-directory
        (expand-file-name "detached"
                          (if (file-writable-p user-emacs-directory)
                              user-emacs-directory
                            "~/.cache/emacs/")))
  
  (setq detached-terminal-data-command
        (if (executable-find "script") 'script 'dtach))
  
  (when (fboundp 'detached-state-transition-echo-message)
    (setq detached-notification-function #'detached-state-transition-echo-message))
  
  (setq detached-show-output-on-attach t)
  (setq detached-compile-session-action
        '(:attach detached-compile-attach
          :view detached-compile-session
          :run detached-compile)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // server // daemon
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/server-setup ()
  "Start emacs server if not running."
  (require 'server)
  (unless (server-running-p)
    (server-start))
  
  (when (daemonp)
    (setq confirm-kill-processes nil)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (when (equal (length (frame-list)) 1)
                  (switch-to-buffer "*scratch*"))))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // unified // interface
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/term ()
  "Open terminal using preferred backend."
  (interactive)
  (pcase hypermodern/terminal-backend
    ('vterm (require 'vterm) (vterm))
    ('eat (require 'eat) (eat))))

(defun hypermodern/term-project ()
  "Open terminal in project root."
  (interactive)
  (pcase hypermodern/terminal-backend
    ('vterm (hypermodern/vterm-project))
    ('eat (hypermodern/eat-project))))

(defun hypermodern/term-toggle ()
  "Toggle popup terminal."
  (interactive)
  (hypermodern/vterm-toggle))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // scripts
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/install-editor-scripts ()
  "Install shell scripts for emacsclient usage."
  (interactive)
  (let ((bin-dir (expand-file-name "~/bin")))
    (make-directory bin-dir t)
    (with-temp-file (expand-file-name "e" bin-dir)
      (insert "#!/bin/sh\nemacsclient -n -a \"\" \"$@\"\n"))
    (with-temp-file (expand-file-name "ew" bin-dir)
      (insert "#!/bin/sh\nemacsclient -t -a \"\" \"$@\"\n"))
    (with-temp-file (expand-file-name "em" bin-dir)
      (insert "#!/bin/sh\nemacsclient -c -n -a \"\" \"$@\"\n"))
    (shell-command (format "chmod +x %s/e %s/ew %s/em" bin-dir bin-dir bin-dir))
    (message "Installed e, ew, em to %s" bin-dir)))

(provide 'hypermodern-terminal)
;;; hypermodern-terminal.el ends here
