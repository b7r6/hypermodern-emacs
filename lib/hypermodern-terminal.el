;;; hypermodern-terminal.el --- terminal that doesn't suck -*- lexical-binding: t; -*-
;;
;; The tmux answer: detached.el for persistence, vterm for speed,
;; eat for remote/tramp, project integration for everything.
;;

(require 'cl-lib)

(defgroup hypermodern-terminal nil
  "Terminal configuration."
  :group 'hypermodern)

(defcustom hypermodern/terminal-backend 'vterm
  "Terminal backend: \\='vterm (fast, local) or \\='eat (tramp-compatible)."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // vterm // the fast one
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package vterm
  :commands (vterm vterm-other-window)
  :config
  ;; Kill buffer when shell exits
  (setq vterm-kill-buffer-on-exit t)

  ;; Max scrollback
  (setq vterm-max-scrollback 100000)

  ;; Don't query on exit
  (setq vterm-always-compile-module t)

  ;; Shell integration - the important part
  (setq vterm-shell (or (getenv "SHELL") "/bin/bash"))

  ;; Copy mode bindings
  (when (fboundp 'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "C-c C-t") #'vterm-copy-mode))
  (when (fboundp 'vterm-yank)
    (define-key vterm-mode-map (kbd "C-c C-y") #'vterm-yank))

  ;; Clear
  (when (fboundp 'vterm-clear)
    (define-key vterm-mode-map (kbd "C-c C-l") #'vterm-clear))

  ;; Make C-g work properly
  (when (fboundp 'vterm--self-insert)
    (define-key vterm-mode-map (kbd "C-g") #'vterm--self-insert))

  ;; Directory tracking (requires shell integration)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local confirm-kill-processes nil)))

  ;; Project-aware vterm
  (defun hypermodern/vterm-project ()
    "Open vterm in project root."
    (interactive)
    (let* ((project (project-current t))
           (default-directory (project-root project))
           (buffer-name (format "*vterm-%s*"
                               (file-name-nondirectory
                                (directory-file-name default-directory)))))
      (if (get-buffer buffer-name)
          (switch-to-buffer buffer-name)
        (vterm buffer-name))))

  ;; Named vterm
  (defun hypermodern/vterm-named (name)
    "Create or switch to vterm with NAME."
    (interactive "sTerminal name: ")
    (let ((buffer-name (format "*vterm-%s*" name)))
      (if (get-buffer buffer-name)
          (switch-to-buffer buffer-name)
        (vterm buffer-name))))

  ;; Toggle vterm at bottom
  (defvar hypermodern/vterm-height 0.3
    "Height of popup vterm as fraction of frame.")

  (defun hypermodern/vterm-toggle ()
    "Toggle vterm at bottom of frame."
    (interactive)
    (let ((buffer (get-buffer "*vterm-popup*")))
      (if (and buffer (get-buffer-window buffer))
          (delete-window (get-buffer-window buffer))
        (let ((window (split-window-below
                       (- (floor (* (frame-height)
                                    (- 1 hypermodern/vterm-height)))))))
          (select-window window)
          (if buffer
              (switch-to-buffer buffer)
            (vterm "*vterm-popup*")))))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // eat // the tramp-compatible one
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package eat
  :commands (eat eat-project eat-other-window)
  :config
  ;; Full terminal capabilities
  (setq eat-term-name "xterm-256color")

  ;; Shell integration (important for directory tracking)
  (setq eat-enable-shell-prompt-annotation t)
  (setq eat-enable-directory-tracking t)

  ;; Works over tramp - the key feature
  (setq eat-enable-auto-line-mode t)

  ;; Kill on exit
  (setq eat-kill-buffer-on-exit t)

  ;; Project eat
  (defun hypermodern/eat-project ()
    "Open eat in project root."
    (interactive)
    (let* ((project (project-current t))
           (default-directory (project-root project)))
      (eat-project)))

  ;; Remote eat - this is the tmux replacement
  (defun hypermodern/eat-remote (host)
    "Open eat on remote HOST via tramp."
    (interactive "sHost: ")
    (let ((default-directory (format "/ssh:%s:~/" host)))
      (eat (format "*eat-%s*" host)))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // detached // the tmux answer
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; detached.el lets you:
;; - Start commands that persist after emacs exits
;; - Reattach to running/finished commands
;; - Works locally and over tramp
;; - No tmux/screen required

(use-package detached
  :commands (detached-shell-command
             detached-compile
             detached-open-session
             detached-list-sessions)
  :config
  ;; Initialize detached
  (when (fboundp 'detached-init)
    (detached-init))
  
  ;; Where to store session info - use writable directory
  (setq detached-db-directory
        (expand-file-name "detached"
                          (if (file-writable-p user-emacs-directory)
                              user-emacs-directory
                            "~/.cache/emacs/")))

  ;; Terminal size for detached sessions
  (setq detached-terminal-data-command
        (if (executable-find "script")
            'script
          'dtach))

  ;; Show notifications on completion
  (when (fboundp 'detached-state-transition-echo-message)
    (setq detached-notification-function #'detached-state-transition-echo-message))

  ;; Duration before showing output (for quick commands)
  (defvar detached-show-output-on-attach)
  (setq detached-show-output-on-attach t)

  ;; Integration with compile
  (defvar detached-compile-session-action)
  (setq detached-compile-session-action
        '(:attach detached-compile-attach
          :view detached-compile-session
          :run detached-compile))

  ;; Key bindings
  :bind (("C-c d c" . detached-shell-command)      ; run command detached
         ("C-c d o" . detached-open-session)        ; attach to session
         ("C-c d l" . detached-list-sessions)       ; list all
         ("C-c d t" . detached-detach-session)))    ; detach current

;; Make compile use detached by default
(with-eval-after-load 'compile
  (when (fboundp 'detached-compile--around)
    (advice-add 'compile :around #'detached-compile--around)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // emacs server // the always-on setup
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Start server if not running
(require 'server)
(unless (server-running-p)
  (server-start))

;; When running as daemon, make new frames behave well
(when (daemonp)
  ;; Don't ask about clients when killing
  (setq confirm-kill-processes nil)

  ;; Create a scratch frame on connect if no frames exist
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (when (equal (length (frame-list)) 1)
                (switch-to-buffer "*scratch*")))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // shell // scripts to install
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Put these in ~/bin or wherever:

;; ~/bin/e (edit file, wait for close)
;; #!/bin/sh
;; emacsclient -n -a "" "$@"

;; ~/bin/ew (edit file in terminal)
;; #!/bin/sh
;; emacsclient -t -a "" "$@"

;; ~/bin/ee (edit file, no wait)
;; #!/bin/sh
;; emacsclient -n -a "" "$@"

;; ~/bin/em (open emacs frame if not exists, or focus)
;; #!/bin/sh
;; emacsclient -c -n -a "" "$@"

(defun hypermodern/install-editor-scripts ()
  "Install shell scripts for emacsclient usage."
  (interactive)
  (let ((bin-dir (expand-file-name "~/bin")))
    (make-directory bin-dir t)

    ;; e - quick edit, return immediately
    (with-temp-file (expand-file-name "e" bin-dir)
      (insert "#!/bin/sh\nemacsclient -n -a \"\" \"$@\"\n"))

    ;; ew - terminal edit, wait
    (with-temp-file (expand-file-name "ew" bin-dir)
      (insert "#!/bin/sh\nemacsclient -t -a \"\" \"$@\"\n"))

    ;; em - new frame
    (with-temp-file (expand-file-name "em" bin-dir)
      (insert "#!/bin/sh\nemacsclient -c -n -a \"\" \"$@\"\n"))

    ;; Make executable
    (shell-command (format "chmod +x %s/e %s/ew %s/em" bin-dir bin-dir bin-dir))

    (message "Installed e, ew, em to %s" bin-dir)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // unified // interface
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/term ()
  "Open terminal using preferred backend."
  (interactive)
  (pcase hypermodern/terminal-backend
    ('vterm (vterm))
    ('eat (eat))))

(defun hypermodern/term-project ()
  "Open terminal in project root."
  (interactive)
  (pcase hypermodern/terminal-backend
    ('vterm (when (fboundp 'hypermodern/vterm-project)
              (hypermodern/vterm-project)))
    ('eat (when (fboundp 'hypermodern/eat-project)
            (hypermodern/eat-project)))))

(defun hypermodern/term-toggle ()
  "Toggle popup terminal."
  (interactive)
  ;; Always use vterm for toggle, it's faster
  (when (fboundp 'hypermodern/vterm-toggle)
    (hypermodern/vterm-toggle)))

;; Bindings
(global-set-key (kbd "C-c T") #'hypermodern/term)
(global-set-key (kbd "C-c C-t") #'hypermodern/term-project)
(global-set-key (kbd "C-`") #'hypermodern/term-toggle)

(provide 'hypermodern-terminal)
;;; hypermodern-terminal.el ends here
