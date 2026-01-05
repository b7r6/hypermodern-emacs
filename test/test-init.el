;;; test-init.el --- Test environment initialization -*- lexical-binding: t; -*-
;;
;; This file initializes the test environment by calling all module setup
;; functions and configuring packages as they would be in init.el.
;;

;; Load modules
(require 'hypermodern-core)
(require 'hypermodern-compile)
(require 'hypermodern-languages)
(require 'hypermodern-navigation)
(require 'hypermodern-terminal)
(require 'hypermodern-windows)
(require 'hypermodern-remote)
(require 'hypermodern-secrets)
(require 'hypermodern-ui nil t)  ; Optional UI module

;; Enable built-in modes that hypermodern setup functions depend on
(recentf-mode 1)
(savehist-mode 1)

;; Initialize core
(hypermodern/core-init)
(hypermodern/files-setup)
(hypermodern/project-setup)
(hypermodern/leader-setup)

;; Initialize compilation
(hypermodern/compile-setup)

;; Set up compile keymaps
(global-set-key (kbd "C-c c") hypermodern/compile-map)
(global-set-key (kbd "<f5>") #'hypermodern/compile)
(global-set-key (kbd "<f6>") #'hypermodern/recompile)
(global-set-key (kbd "<f7>") #'hypermodern/compile-test)

;; Initialize eglot (requires eglot to be loaded)
(when (require 'eglot nil t)
  ;; Eglot may not define eglot-workspace-configuration by default
  (unless (boundp 'eglot-workspace-configuration)
    (defvar eglot-workspace-configuration nil))
  (hypermodern/eglot-setup))

;; Load gcmh if available
(when (require 'gcmh nil t)
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode 1))

;; Initialize ace-window configuration (requires ace-window to be loaded)
(when (require 'ace-window nil t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key (kbd "M-o") #'ace-window))

;; Initialize consult-dir (requires consult-dir to be loaded)
(when (require 'consult-dir nil t)
  (hypermodern/consult-dir-setup))

;; Initialize windows
(hypermodern/windows-init)

;; Initialize shackle (requires shackle to be loaded)
(when (require 'shackle nil t)
  (hypermodern/shackle-setup))

;; Initialize popper (requires popper to be loaded)
(when (require 'popper nil t)
  (hypermodern/popper-setup))

;; Set up navigation keybindings
(global-set-key (kbd "C-c z z") #'hypermodern/zoxide-jump)
(global-set-key (kbd "C-c n") #'hypermodern/navigate)

;; Set up window keybindings
(global-set-key (kbd "C-x 2") #'hypermodern/split-below)
(global-set-key (kbd "C-x 3") #'hypermodern/split-right)
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-\\") #'popper-toggle)
(global-set-key (kbd "M-\\") #'popper-cycle)

;; Set up terminal keybindings
(global-set-key (kbd "C-c T") #'hypermodern/term)
(global-set-key (kbd "C-`") #'hypermodern/term-toggle)

;; Set up detached keybindings
(global-set-key (kbd "C-c d c") #'detached-shell-command)
(global-set-key (kbd "C-c d o") #'detached-open-session)
(global-set-key (kbd "C-c d l") #'detached-list-sessions)

;; Set up format keybinding
(global-set-key (kbd "M-z") #'hypermodern/format-buffer)

;; Set up remote keymaps
(global-set-key (kbd "C-c t") hypermodern/tailscale-map)
(global-set-key (kbd "C-c r") hypermodern/remote-map)

;; Set up magit keybinding (only if git is available)
(when (and (executable-find "git")
           (require 'magit nil t))
  (global-set-key (kbd "C-x g") #'magit-status))

;; Set up avy keybindings (matching init.el)
(when (require 'avy nil t)
  (global-set-key (kbd "C-'") #'avy-goto-char-timer)
  (global-set-key (kbd "C-;") #'avy-goto-word-1))

;; Set up deadgrep keybinding (matching init.el)
(when (require 'deadgrep nil t)
  (global-set-key (kbd "C-c s") #'deadgrep))

;; Set up expand-region keybinding
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-=") #'er/expand-region))

;; Set up multiple-cursors keybindings
(when (require 'multiple-cursors nil t)
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this))

;; Initialize server
(hypermodern/server-setup)

(provide 'test-init)
;;; test-init.el ends here
