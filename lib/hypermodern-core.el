;;; hypermodern-core.el --- the foundations -*- lexical-binding: t; -*-
;;
;; The stuff that just works and should be on by default.
;;

(require 'cl-lib)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // quick wins // stuff that works
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Smooth scrolling (emacs 29+, actually good on pgtk)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Repeat mode - C-x o o o instead of C-x o C-x o C-x o
(repeat-mode 1)

;; Handle minified JS and long lines without freezing
(global-so-long-mode 1)

;; Native comp - don't spam warnings
(when (featurep 'native-compile)
  (defvar native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-jit-compilation t)
  (defvar native-comp-speed)
  (setq native-comp-speed 2))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // gc // the right way
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package gcmh
  :demand t
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))  ; 128MB
  (setq gcmh-idle-delay 5)
  (when (fboundp 'gcmh-mode)
    (gcmh-mode 1)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // project // built-in, works
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package project
  :ensure nil
  :demand t
  :config
  ;; Find projects by these markers, not just .git
  (setq project-vc-extra-root-markers
        '(".project"           ; generic marker
          ".projectile"        ; projectile compat
          "flake.nix"          ; nix
          "BUILD"              ; bazel
          "WORKSPACE"          ; bazel
          "MODULE.bazel"       ; bzlmod
          "Cargo.toml"         ; rust
          "package.json"       ; node
          "pyproject.toml"     ; python
          "setup.py"           ; python
          "go.mod"             ; go
          "*.cabal"            ; haskell
          "dune-project"))     ; ocaml

  ;; Remember projects across sessions
  (setq project-list-file
        (expand-file-name "projects" user-emacs-directory))

  :bind-keymap ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("f" . project-find-file)
              ("g" . consult-ripgrep)
              ("b" . consult-project-buffer)
              ("d" . project-find-dir)
              ("k" . project-kill-buffers)
              ("c" . project-compile)
              ("!" . project-shell-command)
              ("&" . project-async-shell-command)
              ("s" . project-shell)
              ("e" . project-eshell)
              ("t" . hypermodern/term-project)
              ("p" . project-switch-project)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // wgrep // edit grep results in place
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; Works with consult-ripgrep results
(use-package wgrep-deadgrep
  :after deadgrep)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // compilation // make it useful
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package compile
  :ensure nil
  :config
  ;; Scroll with output
  (setq compilation-scroll-output 'first-error)

  ;; ANSI colors in compilation buffer
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  ;; Don't ask about saving
  (setq compilation-ask-about-save nil)

  ;; Kill old compilation before starting new one
  (setq compilation-always-kill t)

  ;; Project-aware compile
  (defun hypermodern/compile ()
    "Compile from project root."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (call-interactively #'compile)))

  :bind (("C-c C-c" . hypermodern/compile)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // dired // make it not suck
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package dired
  :ensure nil
  :commands dired
  :config
  ;; Show human-readable sizes
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; Reuse buffer when navigating
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; Copy/move to other dired window
  (setq dired-dwim-target t)

  ;; Allow wdired editing
  (defvar wdired-allow-to-change-permissions)
  (setq wdired-allow-to-change-permissions t)

  :bind (:map dired-mode-map
              ("C-c C-w" . wdired-change-to-wdired-mode)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // ediff // inline not frames
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // recentf // remember files
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package recentf
  :ensure nil
  :demand t
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 15)
  (setq recentf-auto-cleanup 'never)  ; don't check if files exist

  ;; Use writable directory for recentf file
  (setq recentf-save-file 
        (expand-file-name "recentf"
                          (if (file-writable-p user-emacs-directory)
                              user-emacs-directory
                            "~/.cache/emacs/")))

  ;; Exclude some paths
  (add-to-list 'recentf-exclude "^/tmp/")
  (add-to-list 'recentf-exclude "^/ssh:")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa/")

  (recentf-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // savehist // remember minibuffer history
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package savehist
  :ensure nil
  :demand t
  :config
  ;; Use writable directory for savehist file
  (setq savehist-file
        (expand-file-name "history"
                          (if (file-writable-p user-emacs-directory)
                              user-emacs-directory
                            "~/.cache/emacs/")))
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring compile-command))
  (savehist-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // auto-save // but not in working directory
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq auto-save-default t)
(setq auto-save-include-big-deletions t)

;; Put auto-saves in one place
(let* ((base-dir (if (file-writable-p user-emacs-directory)
                     user-emacs-directory
                   "~/.cache/emacs/"))
       (auto-save-dir (expand-file-name "auto-save/" base-dir)))
  (make-directory auto-save-dir t)
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

;; Backup files too  
(let* ((base-dir (if (file-writable-p user-emacs-directory)
                     user-emacs-directory
                   "~/.cache/emacs/"))
       (backup-dir (expand-file-name "backups/" base-dir)))
  (make-directory backup-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))))

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // undo-fu // simple but good
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))

(use-package undo-fu-session
  :demand t
  :config
  ;; Use writable directory for undo session files
  (setq undo-fu-session-directory
        (expand-file-name "undo-fu-session/"
                          (if (file-writable-p user-emacs-directory)
                              user-emacs-directory
                            "~/.cache/emacs/")))
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (when (fboundp 'undo-fu-session-global-mode)
    (undo-fu-session-global-mode 1)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // which-key // remember what keys do
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package which-key
  :demand t
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-add-column-padding 1)
  (which-key-mode 1))

(provide 'hypermodern-core)
;;; hypermodern-core.el ends here
