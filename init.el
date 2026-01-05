;;; init.el --- hypermodern emacs -*- lexical-binding: t -*-
;;
;; "On The Design Of Text Editors" - https://arxiv.org/abs/2008.06030
;;
;; "There is always a point at which the terrorist ceases to manipulate the media
;;  gestalt. A point at which the violence may well escalate, but beyond which the
;;  terrorist has become symptomatic of the media gestalt itself. Terrorism as we
;;  ordinarily understand it is inately media-related. The Panther Moderns differ from
;;  other terrorists precisely in their degree of self-consciousness, in their
;;  awareness of the extent to which media divorce the act of terrorism from the
;;  original sociopolitical intent."
;;
;; "Skip it." Case said.
;;
;; ARCHITECTURE:
;; - This file owns ALL use-package declarations (load order contract)
;; - lib/ modules provide functions, defvars, and setup functions
;; - Modules are loaded lazily via require or autoload
;;

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // bootstrap // gc // package
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/gc-cons-threshold (* 128 1024 1024))
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold hypermodern/gc-cons-threshold)))

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent))

(defvar hypermodern/nix-managed-p
  (or (getenv "NIX_PROFILES")
      (and load-file-name (string-match-p "/nix/store" load-file-name)))
  "Non-nil if packages managed by Nix.")

(require 'package)
(unless hypermodern/nix-managed-p
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))
  (setq package-archive-priorities '(("melpa" . 99) ("nongnu" . 80) ("gnu" . 70))))

(package-initialize)

(unless (package-installed-p 'use-package)
  (unless hypermodern/nix-managed-p
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure (not hypermodern/nix-managed-p))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // lib path
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(let ((themes-dir (expand-file-name "themes" user-emacs-directory)))
  (when (file-directory-p themes-dir)
    (add-to-list 'custom-theme-load-path themes-dir)
    (add-to-list 'load-path themes-dir)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern-core // load early
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'hypermodern-core)
(hypermodern/core-init)

;; Enable modes that hypermodern/files-setup depends on
(recentf-mode 1)
(savehist-mode 1)

(hypermodern/files-setup)
(hypermodern/leader-setup)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // tree-sitter // grammar paths
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Configure tree-sitter to find grammar libraries from Nix
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (when-let ((ts-dir (getenv "TREE_SITTER_DIR")))
    (add-to-list 'treesit-extra-load-path ts-dir)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // user interface // basics
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq inhibit-startup-screen t
      initial-scratch-message ""
      ring-bell-function 'ignore
      use-dialog-box nil)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-hl-line-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)

(setq-default indent-tabs-mode nil tab-width 2)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Clean modern window dividers - smoke the gutter decorations
(defun hypermodern/reinit-vertical-divider (&optional sync-with-mode-line)
  "Set up clean modern window dividers for both GUI and terminal Emacs.
When SYNC-WITH-MODE-LINE is non-nil, attempt to match the divider color
with the mode-line background color."
  (interactive "P")
  ;; Remove custom face settings to inherit from theme
  (custom-set-faces
   '(vertical-border nil))

  ;; Clean up fringe indicators
  (setq-default fringe-indicator-alist '())
  (fringe-mode '(0 . 0))

  ;; Remove potential interference from mode-line positioning
  (setq-default
   mode-line-format
   (remove 'mode-line-position mode-line-format))

  ;; Set up unicode divider character for terminal Emacs
  (when (boundp 'standard-display-table)
    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    (set-display-table-slot
     standard-display-table 'vertical-border
     (make-glyph-code ?│))))

;; Call after frame initialization to ensure display is ready
(add-hook 'after-init-hook #'hypermodern/reinit-vertical-divider)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // cache directories // prevent writes to Nix store
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; URL cache
(setq url-cache-directory (expand-file-name "url/" hypermodern/cache-dir))

;; Transient (used by magit, etc)
(setq transient-levels-file (expand-file-name "transient/levels.el" hypermodern/cache-dir)
      transient-values-file (expand-file-name "transient/values.el" hypermodern/cache-dir)
      transient-history-file (expand-file-name "transient/history.el" hypermodern/cache-dir))

;; request.el data directory
(with-eval-after-load 'request
  (setq request-storage-directory (expand-file-name "request/" hypermodern/cache-dir)))

;; pdf-tools state
(with-eval-after-load 'pdf-tools
  (setq pdf-info-epdfinfo-program (executable-find "epdfinfo")))

;; company statistics
(with-eval-after-load 'company-statistics
  (setq company-statistics-file (expand-file-name "company-stats-cache.el" hypermodern/cache-dir)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern-ui // load early for Gibson quotes
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'hypermodern-ui nil t)

;; System defaults (override these in user-config.el)
(setq hypermodern/ui-theme 'base16-ono-sendai-tuned
      hypermodern/ui-density 'tight
      hypermodern/ui-font-preset 'auto
      hypermodern/ui-signal 'minimal
      hypermodern/ui-glow-level 'subtle
      hypermodern/ui-enable-pulse t)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // dashboard
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package dashboard
  :demand t
  :config
  (defvar hypermodern/dashboard-banner-file
    (expand-file-name "dashboard-banner.txt" hypermodern/cache-dir))

  (defvar hypermodern/dashboard-banner-text
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                 // hypermodern
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

  (unless (file-exists-p hypermodern/dashboard-banner-file)
    (make-directory (file-name-directory hypermodern/dashboard-banner-file) t)
    (with-temp-file hypermodern/dashboard-banner-file
      (insert hypermodern/dashboard-banner-text)))

  (setq dashboard-startup-banner hypermodern/dashboard-banner-file
        dashboard-banner-logo-title nil
        dashboard-center-content t
        dashboard-set-heading-icons (display-graphic-p)
        dashboard-set-file-icons (display-graphic-p)
        dashboard-items '((recents . 5))
        dashboard-footer-messages
        (when (bound-and-true-p hypermodern/gibson-quotes)
          hypermodern/gibson-quotes)
        dashboard-footer-icon nil)

  (dashboard-setup-startup-hook)

  ;; Force dashboard on startup
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

(global-set-key (kbd "C-c h") #'dashboard-open)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // core packages // use-package declarations
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package gcmh
  :demand t
  :config
  (setq gcmh-high-cons-threshold hypermodern/gc-cons-threshold)
  (gcmh-mode 1))

(use-package project
  :ensure nil
  :demand t
  :config
  (hypermodern/project-setup)
  :bind-keymap ("C-c p" . project-prefix-map))

(use-package compile
  :ensure nil
  :config
  (hypermodern/compile-setup))

(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))

(use-package undo-fu-session
  :demand t
  :config (undo-fu-session-global-mode 1))

(use-package which-key
  :demand t
  :config (which-key-mode 1))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode)

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // completion at point
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  (global-company-mode 1)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("RET" . nil)
              ([return] . nil)
              ("TAB" . company-complete-selection)
              ([tab] . company-complete-selection)))

(use-package yasnippet
  :demand t
  :config
  ;; Use writable directory for snippets (not Nix store)
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" hypermodern/cache-dir)))
  (yas-global-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // minibuffer // completion
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package vertico
  :demand t
  :config
  (setq vertico-cycle t vertico-count 15)
  (vertico-mode 1)
  (vertico-reverse-mode 1))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))

(use-package marginalia
  :demand t
  :config (marginalia-mode 1))

(use-package consult
  :demand t
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find)))

(use-package embark
  :bind (("C-." . embark-act)))  ; embark-dwim available via M-x

(use-package embark-consult :after (embark consult))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // modeline // icons
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package doom-modeline
  :demand t
  :config
  (setq doom-modeline-height 25
        doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-mode 1))

(use-package nerd-icons)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // languages // eglot
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'hypermodern-languages)

(use-package eglot
  :ensure nil
  :defer t
  :config
  (hypermodern/eglot-setup)
  :hook ((python-mode python-ts-mode
                      rust-mode rust-ts-mode
                      c-mode c-ts-mode c++-mode c++-ts-mode
                      typescript-mode typescript-ts-mode
                      nix-mode haskell-mode go-mode) . eglot-ensure))

(use-package treesit-auto
  :if (fboundp 'treesit-available-p)
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(global-set-key (kbd "M-z") #'hypermodern/format-buffer)

;; Language modes
(use-package nix-mode :mode "\\.nix\\'")
(use-package rust-mode :mode "\\.rs\\'")
(use-package haskell-mode :mode "\\.hs\\'")
(use-package yaml-mode :mode "\\.ya?ml\\'")
(use-package markdown-mode :mode "\\.md\\'")

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // terminal
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'hypermodern-terminal)

(use-package vterm
  :commands (vterm vterm-other-window)
  :config (hypermodern/vterm-setup))

(use-package eat
  :commands (eat eat-project)
  :config (hypermodern/eat-setup))

(use-package detached
  :commands (detached-shell-command detached-compile detached-list-sessions)
  :config (hypermodern/detached-setup)
  :bind (("C-c d c" . detached-shell-command)
         ("C-c d o" . detached-open-session)
         ("C-c d l" . detached-list-sessions)))

(hypermodern/server-setup)

(global-set-key (kbd "C-c T") #'hypermodern/term)
(global-set-key (kbd "C-`") #'hypermodern/term-toggle)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // windows // popups
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'hypermodern-windows)
(hypermodern/windows-init)

(use-package shackle
  :demand t
  :config (hypermodern/shackle-setup))

(use-package popper
  :demand t
  :after shackle
  :bind (("C-\\" . popper-toggle)
         ("M-\\" . popper-cycle))
  :config (hypermodern/popper-setup))

(global-set-key (kbd "C-x 2") #'hypermodern/split-below)
(global-set-key (kbd "C-x 3") #'hypermodern/split-right)
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "M-N") #'windmove-right)
(global-set-key (kbd "M-P") #'windmove-left)
(global-set-key (kbd "M-R") #'hypermodern/rotate-windows)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // navigation
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'hypermodern-navigation)

(use-package zoxide
  :commands (zoxide-add zoxide-query)
  :config (hypermodern/zoxide-setup))

(use-package consult-dir
  :after consult
  :config (hypermodern/consult-dir-setup))

(global-set-key (kbd "C-c z z") #'hypermodern/zoxide-jump)
(global-set-key (kbd "C-c n") #'hypermodern/navigate)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // compile
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'hypermodern-compile)

(global-set-key (kbd "C-c c") hypermodern/compile-map)
(global-set-key (kbd "<f5>") #'hypermodern/compile)
(global-set-key (kbd "<f6>") #'hypermodern/recompile)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // remote // tramp
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'hypermodern-remote)

(global-set-key (kbd "C-c t") hypermodern/tailscale-map)
(global-set-key (kbd "C-c r") hypermodern/remote-map)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // git
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package magit
  :commands magit
  :bind ("C-x g" . magit-status))

;; (use-package forge :after magit)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // editing
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         ("C-;" . avy-goto-word-1)))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-background t
        aw-scope 'frame))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-idle-delay 0.1
        company-backends '(company-capf company-dabbrev-code company-dabbrev)))

(use-package deadgrep
  :bind ("C-c s" . deadgrep))

(use-package wgrep)  ; edit grep buffers
(use-package wgrep-deadgrep :after (wgrep deadgrep))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // ai
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(when (require 'hypermodern-ai nil t)
  (hypermodern/ai-bind-defaults))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // ui // theme
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Load custom theme autoloads (makes all base16 themes discoverable)
(require 'hypermodern-themes-loader nil t)

;; Base16 terminal mode: use 256-color approximations instead of ANSI palette
(setq base16-theme-256-color-source 'colors)

(when (featurep 'hypermodern-ui)
  (hypermodern/ui-init)
  (global-set-key (kbd "C-c o u") #'hypermodern/ui-menu))

(unless (featurep 'hypermodern-ui)
  (load-theme 'base16-ono-sendai-tuned t))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // misc
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package rainbow-mode :hook (prog-mode . rainbow-mode))

(use-package direnv :config (direnv-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // keybinding reference
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; Sacred (unchanged):
;;   C-x C-f/s/c  files | C-g cancel | C-/ undo | C-w/M-w/C-y kill/yank
;;   C-a/e C-n/p C-f/b  movement | C-x 0/1/2/3/o windows | M-x commands
;;
;; Upgraded in place:
;;   C-x b   → consult-buffer (was switch-to-buffer)
;;   C-s     → consult-line (was isearch - use C-r for isearch-backward)
;;   M-g g   → consult-goto-line
;;   M-.     → xref-find-definitions (eglot provides backend)
;;   M-,     → xref-go-back
;;
;; Reclaimed from useless defaults:
;;   M-o     → ace-window (was facemenu)
;;   C-'     → avy-goto-char-timer
;;   C-;     → avy-goto-word-1
;;   C-.     → embark-act
;;   C-=     → expand-region
;;
;; Search (M-s prefix):
;;   M-s r   → consult-ripgrep (project)
;;   M-s f   → consult-find (files)
;;   C-c s   → deadgrep (persistent buffer)
;;
;; User prefix (C-c):
;;   C-c c   → compile map
;;   C-c g   → magit-status (or C-x g)
;;   C-c s   → deadgrep
;;   C-c o   → hypermodern ops
;;
;; Help (upgraded):
;;   C-h f/v/k → helpful-callable/variable/key

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // user configuration // your personal config goes here
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Load user-config.el if it exists - this is YOUR file for personal settings
(let ((user-config-file (expand-file-name "user-config.el" user-emacs-directory)))
  (when (file-exists-p user-config-file)
    (load user-config-file nil t)))

(provide 'init)
;;; init.el ends here
