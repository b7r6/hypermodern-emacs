;;; init.el --- -*- lexical-binding: t -*-
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

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // package // management
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Detect if we're running under Nix (packages pre-installed) or standalone
(defvar hypermodern/nix-managed-p
  (or (getenv "NIX_PROFILES")
      (and load-file-name
           (string-match-p "/nix/store" load-file-name)))
  "Non-nil if Emacs packages are managed by Nix.")

(require 'package)

(unless hypermodern/nix-managed-p
  ;; Standalone mode: set up archives and bootstrap use-package
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")))

  (setq package-archive-priorities
        '(("melpa" . 99)
          ("nongnu" . 80)
          ("gnu" . 70)
          ("melpa-stable" . 60)))

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; In Nix mode, packages are already on load-path
(when hypermodern/nix-managed-p
  (package-initialize))

(require 'use-package)

;; Only auto-download packages when not under Nix
(setq use-package-always-ensure (not hypermodern/nix-managed-p))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // lib // path
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

;; Themes directory (for nix builds)
(let ((themes-dir (expand-file-name "themes" user-emacs-directory)))
  (when (file-directory-p themes-dir)
    (add-to-list 'custom-theme-load-path themes-dir)
    (add-to-list 'load-path themes-dir)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern // core
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; gcmh, project.el, wgrep, pixel-scroll, repeat-mode, undo-fu
(eval-when-compile (require 'hypermodern-core nil t))
(require 'hypermodern-core)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // reinit // user // interface
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq copy-region-blink-delay 0)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-hl-line-mode 1)
(column-number-mode 1)
(blink-cursor-mode -1)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq require-final-newline t)
(setq sentence-end-double-space nil)

;; yes/no → y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; don't pop up UI dialogs
(setq use-dialog-box nil)

;; smooth scrolling (beyond pixel-scroll-precision-mode)
(setq scroll-margin 2)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // frame // discipline
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/frame-parameters
  '((vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (internal-border-width . 0)
    (left-fringe . 8)
    (right-fringe . 8))
  "Frame parameters applied to all frames.")

(setq default-frame-alist
      (append hypermodern/frame-parameters default-frame-alist))
(setq initial-frame-alist default-frame-alist)

(defun hypermodern/vsplit ()
  "Split window vertically and switch."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun hypermodern/hsplit ()
  "Split window horizontally and switch."
  (interactive)
  (split-window-right)
  (windmove-right))

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

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // minibuffer // vertico // consult // embark
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package vertico
  :demand t
  :config
  (setq vertico-cycle t
        vertico-count 15
        vertico-resize nil)
  (vertico-mode 1)
  (when (fboundp 'vertico-reverse-mode)
    (vertico-reverse-mode 1)))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t
  :config (marginalia-mode 1))

(use-package consult
  :demand t
  :bind (("C-x b" . consult-buffer)
         ("C-c b" . consult-buffer)
         ("C-s" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // mode // line
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package doom-modeline
  :demand t
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 20)
  (doom-modeline-mode 1))

(use-package nerd-icons)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // dashboard
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/gibson-quotes
  '("he mythform is usually encountered in one of two modes. one mode
assumes that the cyberspace matric is inhabited, or perhaps visited, by
entities whose characteristics correspond with the primary mythoform
of a hidden people."

    "it was the style that mattered and the style was the same.
the moderns were mercenaries, practical jokers, nihilistic technofetishists."

    "all the speed he took, all the turns he'd taken and the corners he'd cut
in night city, and still he'd see the matrix in his sleep, bright lattices
of logic unfolding across that colorless void..."

    "power, in case's world, meant corporate power. the zaibatsus,
the multinationals that shaped the course of human history,
had transcended old barriers."

    "he'd always imagined it as a gradual and willing accommodation of
the machine, the parent organism. it was the root of street cool too, the
knowing posture that implied connection, invisible lines up to hidden
levels of influence."))

(use-package dashboard
  :config
  (defvar hypermodern/dashboard-banner-file
    (expand-file-name "dashboard-banner-0x04.txt"
                      (if (file-writable-p user-emacs-directory)
                          user-emacs-directory
                        "~/.cache/emacs/")))

  (defvar hypermodern/dashboard-banner-text
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                 // hypermodern
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

  (unless (file-exists-p hypermodern/dashboard-banner-file)
    (with-temp-file hypermodern/dashboard-banner-file
      (insert hypermodern/dashboard-banner-text)))

  (setq dashboard-startup-banner hypermodern/dashboard-banner-file
        dashboard-banner-logo-title
        (nth (random (length hypermodern/gibson-quotes)) hypermodern/gibson-quotes)
        dashboard-center-content t
        dashboard-set-heading-icons (display-graphic-p)
        dashboard-set-file-icons (display-graphic-p)
        dashboard-items '((recents . 5)))

  (dashboard-setup-startup-hook))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // general // key // bind
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package general
  :demand t
  :config
  (defun hypermodern/visit-init-file ()
    (interactive)
    (find-file user-init-file))

  (defun hypermodern/show-current-file ()
    (interactive)
    (message (or (buffer-file-name) "<no file>")))

  (defun hypermodern/kill-current-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))

  (general-define-key
   ;; standard movement
   "C-c q"   #'join-line
   "C-c R"   #'revert-buffer  ; Changed from C-c r to C-c R (uppercase) to free C-c r as prefix for remote operations
   "C-c h"   #'dashboard-open
   "C-j"     #'newline-and-indent

   ;; your standard keys
   "M-/"     #'undo
   "M-N"     #'windmove-right
   "M-P"     #'windmove-left
   "M-i"     #'hypermodern/visit-init-file
   "M-R"     #'hypermodern/rotate-windows

   ;; buffers
   "C-x 2"   #'hypermodern/vsplit
   "C-x 3"   #'hypermodern/hsplit
   "C-x k"   #'hypermodern/kill-current-buffer
   "C-c f"   #'hypermodern/show-current-file

   ;; git
   "C-x g"   #'magit))

;; ops prefix
(general-create-definer hypermodern/leader
  :prefix "C-c o")

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // company
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-idle-delay 0.05
        company-require-match nil
        company-show-quick-access t
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-backends '(company-capf
                           company-files
                           company-keywords
                           company-yasnippet
                           company-dabbrev-code
                           company-dabbrev))
  :bind (("M-TAB" . company-complete-common-or-cycle)
         ("C-M-i" . company-complete-common-or-cycle)))

(use-package yasnippet
  :config
  ;; Use writable directory for yasnippet files
  (let ((snippets-dir (expand-file-name "snippets/"
                                        (if (file-writable-p user-emacs-directory)
                                            user-emacs-directory
                                          "~/.cache/emacs/"))))
    (make-directory snippets-dir t)
    (setq yas-snippet-dirs (list snippets-dir)))
  (yas-global-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // tree-sitter
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern // languages
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; eglot, xref, language modes, bazel, objdump, formatting
(eval-when-compile (require 'hypermodern-languages nil t))
(require 'hypermodern-languages)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern // terminal
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; vterm, eat, detached, server
(eval-when-compile (require 'hypermodern-terminal nil t))
(require 'hypermodern-terminal)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern // remote
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; tramp, tailscale
(eval-when-compile (require 'hypermodern-remote nil t))
(require 'hypermodern-remote)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern // secrets
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; multi-context agenix, gpg, auth-source
(eval-when-compile (require 'hypermodern-secrets nil t))
(require 'hypermodern-secrets)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern // ai
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(when (require 'hypermodern-ai nil 'noerror)
  (hypermodern/ai-bind-defaults))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hypermodern // ui
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(when (require 'hypermodern-ui nil 'noerror)
  (setq hypermodern/ui-theme 'base16-ono-sendai-blue-tuned
        hypermodern/ui-density 'tight
        hypermodern/ui-signal 'minimal
        hypermodern/ui-font-preset 'auto
        hypermodern/ui-glow-level 'subtle
        hypermodern/ui-glow-halo 'auto
        hypermodern/ui-enable-pulse t
        hypermodern/ui-cursor-style nil)

  (ignore-errors (require 'base16-ono-sendai-blue-tuned-theme))
  ;; Temporarily disable theme init to debug
  ;; (hypermodern/ui-init)
  (global-set-key (kbd "C-c o u") #'hypermodern/ui-menu))

(unless (featurep 'hypermodern-ui)
  (require 'base16-ono-sendai-blue-tuned-theme nil t)
  (if (load-theme 'base16-ono-sendai-blue-tuned t)
      (message "Theme loaded: base16-ono-sendai-tuned")
    (message "Warning: Could not load base16-ono-sendai-tuned theme"))

  ;; Terminal color fix - set colors directly
  (unless (display-graphic-p)
    (message "Applying terminal color overrides...")
    (let ((base00 "#191c20") (base01 "#1f232a") (base02 "#2a3039")
          (base03 "#3a424f") (base04 "#6b7689") (base05 "#c5d0dd")
          (base08 "#b6e3ff") (base09 "#80ccff") (base0A "#54aeff")
          (base0B "#218bff") (base0D "#4d9fff"))
      (set-face-background 'default base00)
      (set-face-foreground 'default base05)
      (set-face-background 'region base02)
      (set-face-background 'hl-line base01)
      (set-face-foreground 'font-lock-comment-face base03)
      (set-face-foreground 'font-lock-keyword-face base0A)
      (set-face-foreground 'font-lock-string-face base0B)
      (set-face-foreground 'font-lock-function-name-face base0D)
      (set-face-foreground 'font-lock-variable-name-face base08)
      (set-face-foreground 'font-lock-constant-face base09)
      (message "Terminal colors applied"))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // misc // modes
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package magit
  :commands magit)

(use-package forge
  :after magit)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package direnv
  :config (direnv-mode 1))

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         ("C-;" . avy-goto-word-1)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package popper
  :bind ("C-\\" . popper-toggle-latest))

(use-package shackle
  :config
  (setq shackle-rules
        '(("*Help*" :align right :size 0.4 :select t)
          ("*Completions*" :align below :size 0.3)
          ("*compilation*" :align below :size 0.3)
          ("\\*vterm.*\\*" :regexp t :align below :size 0.35 :select t)
          ("*Messages*" :align below :size 0.3)))
  (shackle-mode 1))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // web // eww
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package eww
  :ensure nil
  :commands (eww eww-browse-url))

(defun hypermodern/webkit (url)
  "Browse URL using xwidget-webkit if available."
  (interactive "sURL: ")
  (if (fboundp 'xwidget-webkit-browse-url)
      (xwidget-webkit-browse-url url)
    (user-error "No xwidget-webkit in this Emacs build")))

(use-package atomic-chrome
  :if (locate-library "atomic-chrome")
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (atomic-chrome-start-server))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // comms
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package erc
  :ensure nil
  :commands (erc erc-tls)
  :config
  (setq erc-server "irc.libera.chat"
        erc-nick (or (getenv "ERC_NICK") "b7r6")
        erc-user-full-name user-full-name
        erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#nixos"))
        erc-prompt-for-nickserv-password nil
        erc-use-auth-source-for-nickserv-password t))

(defun hypermodern/irc ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697 :nick erc-nick))

(use-package ement
  :if (locate-library "ement")
  :commands (ement-connect))

(use-package telega
  :if (locate-library "telega")
  :commands (telega))

(use-package mastodon
  :if (locate-library "mastodon")
  :commands (mastodon))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // reading // feeds // docs
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package elfeed
  :if (locate-library "elfeed")
  :commands (elfeed)
  :config
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://hnrss.org/frontpage"
          "https://nixos.org/blog/announcements-rss.xml")))

(use-package pdf-tools
  :if (locate-library "pdf-tools")
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (condition-case err
      (pdf-tools-install)
    (error (message "[pdf-tools] install failed: %s" err))))

(use-package nov
  :if (locate-library "nov")
  :mode ("\\.epub\\'" . nov-mode))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // ops leader
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/call-or-warn (fn label)
  "Call FN interactively if it exists, else complain with LABEL."
  (if (fboundp fn)
      (call-interactively fn)
    (user-error "[hypermodern] %s not available (missing package?)" label)))

(hypermodern/leader
  "u"  #'hypermodern/ui-menu
  "w"  #'eww
  "W"  #'hypermodern/webkit
  "i"  #'hypermodern/irc
  "m"  (lambda () (interactive) (hypermodern/call-or-warn 'ement-connect "ement"))
  "t"  (lambda () (interactive) (hypermodern/call-or-warn 'telega "telega"))
  "M"  (lambda () (interactive) (hypermodern/call-or-warn 'mastodon "mastodon"))
  "f"  (lambda () (interactive) (hypermodern/call-or-warn 'elfeed "elfeed")))

(provide 'init)
;;; init.el ends here
