;;; hypermodern-languages.el --- language server config -*- lexical-binding: t; -*-
;;
;; The protocol wars are over. VSCode won. We implement what servers send.
;; No use-package here - init.el owns load order.
;;

(require 'cl-lib)

;; Silence byte-compiler warnings for external variables
(defvar eglot-ignored-server-capabilities)
(defvar eglot-events-buffer-config)
(defvar eglot-sync-connect)
(defvar eglot-autoshutdown)
(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)
(defvar eglot--managed-mode)
(defvar major-mode-remap-alist)

;; Silence byte-compiler warnings for external functions
(declare-function eglot-format-buffer "eglot" ())
(declare-function eglot-current-server "eglot" ())
(declare-function eglot-ensure "eglot" ())

(defgroup hypermodern-languages nil
  "Language server and mode configuration."
  :group 'programming)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // eglot // server programs
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/eglot-server-programs
  '(;; Python: basedpyright is pyright but maintained and faster
    ((python-mode python-ts-mode)
     . ("basedpyright-langserver" "--stdio"))

    ;; TypeScript/JavaScript
    ((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode)
     . ("typescript-language-server" "--stdio"))

    ;; Rust: rust-analyzer is excellent
    ((rust-mode rust-ts-mode)
     . ("rust-analyzer"))

    ;; C/C++/CUDA: clangd handles all of them
    ((c-mode c-ts-mode c++-mode c++-ts-mode cuda-mode)
     . ("clangd"
        "--background-index"
        "--clang-tidy"
        "--completion-style=detailed"
        "--header-insertion=iwyu"
        "--query-driver=/usr/local/cuda/bin/nvcc,/opt/cuda/bin/nvcc,**/nvcc"))

    ;; Haskell
    ((haskell-mode haskell-ts-mode)
     . ("haskell-language-server-wrapper" "--lsp"))

    ;; Nix: nixd is the good one now
    ((nix-mode nix-ts-mode)
     . ("nixd"))

    ;; Zig
    ((zig-mode zig-ts-mode)
     . ("zls"))

    ;; Shell
    ((sh-mode bash-ts-mode)
     . ("bash-language-server" "start"))

    ;; YAML
    ((yaml-mode yaml-ts-mode)
     . ("yaml-language-server" "--stdio"))

    ;; JSON
    ((json-mode json-ts-mode)
     . ("vscode-json-language-server" "--stdio"))

    ;; Markdown
    (markdown-mode . ("marksman")))
  "Eglot server program associations.")

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // eglot // workspace config
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/eglot-workspace-configuration
  '(:basedpyright (:analysis (:autoSearchPaths t
                              :diagnosticMode "openFilesOnly"
                              :useLibraryCodeForTypes t))
    :rust-analyzer (:cargo (:buildScripts (:enable t))
                    :procMacro (:enable t)
                    :checkOnSave (:command "clippy")))
  "Workspace configuration for language servers.")

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // eglot // setup function (called from init.el use-package :config)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/eglot-setup ()
  "Configure eglot with hypermodern settings.
Call this from (use-package eglot :config ...)."
  ;; Shut up the annoying shit
  (setq eglot-ignored-server-capabilities
        '(:inlayHintProvider
          :documentHighlightProvider
          :colorProvider
          :foldingRangeProvider))

  ;; Don't log everything
  (setq eglot-events-buffer-config '(:size 0))

  ;; Faster operation
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t)

  ;; Our server programs
  (setq eglot-server-programs hypermodern/eglot-server-programs)

  ;; Workspace config
  (setq eglot-workspace-configuration hypermodern/eglot-workspace-configuration))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // treesit // grammars
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/treesit-grammars
  '(bash c cpp css dockerfile go haskell html java javascript
    json lua make markdown nix python ruby rust toml tsx
    typescript yaml zig)
  "Tree-sitter grammars to install.")

(defun hypermodern/treesit-ensure-installed ()
  "Ensure tree-sitter grammars are installed."
  (interactive)
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (dolist (lang hypermodern/treesit-grammars)
      (unless (treesit-language-available-p lang)
        (message "Installing tree-sitter grammar: %s" lang)
        (condition-case err
            (treesit-install-language-grammar lang)
          (error (message "Failed to install %s: %s" lang err)))))))

(defun hypermodern/treesit-setup ()
  "Configure tree-sitter mode remapping."
  (when (fboundp 'treesit-available-p)
    (setq major-mode-remap-alist
          '((python-mode . python-ts-mode)
            (javascript-mode . js-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (json-mode . json-ts-mode)
            (css-mode . css-ts-mode)
            (yaml-mode . yaml-ts-mode)
            (c-mode . c-ts-mode)
            (c++-mode . c++-ts-mode)
            (rust-mode . rust-ts-mode)))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // formatting
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/format-buffer ()
  "Format buffer with eglot if connected, else indent."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode)
           (eglot-current-server))
      (eglot-format-buffer)
    (indent-region (point-min) (point-max))))

(defun hypermodern/format-nix ()
  "Format Nix buffer using nixfmt or alejandra."
  (interactive)
  (let ((formatter (or (executable-find "nixfmt")
                       (executable-find "alejandra"))))
    (if formatter
        (let ((start (point-min))
              (end (point-max)))
          (shell-command-on-region start end formatter (current-buffer) t))
      (message "No Nix formatter found (nixfmt or alejandra)"))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // mode hooks (for auto-starting eglot)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/eglot-auto-modes
  '(python-mode python-ts-mode
    rust-mode rust-ts-mode
    c-mode c-ts-mode c++-mode c++-ts-mode
    typescript-mode typescript-ts-mode tsx-ts-mode
    js-mode js-ts-mode
    haskell-mode nix-mode nix-ts-mode
    go-mode go-ts-mode zig-mode)
  "Modes that auto-start eglot.")

(defun hypermodern/eglot-auto-start ()
  "Add eglot-ensure to appropriate mode hooks."
  (dolist (mode hypermodern/eglot-auto-modes)
    (let ((hook (intern (format "%s-hook" mode))))
      (add-hook hook #'eglot-ensure))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // bazel // helper
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/bazel-compile-commands ()
  "Common Bazel compilation commands."
  '(("build" . "bazel build //...")
    ("test" . "bazel test //...")
    ("run" . "bazel run")))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // assembly modes // objdump, cuobjdump, ptx
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(define-derived-mode objdump-mode asm-mode "Objdump"
  "Mode for viewing objdump output.")

(define-derived-mode cuobjdump-mode asm-mode "CUObjdump"
  "Mode for viewing CUDA cuobjdump output.")

(define-derived-mode ptx-mode asm-mode "PTX"
  "Mode for viewing NVIDIA PTX assembly.")

(provide 'hypermodern-languages)
;;; hypermodern-languages.el ends here
