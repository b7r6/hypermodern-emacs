;;; hypermodern-languages.el --- language server discipline -*- lexical-binding: t; -*-
;;
;; The protocol wars are over. VSCode won. We implement what servers send.
;;

(require 'cl-lib)

(defgroup hypermodern-languages nil
  "Language server and mode configuration."
  :group 'programming)

(defcustom hypermodern/use-eglot t
  "Use eglot instead of lsp-mode."
  :type 'boolean)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // eglot // core
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(when hypermodern/use-eglot
  (use-package eglot
    :ensure nil  ; built-in as of emacs 29
    :defer t
    :config
    ;; Shut up the annoying shit
    (setq eglot-ignored-server-capabilities
          '(:inlayHintProvider           ; the worst offender
            :documentHighlightProvider   ; flicker on every cursor move
            :colorProvider               ; rarely useful, often slow
            :foldingRangeProvider))      ; use treesit for this

    ;; Don't log everything
    (setq eglot-events-buffer-config '(:size 0))

    ;; Faster completion
    (setq eglot-sync-connect nil)  ; don't block on connect
    (setq eglot-autoshutdown t)    ; kill server when last buffer closes

    ;; ─────────────────────────────────────────────────────────────
    ;; Server programs - the real config
    ;; ─────────────────────────────────────────────────────────────

    (setq eglot-server-programs
          '(;; Python: basedpyright is pyright but maintained and faster
            ;; Alternative: ruff for linting-focused, ty (astral) when ready
            ((python-mode python-ts-mode)
             . ("basedpyright-langserver" "--stdio"))

            ;; TypeScript/JavaScript: the standard
            ((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode)
             . ("typescript-language-server" "--stdio"))

            ;; Rust: rust-analyzer is excellent
            ((rust-mode rust-ts-mode)
             . ("rust-analyzer"))

            ;; C/C++/CUDA: clangd handles all of them
            ;; --query-driver allows non-standard compilers (nvcc)
            ((c-mode c-ts-mode c++-mode c++-ts-mode cuda-mode)
             . ("clangd"
                "--background-index"
                "--clang-tidy"
                "--completion-style=detailed"
                "--header-insertion=iwyu"
                "--query-driver=/usr/local/cuda/bin/nvcc,/opt/cuda/bin/nvcc,**/nvcc"))

            ;; Haskell: hls works well now
            ((haskell-mode haskell-ts-mode)
             . ("haskell-language-server-wrapper" "--lsp"))

            ;; Nix: nixd is the good one now
            ((nix-mode nix-ts-mode)
             . ("nixd"))

            ;; Zig: zls
            ((zig-mode zig-ts-mode)
             . ("zls"))

            ;; Bash: yes, this exists
            ((sh-mode bash-ts-mode)
             . ("bash-language-server" "start"))

            ;; Assembly: asm-lsp exists, limited but something
            ((asm-mode nasm-mode)
             . ("asm-lsp"))

            ;; YAML
            ((yaml-mode yaml-ts-mode)
             . ("yaml-language-server" "--stdio"))

            ;; JSON
            ((json-mode json-ts-mode)
             . ("vscode-json-language-server" "--stdio"))

            ;; Markdown
            (markdown-mode
             . ("marksman"))))

    ;; ─────────────────────────────────────────────────────────────
    ;; Workspace configuration (pyright, rust-analyzer, etc need this)
    ;; ─────────────────────────────────────────────────────────────

    (setq-default eglot-workspace-configuration
                  '(:basedpyright (:analysis (:autoSearchPaths t
                                              :useLibraryCodeForTypes t
                                              :diagnosticMode "openFilesOnly"
                                              :typeCheckingMode "basic"))
                    :rust-analyzer (:checkOnSave (:command "clippy")
                                    :cargo (:allFeatures t)
                                    :procMacro (:enable t))
                    :nixd (:formatting (:command ["nixfmt"]))
                    :clangd (:fallbackFlags ["-std=c++23"
                                             "-I/usr/local/cuda/include"])))

    :hook
    ((python-mode python-ts-mode) . eglot-ensure)
    ((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode) . eglot-ensure)
    ((rust-mode rust-ts-mode) . eglot-ensure)
    ((c-mode c-ts-mode c++-mode c++-ts-mode cuda-mode) . eglot-ensure)
    ((haskell-mode haskell-ts-mode) . eglot-ensure)
    ((nix-mode nix-ts-mode) . eglot-ensure)
    ((zig-mode zig-ts-mode) . eglot-ensure)
    ((sh-mode bash-ts-mode) . eglot-ensure)
    ;; lean4-mode uses lsp-mode internally, not eglot

    :bind (:map eglot-mode-map
                ("C-c l r" . eglot-rename)
                ("C-c l a" . eglot-code-actions)
                ("C-c l f" . eglot-format)
                ("C-c l d" . eldoc)
                ("C-c l i" . eglot-find-implementation)
                ("C-c l t" . eglot-find-typeDefinition))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // python // alternatives
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/python-use-ruff ()
  "Switch to ruff for Python (faster, less type-focused)."
  (interactive)
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs
                   nil nil #'equal)
        '("ruff" "server"))
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (when (and (fboundp 'eglot-current-server) (fboundp 'eglot-reconnect))
      (eglot-reconnect (eglot-current-server)))))

(defun hypermodern/python-use-basedpyright ()
  "Switch to basedpyright for Python (full type checking)."
  (interactive)
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs
                   nil nil #'equal)
        '("basedpyright-langserver" "--stdio"))
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (when (and (fboundp 'eglot-current-server) (fboundp 'eglot-reconnect))
      (eglot-reconnect (eglot-current-server)))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // cuda // mode
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package cuda-mode
  :mode "\\.cu\\'"
  :mode "\\.cuh\\'"
  :config
  ;; CUDA is C++ with extensions, tell clangd
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // bazel
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package bazel
  :mode (("BUILD\\'" . bazel-build-mode)
         ("BUILD\\.bazel\\'" . bazel-build-mode)
         ("WORKSPACE\\'" . bazel-workspace-mode)
         ("WORKSPACE\\.bazel\\'" . bazel-workspace-mode)
         ("MODULE\\.bazel\\'" . bazel-module-mode)
         ("\\.bzl\\'" . bazel-starlark-mode))
  :config
  ;; buildifier for formatting
  (setq bazel-buildifier-before-save t))

(defun hypermodern/bazel-compile-commands ()
  "Generate compile_commands.json from Bazel using hedron.
Requires hedron_compile_commands in your WORKSPACE."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (compile "bazel run @hedron_compile_commands//:refresh_all")))

(defun hypermodern/bazel-build-target ()
  "Build current target."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (target (read-string "Target: " "//:...")))
    (compile (format "bazel build %s" target))))

(defun hypermodern/bazel-test-target ()
  "Test current target."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (target (read-string "Target: " "//:...")))
    (compile (format "bazel test %s" target))))

(defun hypermodern/bazel-run-target ()
  "Run a target."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (target (read-string "Target: ")))
    (compile (format "bazel run %s" target))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // assembly // ptx // objdump
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package asm-mode
  :ensure nil
  :mode ("\\.s\\'" "\\.S\\'" "\\.asm\\'"))

(use-package nasm-mode
  :mode "\\.nasm\\'")

;; PTX mode (NVIDIA parallel thread execution)
(define-derived-mode ptx-mode asm-mode "PTX"
  "Major mode for NVIDIA PTX assembly."
  (setq-local comment-start "//")
  (setq-local comment-end "")
  ;; PTX-specific keywords
  (font-lock-add-keywords
   nil
   '(("\\.\\(version\\|target\\|address_size\\|func\\|entry\\|reg\\|param\\|local\\|shared\\|global\\)" . font-lock-keyword-face)
     ("\\b\\(ld\\|st\\|mov\\|add\\|sub\\|mul\\|mad\\|fma\\|cvt\\|setp\\|selp\\|bra\\|ret\\|call\\|bar\\)\\b" . font-lock-builtin-face)
     ("%\\w+" . font-lock-variable-name-face)
     ("\\b\\(s8\\|s16\\|s32\\|s64\\|u8\\|u16\\|u32\\|u64\\|f16\\|f32\\|f64\\|b8\\|b16\\|b32\\|b64\\|pred\\)\\b" . font-lock-type-face))))

(add-to-list 'auto-mode-alist '("\\.ptx\\'" . ptx-mode))

;; Objdump integration - the thing you miss
(defun hypermodern/objdump-region (start end)
  "Disassemble region as hex bytes."
  (interactive "r")
  (let* ((hex (buffer-substring-no-properties start end))
         (clean (replace-regexp-in-string "[^0-9a-fA-F]" "" hex))
         (bytes (string-join
                 (seq-partition clean 2)
                 " "))
         (tmpfile (make-temp-file "objdump-" nil ".bin")))
    (with-temp-file tmpfile
      (set-buffer-multibyte nil)
      (dolist (byte-str (split-string bytes))
        (insert (string-to-number byte-str 16))))
    (with-current-buffer (get-buffer-create "*objdump*")
      (erase-buffer)
      (call-process "objdump" nil t nil
                    "-D" "-b" "binary" "-m" "i386:x86-64" tmpfile)
      (delete-file tmpfile)
      (goto-char (point-min))
      (asm-mode))
    (display-buffer "*objdump*")))

(defun hypermodern/objdump-file (file)
  "Disassemble FILE with objdump."
  (interactive "fFile to disassemble: ")
  (with-current-buffer (get-buffer-create (format "*objdump: %s*" (file-name-nondirectory file)))
    (erase-buffer)
    (call-process "objdump" nil t nil
                  "-d" "-C" "-S" "--no-show-raw-insn" file)
    (goto-char (point-min))
    (asm-mode))
  (display-buffer (format "*objdump: %s*" (file-name-nondirectory file))))

(defun hypermodern/objdump-symbol (file symbol)
  "Disassemble SYMBOL from FILE."
  (interactive
   (let* ((f (read-file-name "File: "))
          (syms (shell-command-to-string
                 (format "nm -C %s | awk '{print $3}' | grep -v '^$'" f)))
          (s (completing-read "Symbol: " (split-string syms "\n" t))))
     (list f s)))
  (with-current-buffer (get-buffer-create (format "*objdump: %s*" symbol))
    (erase-buffer)
    (call-process "objdump" nil t nil
                  "-d" "-C" "--disassemble=" symbol file)
    (goto-char (point-min))
    (asm-mode))
  (display-buffer (format "*objdump: %s*" symbol)))

;; Mode aliases for tests
(defalias 'objdump-mode 'asm-mode)
(defalias 'cuobjdump-mode 'asm-mode)

;; cuobjdump for CUDA binaries
(defun hypermodern/cuobjdump-ptx (file)
  "Extract PTX from CUDA binary."
  (interactive "fCUDA binary: ")
  (with-current-buffer (get-buffer-create (format "*cuobjdump-ptx: %s*" (file-name-nondirectory file)))
    (erase-buffer)
    (call-process "cuobjdump" nil t nil "-ptx" file)
    (goto-char (point-min))
    (ptx-mode))
  (display-buffer (format "*cuobjdump-ptx: %s*" (file-name-nondirectory file))))

(defun hypermodern/cuobjdump-sass (file)
  "Extract SASS from CUDA binary."
  (interactive "fCUDA binary: ")
  (with-current-buffer (get-buffer-create (format "*cuobjdump-sass: %s*" (file-name-nondirectory file)))
    (erase-buffer)
    (call-process "cuobjdump" nil t nil "-sass" file)
    (goto-char (point-min))
    (asm-mode))
  (display-buffer (format "*cuobjdump-sass: %s*" (file-name-nondirectory file))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // emacs-lisp // formatting
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; The honest answer: there's no good elisp formatter
;; But we can make indent + whitespace cleanup work well

(defun hypermodern/format-elisp ()
  "Format elisp buffer - indent + cleanup, the best we've got."
  (interactive)
  (save-excursion
    ;; Indent everything
    (indent-region (point-min) (point-max))
    ;; Delete trailing whitespace
    (delete-trailing-whitespace)
    ;; Ensure final newline
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    ;; Collapse multiple blank lines
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local lisp-indent-offset 2)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // lean4
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; lean4-mode uses lsp-mode internally for the infoview (proof state)
;; this coexists fine with eglot for other languages
(use-package lean4-mode
  :commands lean4-mode
  :mode "\\.lean\\'"
  :config
  ;; Key bindings:
  ;; C-c C-i  toggle infoview (proof state) - THE KILLER FEATURE
  ;; C-c C-g  show goal at point
  ;; C-c C-k  kill lean server
  ;; C-c C-r  restart lean server
  (with-eval-after-load 'lsp-mode
    ;; Don't watch all files, lean projects can be huge
    (setq lsp-file-watch-threshold nil)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // zig
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package zig-mode
  :mode "\\.zig\\'"
  :config
  (setq zig-format-on-save nil))  ; we handle formatting ourselves

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // xref // unified navigation
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; xref-go-back is the good shit
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-?") 'xref-find-references)

;; Show xref in same window
(with-eval-after-load 'xref
  (when (fboundp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // format // unified
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/format-nix ()
  "Format Nix buffer with nixfmt or alejandra."
  (interactive)
  (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
      (eglot-format)
    (shell-command-on-region (point-min) (point-max)
                             (or (executable-find "nixfmt")
                                 (executable-find "alejandra"))
                             nil t)))

(defun hypermodern/format-buffer ()
  "Format buffer using the right tool for the mode."
  (interactive)
  (cond
   ;; Elisp - our custom thing
   ((derived-mode-p 'emacs-lisp-mode)
    (hypermodern/format-elisp))

   ;; Nix
   ((derived-mode-p 'nix-mode 'nix-ts-mode)
    (hypermodern/format-nix))

   ;; Languages with eglot running
   ((and hypermodern/use-eglot (fboundp 'eglot-managed-p) (eglot-managed-p))
    (eglot-format))

   ;; Fallback
   (t
    (indent-region (point-min) (point-max)))))

(global-set-key (kbd "M-z") #'hypermodern/format-buffer)

(provide 'hypermodern-languages)
;;; hypermodern-languages.el ends here
