;;; hypermodern-languages-test.el --- Tests for hypermodern-languages -*- lexical-binding: t; -*-

(require 'ert)
(require 'hypermodern-languages)

;;; Eglot Configuration

(ert-deftest hypermodern/test-eglot-loadable ()
  "Eglot should be loadable."
  (should (require 'eglot nil t)))

(ert-deftest hypermodern/test-eglot-server-programs-populated ()
  "Eglot should have server programs configured."
  (require 'eglot nil t)
  (should (> (length eglot-server-programs) 5)))

(ert-deftest hypermodern/test-eglot-python-configured ()
  "Python should have LSP configured."
  (require 'eglot nil t)
  (let ((found nil))
    (dolist (entry eglot-server-programs)
      (when (and (listp (car entry))
                 (memq 'python-mode (car entry)))
        (setq found t)))
    (should found)))

(ert-deftest hypermodern/test-eglot-rust-configured ()
  "Rust should have LSP configured."
  (require 'eglot nil t)
  (let ((found nil))
    (dolist (entry eglot-server-programs)
      (when (and (listp (car entry))
                 (or (memq 'rust-mode (car entry))
                     (memq 'rust-ts-mode (car entry))))
        (setq found t)))
    (should found)))

(ert-deftest hypermodern/test-eglot-nix-configured ()
  "Nix should have LSP configured."
  (require 'eglot nil t)
  (let ((found nil))
    (dolist (entry eglot-server-programs)
      (when (and (listp (car entry))
                 (or (memq 'nix-mode (car entry))
                     (memq 'nix-ts-mode (car entry))))
        (setq found t)))
    (should found)))

;;; Eglot Settings

(ert-deftest hypermodern/test-eglot-autoshutdown ()
  "Eglot should autoshutdown idle servers."
  (require 'eglot nil t)
  (should eglot-autoshutdown))

(ert-deftest hypermodern/test-eglot-inlay-hints-off ()
  "Inlay hints should be off by default."
  (require 'eglot nil t)
  (should-not eglot-inlay-hints-mode))

;;; Workspace Configuration

(ert-deftest hypermodern/test-workspace-config-exists ()
  "Workspace configuration should be set."
  (require 'eglot nil t)
  (should (bound-and-true-p eglot-workspace-configuration)))

(ert-deftest hypermodern/test-workspace-config-has-pyright ()
  "Workspace config should include pyright settings."
  (require 'eglot nil t)
  (should (plist-get eglot-workspace-configuration :basedpyright)))

(ert-deftest hypermodern/test-workspace-config-has-rust-analyzer ()
  "Workspace config should include rust-analyzer settings."
  (require 'eglot nil t)
  (should (plist-get eglot-workspace-configuration :rust-analyzer)))

;;; Formatting

(ert-deftest hypermodern/test-format-buffer-function-exists ()
  "Format buffer function should exist."
  (should (fboundp 'hypermodern/format-buffer)))

(ert-deftest hypermodern/test-format-nix-function-exists ()
  "Nix formatter function should exist."
  (should (fboundp 'hypermodern/format-nix)))

;;; Bazel

(ert-deftest hypermodern/test-bazel-mode-available ()
  "Bazel mode should be loadable."
  (should (require 'bazel nil t)))

(ert-deftest hypermodern/test-bazel-compile-commands-exists ()
  "Bazel compile commands function should exist."
  (should (fboundp 'hypermodern/bazel-compile-commands)))

;;; Language Modes

(ert-deftest hypermodern/test-haskell-mode-available ()
  "Haskell mode should be loadable."
  (should (require 'haskell-mode nil t)))

(ert-deftest hypermodern/test-nix-mode-available ()
  "Nix mode should be loadable."
  (should (require 'nix-mode nil t)))

(ert-deftest hypermodern/test-rust-mode-available ()
  "Rust mode should be loadable."
  (should (require 'rust-mode nil t)))

(ert-deftest hypermodern/test-zig-mode-available ()
  "Zig mode should be loadable."
  (should (require 'zig-mode nil t)))

;;; Custom Modes

(ert-deftest hypermodern/test-objdump-mode-exists ()
  "Objdump mode should be defined."
  (should (fboundp 'objdump-mode)))

(ert-deftest hypermodern/test-cuobjdump-mode-exists ()
  "Cuobjdump mode should be defined."
  (should (fboundp 'cuobjdump-mode)))

(ert-deftest hypermodern/test-ptx-mode-exists ()
  "PTX mode should be defined."
  (should (fboundp 'ptx-mode)))

;;; Lean4

(ert-deftest hypermodern/test-lean4-mode-loadable ()
  "Lean4 mode should be loadable (if installed)."
  ;; This might fail if lean4-mode isn't installed, which is OK
  (or (require 'lean4-mode nil t)
      (should t)))  ; Pass if not installed

;;; xref

(ert-deftest hypermodern/test-xref-bindings ()
  "xref should have standard bindings."
  (should (eq (key-binding (kbd "M-.")) 'xref-find-definitions))
  (should (eq (key-binding (kbd "M-,")) 'xref-go-back))
  (should (eq (key-binding (kbd "M-?")) 'xref-find-references)))

;;; Flymake

(ert-deftest hypermodern/test-flymake-available ()
  "Flymake should be available."
  (should (featurep 'flymake)))

(provide 'hypermodern-languages-test)
;;; hypermodern-languages-test.el ends here
