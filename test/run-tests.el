;;; run-tests.el --- Test runner for hypermodern-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --batch -l run-tests.el
;; Or via nix: nix run .#test

;;; Code:

(require 'ert)
(require 'ert-x)

;; Setup load path
(let ((lib-dir (expand-file-name "lib" (file-name-directory load-file-name)))
      (test-dir (expand-file-name "test" (file-name-directory load-file-name))))
  (add-to-list 'load-path lib-dir)
  (add-to-list 'load-path test-dir))

;; Load all modules (simulating init.el)
(require 'hypermodern-core)
(require 'hypermodern-languages)
(require 'hypermodern-terminal)
(require 'hypermodern-remote)
(require 'hypermodern-secrets)

;; Load test files
(require 'hypermodern-core-test)
(require 'hypermodern-secrets-test)
(require 'hypermodern-languages-test)
(require 'hypermodern-remote-test)
(require 'hypermodern-terminal-test)
(require 'hypermodern-integration-test)

;; Run tests
(defun hypermodern/run-tests ()
  "Run all tests and report results."
  (interactive)
  (ert-run-tests-batch-and-exit))

;; When run in batch mode
(when noninteractive
  (hypermodern/run-tests))

(provide 'run-tests)
;;; run-tests.el ends here
