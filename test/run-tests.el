;;; run-tests.el --- Test runner for hypermodern-emacs -*- lexical-binding: t; -*-

(require 'ert)

;; Load all test files
(dolist (test-file '("hypermodern-core-test"
                      "hypermodern-compile-test"
                      "hypermodern-languages-test"
                      "hypermodern-terminal-test"
                      "hypermodern-remote-test"
                      "hypermodern-secrets-test"
                      "hypermodern-integration-test"
                      "hypermodern-build-test"
                      "hypermodern-wiring-test"
                      "hypermodern-windows-test"
                      "hypermodern-navigation-test"))
  (require (intern test-file) nil t))

(provide 'run-tests)
;;; run-tests.el ends here
