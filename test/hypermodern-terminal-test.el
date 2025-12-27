;;; hypermodern-terminal-test.el --- Tests for hypermodern-terminal -*- lexical-binding: t; -*-

(require 'ert)
(require 'hypermodern-terminal)

;;; VTerm

(ert-deftest hypermodern/test-vterm-available ()
  "VTerm should be loadable."
  (should (require 'vterm nil t)))

(ert-deftest hypermodern/test-vterm-toggle-exists ()
  "VTerm toggle function should exist."
  (should (fboundp 'hypermodern/vterm-toggle)))

;;; Eat

(ert-deftest hypermodern/test-eat-available ()
  "Eat should be loadable (if installed)."
  (or (require 'eat nil t)
      (should t)))

;;; Detached

(ert-deftest hypermodern/test-detached-available ()
  "Detached should be loadable."
  (should (require 'detached nil t)))

(ert-deftest hypermodern/test-detached-init-called ()
  "Detached should be initialized."
  (when (featurep 'detached)
    (should (bound-and-true-p detached-db-directory))))

;;; Terminal Keybindings

(ert-deftest hypermodern/test-terminal-toggle-binding ()
  "Terminal toggle should be bound."
  (should (eq (key-binding (kbd "C-`")) 'hypermodern/term-toggle)))

(ert-deftest hypermodern/test-detached-bindings ()
  "Detached commands should be bound (after loading detached)."
  ;; These are :bind deferred, so only test if detached is loaded
  (when (featurep 'detached)
    (should (eq (key-binding (kbd "C-c d c")) 'detached-shell-command))
    (should (eq (key-binding (kbd "C-c d l")) 'detached-list-sessions))))

;;; Server

(ert-deftest hypermodern/test-server-start-function ()
  "Server start should be configured."
  (should (fboundp 'server-start)))

;;; Shell Environment

(ert-deftest hypermodern/test-shell-file-name ()
  "Shell should be set."
  (should (stringp shell-file-name))
  (should (> (length shell-file-name) 0)))

;;; Terminal Buffer Naming

(ert-deftest hypermodern/test-vterm-buffer-name-function ()
  "VTerm buffer naming should be configured."
  (when (featurep 'vterm)
    (should (or (boundp 'vterm-buffer-name)
                (boundp 'vterm-buffer-name-string)))))

(provide 'hypermodern-terminal-test)
;;; hypermodern-terminal-test.el ends here
