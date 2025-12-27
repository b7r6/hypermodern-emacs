;;; hypermodern-remote-test.el --- Tests for hypermodern-remote -*- lexical-binding: t; -*-

(require 'ert)
(require 'hypermodern-remote)

;;; TRAMP Configuration

(ert-deftest hypermodern/test-tramp-loaded ()
  "TRAMP should be loaded."
  (should (featurep 'tramp)))

(ert-deftest hypermodern/test-tramp-default-method ()
  "TRAMP default method should be ssh."
  (should (equal tramp-default-method "ssh")))

(ert-deftest hypermodern/test-tramp-controlmaster-disabled ()
  "TRAMP ControlMaster should be disabled (we manage it ourselves)."
  (should-not tramp-use-ssh-controlmaster-options))

(ert-deftest hypermodern/test-tramp-vc-ignored ()
  "VC should ignore remote files for performance."
  (should (string-match-p "tramp" vc-ignore-dir-regexp)))

;;; Tailscale Functions

(ert-deftest hypermodern/test-tailscale-find-file-exists ()
  "Tailscale find-file function should exist."
  (should (fboundp 'hypermodern/tailscale-find-file)))

(ert-deftest hypermodern/test-tailscale-shell-exists ()
  "Tailscale shell function should exist."
  (should (fboundp 'hypermodern/tailscale-shell)))

(ert-deftest hypermodern/test-tailscale-dired-exists ()
  "Tailscale dired function should exist."
  (should (fboundp 'hypermodern/tailscale-dired)))

;;; Tailscale Keybindings

(ert-deftest hypermodern/test-tailscale-keybindings ()
  "Tailscale commands should be bound."
  (should (eq (key-binding (kbd "C-c t f")) 'hypermodern/tailscale-find-file))
  (should (eq (key-binding (kbd "C-c t s")) 'hypermodern/tailscale-shell))
  (should (eq (key-binding (kbd "C-c t d")) 'hypermodern/tailscale-dired)))

;;; Host Completion

(ert-deftest hypermodern/test-tailscale-hosts-returns-list ()
  "Tailscale hosts function should return a list."
  ;; Will be empty if tailscale not running, but should not error
  (should (listp (hypermodern/tailscale-hosts))))

;;; TRAMP Path Construction

(ert-deftest hypermodern/test-tramp-path-format ()
  "TRAMP paths should be well-formed."
  (let ((path (format "/ssh:%s:" "testhost")))
    (should (string-prefix-p "/ssh:" path))
    (should (string-suffix-p ":" path))))

;;; Cleanup Function

(ert-deftest hypermodern/test-tramp-cleanup-exists ()
  "TRAMP cleanup function should exist."
  (should (fboundp 'hypermodern/tramp-cleanup)))

(provide 'hypermodern-remote-test)
;;; hypermodern-remote-test.el ends here
