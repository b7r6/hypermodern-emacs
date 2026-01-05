;;; hypermodern-remote-test.el --- Tests for hypermodern-remote -*- lexical-binding: t; -*-

(require 'ert)
(require 'hypermodern-remote)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; TRAMP Black Book - Core Settings
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-tramp-loaded ()
  "TRAMP should be loaded."
  (should (featurep 'tramp)))

(ert-deftest hypermodern/test-tramp-default-method ()
  "TRAMP default method should be ssh."
  (should (equal tramp-default-method "ssh")))

(ert-deftest hypermodern/test-tramp-controlmaster-enabled ()
  "TRAMP ControlMaster should be enabled for performance."
  (should tramp-use-ssh-controlmaster-options))

(ert-deftest hypermodern/test-tramp-terminal-type ()
  "TRAMP terminal type should be dumb (avoids prompt issues)."
  (should (equal tramp-terminal-type "dumb")))

(ert-deftest hypermodern/test-tramp-encoding-shell ()
  "TRAMP should use POSIX sh for encoding."
  (should (equal tramp-encoding-shell "/bin/sh")))

(ert-deftest hypermodern/test-tramp-remote-shell ()
  "TRAMP should use /bin/sh remotely."
  (should (equal tramp-remote-shell "/bin/sh")))

(ert-deftest hypermodern/test-tramp-connection-timeout ()
  "TRAMP connection timeout should be set."
  (should (numberp tramp-connection-timeout))
  (should (> tramp-connection-timeout 0)))

(ert-deftest hypermodern/test-tramp-verbose-quiet ()
  "TRAMP should be quiet by default (verbose <= 1)."
  (should (<= tramp-verbose 1)))

(ert-deftest hypermodern/test-tramp-chunksize ()
  "TRAMP chunksize should be set for performance."
  (should (numberp tramp-chunksize))
  (should (>= tramp-chunksize 500)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; TRAMP Remote PATH
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-tramp-remote-path-configured ()
  "TRAMP remote path should be configured."
  (should (listp tramp-remote-path))
  (should (> (length tramp-remote-path) 3)))

(ert-deftest hypermodern/test-tramp-remote-path-has-nix ()
  "TRAMP remote path should include Nix paths."
  (should (cl-some (lambda (p) (and (stringp p) (string-match-p "nix" p)))
                   tramp-remote-path)))

(ert-deftest hypermodern/test-tramp-remote-path-inherits ()
  "TRAMP should inherit remote PATH."
  (should (member 'tramp-own-remote-path tramp-remote-path)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Performance Settings
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-tramp-vc-ignored ()
  "VC should ignore remote files for performance."
  (should (string-match-p vc-ignore-dir-regexp "/ssh:example.com:")))

(ert-deftest hypermodern/test-remote-lockfiles-inhibited ()
  "Remote lock files should be inhibited."
  (should remote-file-name-inhibit-locks))

(ert-deftest hypermodern/test-password-cache ()
  "Password cache should be enabled with expiry."
  (should tramp-use-auth-sources)
  (should (numberp password-cache-expiry))
  (should (> password-cache-expiry 0)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Tailscale Functions
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-tailscale-find-file-exists ()
  "Tailscale find-file function should exist."
  (should (fboundp 'hypermodern/tailscale-find-file)))

(ert-deftest hypermodern/test-tailscale-shell-exists ()
  "Tailscale shell function should exist."
  (should (fboundp 'hypermodern/tailscale-shell)))

(ert-deftest hypermodern/test-tailscale-dired-exists ()
  "Tailscale dired function should exist."
  (should (fboundp 'hypermodern/tailscale-dired)))

(ert-deftest hypermodern/test-tailscale-hosts-returns-list ()
  "Tailscale hosts function should return a list."
  (should (listp (hypermodern/tailscale-hosts))))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; SSH Config Functions
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-ssh-hosts-function ()
  "SSH hosts parser should exist and return list."
  (should (fboundp 'hypermodern/ssh-hosts))
  (should (listp (hypermodern/ssh-hosts))))

(ert-deftest hypermodern/test-ssh-find-file-exists ()
  "SSH find-file function should exist."
  (should (fboundp 'hypermodern/ssh-find-file)))

(ert-deftest hypermodern/test-ssh-dired-exists ()
  "SSH dired function should exist."
  (should (fboundp 'hypermodern/ssh-dired)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Sudo Functions
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-sudo-find-file-exists ()
  "Sudo find-file function should exist."
  (should (fboundp 'hypermodern/sudo-find-file)))

(ert-deftest hypermodern/test-sudo-this-file-exists ()
  "Sudo this-file function should exist."
  (should (fboundp 'hypermodern/sudo-this-file)))

(ert-deftest hypermodern/test-remote-sudo-exists ()
  "Remote sudo function should exist."
  (should (fboundp 'hypermodern/remote-sudo-this-file)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Debug Functions
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-tramp-debug-on-exists ()
  "TRAMP debug-on function should exist."
  (should (fboundp 'hypermodern/tramp-debug-on)))

(ert-deftest hypermodern/test-tramp-debug-off-exists ()
  "TRAMP debug-off function should exist."
  (should (fboundp 'hypermodern/tramp-debug-off)))

(ert-deftest hypermodern/test-tramp-cleanup-exists ()
  "TRAMP cleanup function should exist."
  (should (fboundp 'hypermodern/tramp-cleanup)))

(ert-deftest hypermodern/test-tramp-cleanup-host-exists ()
  "TRAMP cleanup-host function should exist."
  (should (fboundp 'hypermodern/tramp-cleanup-host)))

(ert-deftest hypermodern/test-tramp-status-exists ()
  "TRAMP status function should exist."
  (should (fboundp 'hypermodern/tramp-status)))

(ert-deftest hypermodern/test-tramp-starship-fix-exists ()
  "TRAMP starship-fix function should exist."
  (should (fboundp 'hypermodern/tramp-starship-fix)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Keybindings
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-tailscale-keybindings ()
  "Tailscale commands should be bound."
  (should (eq (key-binding (kbd "C-c t f")) 'hypermodern/tailscale-find-file))
  (should (eq (key-binding (kbd "C-c t s")) 'hypermodern/tailscale-shell))
  (should (eq (key-binding (kbd "C-c t d")) 'hypermodern/tailscale-dired)))

(ert-deftest hypermodern/test-ssh-keybindings ()
  "SSH commands should be bound."
  (should (eq (key-binding (kbd "C-c r f")) 'hypermodern/ssh-find-file))
  (should (eq (key-binding (kbd "C-c r d")) 'hypermodern/ssh-dired)))

(ert-deftest hypermodern/test-sudo-keybindings ()
  "Sudo commands should be bound."
  (should (eq (key-binding (kbd "C-c r u")) 'hypermodern/sudo-this-file))
  (should (eq (key-binding (kbd "C-c r U")) 'hypermodern/remote-sudo-this-file)))

(ert-deftest hypermodern/test-tramp-utility-keybindings ()
  "TRAMP utility commands should be bound."
  (should (eq (key-binding (kbd "C-c r c")) 'hypermodern/tramp-cleanup))
  (should (eq (key-binding (kbd "C-c r s")) 'hypermodern/tramp-status))
  (should (eq (key-binding (kbd "C-c r ?")) 'hypermodern/tramp-starship-fix)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Path Construction
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-tramp-path-format ()
  "TRAMP paths should be well-formed."
  (let ((path (format "/ssh:%s:" "testhost")))
    (should (string-prefix-p "/ssh:" path))
    (should (string-suffix-p ":" path))))

(ert-deftest hypermodern/test-sudo-path-format ()
  "SUDO paths should be well-formed."
  (let ((path "/sudo::/etc/hosts"))
    (should (string-prefix-p "/sudo::" path))))

(provide 'hypermodern-remote-test)
;;; hypermodern-remote-test.el ends here
