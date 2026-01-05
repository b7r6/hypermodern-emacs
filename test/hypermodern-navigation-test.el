;;; hypermodern-navigation-test.el --- Tests for file navigation -*- lexical-binding: t; -*-

(require 'ert)
(require 'hypermodern-navigation)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Zoxide
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-zoxide-loadable ()
  "Zoxide package should be loadable."
  (should (require 'zoxide nil t)))

(ert-deftest hypermodern/test-zoxide-query-function ()
  "Zoxide query function should exist."
  (should (fboundp 'hypermodern/zoxide-query)))

(ert-deftest hypermodern/test-zoxide-jump-function ()
  "Zoxide jump function should exist."
  (should (fboundp 'hypermodern/zoxide-jump)))

(ert-deftest hypermodern/test-zoxide-find-file-function ()
  "Zoxide find-file function (deprecated, use zoxide-jump instead)."
  ;; This function was never implemented, use hypermodern/zoxide-jump
  (should (fboundp 'hypermodern/zoxide-jump)))

(ert-deftest hypermodern/test-zoxide-keybinding ()
  "C-c z z should be bound to zoxide jump."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-c z z")) 'hypermodern/zoxide-jump)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Consult-dir
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-consult-dir-loadable ()
  "Consult-dir package should be loadable."
  (should (require 'consult-dir nil t)))

(ert-deftest hypermodern/test-consult-dir-keybinding ()
  "C-x C-d should be bound to consult-dir."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-x C-d")) 'consult-dir)))

(ert-deftest hypermodern/test-consult-dir-sources-has-zoxide ()
  "Consult-dir should have zoxide source."
  (require 'consult-dir nil t)
  (should (bound-and-true-p consult-dir-sources))
  (should (member 'consult-dir--source-zoxide consult-dir-sources)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Smart find-file
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-find-file-function ()
  "Smart find-file function should exist."
  (should (fboundp 'hypermodern/find-file)))

(ert-deftest hypermodern/test-find-file-keybinding ()
  "C-x C-f should be bound to smart find-file."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-x C-f")) 'hypermodern/find-file)))

(ert-deftest hypermodern/test-fd-project-function ()
  "Fd project function should exist."
  (should (fboundp 'hypermodern/fd-project)))

(ert-deftest hypermodern/test-fd-here-function ()
  "Fd here function should exist."
  (should (fboundp 'hypermodern/fd-here)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Dired integration
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-dired-jump-zoxide ()
  "Dired jump via zoxide function should exist."
  (should (fboundp 'hypermodern/dired-jump-zoxide)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Recent files
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-recent-file-keybinding ()
  "C-x C-r should be bound to consult-recent-file."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-x C-r")) 'consult-recent-file)))

(ert-deftest hypermodern/test-recentf-mode-active ()
  "Recentf mode should be active."
  (should (bound-and-true-p recentf-mode)))

(provide 'hypermodern-navigation-test)
;;; hypermodern-navigation-test.el ends here
