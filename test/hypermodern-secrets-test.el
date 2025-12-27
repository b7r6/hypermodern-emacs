;;; hypermodern-secrets-test.el --- Tests for hypermodern-secrets -*- lexical-binding: t; -*-

(require 'ert)
(require 'hypermodern-secrets)

;;; Context Data Structure

(ert-deftest hypermodern/test-contexts-defined ()
  "Default contexts should be defined."
  (should (assq 'personal hypermodern/secrets-contexts))
  (should (assq 'weyl hypermodern/secrets-contexts)))

(ert-deftest hypermodern/test-context-has-required-keys ()
  "Each context must have required properties."
  (dolist (ctx hypermodern/secrets-contexts)
    (let ((name (car ctx))
          (plist (cdr ctx)))
      (should (plist-get plist :name))
      (should (plist-get plist :authinfo))
      (should (plist-get plist :agenix-secrets)))))

(ert-deftest hypermodern/test-context-get ()
  "Should retrieve context properties."
  (should (stringp (hypermodern/secrets-context-get :name 'personal)))
  (should (stringp (hypermodern/secrets-context-get :authinfo 'personal))))

(ert-deftest hypermodern/test-context-get-missing ()
  "Should return nil for missing properties."
  (should-not (hypermodern/secrets-context-get :nonexistent 'personal)))

(ert-deftest hypermodern/test-context-get-invalid-context ()
  "Should return nil for invalid context."
  (should-not (hypermodern/secrets-context-get :name 'nonexistent)))

;;; Context Switching

(ert-deftest hypermodern/test-context-switch-updates-current ()
  "Switching context should update current context."
  (let ((orig hypermodern/secrets-current-context))
    (unwind-protect
        (progn
          (hypermodern/secrets-switch-context 'personal)
          (should (eq hypermodern/secrets-current-context 'personal))
          (hypermodern/secrets-switch-context 'weyl)
          (should (eq hypermodern/secrets-current-context 'weyl)))
      ;; Restore
      (hypermodern/secrets-switch-context orig))))

(ert-deftest hypermodern/test-context-switch-updates-auth-sources ()
  "Switching context should update auth-sources."
  (let ((orig hypermodern/secrets-current-context)
        (orig-auth auth-sources))
    (unwind-protect
        (progn
          (hypermodern/secrets-switch-context 'personal)
          (let ((expected (hypermodern/secrets-context-get :authinfo 'personal)))
            (should (member expected auth-sources))))
      ;; Restore
      (setq auth-sources orig-auth)
      (hypermodern/secrets-switch-context orig))))

;;; Auth-Source Backend

(ert-deftest hypermodern/test-auth-source-backend-registered ()
  "Agenix auth-source backend should be registered."
  ;; The backend is registered via a lambda, so just check the function exists
  (should (fboundp 'hypermodern/agenix-auth-source-backend)))

(ert-deftest hypermodern/test-agenix-secret-patterns ()
  "Secret name patterns should be well-formed."
  (let ((patterns (hypermodern/agenix-secret-names "example.com")))
    (should (member "example-api-key" patterns))
    (should (member "example.com" patterns))
    (should (member "api/example" patterns))))

;;; Agenix Functions

(ert-deftest hypermodern/test-agenix-read-secret-nonexistent ()
  "Reading nonexistent secret should return nil."
  (should-not (hypermodern/agenix-read-secret "definitely-not-a-real-secret-12345")))

;;; GPG Functions

(ert-deftest hypermodern/test-gpg-list-keys-function-exists ()
  "GPG key listing function should exist."
  (should (fboundp 'hypermodern/gpg-list-keys)))

(ert-deftest hypermodern/test-gpg-export-function-exists ()
  "GPG export function should exist."
  (should (fboundp 'hypermodern/gpg-export-key)))

;;; Transient Menu

(ert-deftest hypermodern/test-secrets-menu-defined ()
  "Secrets transient menu should be defined."
  (should (fboundp 'hypermodern/secrets-menu)))

;;; Integration: get-secret

(ert-deftest hypermodern/test-get-secret-returns-string-or-nil ()
  "get-secret should return string or nil, never error."
  (let ((result (hypermodern/get-secret "test-nonexistent-key")))
    (should (or (null result) (stringp result)))))

;;; Invariants

(ert-deftest hypermodern/test-context-switch-idempotent ()
  "Switching to same context twice should be idempotent."
  (let ((orig hypermodern/secrets-current-context))
    (unwind-protect
        (progn
          (hypermodern/secrets-switch-context 'personal)
          (let ((auth1 (copy-sequence auth-sources)))
            (hypermodern/secrets-switch-context 'personal)
            (should (equal auth1 auth-sources))))
      (hypermodern/secrets-switch-context orig))))

(ert-deftest hypermodern/test-context-roundtrip ()
  "Switching away and back should restore state."
  (let ((orig hypermodern/secrets-current-context))
    (unwind-protect
        (progn
          (hypermodern/secrets-switch-context 'personal)
          (let ((auth1 (copy-sequence auth-sources)))
            (hypermodern/secrets-switch-context 'weyl)
            (hypermodern/secrets-switch-context 'personal)
            (should (equal auth1 auth-sources))))
      (hypermodern/secrets-switch-context orig))))

(provide 'hypermodern-secrets-test)
;;; hypermodern-secrets-test.el ends here
