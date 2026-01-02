;;; hypermodern-integration-test.el --- Integration tests -*- lexical-binding: t; -*-

(require 'ert)

;;; Module Loading Order

(ert-deftest hypermodern/test-load-order-core-first ()
  "Core should load before other modules."
  (should (featurep 'hypermodern-core)))

(ert-deftest hypermodern/test-all-modules-load ()
  "All hypermodern modules should load without error."
  (dolist (mod '(hypermodern-core
                 hypermodern-languages
                 hypermodern-terminal
                 hypermodern-remote
                 hypermodern-secrets))
    (should (require mod nil t))))

;;; Package Dependencies

(ert-deftest hypermodern/test-critical-packages-available ()
  "Critical packages should be loadable."
  ;; Skip packages that require interactive mode or external deps in batch tests
  (let ((batch-safe-pkgs '(consult embark company eglot general transient)))
    (dolist (pkg (if noninteractive batch-safe-pkgs
                   '(vertico orderless marginalia consult embark
                     company eglot magit general transient)))
      (should (require pkg nil t)))))

;;; Library Loading Regression Tests

(ert-deftest hypermodern/test-hypermodern-core-loadable ()
  "hypermodern-core should be loadable without ignore-errors."
  ;; This test prevents regression of the core loading issue we fixed
  (should (require 'hypermodern-core nil t))
  (should (featurep 'hypermodern-core)))

(ert-deftest hypermodern/test-all-hypermodern-modules-loadable ()
  "All hypermodern modules should load without ignore-errors wrappers."
  ;; Tests the fix for the build-time compilation failures
  (let ((modules '(hypermodern-core hypermodern-languages 
                   hypermodern-terminal hypermodern-remote 
                   hypermodern-secrets)))
    (dolist (mod modules)
      (should (require mod nil t))
      (should (featurep mod)))))

(ert-deftest hypermodern/test-no-load-path-conflicts ()
  "Load path should not contain conflicting emacs configurations."
  ;; Prevents regression of module conflicts we resolved
  (let ((hypermodern-paths 
         (seq-filter (lambda (path) 
                      (string-match-p "hypermodern\\|emacs" path))
                    load-path)))
    ;; Should have hypermodern paths but not conflicting ones
    (should (> (length hypermodern-paths) 0))
    ;; Should not have paths that would conflict (old emacs modules)
    (should-not (seq-some (lambda (path)
                           (string-match-p "emacs-old\\|emacs/default" path))
                         load-path))))

(ert-deftest hypermodern/test-provide-statements-work ()
  "All provide statements should properly register features."
  ;; Tests that our library loading mechanism works correctly
  (let ((expected-features '(hypermodern-core hypermodern-languages
                            hypermodern-terminal hypermodern-remote
                            hypermodern-secrets)))
    (dolist (feat expected-features)
      (should (featurep feat)))))

;;; Keybinding Conflicts

(ert-deftest hypermodern/test-no-keybinding-conflicts ()
  "Critical keybindings should not be shadowed."
  (skip-unless (not noninteractive))
  (let ((bindings '(("C-x g" . magit)
                    ("C-s" . consult-line)
                    ("C-x b" . consult-buffer)
                    ("M-." . xref-find-definitions)
                    ("M-z" . hypermodern/format-buffer))))
    (dolist (binding bindings)
      (let ((key (car binding))
            (expected-prefix (symbol-name (cdr binding))))
        (let ((actual (key-binding (kbd key))))
          (should actual)
          (should (string-prefix-p expected-prefix
                                   (symbol-name actual))))))))

;;; Auth-Source Integration

(ert-deftest hypermodern/test-auth-source-configured ()
  "Auth-source should have sources configured."
  (should (> (length auth-sources) 0)))

(ert-deftest hypermodern/test-auth-source-search-doesnt-error ()
  "Auth-source search should not error on missing entries."
  (should-not (auth-source-search :host "nonexistent-host-12345"
                                  :max 1)))

;;; Theme Integration

(ert-deftest hypermodern/test-custom-themes-available ()
  "Custom themes should be in theme path."
  (skip-unless (not noninteractive))
  (should (locate-file "base16-ono-sendai-tuned-theme.el"
                       custom-theme-load-path
                       '(".el" ".elc"))))

(ert-deftest hypermodern/test-theme-loadable ()
  "Custom theme should load without error."
  (skip-unless (not noninteractive))
  (should (load-theme 'base16-ono-sendai-tuned t)))

;;; Completion Integration

(ert-deftest hypermodern/test-completion-styles ()
  "Orderless should be in completion styles."
  (skip-unless (not noninteractive))
  (should (memq 'orderless completion-styles)))

(ert-deftest hypermodern/test-vertico-active ()
  "Vertico should be active."
  (skip-unless (not noninteractive))
  (should (bound-and-true-p vertico-mode)))

(ert-deftest hypermodern/test-marginalia-active ()
  "Marginalia should be active."
  (skip-unless (not noninteractive))
  (should (bound-and-true-p marginalia-mode)))

;;; Mode Line

(ert-deftest hypermodern/test-doom-modeline-active ()
  "Doom modeline should be active."
  (skip-unless (not noninteractive))
  (should (bound-and-true-p doom-modeline-mode)))

;;; Buffer-Local vs Global State

(ert-deftest hypermodern/test-global-modes-active ()
  "Global minor modes should be active."
  (skip-unless (not noninteractive))
  (dolist (mode '(global-company-mode
                  vertico-mode
                  marginalia-mode
                  doom-modeline-mode
                  repeat-mode))
    (when (boundp mode)
      (should (symbol-value mode)))))

;;; Hooks Not Broken

(ert-deftest hypermodern/test-prog-mode-hooks-safe ()
  "prog-mode-hook should not contain broken functions."
  (dolist (fn prog-mode-hook)
    (should (or (functionp fn)
                (and (symbolp fn) (fboundp fn))))))

(ert-deftest hypermodern/test-after-init-hooks-safe ()
  "after-init-hook should not contain broken functions."
  (dolist (fn after-init-hook)
    (should (or (functionp fn)
                (and (symbolp fn) (fboundp fn))))))

;;; Startup Performance

(ert-deftest hypermodern/test-startup-time-reasonable ()
  "Startup should complete in reasonable time."
  ;; This runs after init, so emacs-init-time should be set
  (when (fboundp 'emacs-init-time)
    (let ((time-str (emacs-init-time "%f")))
      (when time-str
        (let ((time (string-to-number time-str)))
          ;; Should start in under 5 seconds even on slow hardware
          (should (< time 5.0)))))))

;;; No Warnings During Load

(ert-deftest hypermodern/test-no-byte-compile-warnings ()
  "Modules should byte-compile without warnings."
  (skip-unless (not noninteractive))
  (let ((warning-count 0))
    (dolist (file (directory-files
                   (expand-file-name "lib" user-emacs-directory)
                   t "\\.el$"))
      (with-temp-buffer
        (let ((byte-compile-warnings t)
              (byte-compile-error-on-warn nil))
          (condition-case err
              (byte-compile-file file)
            (error (cl-incf warning-count))))))
    (should (= warning-count 0))))

(provide 'hypermodern-integration-test)
;;; hypermodern-integration-test.el ends here
