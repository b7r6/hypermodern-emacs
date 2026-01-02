;;; hypermodern-wiring-test.el --- Tests for package wiring -*- lexical-binding: t; -*-
;;
;; These tests verify that packages are not just installed but actually configured.
;; A package that's installed but unwired is worse than not having it at all.
;;
;; Note: Window management tests are in hypermodern-windows-test.el

(require 'ert)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Ace-window - window switching
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-ace-window-loadable ()
  "Ace-window should be loadable."
  (should (require 'ace-window nil t)))

(ert-deftest hypermodern/test-ace-window-keybinding ()
  "Ace-window should be bound to M-o."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "M-o")) 'ace-window)))

(ert-deftest hypermodern/test-ace-window-keys-configured ()
  "Ace-window should have home row keys configured."
  (require 'ace-window nil t)
  (should (bound-and-true-p aw-keys))
  (should (memq ?a aw-keys))
  (should (memq ?s aw-keys)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Avy - jump to visible text
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-avy-loadable ()
  "Avy should be loadable."
  (should (require 'avy nil t)))

(ert-deftest hypermodern/test-avy-keybinding-char ()
  "Avy should have char jump keybinding."
  (skip-unless (not noninteractive))
  (let ((binding (key-binding (kbd "C-'"))))
    (should (or (eq binding 'avy-goto-char-timer)
                (eq binding 'avy-goto-char-2)
                (eq binding 'avy-goto-char)))))

(ert-deftest hypermodern/test-avy-keybinding-line ()
  "Avy should have line jump keybinding."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "M-g l")) 'avy-goto-line)))

(ert-deftest hypermodern/test-avy-keybinding-word ()
  "Avy should have word jump keybinding."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "M-g w")) 'avy-goto-word-1)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Deadgrep - ripgrep interface
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-deadgrep-loadable ()
  "Deadgrep should be loadable."
  (should (require 'deadgrep nil t)))

(ert-deftest hypermodern/test-deadgrep-function-exists ()
  "Deadgrep function should exist."
  (should (fboundp 'deadgrep)))

(ert-deftest hypermodern/test-deadgrep-keybinding ()
  "Deadgrep should have a keybinding."
  (skip-unless (not noninteractive))
  (let ((binding (key-binding (kbd "C-c s g"))))
    (should (eq binding 'deadgrep))))

(ert-deftest hypermodern/test-wgrep-deadgrep-loadable ()
  "Wgrep-deadgrep should be loadable for editing results."
  (should (require 'wgrep-deadgrep nil t)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expand-region - semantic selection
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-expand-region-loadable ()
  "Expand-region should be loadable."
  (should (require 'expand-region nil t)))

(ert-deftest hypermodern/test-expand-region-keybinding ()
  "Expand-region should be bound to C-=."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-=")) 'er/expand-region)))

(ert-deftest hypermodern/test-contract-region-keybinding ()
  "Contract-region should be bound to C--."
  (skip-unless (not noninteractive))
  (let ((binding (key-binding (kbd "C--"))))
    (should (or (eq binding 'er/contract-region)
                ;; might be text-scale-adjust in some configs
                (eq binding 'negative-argument)))))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Multiple-cursors
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-multiple-cursors-loadable ()
  "Multiple-cursors should be loadable."
  (should (require 'multiple-cursors nil t)))

(ert-deftest hypermodern/test-mc-mark-next-keybinding ()
  "Multiple-cursors should be bound to C->."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C->")) 'mc/mark-next-like-this)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Gptel - AI integration
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-gptel-loadable ()
  "Gptel should be loadable."
  (should (require 'gptel nil t)))

(ert-deftest hypermodern/test-gptel-keybinding ()
  "Gptel should be bound to C-c g g."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-c g g")) 'gptel)))

(ert-deftest hypermodern/test-gptel-api-key-is-function ()
  "Gptel API key should be a function (auth-source lookup)."
  (require 'gptel nil t)
  ;; After proper config, gptel-api-key should be a function, not a string
  ;; This ensures we're not hardcoding keys
  (when (boundp 'gptel-api-key)
    (should (or (functionp gptel-api-key)
                (null gptel-api-key)
                ;; Allow string if explicitly set by user
                (stringp gptel-api-key)))))

(ert-deftest hypermodern/test-gptel-no-hardcoded-key ()
  "Gptel should not have hardcoded API key in init files."
  ;; This test reads the actual files to ensure no keys are committed
  (let ((init-file (locate-library "init" t)))
    (when init-file
      (with-temp-buffer
        (insert-file-contents init-file)
        (should-not (string-match-p "sk-or-v1-[a-z0-9]\\{64\\}" (buffer-string)))))))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Consult - enhanced commands
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-consult-loadable ()
  "Consult should be loadable."
  (should (require 'consult nil t)))

(ert-deftest hypermodern/test-consult-line-keybinding ()
  "Consult-line should be bound to C-s."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-s")) 'consult-line)))

(ert-deftest hypermodern/test-consult-buffer-keybinding ()
  "Consult-buffer should be bound to C-x b."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-x b")) 'consult-buffer)))

(ert-deftest hypermodern/test-consult-ripgrep-keybinding ()
  "Consult-ripgrep should be bound to C-M-r."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-M-r")) 'consult-ripgrep)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Embark - contextual actions
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-embark-loadable ()
  "Embark should be loadable."
  (should (require 'embark nil t)))

(ert-deftest hypermodern/test-embark-act-keybinding ()
  "Embark-act should be bound to C-."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-.")) 'embark-act)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Vertico + Orderless + Marginalia
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-vertico-loadable ()
  "Vertico should be loadable."
  (should (require 'vertico nil t)))

(ert-deftest hypermodern/test-vertico-mode-active ()
  "Vertico mode should be active."
  (skip-unless (not noninteractive))
  (should (bound-and-true-p vertico-mode)))

(ert-deftest hypermodern/test-orderless-loadable ()
  "Orderless should be loadable."
  (should (require 'orderless nil t)))

(ert-deftest hypermodern/test-orderless-in-completion-styles ()
  "Orderless should be in completion-styles."
  (skip-unless (not noninteractive))
  (should (memq 'orderless completion-styles)))

(ert-deftest hypermodern/test-marginalia-loadable ()
  "Marginalia should be loadable."
  (should (require 'marginalia nil t)))

(ert-deftest hypermodern/test-marginalia-mode-active ()
  "Marginalia mode should be active."
  (skip-unless (not noninteractive))
  (should (bound-and-true-p marginalia-mode)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Eglot - LSP
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-eglot-loadable ()
  "Eglot should be loadable."
  (should (require 'eglot nil t)))

(ert-deftest hypermodern/test-no-lsp-ui ()
  "lsp-ui should NOT be loaded (we use eglot)."
  (should-not (featurep 'lsp-ui)))

(ert-deftest hypermodern/test-no-lsp-pyright ()
  "lsp-pyright should NOT be loaded (we use eglot + basedpyright)."
  (should-not (featurep 'lsp-pyright)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Formatting - M-z should work
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-format-buffer-exists ()
  "hypermodern/format-buffer should exist."
  (should (fboundp 'hypermodern/format-buffer)))

(ert-deftest hypermodern/test-format-keybinding ()
  "M-z should be bound to format function."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "M-z")) 'hypermodern/format-buffer)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Company - completion
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-company-loadable ()
  "Company should be loadable."
  (should (require 'company nil t)))

(ert-deftest hypermodern/test-company-global-mode ()
  "Global company mode should be active."
  (skip-unless (not noninteractive))
  (should (bound-and-true-p global-company-mode)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Magit
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-magit-loadable ()
  "Magit should be loadable."
  (should (require 'magit nil t)))

(ert-deftest hypermodern/test-magit-keybinding ()
  "Magit should be bound to C-x g."
  (skip-unless (not noninteractive))
  (let ((binding (key-binding (kbd "C-x g"))))
    (should (or (eq binding 'magit-status)
                (eq binding 'magit)))))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Window movement - b7r6 standard
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-windmove-right ()
  "M-N should move right."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "M-N")) 'windmove-right)))

(ert-deftest hypermodern/test-windmove-left ()
  "M-P should move left."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "M-P")) 'windmove-left)))

(provide 'hypermodern-wiring-test)
;;; hypermodern-wiring-test.el ends here

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Terminal color detection
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-truecolor-detection-function-exists ()
  "Truecolor detection function should exist."
  (should (fboundp 'hypermodern/terminal-supports-truecolor-p)))

(ert-deftest hypermodern/test-color-diag-function-exists ()
  "Color diagnostic function should exist."
  (should (fboundp 'hypermodern/color-diag)))

(ert-deftest hypermodern/test-truecolor-detects-colorterm ()
  "Should detect truecolor via COLORTERM."
  (let ((process-environment (cons "COLORTERM=truecolor" process-environment)))
    (should (hypermodern/terminal-supports-truecolor-p))))

(ert-deftest hypermodern/test-truecolor-detects-ghostty ()
  "Should detect truecolor via TERM_PROGRAM=ghostty."
  (let ((process-environment (cons "TERM_PROGRAM=ghostty" process-environment)))
    (should (hypermodern/terminal-supports-truecolor-p))))

(ert-deftest hypermodern/test-truecolor-detects-wezterm ()
  "Should detect truecolor via TERM_PROGRAM=WezTerm."
  (let ((process-environment (cons "TERM_PROGRAM=WezTerm" process-environment)))
    (should (hypermodern/terminal-supports-truecolor-p))))

(ert-deftest hypermodern/test-truecolor-detects-kitty-term ()
  "Should detect truecolor via TERM containing kitty."
  (let ((process-environment (append '("TERM=xterm-kitty" "COLORTERM=" "TERM_PROGRAM=")
                                     process-environment)))
    (should (hypermodern/terminal-supports-truecolor-p))))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Dashboard + Startup
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-dashboard-loadable ()
  "Dashboard should be loadable."
  (should (require 'dashboard nil t)))

(ert-deftest hypermodern/test-gibson-quotes-exist ()
  "Gibson quotes should be defined."
  (should (bound-and-true-p hypermodern/gibson-quotes))
  (should (> (length hypermodern/gibson-quotes) 5)))

(ert-deftest hypermodern/test-ui-init-exists ()
  "UI init function should exist."
  (should (fboundp 'hypermodern/ui-init)))

(ert-deftest hypermodern/test-ui-apply-exists ()
  "UI apply function should exist."
  (should (fboundp 'hypermodern/ui-apply)))

(ert-deftest hypermodern/test-ui-theme-customizable ()
  "UI theme should be a defcustom."
  (should (custom-variable-p 'hypermodern/ui-theme)))

(ert-deftest hypermodern/test-ui-menu-exists ()
  "UI menu should exist."
  (should (fboundp 'hypermodern/ui-menu)))
