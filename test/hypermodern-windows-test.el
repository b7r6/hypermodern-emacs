;;; hypermodern-windows-test.el --- Tests for window discipline -*- lexical-binding: t; -*-

(require 'ert)
(require 'hypermodern-windows)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Core settings - the law
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-no-auto-split-height ()
  "split-height-threshold should be nil (no auto splits)."
  (should (null split-height-threshold)))

(ert-deftest hypermodern/test-no-auto-split-width ()
  "split-width-threshold should be nil (no auto splits)."
  (should (null split-width-threshold)))

(ert-deftest hypermodern/test-no-popup-windows ()
  "pop-up-windows should be nil."
  (should (null pop-up-windows)))

(ert-deftest hypermodern/test-no-popup-frames ()
  "pop-up-frames should be nil."
  (should (null pop-up-frames)))

(ert-deftest hypermodern/test-no-even-windows ()
  "even-window-sizes should be nil (no auto resize)."
  (should (null even-window-sizes)))

(ert-deftest hypermodern/test-no-window-combination-resize ()
  "window-combination-resize should be nil."
  (should (null window-combination-resize)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Shackle
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-shackle-loadable ()
  "Shackle should be loadable."
  (should (require 'shackle nil t)))

(ert-deftest hypermodern/test-shackle-mode-active ()
  "Shackle mode should be active."
  (skip-unless (not noninteractive))
  (should (bound-and-true-p shackle-mode)))

(ert-deftest hypermodern/test-shackle-rules-configured ()
  "Shackle should have rules configured."
  (require 'shackle nil t)
  (should (bound-and-true-p shackle-rules))
  (should (> (length shackle-rules) 10)))

(ert-deftest hypermodern/test-shackle-default-same-window ()
  "Shackle default should be same window."
  (require 'shackle nil t)
  (should (bound-and-true-p shackle-default-rule))
  (should (plist-get shackle-default-rule :same)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Popper
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-popper-loadable ()
  "Popper should be loadable."
  (should (require 'popper nil t)))

(ert-deftest hypermodern/test-popper-mode-active ()
  "Popper mode should be active."
  (skip-unless (not noninteractive))
  (should (bound-and-true-p popper-mode)))

(ert-deftest hypermodern/test-popper-reference-buffers-configured ()
  "Popper should have reference buffers configured."
  (require 'popper nil t)
  (should (bound-and-true-p popper-reference-buffers))
  (should (> (length popper-reference-buffers) 15)))

(ert-deftest hypermodern/test-popper-has-messages ()
  "Popper should track *Messages*."
  (require 'popper nil t)
  (should (member "\\*Messages\\*" popper-reference-buffers)))

(ert-deftest hypermodern/test-popper-has-compilation ()
  "Popper should track compilation."
  (require 'popper nil t)
  (should (or (member "\\*compilation\\*" popper-reference-buffers)
              (member 'compilation-mode popper-reference-buffers))))

(ert-deftest hypermodern/test-popper-has-help ()
  "Popper should track help."
  (require 'popper nil t)
  (should (or (member "\\*Help\\*" popper-reference-buffers)
              (member 'help-mode popper-reference-buffers))))

(ert-deftest hypermodern/test-popper-delegates-to-shackle ()
  "Popper should delegate display to shackle."
  (require 'popper nil t)
  (should (null popper-display-control)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Functions exist
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-dedicate-window-exists ()
  "Dedicate window function should exist."
  (should (fboundp 'hypermodern/dedicate-window)))

(ert-deftest hypermodern/test-split-right-exists ()
  "Split right function should exist."
  (should (fboundp 'hypermodern/split-right)))

(ert-deftest hypermodern/test-split-below-exists ()
  "Split below function should exist."
  (should (fboundp 'hypermodern/split-below)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Winner mode
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-winner-mode-active ()
  "Winner mode should be active for undo."
  (should (bound-and-true-p winner-mode)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Keybindings
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern/test-popper-toggle-keybinding ()
  "C-\\ should toggle popup."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-\\")) 'popper-toggle)))

(ert-deftest hypermodern/test-popper-cycle-keybinding ()
  "M-\\ should cycle popup."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "M-\\")) 'popper-cycle)))

(ert-deftest hypermodern/test-split-right-keybinding ()
  "C-x 3 should split right."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-x 3")) 'hypermodern/split-right)))

(ert-deftest hypermodern/test-split-below-keybinding ()
  "C-x 2 should split below."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-x 2")) 'hypermodern/split-below)))

(ert-deftest hypermodern/test-winner-undo-keybinding ()
  "C-c w u should undo window changes."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-c w u")) 'winner-undo)))

(ert-deftest hypermodern/test-dedicate-keybinding ()
  "C-c w d should dedicate window."
  (skip-unless (not noninteractive))
  (should (eq (key-binding (kbd "C-c w d")) 'hypermodern/dedicate-window)))

(provide 'hypermodern-windows-test)
;;; hypermodern-windows-test.el ends here
