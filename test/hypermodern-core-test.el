;;; hypermodern-core-test.el --- Tests for hypermodern-core -*- lexical-binding: t; -*-

(require 'ert)
(require 'hypermodern-core)

;;; GC and Performance

(ert-deftest hypermodern/test-gc-threshold-set ()
  "GC threshold should be elevated for performance."
  (should (>= gc-cons-threshold (* 64 1024 1024))))

(ert-deftest hypermodern/test-gcmh-loaded ()
  "GCMH should be active."
  (should (featurep 'gcmh))
  (should (bound-and-true-p gcmh-mode)))

;;; Project.el

(ert-deftest hypermodern/test-project-find-functions ()
  "Project detection should include vc backend."
  (should (memq 'project-try-vc project-find-functions)))

(ert-deftest hypermodern/test-project-root-markers ()
  "Should recognize common project markers."
  (let ((markers '("Cargo.toml" "package.json" "flake.nix"
                   "BUILD" "WORKSPACE" "pyproject.toml")))
    (dolist (marker markers)
      (should (member marker project-vc-extra-root-markers)))))

;;; Undo

(ert-deftest hypermodern/test-undo-fu-available ()
  "undo-fu should be loadable."
  (should (require 'undo-fu nil t)))

;;; Pixel Scroll

(ert-deftest hypermodern/test-pixel-scroll-available ()
  "Pixel scroll should be available in GUI."
  (when (display-graphic-p)
    (should (fboundp 'pixel-scroll-precision-mode))))

;;; Repeat Mode

(ert-deftest hypermodern/test-repeat-mode ()
  "Repeat mode should be active."
  (should (bound-and-true-p repeat-mode)))

;;; wgrep

(ert-deftest hypermodern/test-wgrep-available ()
  "wgrep should be loadable."
  (should (require 'wgrep nil t)))

(provide 'hypermodern-core-test)
;;; hypermodern-core-test.el ends here
