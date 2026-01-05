;;; hypermodern-core.el --- foundation functions -*- lexical-binding: t; -*-
;;
;; Core functions and settings. No use-package here - that's init.el's job.
;;

(require 'cl-lib)

;; Silence byte-compiler warnings for external variables
(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-jit-compilation)
(defvar native-comp-speed)
(defvar recentf-save-file)
(defvar recentf-max-saved-items)
(defvar recentf-exclude)
(defvar savehist-file)
(defvar savehist-additional-variables)
(defvar undo-fu-session-directory)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // quick wins // mode enables
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/core-init ()
  "Initialize core modes and settings."
  ;; Smooth scrolling (emacs 29+, good on pgtk)
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))

  ;; Repeat mode - C-x o o o instead of C-x o C-x o C-x o
  (repeat-mode 1)

  ;; Handle minified JS and long lines without freezing
  (global-so-long-mode 1)

  ;; Native comp - don't spam warnings
  (when (featurep 'native-compile)
    (setq native-comp-async-report-warnings-errors 'silent)
    (setq native-comp-jit-compilation t)
    (setq native-comp-speed 2)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // project // root markers
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/project-root-markers
  '(".project" ".projectile" "flake.nix" "BUILD" "WORKSPACE"
    "MODULE.bazel" "Cargo.toml" "package.json" "pyproject.toml"
    "setup.py" "go.mod" "*.cabal" "dune-project")
  "Files that mark a directory as a project root.")

(defun hypermodern/project-setup ()
  "Configure project.el with our root markers."
  (setq project-vc-extra-root-markers hypermodern/project-root-markers)
  (setq project-list-file
        (expand-file-name "projects" user-emacs-directory)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // compile // project-aware
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/compile ()
  "Compile from project root."
  (interactive)
  (let ((default-directory
         (if-let ((proj (project-current)))
             (project-root proj)
           default-directory)))
    (call-interactively #'compile)))

(defun hypermodern/compile-setup ()
  "Configure compilation buffer behavior."
  (setq compilation-scroll-output 'first-error)
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t)
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // files // cache dir
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/cache-dir
  (expand-file-name (if (file-writable-p user-emacs-directory)
                        user-emacs-directory
                      "~/.cache/emacs/"))
  "Directory for cached files (auto-save, backup, etc).")

(defun hypermodern/files-setup ()
  "Configure auto-save, backup, recentf, savehist locations."
  (make-directory hypermodern/cache-dir t)
  
  ;; Auto-save
  (let ((auto-save-dir (expand-file-name "auto-save/" hypermodern/cache-dir)))
    (make-directory auto-save-dir t)
    (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))
  
  ;; Backups
  (let ((backup-dir (expand-file-name "backups/" hypermodern/cache-dir)))
    (make-directory backup-dir t)
    (setq backup-directory-alist `(("." . ,backup-dir))))
  
  ;; Recentf
  (setq recentf-save-file (expand-file-name "recentf" hypermodern/cache-dir))
  (setq recentf-max-saved-items 500)
  (add-to-list 'recentf-exclude "^/tmp/")
  (add-to-list 'recentf-exclude "^/ssh:")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  
  ;; Savehist
  (setq savehist-file (expand-file-name "history" hypermodern/cache-dir))
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring compile-command))
  
  ;; Undo-fu-session (if available)
  (setq undo-fu-session-directory
        (expand-file-name "undo-fu-session/" hypermodern/cache-dir)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // leader // key prefix
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/ops-leader-map (make-sparse-keymap)
  "Keymap for hypermodern operations (C-c o).")

(defun hypermodern/leader (&rest bindings)
  "Bind keys in `hypermodern/ops-leader-map'.
BINDINGS is a plist of (KEY . COMMAND) pairs."
  (while bindings
    (let ((key (pop bindings))
          (cmd (pop bindings)))
      (define-key hypermodern/ops-leader-map (kbd key) cmd))))

(defun hypermodern/leader-setup ()
  "Set up the C-c o leader key."
  (global-set-key (kbd "C-c o") hypermodern/ops-leader-map))

(provide 'hypermodern-core)
;;; hypermodern-core.el ends here
