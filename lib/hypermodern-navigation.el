;;; hypermodern-navigation.el --- Enhanced File Navigation -*- lexical-binding: t; -*-
;;
;; Zoxide, consult-dir, fd integration. No use-package here.
;;

(require 'cl-lib)

;; Silence byte-compiler warnings for external variables
(defvar consult-dir-sources)

;; Silence byte-compiler warnings for external functions
(declare-function consult--read "consult" (candidates &rest options))
(declare-function consult-recent-file "consult" ())
(declare-function consult-fd "consult" (&optional dir initial))
(declare-function zoxide-add "zoxide" ())

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // zoxide // functions
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/zoxide-query (&optional query)
  "Query zoxide and return a list of directories."
  (let* ((q (shell-quote-argument (or query "")))
         (output (shell-command-to-string (format "zoxide query -l %s" q))))
    (when (and output (not (string-empty-p output)))
      (split-string output "\n" t))))

(defun hypermodern/zoxide-jump ()
  "Jump to a zoxide directory using consult."
  (interactive)
  (require 'consult)
  (let ((dirs (hypermodern/zoxide-query)))
    (if dirs
        (let ((dir (consult--read dirs :prompt "Zoxide: " :require-match t)))
          (when dir
            (find-file dir)))
      (message "No zoxide entries yet. Navigate around first."))))

(defun hypermodern/zoxide-setup ()
  "Configure zoxide. Call from (use-package zoxide :config ...)."
  (add-hook 'find-file-hook #'zoxide-add)
  (add-hook 'dired-mode-hook #'zoxide-add))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // consult-dir // zoxide source
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar consult-dir--source-zoxide
  `(:name "Zoxide"
    :narrow ?z
    :category file
    :face consult-file
    :history file-name-history
    :enabled ,(lambda () (executable-find "zoxide"))
    :items ,#'hypermodern/zoxide-query)
  "Zoxide directory source for `consult-dir'.")

(defun hypermodern/consult-dir-setup ()
  "Configure consult-dir. Call from (use-package consult-dir :config ...)."
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // fd // functions
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/fd-project ()
  "Find file in the current project using fd."
  (interactive)
  (require 'consult)
  (require 'project)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (consult-fd))
    (consult-fd)))

(defun hypermodern/fd-here ()
  "Find a file from the current directory using fd."
  (interactive)
  (require 'consult)
  (consult-fd default-directory))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // smart find
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/find-file ()
  "Smartly find a file."
  (interactive)
  (call-interactively #'find-file))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // dired
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/dired-jump-zoxide ()
  "Jump to a zoxide directory in dired."
  (interactive)
  (require 'consult)
  (let ((dirs (hypermodern/zoxide-query)))
    (if dirs
        (let ((dir (consult--read dirs :prompt "Dired (zoxide): " :require-match t)))
          (when dir
            (dired dir)))
      (message "No zoxide entries yet."))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // unified
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/navigate ()
  "Unified entry point for all navigation methods."
  (interactive)
  (require 'consult)
  (let ((choices '(("Find File" . hypermodern/find-file)
                   ("Jump to Zoxide Directory" . hypermodern/zoxide-jump)
                   ("Find Recent Files" . consult-recent-file)
                   ("Find in Current Directory" . hypermodern/fd-here)
                   ("Find in Project" . hypermodern/fd-project))))
    (let* ((selection (consult--read choices :prompt "Choose navigation method: ")))
      (when selection
        (funcall (cdr selection))))))

(provide 'hypermodern-navigation)
;;; hypermodern-navigation.el ends here
