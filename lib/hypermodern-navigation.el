;;; hypermodern-navigation.el --- Enhanced File Navigation -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'consult)
(require 'zoxide)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Integration with zoxide
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package zoxide
  :ensure t
  :demand t
  :config
  (add-hook 'find-file-hook #'zoxide-add)
  (add-hook 'dired-mode-hook #'zoxide-add)
  :bind (("C-c z z" . hypermodern/zoxide-jump)
          ("C-c z d" . hypermodern/dired-jump-zoxide)
          ("C-c z a" . zoxide-add)))

(defun hypermodern/zoxide-query (&optional query)
  "Query zoxide and return a list of directories."
  (let* ((q (shell-quote-argument (or query "")))
          (output (shell-command-to-string (format "zoxide query -l %s" q))))
    (when (and output (not (string-empty-p output)))
      (split-string output "\n" t))))

(defun hypermodern/zoxide-jump ()
  "Jump to a zoxide directory using consult."
  (interactive)
  (let ((dirs (hypermodern/zoxide-query)))
    (if dirs
      (let ((dir (consult--read dirs :prompt "Zoxide: " :require-match t)))
        (when dir
          (find-file dir)))
      (message "No zoxide entries yet. Navigate around first."))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Integration with consult-dir
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

(use-package consult-dir
  :ensure t
  :demand t
  :after consult
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Integration with fd
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/fd-project ()
  "Find file in the current project using fd."
  (interactive)
  (if-let ((project (project-current)))
    (let ((default-directory (project-root project)))
      (consult-fd))
    (consult-fd)))

(defun hypermodern/fd-here ()
  "Find a file from the current directory using fd."
  (interactive)
  (consult-fd default-directory))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Smart File Finding
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/find-file ()
  "Smartly find a file: project root if in project, else current dir.
With prefix argument, use consult-dir first to pick directory."
  (interactive)
  (call-interactively #'find-file))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Dired Integration
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/dired-jump-zoxide ()
  "Jump to a zoxide directory in dired."
  (interactive)
  (let ((dirs (hypermodern/zoxide-query)))
    (if dirs
      (let ((dir (consult--read dirs :prompt "Dired (zoxide): " :require-match t)))
        (when dir
          (dired dir)))
      (message "No zoxide entries yet."))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Unified Navigation Command
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/navigate ()
  "Unified entry point for all navigation methods."
  (interactive)
  (let ((choices '(("Find File" . hypermodern/find-file)
                    ("Jump to Zoxide Directory" . hypermodern/zoxide-jump)
                    ("Find Recent Files" . consult-recent-file)
                    ("Find in Current Directory" . hypermodern/fd-here)
                    ("Find in Project" . hypermodern/fd-project))))
    (let* ((selection (consult--read choices :prompt "Choose navigation method: ")))
      (when selection
        (funcall (cdr selection))))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Keybindings
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(global-set-key (kbd "C-c n") #'hypermodern/navigate)         ; Unified navigation command
(global-set-key (kbd "C-x C-f") #'hypermodern/find-file)      ; Smart find-file
(global-set-key (kbd "C-x F") #'hypermodern/fd-project)       ; fd from project root
(global-set-key (kbd "C-x f") #'hypermodern/fd-here)          ; fd from current dir
(global-set-key (kbd "C-x r") #'consult-recent-file)          ; Recent files

(provide 'hypermodern-navigation)
;;; hypermodern-navigation.el ends here
