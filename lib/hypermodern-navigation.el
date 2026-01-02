;;; hypermodern-navigation.el --- file navigation discipline -*- lexical-binding: t; -*-
;;
;; The strategy:
;; - zoxide: frecency-based directory jumping (your shell habits, in emacs)
;; - consult-dir: unified directory switching (bookmarks + projects + recentf + zoxide)
;; - consult-fd: find files fast
;; - project.el: project-aware everything
;;
;; No projectile. project.el is built-in and works.
;;

(require 'cl-lib)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // zoxide // frecency-based directory jumping
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; zoxide.el from MELPA/sourcehut
(use-package zoxide
  :demand t
  :config
  ;; Add directories to zoxide when we visit them
  (add-hook 'find-file-hook #'zoxide-add)
  (add-hook 'dired-mode-hook #'zoxide-add)
  
  :bind (("C-c z z" . zoxide-find-file)
         ("C-c z d" . zoxide-cd)
         ("C-c z a" . zoxide-add)
         ("C-c z q" . zoxide-query)))

;; Custom zoxide integration with consult
(defun hypermodern/zoxide-query (&optional query)
  "Query zoxide and return list of directories."
  (let* ((q (or query ""))
         (output (shell-command-to-string (format "zoxide query -l %s" q))))
    (when (and output (not (string-empty-p output)))
      (split-string output "\n" t))))

(defun hypermodern/zoxide-jump ()
  "Jump to a zoxide directory using consult."
  (interactive)
  (let ((dirs (hypermodern/zoxide-query)))
    (if dirs
        (let ((dir (consult--read dirs
                                  :prompt "Zoxide: "
                                  :category 'file
                                  :sort nil  ; zoxide already sorted by frecency
                                  :require-match t)))
          (when dir
            (find-file dir)))
      (message "No zoxide entries yet. Navigate around first."))))

(defun hypermodern/zoxide-find-file ()
  "Jump to a zoxide directory, then find file within it."
  (interactive)
  (let ((dirs (hypermodern/zoxide-query)))
    (if dirs
        (let ((dir (consult--read dirs
                                  :prompt "Zoxide → find file: "
                                  :category 'file
                                  :sort nil
                                  :require-match t)))
          (when dir
            (let ((default-directory (file-name-as-directory dir)))
              (call-interactively #'find-file))))
      (message "No zoxide entries yet. Navigate around first."))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // consult-dir // unified directory switching
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(use-package consult-dir
  :demand t
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  
  :config
  ;; Custom zoxide source for consult-dir
  (defvar consult-dir--source-zoxide
    `(:name "Zoxide"
      :narrow ?z
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,(lambda () (executable-find "zoxide"))
      :items ,#'hypermodern/zoxide-query)
    "Zoxide directory source for `consult-dir'.")
  
  ;; Add zoxide to sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t)
  
  ;; Also add TRAMP ssh hosts if you use them
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // fd // fast find
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; consult-fd is already in consult, just wire it up nicely
(defun hypermodern/fd-project ()
  "Find file in current project using fd."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (consult-fd))
    (consult-fd)))

(defun hypermodern/fd-here ()
  "Find file from current directory using fd."
  (interactive)
  (consult-fd default-directory))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // unified find-file
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/find-file ()
  "Smart find-file: project root if in project, else current dir.
With prefix arg, use consult-dir first to pick directory."
  (interactive)
  (if current-prefix-arg
      ;; C-u: pick directory first via consult-dir
      (let ((consult-dir-shadow-filenames nil))
        (consult-dir)
        (call-interactively #'find-file))
    ;; Normal: project-aware find
    (if-let ((project (project-current)))
        (project-find-file)
      (call-interactively #'find-file))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // dired integration
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/dired-jump-zoxide ()
  "Jump to a zoxide directory in dired."
  (interactive)
  (let ((dirs (hypermodern/zoxide-query)))
    (if dirs
        (let ((dir (consult--read dirs
                                  :prompt "Dired (zoxide): "
                                  :category 'file
                                  :sort nil
                                  :require-match t)))
          (when dir
            (dired dir)))
      (message "No zoxide entries yet."))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // keybindings
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; File finding
(global-set-key (kbd "C-x C-f") #'hypermodern/find-file)  ; smart find-file
(global-set-key (kbd "C-x f") #'consult-fd)               ; fd from here
(global-set-key (kbd "C-x F") #'hypermodern/fd-project)   ; fd from project root

;; Zoxide
(global-set-key (kbd "C-c z") nil)  ; clear prefix
(global-set-key (kbd "C-c z z") #'hypermodern/zoxide-jump)
(global-set-key (kbd "C-c z f") #'hypermodern/zoxide-find-file)
(global-set-key (kbd "C-c z d") #'hypermodern/dired-jump-zoxide)

;; Recent files
(global-set-key (kbd "C-x C-r") #'consult-recent-file)

(provide 'hypermodern-navigation)
;;; hypermodern-navigation.el ends here
