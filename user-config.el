;;; user-config.el --- Your personal configuration -*- lexical-binding: t -*-
;;
;; This is YOUR file. Put all your personal config here.
;; init.el loads this at the end, so you can override anything.
;;

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // appearance // current defaults shown, uncomment to change
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Theme - options: base16-ono-sendai-tuned (dark), base16-maas-neoform (light)
(setq hypermodern/ui-theme 'base16-ono-sendai-razorgirl)

;; Font - options: 'auto, 'berkeley, 'iosevka, 'jetbrains, 'fira, 'source-code-pro,
;;                 'ibm-plex, 'cascadia, 'hack, 'victor-mono, 'inconsolata, 'system
(setq hypermodern/ui-font-preset 'ibm-plex)

;; Density - options: 'comfortable, 'tight, 'compact
(setq hypermodern/ui-density 'tight)

;; Signal (mode-line visibility) - options: 'loud, 'normal, 'minimal, 'quiet
(setq hypermodern/ui-signal 'loud)

;; Glow (syntax highlighting intensity) - options: 'bright, 'subtle, 'dim, 'off
(setq hypermodern/ui-glow-level 'bright)

;; Pulse animation when switching windows - options: t, nil
(setq hypermodern/ui-enable-pulse t)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // keybindings
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Current defaults (uncomment and change as needed):
;;
;; Window movement:
;;   M-N     → windmove-right
;;   M-P     → windmove-left
;;   M-R     → hypermodern/rotate-windows
;;   M-o     → ace-window (with overlay)
;;
;; Format & save:
;;   M-z     → hypermodern/format-buffer
;;
;; Leader (C-c):
;;   C-c h   → dashboard-open
;;   C-c g   → magit-status
;;   C-c s   → deadgrep
;;
;; Add your own:
;; (global-set-key (kbd "C-c x") #'my-function)
;; (define-key hypermodern/leader-map (kbd "x") #'my-function)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // package settings
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Current defaults (change as needed):

;; Company (completion)
;; (setq company-idle-delay 0.2)              ; Default: 0.2
;; (setq company-minimum-prefix-length 2)     ; Default: 2

;; Magit
;; (setq magit-display-buffer-function
;;       #'magit-display-buffer-fullframe-status-v1)

;; Vertico (completion UI)
;; (setq vertico-count 20)                    ; Default: from vertico

;; Project
;; (setq project-switch-commands              ; Default project switch menu
;;       '((project-find-file "Find file")
;;         (project-dired "Dired")
;;         (consult-ripgrep "Ripgrep")))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // personal functions
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Add your own functions here
;; Example:
;;
;; (defun my-insert-date ()
;;   "Insert current date."
;;   (interactive)
;;   (insert (format-time-string "%Y-%m-%d")))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // hooks
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Add your personal hooks here
;; Examples:
;;
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

(provide 'user-config)
;;; user-config.el ends here
