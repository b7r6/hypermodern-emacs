;;; hypermodern-remote.el --- tramp and tailscale -*- lexical-binding: t; -*-
;;
;; Edit bashrc on a machine you're not connected to? This is how.
;; For secrets management, see hypermodern-secrets.el
;;

(require 'cl-lib)
(require 'tramp)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // tramp // make it not suck
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Speed
(setq tramp-default-method "ssh")
(setq tramp-use-ssh-controlmaster-options t)
(setq tramp-ssh-controlmaster-options
      (concat "-o ControlMaster=auto "
              "-o ControlPath=/tmp/ssh-%%r@%%h:%%p "
              "-o ControlPersist=600"))

;; Don't litter
(setq tramp-persistency-file-name
      (expand-file-name "tramp" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave" user-emacs-directory))

;; Faster file detection
(setq tramp-chunksize 500)
(setq tramp-verbose 1)  ; shut up unless debugging

;; Remote path - make sure we find binaries
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path "/home/linuxbrew/.linuxbrew/bin")
(add-to-list 'tramp-remote-path "~/.local/bin")
(add-to-list 'tramp-remote-path "~/.nix-profile/bin")

;; Don't version control remote files
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // tailscale // integration
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/tailscale-cache nil
  "Cached tailscale status.")

(defvar hypermodern/tailscale-cache-time nil
  "When we last refreshed tailscale cache.")

(defun hypermodern/tailscale-hosts ()
  "Get list of tailscale hosts, cached for 60 seconds."
  (when (or (null hypermodern/tailscale-cache)
            (null hypermodern/tailscale-cache-time)
            (> (- (float-time) hypermodern/tailscale-cache-time) 60))
    (setq hypermodern/tailscale-cache
          (ignore-errors
            (let ((json (shell-command-to-string "tailscale status --json")))
              (when (and json (not (string-empty-p json)))
                (let* ((data (json-parse-string json :object-type 'alist))
                       (peers (alist-get 'Peer data))
                       (self (alist-get 'Self data)))
                  (append
                   ;; Self
                   (when self
                     (list (cons (alist-get 'HostName self)
                                 (alist-get 'DNSName self))))
                   ;; Peers
                   (mapcar (lambda (peer)
                             (cons (alist-get 'HostName (cdr peer))
                                   (alist-get 'DNSName (cdr peer))))
                           peers)))))))
    (setq hypermodern/tailscale-cache-time (float-time)))
  hypermodern/tailscale-cache)

(defun hypermodern/tailscale-find-file ()
  "Open file on a tailscale host."
  (interactive)
  (let* ((hosts (hypermodern/tailscale-hosts))
         (names (mapcar #'car hosts))
         (host (completing-read "Tailscale host: " names nil t))
         (dns (cdr (assoc host hosts)))
         ;; Use the short name, tailscale MagicDNS handles it
         (path (read-file-name
                (format "File on %s: " host)
                (format "/ssh:%s:" host))))
    (find-file path)))

(defun hypermodern/tailscale-shell ()
  "Open vterm on a tailscale host."
  (interactive)
  (let* ((hosts (hypermodern/tailscale-hosts))
         (names (mapcar #'car hosts))
         (host (completing-read "Tailscale host: " names nil t)))
    (let ((default-directory (format "/ssh:%s:" host)))
      (vterm (format "*vterm-%s*" host)))))

(defun hypermodern/tailscale-dired ()
  "Open dired on a tailscale host home directory."
  (interactive)
  (let* ((hosts (hypermodern/tailscale-hosts))
         (names (mapcar #'car hosts))
         (host (completing-read "Tailscale host: " names nil t)))
    (dired (format "/ssh:%s:~/" host))))

;; Quick bindings
(global-set-key (kbd "C-c t f") #'hypermodern/tailscale-find-file)
(global-set-key (kbd "C-c t s") #'hypermodern/tailscale-shell)
(global-set-key (kbd "C-c t d") #'hypermodern/tailscale-dired)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // tramp // unknown remotes
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; The "edit bashrc on a machine I'm not connected to" workflow
(defun hypermodern/remote-find-file (host &optional file)
  "Open FILE on HOST, prompting for both if needed.
Works with any SSH-accessible host, not just tailscale."
  (interactive
   (list (read-string "Host (user@host or host): ")
         nil))
  (let* ((path (format "/ssh:%s:" host))
         (file (or file
                   (read-file-name "File: " path))))
    (find-file file)))

(defun hypermodern/remote-bashrc (host)
  "Edit .bashrc on HOST."
  (interactive "sHost: ")
  (find-file (format "/ssh:%s:~/.bashrc" host)))

(defun hypermodern/remote-ssh-config ()
  "Edit local SSH config."
  (interactive)
  (find-file "~/.ssh/config"))

(provide 'hypermodern-remote)
;;; hypermodern-remote.el ends here
