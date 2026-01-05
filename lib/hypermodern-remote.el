;;; hypermodern-remote.el --- tramp black book -*- lexical-binding: t; -*-
;;
;; The TRAMP fixes nobody writes down because they're too useful.
;;
;; You should never have to:
;; - Set TERM=dumb on remotes
;; - Wonder why "Waiting for prompt" hangs forever
;; - Debug PS1 weirdness
;; - Manually clean up hung connections
;;

(require 'cl-lib)
(require 'tramp)

;; Forward declarations for optional mode functions
(declare-function flycheck-mode "flycheck" t)
(declare-function flymake-mode "flymake" t)
(declare-function lsp-mode "lsp-mode" t)

;; Forward declarations for TRAMP variables
(defvar tramp-default-method)
(defvar tramp-use-ssh-controlmaster-options)
(defvar tramp-ssh-controlmaster-options)
(defvar tramp-shell-prompt-pattern)
(defvar tramp-terminal-type)
(defvar tramp-encoding-shell)
(defvar tramp-remote-shell)
(defvar tramp-remote-shell-login)
(defvar tramp-remote-shell-args)
(defvar tramp-connection-timeout)
(defvar tramp-completion-reread-directory-timeout)
(defvar tramp-chunksize)
(defvar tramp-verbose)
(defvar tramp-use-connection-share)
(defvar tramp-inline-compress-start-size)
(defvar tramp-copy-size-limit)
(defvar tramp-completion-use-cache)
(defvar tramp-remote-path)
(defvar tramp-use-auth-sources)
(defvar tramp-persistency-file-name)
(defvar tramp-auto-save-directory)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // tramp // the black book
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; --- Connection method ---
(setq tramp-default-method "ssh")

;; --- SSH multiplexing ---
;; Reuse connections. This is the single biggest speedup.
;; Note: if you have ControlMaster in ~/.ssh/config, set this to nil
;; to avoid conflicts.
(setq tramp-use-ssh-controlmaster-options t)
(setq tramp-ssh-controlmaster-options
      (concat "-o ControlMaster=auto "
              "-o ControlPath=" (expand-file-name "tramp-%%r@%%h:%%p" temporary-file-directory) " "
              "-o ControlPersist=600 "
              "-o ServerAliveInterval=60 "
              "-o ServerAliveCountMax=3"))

;; --- The prompt detection fix ---
;; This is THE fix. TRAMP detects the shell prompt to know when commands
;; complete. Fancy prompts (starship, oh-my-zsh, powerline) break this.
;; The fix: set a known prompt on the remote, don't touch their shell config.
(setq tramp-shell-prompt-pattern
      "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[[:digit:];]*[[:alpha:]] *\\)*")

;; Even more aggressive: TRAMP sets its own prompt on connection
(setq tramp-terminal-type "dumb")  ; Tell remote we're dumb
(setq tramp-encoding-shell "/bin/sh")  ; Use POSIX sh, not their fancy shell

;; --- Remote shell initialization ---
;; Don't let their .bashrc/.zshrc mess us up
(setq tramp-remote-shell "/bin/sh")
(setq tramp-remote-shell-login nil)  ; Don't use login shell
(setq tramp-remote-shell-args '("-c"))

;; --- Starship/fancy prompt integration ---
;; If you use starship/oh-my-zsh/etc, add this to your shell rc files to
;; disable them when TRAMP connects:
;;
;; For bash (~/.bashrc):
;;   [[ $TERM == "dumb" ]] && unset PROMPT_COMMAND && PS1='$ ' && return
;;
;; For zsh (~/.zshrc):
;;   [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
;;
;; This detects TRAMP's TERM=dumb and bails before loading starship.
;; Put this EARLY in your rc file, before any prompt/plugin initialization.

;; But we need to work with remote environment too
(connection-local-set-profile-variables
 'tramp-connection-local-default-shell-profile
 '((shell-file-name . "/bin/sh")
   (shell-command-switch . "-c")))

;; --- Timeouts ---
;; Don't wait forever
(setq tramp-connection-timeout 10)
(setq tramp-completion-reread-directory-timeout 60)

;; --- Performance ---
(setq tramp-chunksize 500)           ; Bigger chunks for faster transfer
(setq tramp-verbose 1)               ; Shut up unless debugging (0-10)
(setq tramp-use-connection-share t)  ; Share connections across buffers

;; Inline small files (below 10KB), transfer large ones
(setq tramp-inline-compress-start-size 10000)
(setq tramp-copy-size-limit 10000)

;; Cache remote file properties aggressively
(setq remote-file-name-inhibit-cache nil)  ; Don't inhibit
(setq tramp-completion-use-cache t)

;; --- Remote PATH ---
;; Make sure we find binaries. TRAMP will search these.
(setq tramp-remote-path
      '(tramp-own-remote-path           ; Inherit PATH from remote shell
        "/run/current-system/sw/bin"    ; NixOS system
        "~/.nix-profile/bin"            ; Nix user profile
        "/home/linuxbrew/.linuxbrew/bin" ; Linuxbrew
        "~/.local/bin"                  ; Python, pipx, etc
        "~/.cargo/bin"                  ; Rust
        "/usr/local/bin"
        "/usr/bin"
        "/bin"))

;; --- VC and project detection ---
;; Don't try to version control remote files (slow)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Don't try to detect projects remotely (slow)
(setq project-vc-merge-submodules nil)

;; --- File locations ---
;; Use writable directory for tramp files
(let ((base-dir (if (file-writable-p user-emacs-directory)
                    user-emacs-directory
                  "~/.cache/emacs/")))
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" base-dir))
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave" base-dir))
  ;; Ensure directory exists in writable location
  (make-directory tramp-auto-save-directory t))

;; --- Backup handling ---
;; Don't backup remote files locally (security + speed)
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; --- Lock files ---
;; Don't create .#lockfiles on remote systems
(setq remote-file-name-inhibit-locks t)

;; --- Password caching ---
;; Use auth-source for passwords (integrates with authinfo.gpg)
(setq tramp-use-auth-sources t)
(setq password-cache-expiry 3600)  ; Cache passwords for 1 hour

;; --- Hooks for remote buffers ---
(defun hypermodern/tramp-setup-buffer ()
  "Setup for remote buffers - disable expensive modes."
  (when (file-remote-p default-directory)
    ;; Disable expensive modes
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))
    (when (bound-and-true-p lsp-mode)
      (lsp-mode -1))
    ;; Eglot can work remotely but often slow
    (setq-local eglot-stay-out-of '(flymake))
    ;; Disable auto-revert (expensive over network)
    (when (bound-and-true-p auto-revert-mode)
      (auto-revert-mode -1))))

(add-hook 'find-file-hook #'hypermodern/tramp-setup-buffer)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // tramp // debugging
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/tramp-debug-on ()
  "Enable TRAMP debugging."
  (interactive)
  (setq tramp-verbose 10)
  (message "TRAMP debug on. Check *tramp/...* buffers."))

(defun hypermodern/tramp-debug-off ()
  "Disable TRAMP debugging."
  (interactive)
  (setq tramp-verbose 1)
  (message "TRAMP debug off."))

(defun hypermodern/tramp-cleanup ()
  "Clean up ALL TRAMP connections and cache. Nuclear option."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers)
  ;; Also kill the connection buffers
  (dolist (buf (buffer-list))
    (when (string-prefix-p " *tramp" (buffer-name buf))
      (kill-buffer buf)))
  (message "TRAMP: all connections cleaned."))

(defun hypermodern/tramp-cleanup-host (host)
  "Clean up TRAMP connection to HOST."
  (interactive "sHost: ")
  (tramp-cleanup-connection
   (tramp-dissect-file-name (format "/ssh:%s:" host)))
  (message "TRAMP: cleaned connection to %s" host))

(defun hypermodern/tramp-status ()
  "Show TRAMP connection status."
  (interactive)
  (with-current-buffer (get-buffer-create "*tramp-status*")
    (erase-buffer)
    (insert "=== TRAMP Status ===\n\n")
    (insert (format "Default method: %s\n" tramp-default-method))
    (insert (format "Verbosity: %d\n" tramp-verbose))
    (insert (format "ControlMaster: %s\n" tramp-use-ssh-controlmaster-options))
    (insert "\n=== Active connections ===\n\n")
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (file-remote-p (buffer-file-name buf)))
        (insert (format "  %s\n" (buffer-file-name buf)))))
    (insert "\n=== Connection buffers ===\n\n")
    (dolist (buf (buffer-list))
      (when (string-prefix-p " *tramp" (buffer-name buf))
        (insert (format "  %s\n" (buffer-name buf)))))
    (pop-to-buffer (current-buffer))))

(defun hypermodern/tramp-starship-fix ()
  "Show shell config to fix starship/fancy prompts with TRAMP.
Copy these lines to the TOP of your shell rc files on remote hosts."
  (interactive)
  (with-current-buffer (get-buffer-create "*tramp-starship-fix*")
    (erase-buffer)
    (insert "=== Fix Starship/Fancy Prompts for TRAMP ===\n\n")
    (insert "Add ONE of these to the TOP of your shell rc file (before any prompt init):\n\n")
    (insert "--- For bash (~/.bashrc) ---\n")
    (insert "[[ $TERM == \"dumb\" ]] && unset PROMPT_COMMAND && PS1='$ ' && return\n\n")
    (insert "--- For zsh (~/.zshrc) ---\n")
    (insert "[[ $TERM == \"dumb\" ]] && unsetopt zle && PS1='$ ' && return\n\n")
    (insert "--- For fish (~/.config/fish/config.fish) ---\n")
    (insert "if test \"$TERM\" = \"dumb\"\n")
    (insert "    function fish_prompt; echo '$ '; end\n")
    (insert "    exit\n")
    (insert "end\n\n")
    (insert "--- Why this works ---\n")
    (insert "TRAMP sets TERM=dumb to signal \"I'm not a human, give me simple output\".\n")
    (insert "Detecting this and bailing early prevents starship/oh-my-zsh/etc from loading.\n")
    (insert "This fixes \"Waiting for prompt\" hangs and prompt detection failures.\n")
    (markdown-mode)
    (view-mode)
    (pop-to-buffer (current-buffer))))

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
            (let ((json (shell-command-to-string "tailscale status --json 2>/dev/null")))
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
      (if (fboundp 'vterm)
          (vterm (format "*vterm-%s*" host))
        (user-error "vterm is not available. Install vterm package to use remote shells")))))

(defun hypermodern/tailscale-dired ()
  "Open dired on a tailscale host home directory."
  (interactive)
  (let* ((hosts (hypermodern/tailscale-hosts))
         (names (mapcar #'car hosts))
         (host (completing-read "Tailscale host: " names nil t)))
    (dired (format "/ssh:%s:~/" host))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // ssh hosts // from ~/.ssh/config
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/ssh-hosts ()
  "Parse ~/.ssh/config for host names."
  (let ((config-file (expand-file-name "~/.ssh/config")))
    (when (file-exists-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (let (hosts)
          (while (re-search-forward "^Host[ \t]+\\([^*\n]+\\)" nil t)
            (let ((host (match-string 1)))
              (unless (string-match-p "\\*" host)  ; Skip wildcards
                (push (string-trim host) hosts))))
          (nreverse hosts))))))

(defun hypermodern/ssh-find-file ()
  "Open file on an SSH host from ~/.ssh/config."
  (interactive)
  (let* ((hosts (hypermodern/ssh-hosts))
         (host (completing-read "SSH host: " hosts nil t))
         (path (read-file-name
                (format "File on %s: " host)
                (format "/ssh:%s:" host))))
    (find-file path)))

(defun hypermodern/ssh-dired ()
  "Open dired on an SSH host from ~/.ssh/config."
  (interactive)
  (let* ((hosts (hypermodern/ssh-hosts))
         (host (completing-read "SSH host: " hosts nil t)))
    (dired (format "/ssh:%s:~/" host))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // sudo // local and remote
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/sudo-find-file (file)
  "Open FILE with sudo."
  (interactive "FFind file (sudo): ")
  (find-file (concat "/sudo::" (expand-file-name file))))

(defun hypermodern/sudo-this-file ()
  "Reopen current file with sudo."
  (interactive)
  (if buffer-file-name
      (let ((pos (point)))
        (find-alternate-file (concat "/sudo::" buffer-file-name))
        (goto-char pos))
    (message "Buffer is not visiting a file")))

(defun hypermodern/remote-sudo-this-file ()
  "Reopen current remote file with sudo on the remote host."
  (interactive)
  (if (and buffer-file-name (file-remote-p buffer-file-name))
      (let* ((pos (point))
             (vec (tramp-dissect-file-name buffer-file-name))
             (host (tramp-file-name-host vec))
             (user (tramp-file-name-user vec))
             (localname (tramp-file-name-localname vec))
             (sudo-path (format "/ssh:%s|sudo::%s"
                                (if user (format "%s@%s" user host) host)
                                localname)))
        (find-alternate-file sudo-path)
        (goto-char pos))
    (if buffer-file-name
        (hypermodern/sudo-this-file)
      (message "Buffer is not visiting a file"))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // keymaps // for binding in init.el
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/tailscale-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'hypermodern/tailscale-find-file)
    (define-key map (kbd "s") #'hypermodern/tailscale-shell)
    (define-key map (kbd "d") #'hypermodern/tailscale-dired)
    map)
  "Keymap for Tailscale commands.")

(defvar hypermodern/remote-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'hypermodern/ssh-find-file)
    (define-key map (kbd "d") #'hypermodern/ssh-dired)
    (define-key map (kbd "c") #'hypermodern/tramp-cleanup)
    (define-key map (kbd "s") #'hypermodern/tramp-status)
    (define-key map (kbd "?") #'hypermodern/tramp-starship-fix)
    (define-key map (kbd "u") #'hypermodern/sudo-this-file)
    (define-key map (kbd "U") #'hypermodern/remote-sudo-this-file)
    map)
  "Keymap for remote/TRAMP commands.")

(provide 'hypermodern-remote)
;;; hypermodern-remote.el ends here
