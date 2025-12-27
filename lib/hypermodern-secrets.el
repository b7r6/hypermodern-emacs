;;; hypermodern-secrets.el --- secrets across contexts -*- lexical-binding: t; -*-
;;
;; Agenix-native secrets management with multi-context support.
;; Because home/work is for people with one job.
;;

(require 'cl-lib)
(require 'auth-source)

(defgroup hypermodern-secrets nil
  "Multi-context secrets management."
  :group 'hypermodern)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // contexts // the core abstraction
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defcustom hypermodern/secrets-contexts
  '((personal
     :name "Personal"
     :authinfo "~/.authinfo.gpg"
     :agenix-secrets "/run/agenix"
     :agenix-source "~/.config/agenix"
     :gpg-key nil)  ; uses default

    (weyl
     :name "Weyl AI"
     :authinfo "~/weyl/.authinfo.gpg"
     :agenix-secrets "/run/agenix"
     :agenix-source "~/weyl/secrets"
     :gpg-key nil)

    ;; Add your contexts here
    ;; (client-foo
    ;;  :name "Client Foo"
    ;;  :authinfo "~/clients/foo/.authinfo.gpg"
    ;;  :agenix-secrets "/run/agenix/foo"
    ;;  :agenix-source "~/clients/foo/secrets"
    ;;  :gpg-key "ABCD1234")
    )
  "Alist of secret contexts.

Each entry is (SYMBOL . PLIST) where PLIST contains:
  :name           Human-readable name
  :authinfo       Path to authinfo.gpg for this context
  :agenix-secrets Path where agenix decrypts secrets at runtime
  :agenix-source  Path to .age source files (for editing)
  :gpg-key        GPG key ID for this context (nil = default)"
  :type '(alist :key-type symbol
                :value-type (plist :key-type keyword :value-type string)))

(defvar hypermodern/secrets-current-context 'personal
  "Current active secrets context.")

(defun hypermodern/secrets-context-get (key &optional context)
  "Get KEY from CONTEXT (default: current context)."
  (let* ((ctx (or context hypermodern/secrets-current-context))
         (plist (alist-get ctx hypermodern/secrets-contexts)))
    (plist-get plist key)))

(defun hypermodern/secrets-switch-context (context)
  "Switch to secrets CONTEXT."
  (interactive
   (list (intern
          (completing-read
           "Context: "
           (mapcar (lambda (c)
                     (cons (plist-get (cdr c) :name) (car c)))
                   hypermodern/secrets-contexts)
           nil t nil nil
           (symbol-name hypermodern/secrets-current-context)))))
  (setq hypermodern/secrets-current-context context)

  ;; Update auth-sources
  (let ((authinfo (hypermodern/secrets-context-get :authinfo)))
    (setq auth-sources
          (list authinfo
                "~/.authinfo"
                "~/.netrc")))

  ;; Clear cached auth
  (auth-source-forget-all-cached)

  (message "Secrets context: %s" (hypermodern/secrets-context-get :name)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // agenix // the real integration
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Agenix workflow:
;; 1. Source files live in :agenix-source (e.g., ~/weyl/secrets/*.age)
;; 2. NixOS decrypts them to :agenix-secrets (e.g., /run/agenix/*)
;; 3. We read from decrypted location at runtime
;; 4. We edit via `agenix -e` which handles decrypt/edit/re-encrypt

(defun hypermodern/agenix-secrets-dir (&optional context)
  "Get agenix runtime secrets directory for CONTEXT."
  (expand-file-name (hypermodern/secrets-context-get :agenix-secrets context)))

(defun hypermodern/agenix-source-dir (&optional context)
  "Get agenix source directory for CONTEXT."
  (expand-file-name (hypermodern/secrets-context-get :agenix-source context)))

(defun hypermodern/agenix-list-secrets (&optional context)
  "List available agenix secrets for CONTEXT."
  (let ((dir (hypermodern/agenix-secrets-dir context)))
    (when (file-directory-p dir)
      (directory-files dir nil "^[^.]"))))

(defun hypermodern/agenix-list-source-files (&optional context)
  "List agenix source .age files for CONTEXT."
  (let ((dir (hypermodern/agenix-source-dir context)))
    (when (file-directory-p dir)
      (directory-files dir nil "\\.age$"))))

(defun hypermodern/agenix-read-secret (name &optional context)
  "Read decrypted agenix secret NAME for CONTEXT.
Returns the secret value as a string, or nil if not found."
  (let ((file (expand-file-name name (hypermodern/agenix-secrets-dir context))))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (string-trim (buffer-string))))))

(defun hypermodern/agenix-edit (secret &optional context)
  "Edit agenix SECRET for CONTEXT.
Uses `agenix -e` which decrypts, opens editor, re-encrypts."
  (interactive
   (let* ((ctx hypermodern/secrets-current-context)
          (files (hypermodern/agenix-list-source-files ctx))
          (choice (completing-read
                   (format "Edit secret (%s): "
                           (hypermodern/secrets-context-get :name ctx))
                   files nil t)))
     (list choice ctx)))
  (let* ((ctx (or context hypermodern/secrets-current-context))
         (source-dir (hypermodern/agenix-source-dir ctx))
         (default-directory source-dir))
    ;; agenix -e decrypts to temp, opens $EDITOR, re-encrypts on save
    ;; We set EDITOR to emacsclient so it opens in current emacs
    (let ((process-environment
           (cons "EDITOR=emacsclient -c" process-environment)))
      (start-process "agenix-edit" nil "agenix" "-e" secret))))

(defun hypermodern/agenix-create (name &optional context)
  "Create new agenix secret NAME for CONTEXT."
  (interactive
   (list (read-string "Secret name (without .age): ")
         hypermodern/secrets-current-context))
  (let* ((ctx (or context hypermodern/secrets-current-context))
         (source-dir (hypermodern/agenix-source-dir ctx))
         (default-directory source-dir)
         (filename (concat name ".age")))
    ;; First time creation - agenix -e will create the file
    (let ((process-environment
           (cons "EDITOR=emacsclient -c" process-environment)))
      (start-process "agenix-create" nil "agenix" "-e" filename))))

(defun hypermodern/agenix-rekey (&optional context)
  "Rekey all secrets for CONTEXT (after changing keys in secrets.nix)."
  (interactive)
  (let* ((ctx (or context hypermodern/secrets-current-context))
         (source-dir (hypermodern/agenix-source-dir ctx))
         (default-directory source-dir))
    (compile "agenix -r")))

(defun hypermodern/agenix-browse (&optional context)
  "Browse decrypted secrets directory for CONTEXT."
  (interactive)
  (let ((dir (hypermodern/agenix-secrets-dir
              (or context hypermodern/secrets-current-context))))
    (if (file-directory-p dir)
        (dired dir)
      (message "Secrets directory does not exist: %s" dir))))

(defun hypermodern/agenix-browse-source (&optional context)
  "Browse source .age files for CONTEXT."
  (interactive)
  (let ((dir (hypermodern/agenix-source-dir
              (or context hypermodern/secrets-current-context))))
    (if (file-directory-p dir)
        (dired dir)
      (message "Source directory does not exist: %s" dir))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // auth-source // agenix backend
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; This lets you use agenix secrets via the standard auth-source API.
;; Secret file naming convention:
;;   /run/agenix/openrouter-api-key     -> host: openrouter.ai
;;   /run/agenix/github-token           -> host: github.com
;;   /run/agenix/aws-access-key-id      -> host: aws, user: access-key-id
;;
;; Or structured:
;;   /run/agenix/api/openrouter         -> host: openrouter.ai
;;   /run/agenix/tokens/github          -> host: github.com

(defun hypermodern/agenix-secret-names (host)
  "Generate possible secret names for HOST."
  (list
   ;; Direct: openrouter-api-key, github-token
   (format "%s-api-key" (replace-regexp-in-string "\\..*" "" host))
   (format "%s-token" (replace-regexp-in-string "\\..*" "" host))
   (format "%s-password" (replace-regexp-in-string "\\..*" "" host))
   ;; Just the hostname part
   (replace-regexp-in-string "\\..*" "" host)
   ;; Full hostname
   host
   ;; Subdirs
   (format "api/%s" (replace-regexp-in-string "\\..*" "" host))
   (format "tokens/%s" (replace-regexp-in-string "\\..*" "" host))))

(defun hypermodern/agenix-auth-source-search (&rest spec)
  "Search agenix secrets matching SPEC.
SPEC is a plist with :host, :user, :port, :max keys."
  (let* ((host (plist-get spec :host))
         (user (plist-get spec :user))
         (max (or (plist-get spec :max) 1))
         (secrets-dir (hypermodern/agenix-secrets-dir))
         (results '()))

    (when (and host (file-directory-p secrets-dir))
      ;; Try various naming conventions
      (let ((candidates (hypermodern/agenix-secret-names host)))

        (dolist (candidate candidates)
          (let ((file (expand-file-name candidate secrets-dir)))
            (when (and (file-exists-p file)
                       (< (length results) max))
              (push
               (list :host host
                     :user (or user "apikey")
                     :secret (lambda ()
                               (with-temp-buffer
                                 (insert-file-contents file)
                                 (string-trim (buffer-string)))))
               results))))))

    (nreverse results)))

;; Register as auth-source backend
(defun hypermodern/agenix-auth-source-backend ()
  "Create an agenix auth-source backend."
  (auth-source-backend
   :source "agenix"
   :type 'agenix
   :search-function #'hypermodern/agenix-auth-source-search))

;; Add to auth-source backends
(with-eval-after-load 'auth-source
  (add-to-list 'auth-source-backend-parser-functions
               (lambda (entry)
                 (when (eq entry 'agenix)
                   (hypermodern/agenix-auth-source-backend))))

  ;; Add 'agenix to auth-sources to enable
  ;; (add-to-list 'auth-sources 'agenix)
  )

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // convenience // functions
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/get-secret (name &optional context)
  "Get secret NAME from agenix, falling back to auth-source.
Tries agenix first (fast, no GPG), then auth-source."
  (or (hypermodern/agenix-read-secret name context)
      (let ((found (car (auth-source-search
                         :host name
                         :max 1))))
        (when found
          (let ((secret (plist-get found :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))))))

(defun hypermodern/insert-secret (name)
  "Insert secret NAME at point."
  (interactive
   (list (completing-read
          "Secret: "
          (append (hypermodern/agenix-list-secrets)
                  '("openrouter.ai" "github.com" "api.anthropic.com")))))
  (let ((secret (hypermodern/get-secret name)))
    (if secret
        (insert secret)
      (message "Secret not found: %s" name))))

(defun hypermodern/copy-secret (name)
  "Copy secret NAME to kill ring (clipboard)."
  (interactive
   (list (completing-read
          "Secret: "
          (hypermodern/agenix-list-secrets))))
  (let ((secret (hypermodern/get-secret name)))
    (if secret
        (progn
          (kill-new secret)
          (message "Secret copied to clipboard"))
      (message "Secret not found: %s" name))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // gpg // key management
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/gpg-list-keys ()
  "List GPG secret keys."
  (interactive)
  (with-current-buffer (get-buffer-create "*gpg-keys*")
    (erase-buffer)
    (call-process "gpg" nil t nil "--list-secret-keys" "--keyid-format=long")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun hypermodern/gpg-export-key (key-id output-dir)
  "Export GPG KEY-ID (public and secret) to OUTPUT-DIR."
  (interactive
   (list (read-string "Key ID: ")
         (read-directory-name "Export to: " "~/")))
  (make-directory output-dir t)
  (let ((default-directory output-dir))
    (shell-command (format "gpg --armor --export %s > public.asc" key-id))
    (shell-command (format "gpg --armor --export-secret-keys %s > secret.asc" key-id))
    (shell-command "gpg --export-ownertrust > ownertrust.txt")
    (message "Exported to %s - ENCRYPT THIS DIRECTORY" output-dir)))

(defun hypermodern/gpg-import-backup (backup-dir)
  "Import GPG keys from BACKUP-DIR."
  (interactive (list (read-directory-name "Import from: " "~/")))
  (let ((default-directory backup-dir))
    (when (file-exists-p "secret.asc")
      (shell-command "gpg --import secret.asc"))
    (when (file-exists-p "public.asc")
      (shell-command "gpg --import public.asc"))
    (when (file-exists-p "ownertrust.txt")
      (shell-command "gpg --import-ownertrust ownertrust.txt"))
    (message "Import complete")))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // transient // menu
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(require 'transient nil t)

(when (featurep 'transient)
  (transient-define-prefix hypermodern/secrets-menu ()
    "Secrets management."
    [:description
     (lambda () (format "Context: %s"
                        (hypermodern/secrets-context-get :name)))
     ["Context"
      ("c" "Switch context" hypermodern/secrets-switch-context)]
     ["Agenix"
      ("e" "Edit secret" hypermodern/agenix-edit)
      ("n" "New secret" hypermodern/agenix-create)
      ("r" "Rekey all" hypermodern/agenix-rekey)
      ("b" "Browse decrypted" hypermodern/agenix-browse)
      ("s" "Browse source" hypermodern/agenix-browse-source)]
     ["Actions"
      ("y" "Copy secret" hypermodern/copy-secret)
      ("i" "Insert secret" hypermodern/insert-secret)]
     ["GPG"
      ("l" "List keys" hypermodern/gpg-list-keys)
      ("x" "Export key" hypermodern/gpg-export-key)
      ("m" "Import backup" hypermodern/gpg-import-backup)]])

  (global-set-key (kbd "C-c s") #'hypermodern/secrets-menu)
  ;; Declare function for byte compiler
  (declare-function hypermodern/secrets-menu "hypermodern-secrets"))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // init // detect context
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun hypermodern/secrets-detect-context ()
  "Detect context from environment or hostname."
  (cond
   ;; Explicit env var wins
   ((getenv "HYPERMODERN_CONTEXT")
    (intern (getenv "HYPERMODERN_CONTEXT")))
   ;; Check for context-specific markers
   ((file-exists-p "~/weyl/.context")
    'weyl)
   ;; Hostname patterns
   ((string-match-p "weyl\\|gpu\\|inference" (system-name))
    'weyl)
   ;; Default
   (t 'personal)))

;; Initialize on load
(hypermodern/secrets-switch-context (hypermodern/secrets-detect-context))

(provide 'hypermodern-secrets)
;;; hypermodern-secrets.el ends here
