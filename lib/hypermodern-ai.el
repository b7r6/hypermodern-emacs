;;; hypermodern-ai.el --- God-mode AI control room -*- lexical-binding: t; -*-

;; Minimal, disciplined “AI cockpit” for Emacs.
;; - One transient menu for chat/agents/review
;; - Routes everything through diffs (Magit/Ediff)
;; - Runs terminal agents in a per-project vterm
;; - Degrades gracefully: if something isn’t installed, it won’t explode

(require 'project)
(require 'subr-x) ;; string-trim, string-empty-p

;; transient is bundled with Magit, but can be required independently.
(require 'transient nil t)

(defgroup hypermodern-ai nil
  "God-mode AI coding control room."
  :group 'tools)

(defcustom hypermodern/ai-claude-command "claude"
  "Shell command to start Claude Code (or your preferred Claude CLI)."
  :type 'string)

(defcustom hypermodern/ai-codex-command "codex"
  "Shell command to start Codex CLI (or your preferred Codex agent)."
  :type 'string)

(defcustom hypermodern/ai-terminal 'vterm
  "Terminal backend used for agent CLIs.

- \\='vterm: requires vterm
- \\='eat: requires eat

If the selected backend is unavailable, we fall back to `term` (built-in),
but you'll want vterm/eat for the good life."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat)
                 (const :tag "term" term)))

(defcustom hypermodern/ai-worktree-parent ".."
  "Where to create AI worktrees (relative to project root)."
  :type 'string)

(defcustom hypermodern/ai-worktree-branch-prefix "ai/"
  "Prefix for AI worktree branches."
  :type 'string)

(defcustom hypermodern/ai-agents-file "AGENTS.md"
  "Instructions file for agent CLIs (Codex reads this; Claude can too)."
  :type 'string)

(defcustom hypermodern/ai-agents-template
  "# AGENTS\n\n## Mission\n- You are a coding agent working in this repo.\n- Default stance: *safe + correct*.\n- When uncertain, ask for clarification in comments in the diff.\n\n## Rules\n- Do not change public APIs without updating tests + docs.\n- Prefer small diffs.\n- Add/extend tests for behavior changes.\n- Run the smallest relevant test command and include results in your summary.\n\n## Style\n- Match existing code style.\n- Don’t reformat unrelated code.\n\n## Commands\n- Tests: (fill me in)\n- Lint:  (fill me in)\n"
  "Initial content for AGENTS.md."
  :type 'string)

(defun hypermodern/ai--project-root ()
  "Return project root or `default-directory`."
  (if-let ((pr (project-current nil)))
      (project-root pr)
    default-directory))

(defun hypermodern/ai--git-root ()
  "Return git toplevel directory, or nil if not in a git repo."
  (let ((default-directory (hypermodern/ai--project-root)))
    (when (eq 0 (call-process "git" nil 0 nil "rev-parse" "--is-inside-work-tree"))
      (with-temp-buffer
        (when (eq 0 (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
          (string-trim (buffer-string)))))))

(defun hypermodern/ai--bufname (tag)
  (let* ((root (hypermodern/ai--project-root))
         (proj (file-name-nondirectory (directory-file-name root))))
    (format "*%s:%s*" tag proj)))

(defun hypermodern/ai--command-exists-p (cmd)
  (and (stringp cmd)
       (not (string-empty-p cmd))
       (executable-find (car (split-string cmd " " t)))))

(defun hypermodern/ai--term-open (name)
  "Open/reuse a terminal buffer NAME (string)."
  (let ((bufname (hypermodern/ai--bufname name))
        (default-directory (hypermodern/ai--project-root)))
    (cond
     ;; vterm
     ((and (eq hypermodern/ai-terminal 'vterm)
           (fboundp 'vterm))
      (if (get-buffer bufname)
          (pop-to-buffer bufname)
        (vterm bufname))
      (get-buffer bufname))

     ;; eat
     ((and (eq hypermodern/ai-terminal 'eat)
           (fboundp 'eat))
      (if (get-buffer bufname)
          (pop-to-buffer bufname)
        (progn
          (eat)
          (rename-buffer bufname t)))
      (get-buffer bufname))

     ;; built-in term fallback
     (t
      (if (get-buffer bufname)
          (pop-to-buffer bufname)
        (term "/bin/sh"))
      (with-current-buffer (current-buffer)
        (rename-buffer bufname t))
      (get-buffer bufname)))))

(defun hypermodern/ai--term-send (buf cmd)
  "Send CMD to BUF terminal buffer."  
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (cond
       ((derived-mode-p 'vterm-mode)
        (when (fboundp 'vterm-send-string)
          (vterm-send-string cmd)
          (when (fboundp 'vterm-send-return) (vterm-send-return))))
       ((derived-mode-p 'eat-mode)
        ;; Eat APIs vary by version; try to send, else just insert.
        (cond
         ((fboundp 'eat-term-send-string)
          (eat-term-send-string (concat cmd "\n")))
         ((fboundp 'eat-send-string)
          (eat-send-string (concat cmd "\n")))
         (t
          (insert cmd)
          (newline)
          (when (fboundp 'eat-send-input)
            (eat-send-input)))))
       ((derived-mode-p 'term-mode)
        (when (fboundp 'term-send-raw-string)
          (term-send-raw-string (concat cmd "\n"))))
       (t
        (insert cmd)
        (newline))))))

(defun hypermodern/ai-run-claude ()
  "Open Claude Code agent in a per-project terminal."  
  (interactive)
  (unless (hypermodern/ai--command-exists-p hypermodern/ai-claude-command)
    (user-error "Claude command not found: %s" hypermodern/ai-claude-command))
  (let ((buf (hypermodern/ai--term-open "claude")))
    (hypermodern/ai--term-send buf hypermodern/ai-claude-command)))

(defun hypermodern/ai-run-codex ()
  "Open Codex CLI agent in a per-project terminal."  
  (interactive)
  (unless (hypermodern/ai--command-exists-p hypermodern/ai-codex-command)
    (user-error "Codex command not found: %s" hypermodern/ai-codex-command))
  (let ((buf (hypermodern/ai--term-open "codex")))
    (hypermodern/ai--term-send buf hypermodern/ai-codex-command)))

(defun hypermodern/ai-worktree ()
  "Create a git worktree for agent work (safe sandbox)."  
  (interactive)
  (let* ((root (or (hypermodern/ai--git-root)
                   (user-error "Not inside a git repo")))
         (proj (file-name-nondirectory (directory-file-name root)))
         (ts   (format-time-string "%Y%m%d-%H%M%S"))
         (branch (concat hypermodern/ai-worktree-branch-prefix ts))
         (parent (expand-file-name hypermodern/ai-worktree-parent root))
         (dir (expand-file-name (format "%s--%s" proj ts) parent)))
    (make-directory parent t)
    (let ((default-directory root))
      (let ((cmd (format "git worktree add -b %s %s"
                         (shell-quote-argument branch)
                         (shell-quote-argument dir))))
        (message "[ai] %s" cmd)
        (unless (eq 0 (call-process-shell-command cmd))
          (user-error "git worktree failed"))))
    (dired dir)
    (message "[ai] worktree → %s (%s)" dir branch)))

(defun hypermodern/ai-open-agents-file ()
  "Open AGENTS.md in project root."  
  (interactive)
  (let* ((root (hypermodern/ai--project-root))
         (path (expand-file-name hypermodern/ai-agents-file root)))
    (find-file path)))

(defun hypermodern/ai-init-agents-file ()
  "Create AGENTS.md in project root if missing."  
  (interactive)
  (let* ((root (hypermodern/ai--project-root))
         (path (expand-file-name hypermodern/ai-agents-file root)))
    (if (file-exists-p path)
        (find-file path)
      (with-temp-file path
        (insert hypermodern/ai-agents-template))
      (find-file path)
      (message "[ai] created %s" path))))

;; --- Chat lane (gptel) wrappers ---

(defun hypermodern/ai-gptel ()
  (interactive)
  (if (fboundp 'gptel)
      (call-interactively 'gptel)
    (user-error "gptel not installed")))

(defun hypermodern/ai-gptel-send ()
  (interactive)
  (if (fboundp 'gptel-send)
      (call-interactively 'gptel-send)
    (user-error "gptel-send not available")))

(defun hypermodern/ai-gptel-rewrite ()
  (interactive)
  (if (fboundp 'gptel-rewrite)
      (call-interactively 'gptel-rewrite)
    (user-error "gptel-rewrite not available")))

(defun hypermodern/ai-gptel-add ()
  "Add region/buffer to gptel context if supported."  
  (interactive)
  (cond
   ((fboundp 'gptel-add)
    (call-interactively 'gptel-add))
   (t
    (message "[ai] gptel-add not available in this gptel version"))))

(defun hypermodern/ai-gptel-add-file ()
  "Add a file to gptel context if supported."  
  (interactive)
  (cond
   ((fboundp 'gptel-add-file)
    (call-interactively 'gptel-add-file))
   (t
    (message "[ai] gptel-add-file not available in this gptel version"))))

;; --- Review lane wrappers ---

(defun hypermodern/ai-magit-status ()
  (interactive)
  (if (fboundp 'magit-status)
      (call-interactively 'magit-status)
    (user-error "magit not installed")))

(defun hypermodern/ai-ediff-dwim ()
  (interactive)
  (cond
   ((fboundp 'magit-ediff-dwim)
    (call-interactively 'magit-ediff-dwim))
   ((fboundp 'ediff-revision)
    (call-interactively 'ediff-revision))
   (t
    (user-error "No ediff command available"))))

;; --- Aider in Emacs lane wrappers ---

(defun hypermodern/ai-aider-menu ()
  (interactive)
  (cond
   ((fboundp 'aidermacs-transient-menu)
    (call-interactively 'aidermacs-transient-menu))
   ((fboundp 'aidermacs-run-aidermacs)
    (call-interactively 'aidermacs-run-aidermacs))
   (t
    (user-error "aidermacs not installed"))))

(defun hypermodern/ai-aider-run ()
  (interactive)
  (if (fboundp 'aidermacs-run-aidermacs)
      (call-interactively 'aidermacs-run-aidermacs)
    (user-error "aidermacs-run-aidermacs not available")))

;; --- The Control Room (Transient) ---

(when (featurep 'transient)
  (transient-define-prefix hypermodern/ai-menu ()
    "hypermodern: AI Control Room"
    [["Chat (gptel)"
      ("g" "chat" hypermodern/ai-gptel)
      ("s" "send" hypermodern/ai-gptel-send)
      ("r" "rewrite" hypermodern/ai-gptel-rewrite)
      ("a" "add ctx" hypermodern/ai-gptel-add)
      ("f" "add file" hypermodern/ai-gptel-add-file)]
     ["Agents"
      ("A" "aider menu" hypermodern/ai-aider-menu)
      ("aa" "aider run" hypermodern/ai-aider-run)
      ("c" "claude" hypermodern/ai-run-claude)
      ("x" "codex" hypermodern/ai-run-codex)
      ("w" "worktree" hypermodern/ai-worktree)]
     ["Repo"
      ("G" "magit" hypermodern/ai-magit-status)
      ("e" "ediff dwim" hypermodern/ai-ediff-dwim)
      ("i" "AGENTS.md" hypermodern/ai-open-agents-file)
      ("I" "init AGENTS" hypermodern/ai-init-agents-file)]]))

(defun hypermodern/ai-bind-defaults ()
  "Bind default keys for the AI control room.

- If `general` is present and you have a `C-c o` ops prefix, we attach there.
- Otherwise we bind C-c o a globally."  
  (interactive)
  (cond
   ((and (fboundp 'general-define-key)
         (boundp 'hypermodern/leader))
    ;; Your config may define a leader; if so, use it.
    (condition-case _
        (funcall hypermodern/leader "a" #'hypermodern/ai-menu)
      (error
       (general-define-key "C-c o a" #'hypermodern/ai-menu))))
   (t
    (global-set-key (kbd "C-c o a") #'hypermodern/ai-menu))))

;; Declare function for byte compiler
(declare-function hypermodern/ai-menu "hypermodern-ai")

(provide 'hypermodern-ai)
;;; hypermodern-ai.el ends here
