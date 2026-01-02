;;; hypermodern-compile.el --- compilation that actually works -*- lexical-binding: t; -*-
;;
;; The problem: compile-mode assumes 1985. Progress bars, colors, fancy
;; prompts all break it. And you have to cd to the project root manually.
;;
;; The fix: eat backend for terminal emulation + smart project detection +
;; comprehensive error regexes. M-x compile just works.

(require 'cl-lib)
(require 'compile)
(require 'project)

(defgroup hypermodern-compile nil
  "Compilation with terminal emulation and smart defaults."
  :group 'hypermodern)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // eat backend // terminal emulation for compile
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defcustom hypermodern/compile-use-eat t
  "Use eat terminal emulation for compilation.
This fixes progress bars, colors, and fancy terminal output."
  :type 'boolean
  :group 'hypermodern-compile)

(defun hypermodern/compile--eat-exec (name buffer command)
  "Run COMMAND in BUFFER using eat terminal emulation.
NAME is the process name."
  (require 'eat)
  (with-current-buffer
      (eat-exec buffer name
                (or explicit-shell-file-name shell-file-name "/bin/bash")
                nil
                (list "-ilc" command))
    (eat-emacs-mode)
    ;; Enable scrolling sync
    (setq-local eat--synchronize-scroll-function #'eat--synchronize-scroll)
    (get-buffer-process (current-buffer))))

(defun hypermodern/compile--hijack-process (orig-fn &rest args)
  "Advice for `compilation-start' to use eat backend.
ORIG-FN is the original function, ARGS are its arguments."
  (if hypermodern/compile-use-eat
      (progn
        (advice-add #'start-file-process-shell-command
                    :override #'hypermodern/compile--eat-exec)
        (unwind-protect
            (apply orig-fn args)
          (advice-remove #'start-file-process-shell-command
                         #'hypermodern/compile--eat-exec)))
    (apply orig-fn args)))

(defun hypermodern/compile--setup-eat-process (proc)
  "Set up eat process filter and sentinel for PROC."
  (when (and hypermodern/compile-use-eat
             (require 'eat nil t))
    (set-process-filter proc #'eat--filter)
    (add-function :after (process-sentinel proc) #'eat--sentinel)))

(defun hypermodern/compile--kill-with-C-c ()
  "Kill compilation by sending C-c instead of SIGKILL."
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (when-let ((proc (get-buffer-process buffer)))
      (with-current-buffer buffer
        (if (and hypermodern/compile-use-eat
                 (fboundp 'eat-self-input))
            ;; Send C-c through eat
            (eat-self-input 1 ?\C-c)
          ;; Fallback to interrupt
          (interrupt-process proc))))))

;; Wire it up
(with-eval-after-load 'compile
  (advice-add #'compilation-start :around #'hypermodern/compile--hijack-process)
  (add-hook 'compilation-start-hook #'hypermodern/compile--setup-eat-process)
  (advice-add #'kill-compilation :override #'hypermodern/compile--kill-with-C-c))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // project root detection // no more cd
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/compile-root-markers
  '(;; Nix
    "flake.nix" "default.nix" "shell.nix"
    ;; Rust
    "Cargo.toml"
    ;; JavaScript/TypeScript
    "package.json" "deno.json" "bun.lockb"
    ;; Python
    "pyproject.toml" "setup.py" "setup.cfg" "requirements.txt"
    ;; Go
    "go.mod"
    ;; Bazel
    "WORKSPACE" "WORKSPACE.bazel" "BUILD.bazel" "MODULE.bazel"
    ;; Make
    "Makefile" "GNUmakefile" "makefile"
    ;; CMake
    "CMakeLists.txt"
    ;; Haskell
    "cabal.project" "stack.yaml" "*.cabal"
    ;; Java/JVM
    "pom.xml" "build.gradle" "build.gradle.kts"
    ;; Generic
    ".git" ".projectile" ".project")
  "Files that indicate project root, in priority order.")

(defun hypermodern/compile--find-root (&optional dir)
  "Find project root starting from DIR.
Returns the directory containing the first marker found, or DIR."
  (let ((dir (or dir default-directory)))
    (or
     ;; Try project.el first
     (when-let ((project (project-current nil dir)))
       (project-root project))
     ;; Then walk up looking for markers
     (cl-loop for marker in hypermodern/compile-root-markers
              for found = (locate-dominating-file dir marker)
              when found return found)
     ;; Fallback to current
     dir)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // smart commands // detect build system
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/compile-command-alist
  '(;; Nix - check flake first
    ("flake.nix" . "nix build")
    ("default.nix" . "nix-build")
    ("shell.nix" . "nix-shell --run 'echo ready'")
    ;; Rust
    ("Cargo.toml" . "cargo build")
    ;; JavaScript/TypeScript
    ("package.json" . hypermodern/compile--npm-command)
    ("deno.json" . "deno task build")
    ("bun.lockb" . "bun run build")
    ;; Python
    ("pyproject.toml" . hypermodern/compile--python-command)
    ("setup.py" . "python setup.py build")
    ;; Go
    ("go.mod" . "go build ./...")
    ;; Bazel
    ("WORKSPACE" . "bazel build //...")
    ("WORKSPACE.bazel" . "bazel build //...")
    ("MODULE.bazel" . "bazel build //...")
    ;; Make
    ("Makefile" . "make -k")
    ("GNUmakefile" . "make -k")
    ;; CMake (assume build dir exists)
    ("CMakeLists.txt" . "cmake --build build")
    ;; Haskell
    ("cabal.project" . "cabal build")
    ("stack.yaml" . "stack build")
    ;; Java
    ("pom.xml" . "mvn compile")
    ("build.gradle" . "./gradlew build")
    ("build.gradle.kts" . "./gradlew build"))
  "Alist of (marker . command) for auto-detection.
Command can be a string or a function returning a string.")

(defun hypermodern/compile--npm-command ()
  "Determine npm command from package.json scripts."
  (let ((package-json (expand-file-name "package.json")))
    (if (file-exists-p package-json)
        (with-temp-buffer
          (insert-file-contents package-json)
          (when-let* ((json (ignore-errors (json-parse-buffer :object-type 'alist)))
                      (scripts (alist-get 'scripts json)))
            (cond
             ((alist-get 'build scripts) "npm run build")
             ((alist-get 'compile scripts) "npm run compile")
             ((alist-get 'test scripts) "npm test")
             (t "npm install"))))
      "npm install")))

(defun hypermodern/compile--python-command ()
  "Determine python build command from pyproject.toml."
  (let ((pyproject (expand-file-name "pyproject.toml")))
    (if (file-exists-p pyproject)
        (with-temp-buffer
          (insert-file-contents pyproject)
          (cond
           ((search-forward "[tool.poetry]" nil t) "poetry build")
           ((search-forward "[tool.hatch]" nil t) "hatch build")
           ((search-forward "[build-system]" nil t) "python -m build")
           (t "pip install -e .")))
      "pip install -e .")))

(defun hypermodern/compile--detect-command (root)
  "Detect build command for project at ROOT."
  (let ((default-directory root))
    (cl-loop for (marker . cmd) in hypermodern/compile-command-alist
             when (or (file-exists-p marker)
                      (and (string-match-p "\\*" marker)
                           (directory-files root nil
                                           (wildcard-to-regexp marker))))
             return (if (functionp cmd)
                       (funcall cmd)
                     cmd)
             finally return "make -k")))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // per-project memory // remember commands
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/compile--project-commands (make-hash-table :test 'equal)
  "Hash table mapping project roots to last compile command.")

(defun hypermodern/compile--get-project-command (root)
  "Get remembered compile command for ROOT, or detect one."
  (or (gethash root hypermodern/compile--project-commands)
      (hypermodern/compile--detect-command root)))

(defun hypermodern/compile--save-project-command (root command)
  "Save COMMAND as the compile command for ROOT."
  (puthash root command hypermodern/compile--project-commands))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // error regexes // the works
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/compile-error-regexes
  '(;; Rust - cargo/rustc
    ;; error[E0382]: borrow of moved value
    ;;   --> src/main.rs:10:5
    (rust-error
     "^\\s-*--> \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
     1 2 3 2)  ; file line col, type=error

    ;; Rust - also match the "error:" lines for context
    (rust-error-header
     "^error\\(?:\\[E[0-9]+\\]\\)?:"
     nil nil nil 0)  ; just highlight, no navigation

    ;; Nix
    ;; error: ... at /nix/store/.../default.nix:10:5:
    (nix-error
     "at \\(/[^:]+\\.nix\\):\\([0-9]+\\):\\([0-9]+\\)"
     1 2 3 2)

    ;; Nix - trace locations
    ;; … while evaluating the attribute 'buildInputs'
    ;;   at /home/user/proj/default.nix:15:3:
    (nix-trace
     "^\\s-+at \\(/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\):"
     1 2 3 1)  ; type=warning

    ;; Python traceback
    ;; File "foo.py", line 10, in <module>
    (python-traceback
     "^\\s-*File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
     1 2 nil 2)

    ;; Python pytest
    ;; test_foo.py:10: AssertionError
    (python-pytest
     "^\\([^:\n]+\\.py\\):\\([0-9]+\\):"
     1 2 nil 2)

    ;; Go
    ;; ./main.go:10:5: undefined
    (golang
     "^\\([^:\n]+\\.go\\):\\([0-9]+\\):\\([0-9]+\\):"
     1 2 3 2)

    ;; TypeScript/ESLint
    ;; src/foo.ts(10,5): error TS2322
    (typescript-paren
     "^\\([^(\n]+\\)(\\([0-9]+\\),\\([0-9]+\\)):"
     1 2 3 2)

    ;; TypeScript/ESLint - colon format
    ;; src/foo.ts:10:5 - error TS2322
    (typescript-colon
     "^\\([^:\n]+\\.tsx?\\):\\([0-9]+\\):\\([0-9]+\\)"
     1 2 3 2)

    ;; ESLint
    ;; /path/to/file.js
    ;;   10:5  error  Description
    (eslint
     "^\\s-+\\([0-9]+\\):\\([0-9]+\\)\\s-+\\(error\\|warning\\)"
     nil 1 2 (3 . ("error" . "warning")))

    ;; Java/Kotlin - gradle/maven
    ;; src/main/java/Foo.java:10: error:
    (java-error
     "^\\([^:\n]+\\.\\(java\\|kt\\|scala\\)\\):\\([0-9]+\\):"
     1 3 nil 2)

    ;; Java stack trace
    ;; at com.foo.Bar.method(Bar.java:10)
    (java-stack
     "at [^(]+(\\([^:]+\\):\\([0-9]+\\))"
     1 2 nil 1)

    ;; Haskell - GHC
    ;; src/Foo.hs:10:5: error:
    (haskell-ghc
     "^\\([^:\n]+\\.hs\\):\\([0-9]+\\):\\([0-9]+\\):"
     1 2 3 2)

    ;; Haskell - stack
    ;; Foo.hs:10:5:
    (haskell-stack
     "^\\s-*\\([^:\n]+\\.hs\\):\\([0-9]+\\):\\([0-9]+\\):"
     1 2 3 2)

    ;; Bazel
    ;; ERROR: /path/to/BUILD:10:5:
    (bazel-error
     "^ERROR: \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):"
     1 2 3 2)

    ;; Bazel - also INFO/WARNING
    (bazel-info
     "^\\(INFO\\|WARNING\\): \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):"
     2 3 4 (1 . ("WARNING" . "INFO")))

    ;; C/C++ - GCC/Clang
    ;; foo.c:10:5: error:
    ;; (already in default compilation-error-regexp-alist, but ensure it)

    ;; CMake
    ;; CMake Error at CMakeLists.txt:10:
    (cmake-error
     "^CMake Error at \\([^:]+\\):\\([0-9]+\\)"
     1 2 nil 2)

    ;; Shell/Bash
    ;; ./script.sh: line 10:
    (shell-error
     "^\\([^:\n]+\\.sh\\): line \\([0-9]+\\):"
     1 2 nil 2)

    ;; Zig
    ;; src/main.zig:10:5: error:
    (zig-error
     "^\\([^:\n]+\\.zig\\):\\([0-9]+\\):\\([0-9]+\\):"
     1 2 3 2)

    ;; Odin
    ;; src/main.odin(10:5) Error:
    (odin-error
     "^\\([^(\n]+\\.odin\\)(\\([0-9]+\\):\\([0-9]+\\))"
     1 2 3 2)

    ;; CUDA
    ;; foo.cu(10): error:
    (cuda-error
     "^\\([^(\n]+\\.cu\\)(\\([0-9]+\\)):"
     1 2 nil 2)

    ;; Generic "file:line" fallback - low priority
    (generic-file-line
     "^\\([a-zA-Z]?:?[^:\n]+\\):\\([0-9]+\\):"
     1 2 nil 1))
  "Additional error regexes for modern tooling.
Format: (NAME REGEXP FILE LINE COL TYPE)")

(defun hypermodern/compile--setup-error-regexes ()
  "Add custom error regexes to compilation-error-regexp-alist."
  (dolist (entry hypermodern/compile-error-regexes)
    (let ((name (car entry))
          (rest (cdr entry)))
      ;; Add to alist-alist
      (setf (alist-get name compilation-error-regexp-alist-alist) rest)
      ;; Add name to active list if not present
      (unless (memq name compilation-error-regexp-alist)
        (push name compilation-error-regexp-alist)))))

(with-eval-after-load 'compile
  (hypermodern/compile--setup-error-regexes))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // main commands // the interface
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;;###autoload
(defun hypermodern/compile (&optional arg)
  "Compile from project root with smart defaults.
With prefix ARG, prompt for command even if one is remembered.
With double prefix ARG (C-u C-u), prompt for directory first."
  (interactive "P")
  (let* ((root (if (equal arg '(16))  ; C-u C-u
                   (read-directory-name "Compile in: ")
                 (hypermodern/compile--find-root)))
         (default-directory root)
         (default-cmd (hypermodern/compile--get-project-command root))
         (command (if (or arg (not default-cmd))
                      (compilation-read-command default-cmd)
                    default-cmd)))
    ;; Remember the command
    (hypermodern/compile--save-project-command root command)
    ;; Run it
    (compile command)))

;;;###autoload
(defun hypermodern/recompile ()
  "Recompile with the last command in the project root.
If called from a compilation buffer, recompile there.
Otherwise, find project root and use remembered command."
  (interactive)
  (if (derived-mode-p 'compilation-mode)
      (recompile)
    (let* ((root (hypermodern/compile--find-root))
           (default-directory root)
           (command (hypermodern/compile--get-project-command root)))
      (if command
          (compile command)
        (call-interactively #'hypermodern/compile)))))

;;;###autoload
(defun hypermodern/compile-test ()
  "Run tests for the current project."
  (interactive)
  (let* ((root (hypermodern/compile--find-root))
         (default-directory root)
         (command (cond
                   ((file-exists-p "Cargo.toml") "cargo test")
                   ((file-exists-p "package.json")
                    (if (file-exists-p "node_modules/.bin/jest")
                        "npm test"
                      "npm test"))
                   ((file-exists-p "pyproject.toml") "pytest")
                   ((file-exists-p "setup.py") "python -m pytest")
                   ((file-exists-p "go.mod") "go test ./...")
                   ((file-exists-p "Makefile") "make test")
                   ((file-exists-p "cabal.project") "cabal test")
                   ((file-exists-p "stack.yaml") "stack test")
                   (t "make test"))))
    (compile command)))

;;;###autoload
(defun hypermodern/compile-run ()
  "Run the current project."
  (interactive)
  (let* ((root (hypermodern/compile--find-root))
         (default-directory root)
         (command (cond
                   ((file-exists-p "Cargo.toml") "cargo run")
                   ((file-exists-p "package.json") "npm start")
                   ((file-exists-p "go.mod") "go run .")
                   ((file-exists-p "main.py") "python main.py")
                   ((file-exists-p "Makefile") "make run")
                   (t "make run"))))
    (compile command)))

;;;###autoload
(defun hypermodern/compile-clean ()
  "Clean build artifacts for the current project."
  (interactive)
  (let* ((root (hypermodern/compile--find-root))
         (default-directory root)
         (command (cond
                   ((file-exists-p "Cargo.toml") "cargo clean")
                   ((file-exists-p "package.json") "rm -rf node_modules && npm install")
                   ((file-exists-p "go.mod") "go clean")
                   ((file-exists-p "Makefile") "make clean")
                   ((file-exists-p "CMakeLists.txt") "rm -rf build && mkdir build")
                   (t "make clean"))))
    (when (yes-or-no-p (format "Run '%s'? " command))
      (compile command))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // nix integration // special handling
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;;###autoload
(defun hypermodern/compile-nix-build ()
  "Run nix build in project root."
  (interactive)
  (let* ((root (hypermodern/compile--find-root))
         (default-directory root))
    (compile "nix build")))

;;;###autoload
(defun hypermodern/compile-nix-check ()
  "Run nix flake check in project root."
  (interactive)
  (let* ((root (hypermodern/compile--find-root))
         (default-directory root))
    (compile "nix flake check")))

;;;###autoload
(defun hypermodern/compile-nix-develop ()
  "Enter nix develop shell (opens in terminal)."
  (interactive)
  (let* ((root (hypermodern/compile--find-root))
         (default-directory root))
    (if (fboundp 'eat)
        (eat "nix develop")
      (compile "nix develop --command $SHELL"))))

;;;###autoload
(defun hypermodern/compile-nh-home-switch ()
  "Run nh home switch."
  (interactive)
  (let ((default-directory (or (getenv "HOME") "~")))
    (compile "nh home switch")))

;;;###autoload
(defun hypermodern/compile-nh-os-switch ()
  "Run nh os switch."
  (interactive)
  (compile "nh os switch"))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // keybindings // C-c c prefix
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar hypermodern/compile-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'hypermodern/compile)
    (define-key map (kbd "r") #'hypermodern/recompile)
    (define-key map (kbd "t") #'hypermodern/compile-test)
    (define-key map (kbd "x") #'hypermodern/compile-run)
    (define-key map (kbd "k") #'hypermodern/compile-clean)
    ;; Nix
    (define-key map (kbd "n b") #'hypermodern/compile-nix-build)
    (define-key map (kbd "n c") #'hypermodern/compile-nix-check)
    (define-key map (kbd "n d") #'hypermodern/compile-nix-develop)
    (define-key map (kbd "n h") #'hypermodern/compile-nh-home-switch)
    (define-key map (kbd "n o") #'hypermodern/compile-nh-os-switch)
    map)
  "Keymap for compile commands.")

(global-set-key (kbd "C-c c") hypermodern/compile-map)

;; Also override default compile bindings
(global-set-key (kbd "<f5>") #'hypermodern/compile)
(global-set-key (kbd "<f6>") #'hypermodern/recompile)
(global-set-key (kbd "<f7>") #'hypermodern/compile-test)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // compilation buffer improvements // 
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Auto-scroll to first error
(setq compilation-scroll-output 'first-error)

;; Don't ask to kill running compilation
(setq compilation-always-kill t)

;; Make ANSI colors work (backup if eat isn't used)
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Truncate lines in compilation buffer
(add-hook 'compilation-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (setq-local show-trailing-whitespace nil)))

;; Parse errors on the fly
(setq compilation-auto-jump-to-first-error nil)  ; Don't jump automatically

;; Show more context
(setq compilation-context-lines 3)

;; Stop hiding matched text (some people find this annoying)
(setq compilation-message-face nil)

(provide 'hypermodern-compile)
;;; hypermodern-compile.el ends here
