;;; hypermodern-compile-test.el --- tests for hypermodern-compile -*- lexical-binding: t; -*-

(require 'ert)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // eat backend tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-eat-backend-setting ()
  "Test that eat backend setting exists."
  (should (boundp 'hypermodern/compile-use-eat))
  (should (eq hypermodern/compile-use-eat t)))

(ert-deftest hypermodern-compile-test-eat-exec-function ()
  "Test that eat exec function is defined."
  (should (fboundp 'hypermodern/compile--eat-exec)))

(ert-deftest hypermodern-compile-test-hijack-advice ()
  "Test that compilation-start advice is defined."
  (should (fboundp 'hypermodern/compile--hijack-process)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // project root detection tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-root-markers ()
  "Test that root markers list exists and is populated."
  (should (boundp 'hypermodern/compile-root-markers))
  (should (listp hypermodern/compile-root-markers))
  (should (member "Cargo.toml" hypermodern/compile-root-markers))
  (should (member "package.json" hypermodern/compile-root-markers))
  (should (member "flake.nix" hypermodern/compile-root-markers))
  (should (member "go.mod" hypermodern/compile-root-markers))
  (should (member "pyproject.toml" hypermodern/compile-root-markers))
  (should (member "Makefile" hypermodern/compile-root-markers))
  (should (member "WORKSPACE" hypermodern/compile-root-markers)))

(ert-deftest hypermodern-compile-test-find-root-function ()
  "Test that find-root function exists."
  (should (fboundp 'hypermodern/compile--find-root)))

(ert-deftest hypermodern-compile-test-find-root-returns-directory ()
  "Test that find-root returns a directory."
  (let ((result (hypermodern/compile--find-root)))
    (should (stringp result))
    (should (file-directory-p result))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // smart command detection tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-command-alist ()
  "Test that command alist exists and is populated."
  (should (boundp 'hypermodern/compile-command-alist))
  (should (listp hypermodern/compile-command-alist)))

(ert-deftest hypermodern-compile-test-command-alist-cargo ()
  "Test Cargo.toml detection."
  (let ((entry (assoc "Cargo.toml" hypermodern/compile-command-alist)))
    (should entry)
    (should (equal (cdr entry) "cargo build"))))

(ert-deftest hypermodern-compile-test-command-alist-nix ()
  "Test flake.nix detection."
  (let ((entry (assoc "flake.nix" hypermodern/compile-command-alist)))
    (should entry)
    (should (equal (cdr entry) "nix build"))))

(ert-deftest hypermodern-compile-test-command-alist-go ()
  "Test go.mod detection."
  (let ((entry (assoc "go.mod" hypermodern/compile-command-alist)))
    (should entry)
    (should (equal (cdr entry) "go build ./..."))))

(ert-deftest hypermodern-compile-test-command-alist-bazel ()
  "Test WORKSPACE detection."
  (let ((entry (assoc "WORKSPACE" hypermodern/compile-command-alist)))
    (should entry)
    (should (equal (cdr entry) "bazel build //..."))))

(ert-deftest hypermodern-compile-test-npm-command-function ()
  "Test npm command detection function exists."
  (should (fboundp 'hypermodern/compile--npm-command)))

(ert-deftest hypermodern-compile-test-python-command-function ()
  "Test python command detection function exists."
  (should (fboundp 'hypermodern/compile--python-command)))

(ert-deftest hypermodern-compile-test-detect-command-function ()
  "Test detect command function exists."
  (should (fboundp 'hypermodern/compile--detect-command)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // per-project memory tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-project-commands-hash ()
  "Test that project commands hash table exists."
  (should (boundp 'hypermodern/compile--project-commands))
  (should (hash-table-p hypermodern/compile--project-commands)))

(ert-deftest hypermodern-compile-test-save-and-get-command ()
  "Test saving and retrieving project commands."
  (let ((test-root "/tmp/test-project"))
    ;; Save a command
    (hypermodern/compile--save-project-command test-root "make foo")
    ;; Retrieve it
    (should (equal (gethash test-root hypermodern/compile--project-commands)
                   "make foo"))
    ;; Clean up
    (remhash test-root hypermodern/compile--project-commands)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // error regex tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-error-regexes-defined ()
  "Test that error regexes list is defined."
  (should (boundp 'hypermodern/compile-error-regexes))
  (should (listp hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-rust-error-regex ()
  "Test Rust error regex is defined."
  (should (assq 'rust-error hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-nix-error-regex ()
  "Test Nix error regex is defined."
  (should (assq 'nix-error hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-python-traceback-regex ()
  "Test Python traceback regex is defined."
  (should (assq 'python-traceback hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-golang-regex ()
  "Test Go error regex is defined."
  (should (assq 'golang hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-typescript-regex ()
  "Test TypeScript error regex is defined."
  (should (assq 'typescript-paren hypermodern/compile-error-regexes))
  (should (assq 'typescript-colon hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-java-regex ()
  "Test Java error regex is defined."
  (should (assq 'java-error hypermodern/compile-error-regexes))
  (should (assq 'java-stack hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-haskell-regex ()
  "Test Haskell error regex is defined."
  (should (assq 'haskell-ghc hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-bazel-regex ()
  "Test Bazel error regex is defined."
  (should (assq 'bazel-error hypermodern/compile-error-regexes)))

(ert-deftest hypermodern-compile-test-cuda-regex ()
  "Test CUDA error regex is defined."
  (should (assq 'cuda-error hypermodern/compile-error-regexes)))

;; Test actual regex matching
(ert-deftest hypermodern-compile-test-rust-regex-match ()
  "Test Rust error regex matches actual output."
  (let ((regex (nth 1 (assq 'rust-error hypermodern/compile-error-regexes))))
    (should (string-match regex "  --> src/main.rs:10:5"))
    (should (equal (match-string 1 "  --> src/main.rs:10:5") "src/main.rs"))
    (should (equal (match-string 2 "  --> src/main.rs:10:5") "10"))
    (should (equal (match-string 3 "  --> src/main.rs:10:5") "5"))))

(ert-deftest hypermodern-compile-test-python-regex-match ()
  "Test Python traceback regex matches actual output."
  (let ((regex (nth 1 (assq 'python-traceback hypermodern/compile-error-regexes))))
    (should (string-match regex "  File \"foo.py\", line 42, in <module>"))
    (should (equal (match-string 1 "  File \"foo.py\", line 42, in <module>") "foo.py"))
    (should (equal (match-string 2 "  File \"foo.py\", line 42, in <module>") "42"))))

(ert-deftest hypermodern-compile-test-go-regex-match ()
  "Test Go error regex matches actual output."
  (let ((regex (nth 1 (assq 'golang hypermodern/compile-error-regexes))))
    (should (string-match regex "./main.go:15:3: undefined: foo"))
    (should (equal (match-string 1 "./main.go:15:3: undefined: foo") "./main.go"))
    (should (equal (match-string 2 "./main.go:15:3: undefined: foo") "15"))
    (should (equal (match-string 3 "./main.go:15:3: undefined: foo") "3"))))

(ert-deftest hypermodern-compile-test-nix-regex-match ()
  "Test Nix error regex matches actual output."
  (let ((regex (nth 1 (assq 'nix-error hypermodern/compile-error-regexes))))
    (should (string-match regex "at /home/user/proj/flake.nix:42:7:"))
    (should (equal (match-string 1 "at /home/user/proj/flake.nix:42:7:")
                   "/home/user/proj/flake.nix"))
    (should (equal (match-string 2 "at /home/user/proj/flake.nix:42:7:") "42"))
    (should (equal (match-string 3 "at /home/user/proj/flake.nix:42:7:") "7"))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // main command tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-main-command ()
  "Test main compile command is defined."
  (should (fboundp 'hypermodern/compile)))

(ert-deftest hypermodern-compile-test-recompile-command ()
  "Test recompile command is defined."
  (should (fboundp 'hypermodern/recompile)))

(ert-deftest hypermodern-compile-test-test-command ()
  "Test compile-test command is defined."
  (should (fboundp 'hypermodern/compile-test)))

(ert-deftest hypermodern-compile-test-run-command ()
  "Test compile-run command is defined."
  (should (fboundp 'hypermodern/compile-run)))

(ert-deftest hypermodern-compile-test-clean-command ()
  "Test compile-clean command is defined."
  (should (fboundp 'hypermodern/compile-clean)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // nix integration tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-nix-build ()
  "Test nix build command is defined."
  (should (fboundp 'hypermodern/compile-nix-build)))

(ert-deftest hypermodern-compile-test-nix-check ()
  "Test nix check command is defined."
  (should (fboundp 'hypermodern/compile-nix-check)))

(ert-deftest hypermodern-compile-test-nix-develop ()
  "Test nix develop command is defined."
  (should (fboundp 'hypermodern/compile-nix-develop)))

(ert-deftest hypermodern-compile-test-nh-home-switch ()
  "Test nh home switch command is defined."
  (should (fboundp 'hypermodern/compile-nh-home-switch)))

(ert-deftest hypermodern-compile-test-nh-os-switch ()
  "Test nh os switch command is defined."
  (should (fboundp 'hypermodern/compile-nh-os-switch)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // keybinding tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-keymap ()
  "Test compile keymap is defined."
  (should (boundp 'hypermodern/compile-map))
  (should (keymapp hypermodern/compile-map)))

(ert-deftest hypermodern-compile-test-keymap-bindings ()
  "Test compile keymap has expected bindings."
  (should (eq (lookup-key hypermodern/compile-map (kbd "c"))
              #'hypermodern/compile))
  (should (eq (lookup-key hypermodern/compile-map (kbd "r"))
              #'hypermodern/recompile))
  (should (eq (lookup-key hypermodern/compile-map (kbd "t"))
              #'hypermodern/compile-test)))

(ert-deftest hypermodern-compile-test-global-binding ()
  "Test C-c c is bound to compile map."
  (should (eq (lookup-key global-map (kbd "C-c c"))
              hypermodern/compile-map)))

(ert-deftest hypermodern-compile-test-function-key-bindings ()
  "Test function key bindings."
  (should (eq (lookup-key global-map (kbd "<f5>"))
              #'hypermodern/compile))
  (should (eq (lookup-key global-map (kbd "<f6>"))
              #'hypermodern/recompile))
  (should (eq (lookup-key global-map (kbd "<f7>"))
              #'hypermodern/compile-test)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; // compilation settings tests //
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(ert-deftest hypermodern-compile-test-scroll-setting ()
  "Test compilation scroll setting."
  (should (eq compilation-scroll-output 'first-error)))

(ert-deftest hypermodern-compile-test-always-kill-setting ()
  "Test compilation always kill setting."
  (should (eq compilation-always-kill t)))

(provide 'hypermodern-compile-test)
;;; hypermodern-compile-test.el ends here
