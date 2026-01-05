# hypermodern-emacs architecture

## The Problem

The previous modular structure had use-package declarations spread across lib/ modules. This caused load order issues because:

1. use-package controls when packages load
2. Modules were loaded via `require` before use-package was ready
3. Multiple modules could configure the same package, causing conflicts

## The Solution

**init.el owns ALL use-package declarations.** This is the load order contract.

**lib/ modules provide:**
- `defvar` / `defcustom` for configuration variables
- Setup functions called from init.el's use-package `:config` blocks
- Interactive functions (autoloaded or called on demand)
- Keymaps (bound in init.el)

## Module Pattern

```elisp
;;; hypermodern-foo.el -*- lexical-binding: t; -*-
;; NO use-package in modules!

(defvar hypermodern/foo-setting "default"
  "Configuration variable.")

(defun hypermodern/foo-setup ()
  "Configure foo. Call from (use-package foo :config ...)."
  (setq foo-option hypermodern/foo-setting))

(defun hypermodern/foo-do-thing ()
  "Interactive command."
  (interactive)
  (require 'foo)
  ...)

(defvar hypermodern/foo-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'hypermodern/foo-do-thing)
    map)
  "Keymap for foo commands.")

(provide 'hypermodern-foo)
```

## init.el Pattern

```elisp
(require 'hypermodern-foo)  ; loads functions, defvars, keymaps

(use-package foo
  :config
  (hypermodern/foo-setup))

(global-set-key (kbd "C-c f") hypermodern/foo-map)
```

## File Summary

| File | Purpose |
|------|---------|
| init.el | All use-package, load order, keybindings |
| early-init.el | Pre-GUI optimizations |
| lib/hypermodern-core.el | Foundation functions, leader key |
| lib/hypermodern-languages.el | Eglot config, treesit, format |
| lib/hypermodern-terminal.el | vterm, eat, detached setup |
| lib/hypermodern-windows.el | shackle, popper config |
| lib/hypermodern-navigation.el | zoxide, consult-dir |
| lib/hypermodern-compile.el | eat backend, smart compile |
| lib/hypermodern-remote.el | TRAMP black book |
| lib/hypermodern-ai.el | gptel config |
| lib/hypermodern-ui.el | Theme, density, glow control |
| lib/hypermodern-secrets.el | auth-source, agenix |
