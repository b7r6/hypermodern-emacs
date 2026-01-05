# Core Emacs packages for hypermodern-emacs
# Returns: pkgs -> epkgs -> [package]
#
# Philosophy: install what's needed, configure in elisp
# LSP: eglot (built-in) for everything except lean4 (needs lsp-mode)
#
pkgs: epkgs:
let
  lean4-mode = epkgs.melpaBuild {
    pname = "lean4-mode";
    version = "1.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "leanprover-community";
      repo = "lean4-mode";
      rev = "master";
      sha256 = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
    };
    packageRequires = with epkgs; [ dash f flycheck lsp-mode magit-section s ];
    recipe = pkgs.writeText "recipe" ''
      (lean4-mode :repo "leanprover-community/lean4-mode" :fetcher github :files ("*.el" "data"))
    '';
  };
in
with epkgs; [
  # ─────────────────────────────────────────────────────────────
  # Theme framework
  # ─────────────────────────────────────────────────────────────
  base16-theme

  # ─────────────────────────────────────────────────────────────
  # UI chrome
  # ─────────────────────────────────────────────────────────────
  doom-modeline
  nerd-icons
  dashboard

  # ─────────────────────────────────────────────────────────────
  # Completion & navigation (vertico stack)
  # ─────────────────────────────────────────────────────────────
  vertico
  orderless
  marginalia
  consult
  embark
  embark-consult

  # ─────────────────────────────────────────────────────────────
  # Keybinding discovery
  # ─────────────────────────────────────────────────────────────
  general
  which-key

  # ─────────────────────────────────────────────────────────────
  # Window management
  # ─────────────────────────────────────────────────────────────
  popper      # popup buffer tracking - battle-tested
  shackle     # display-buffer rules - battle-tested
  ace-window

  # ─────────────────────────────────────────────────────────────
  # Visual polish
  # ─────────────────────────────────────────────────────────────
  dimmer
  solaire-mode
  ligature
  mixed-pitch
  olivetti
  rainbow-mode

  # ─────────────────────────────────────────────────────────────
  # Completion at point
  # ─────────────────────────────────────────────────────────────
  company
  yasnippet

  # ─────────────────────────────────────────────────────────────
  # Navigation & search
  # ─────────────────────────────────────────────────────────────
  avy
  fzf
  deadgrep
  rg
  wgrep
  wgrep-deadgrep
  zoxide        # frecency-based directory jumping
  consult-dir   # unified directory switching

  # ─────────────────────────────────────────────────────────────
  # Language modes
  # ─────────────────────────────────────────────────────────────
  # Web
  typescript-mode
  js2-mode
  web-mode
  yaml-mode
  markdown-mode
  json-mode

  # Systems
  dockerfile-mode
  nix-mode
  rust-mode
  cuda-mode
  zig-mode
  nasm-mode
  bazel

  # .NET
  csharp-mode
  fsharp-mode

  # Functional
  haskell-mode
  lean4-mode

  # ─────────────────────────────────────────────────────────────
  # LSP - eglot is built-in, lsp-mode only for lean4
  # ─────────────────────────────────────────────────────────────
  lsp-mode  # lean4-mode needs this

  # ─────────────────────────────────────────────────────────────
  # Structural editing
  # ─────────────────────────────────────────────────────────────
  paredit
  paredit-everywhere
  smartparens
  expand-region
  multiple-cursors

  # ─────────────────────────────────────────────────────────────
  # Core utilities
  # ─────────────────────────────────────────────────────────────
  exec-path-from-shell
  gcmh
  undo-fu
  undo-fu-session
  helpful

  # ─────────────────────────────────────────────────────────────
  # Terminal & env
  # ─────────────────────────────────────────────────────────────
  vterm
  eat
  detached
  clipetty
  direnv

  # ─────────────────────────────────────────────────────────────
  # Secrets
  # ─────────────────────────────────────────────────────────────
  pass

  # ─────────────────────────────────────────────────────────────
  # Git
  # ─────────────────────────────────────────────────────────────
  magit

  # ─────────────────────────────────────────────────────────────
  # AI
  # ─────────────────────────────────────────────────────────────
  gptel

  # ─────────────────────────────────────────────────────────────
  # Tree-sitter & formatting
  # ─────────────────────────────────────────────────────────────
  treesit-auto
  format-all

  # ─────────────────────────────────────────────────────────────
  # External integration
  # ─────────────────────────────────────────────────────────────
  atomic-chrome
  elfeed
  pdf-tools
  nov
]
