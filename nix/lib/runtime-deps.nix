# Runtime dependencies for LSPs, formatters, and tools
# Returns: pkgs -> [package]
pkgs: with pkgs; [
  # Language servers
  basedpyright
  nodePackages.typescript-language-server
  rust-analyzer
  clang-tools
  haskell-language-server
  nixd
  zls
  nodePackages.bash-language-server
  marksman
  nodePackages.vscode-json-languageserver
  yaml-language-server

  # Formatters
  nixfmt-rfc-style
  nodePackages.prettier
  rustfmt
  ormolu

  # Build tools
  bazel gnumake cmake

  # Search & navigation
  ripgrep fd fzf zoxide

  # Git
  git delta

  # Database (for forge, org-roam, etc.)
  sqlite

  # Env management
  direnv nix-direnv

  # Tree-sitter grammars - all available (excluding broken ones)
] ++ (builtins.filter
  (g: !(g.meta.broken or false))
  (builtins.attrValues (builtins.removeAttrs pkgs.tree-sitter-grammars ["recurseForDerivations"]))
) ++ [

  # Fonts - classic coding fonts
  source-code-pro
  fira-code
  fira-mono
  jetbrains-mono
  ibm-plex
  hack-font
  inconsolata
  dejavu_fonts
  cascadia-code
  iosevka
  victor-mono
  fantasque-sans-mono
  ubuntu-classic
]
