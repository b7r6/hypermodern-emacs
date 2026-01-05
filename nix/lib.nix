# Hypermodern Emacs library functions
# Exposed as flake.lib, used internally via inputs.self.lib."kebab-name"
{ self, lib }:
let
  theme-lib = import ./lib/themes.nix { inherit self lib; };

  mk-base16-theme = import ./lib/mk-base16-theme.nix;
  mk-theme-loader = import ./lib/mk-theme-loader.nix;
  mk-local-libs = import ./lib/mk-local-libs.nix { inherit self lib; };
  core-packages = import ./lib/core-packages.nix;
  runtime-deps = import ./lib/runtime-deps.nix;
  theme-schemes = theme-lib.schemes;

  mk-all-themes =
    pkgs: epkgs:
    pkgs.symlinkJoin {
      name = "hypermodern-emacs-themes";
      paths = lib.mapAttrsToList (_: mk-base16-theme pkgs epkgs) theme-schemes;
    };

  # Test file loading args - shared between apps.test and checks.test
  test-load-args = self: ''
    -L ${self} -L ${self}/lib -L ${self}/test \
    -l ert \
    -l early-init \
    -l test-init \
    -l hypermodern-core-test -l hypermodern-secrets-test \
    -l hypermodern-compile-test \
    -l hypermodern-languages-test -l hypermodern-remote-test \
    -l hypermodern-terminal-test -l hypermodern-integration-test \
    -l hypermodern-build-test -l hypermodern-wiring-test \
    -l hypermodern-windows-test -l hypermodern-navigation-test \
    -f ert-run-tests-batch-and-exit'';

  # Byte-compile script args - shared between apps.byte-compile and checks.byte-compile
  byte-compile-script = self: ''
    workdir=$(mktemp -d)
    trap "rm -rf $workdir" EXIT
    cp ${self}/lib/*.el "$workdir/"
    emacs --batch \
      -L "$workdir" \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile "$workdir"/*.el'';

  mk-hypermodern-emacs =
    pkgs:
    let
      emacsPackages = pkgs.emacsPackagesFor pkgs.emacs30-pgtk;
      themePkgs = lib.mapAttrsToList (_: scheme: mk-base16-theme pkgs emacsPackages scheme) theme-schemes;
      themeLoader = mk-theme-loader pkgs emacsPackages theme-schemes;
    in
    emacsPackages.emacsWithPackages (epkgs: (core-packages pkgs epkgs) ++ themePkgs ++ [ themeLoader ]);

  mk-hypermodern-emacs-with-deps =
    pkgs:
    let
      emacs = mk-hypermodern-emacs pkgs;
      deps = runtime-deps pkgs;
      # Create directory with all tree-sitter grammars in Emacs-compatible format
      # Filter out broken grammars (e.g., tree-sitter-razor)
      allGrammars = builtins.filter
        (g: !(g.meta.broken or false))
        (builtins.attrValues (builtins.removeAttrs pkgs.tree-sitter-grammars ["recurseForDerivations"]));
      grammarLinks = lib.concatMapStringsSep "\n" (grammar:
        let
          # Remove tree-sitter- prefix and replace / with - (for scoped packages like @tlaplus/tlaplus)
          rawName = lib.removePrefix "tree-sitter-" grammar.pname;
          name = builtins.replaceStrings ["/"] ["-"] rawName;
        in "ln -s ${grammar}/parser $out/libtree-sitter-${name}.so"
      ) allGrammars;
      treesitterGrammars = pkgs.runCommand "treesitter-grammars-emacs" { } ''
        mkdir -p $out
        ${grammarLinks}
      '';
    in
    pkgs.symlinkJoin {
      name = "hypermodern-emacs-full";
      paths = [ emacs ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/emacs \
          --prefix PATH : ${lib.makeBinPath deps} \
          --prefix TREE_SITTER_DIR : ${treesitterGrammars}
        wrapProgram $out/bin/emacsclient \
          --prefix PATH : ${lib.makeBinPath deps}
      '';
      passthru = {
        inherit deps;
        unwrapped = emacs;
      };
    };
in
{
  themes = theme-lib.all;
  "theme-schemes" = theme-schemes;
  "mk-base16-theme" = mk-base16-theme;
  "mk-local-libs" = mk-local-libs;
  "core-packages" = core-packages;
  "runtime-deps" = runtime-deps;
  "mk-all-themes" = mk-all-themes;
  "test-load-args" = test-load-args;
  "byte-compile-script" = byte-compile-script;
  "mk-hypermodern-emacs" = mk-hypermodern-emacs;
  "mk-hypermodern-emacs-with-deps" = mk-hypermodern-emacs-with-deps;
}
