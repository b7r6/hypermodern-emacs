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

  mk-all-themes = pkgs: epkgs:
    pkgs.symlinkJoin {
      name = "hypermodern-emacs-themes";
      paths = lib.mapAttrsToList (_: mk-base16-theme pkgs epkgs) theme-schemes;
    };

  mk-hypermodern-emacs = pkgs:
    let
      emacsPackages = pkgs.emacsPackagesFor pkgs.emacs30-pgtk;
      themePkgs = lib.mapAttrsToList (_: scheme: mk-base16-theme pkgs emacsPackages scheme) theme-schemes;
      themeLoader = mk-theme-loader pkgs emacsPackages theme-schemes;
    in
    emacsPackages.emacsWithPackages (epkgs:
      (core-packages pkgs epkgs) ++ themePkgs ++ [ themeLoader ]
    );

  mk-hypermodern-emacs-with-deps = pkgs:
    let
      emacs = mk-hypermodern-emacs pkgs;
      deps = runtime-deps pkgs;
    in
    pkgs.symlinkJoin {
      name = "hypermodern-emacs-full";
      paths = [ emacs ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/emacs --prefix PATH : ${lib.makeBinPath deps}
        wrapProgram $out/bin/emacsclient --prefix PATH : ${lib.makeBinPath deps}
      '';
      passthru = { inherit deps; unwrapped = emacs; };
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
  "mk-hypermodern-emacs" = mk-hypermodern-emacs;
  "mk-hypermodern-emacs-with-deps" = mk-hypermodern-emacs-with-deps;
}
