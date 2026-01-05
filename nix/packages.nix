{ inputs, lib, ... }:
{
  perSystem = { pkgs, ... }:
    let
      inherit (inputs) self;
      configDir = import ./per-system/config-dir.nix self pkgs;
      wrapWithConfig = import ./per-system/wrap-with-config.nix pkgs configDir;

      themePackages = lib.mapAttrs'
        (_: scheme: {
          name = "theme-${scheme.slug}";
          value = self.lib."mk-base16-theme" pkgs pkgs.emacsPackages scheme;
        })
        self.lib."theme-schemes";
    in
    {
      packages =
        let
          full = wrapWithConfig (self.lib."mk-hypermodern-emacs-with-deps" pkgs);
        in
        {
          # Default includes all runtime deps (LSPs, formatters, sqlite, etc.)
          inherit full;
          default = full;
          bare = self.lib."mk-hypermodern-emacs" pkgs;
          bare-full = self.lib."mk-hypermodern-emacs-with-deps" pkgs;
          libs = self.lib."mk-local-libs" pkgs;
          config = configDir;
          themes = self.lib."mk-all-themes" pkgs pkgs.emacsPackages;
        } // themePackages;
    };
}
