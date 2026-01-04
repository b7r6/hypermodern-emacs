# Creates a package that loads all hypermodern theme autoloads
#
# Why this exists: trivialBuild doesn't automatically load autoloads files at startup.
# This package explicitly requires each theme's autoloads file, which registers the
# theme directories in custom-theme-load-path, making themes discoverable via
# (custom-available-themes) and loadable via (load-theme).
#
# Alternative: Use elpaBuild/melpaBuild which handle autoloads properly, but would
# require more complex packaging setup.
pkgs: epkgs: themeSchemes:
let
  themeSyms = builtins.map (name: "base16-${themeSchemes.${name}.slug}") (builtins.attrNames themeSchemes);
  requireStatements = builtins.concatStringsSep "\n" (builtins.map (sym: "(require '${sym}-theme-autoloads nil t)") themeSyms);
in
epkgs.trivialBuild {
  pname = "hypermodern-themes-loader";
  version = "0.1.0";

  src = pkgs.runCommand "hypermodern-themes-loader-source" {} ''
    mkdir -p $out
    cat > $out/hypermodern-themes-loader.el << 'EOF'
;;; hypermodern-themes-loader.el --- Load all hypermodern theme autoloads -*- lexical-binding: t; -*-
;;; Commentary:
;; Automatically loads all hypermodern theme autoloads to make them discoverable.
;;; Code:

${requireStatements}

(provide 'hypermodern-themes-loader)
;;; hypermodern-themes-loader.el ends here
EOF
  '';
}
