# Build a base16 theme package from a scheme definition
# Returns: pkgs -> epkgs -> scheme -> derivation
pkgs: epkgs: scheme:
let
  themeSym = "base16-${scheme.slug}";
  colorsVar = "${themeSym}-theme-colors";
  p = scheme.palette;
  c = k: builtins.getAttr k p;
in
epkgs.trivialBuild {
  pname = "${themeSym}-theme";
  version = "0.1.0";

  src = pkgs.runCommand "${themeSym}-theme-source" { } ''
    mkdir -p $out
    cat > $out/${themeSym}-theme.el << 'ELISP'
;;; ${themeSym}-theme.el --- Base16 ${scheme.name} theme -*- lexical-binding: t; -*-
(require 'base16-theme)

(defvar ${colorsVar}
  '(:base00 "${c "base00"}" :base01 "${c "base01"}" :base02 "${c "base02"}" :base03 "${c "base03"}"
    :base04 "${c "base04"}" :base05 "${c "base05"}" :base06 "${c "base06"}" :base07 "${c "base07"}"
    :base08 "${c "base08"}" :base09 "${c "base09"}" :base0A "${c "base0A"}" :base0B "${c "base0B"}"
    :base0C "${c "base0C"}" :base0D "${c "base0D"}" :base0E "${c "base0E"}" :base0F "${c "base0F"}"))

(deftheme ${themeSym})
(base16-theme-define '${themeSym} ${colorsVar})
(provide-theme '${themeSym})
(provide '${themeSym}-theme)
;;; ${themeSym}-theme.el ends here
ELISP

    # trivialBuild doesn't extract autoload cookies or auto-load autoloads files,
    # so we manually create an autoloads file that registers the theme directory
    # in custom-theme-load-path. This is loaded by hypermodern-themes-loader.
    cat > $out/${themeSym}-theme-autoloads.el << 'AUTOLOADS'
;;; ${themeSym}-theme-autoloads.el --- automatically extracted autoloads -*- lexical-binding: t -*-
;;; Code:
(when load-file-name
  (add-to-list 'load-path (directory-file-name (file-name-directory load-file-name)))
  (add-to-list 'custom-theme-load-path (file-name-directory load-file-name)))

(provide '${themeSym}-theme-autoloads)
;;; ${themeSym}-theme-autoloads.el ends here
AUTOLOADS
  '';

  packageRequires = [ epkgs.base16-theme ];
}
