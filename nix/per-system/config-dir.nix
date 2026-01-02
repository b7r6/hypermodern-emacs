# Build a config directory with init.el and libs
# Returns: self -> pkgs -> derivation
self: pkgs:
pkgs.runCommand "hypermodern-emacs-config" { } ''
  mkdir -p $out/lib $out/themes
  cp ${self}/init.el $out/init.el
  cp ${self}/early-init.el $out/early-init.el
  for f in ${self}/lib/*.el; do cp "$f" $out/lib/; done
  for f in ${self}/themes/*.el; do cp "$f" $out/themes/; done
''
