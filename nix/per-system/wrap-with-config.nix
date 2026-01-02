# Wrap an emacs derivation to use our config directory
# Returns: pkgs -> configDir -> emacsPkg -> derivation
pkgs: configDir: emacsPkg:
pkgs.runCommand "hypermodern-emacs-configured" {
  nativeBuildInputs = [ pkgs.makeWrapper ];
  meta.mainProgram = "emacs";
} ''
  mkdir -p $out/bin
  makeWrapper ${emacsPkg}/bin/emacs $out/bin/emacs \
    --add-flags "--init-directory=${configDir} --load ${configDir}/init.el"
  ln -s ${emacsPkg}/bin/emacsclient $out/bin/emacsclient
  ln -s ${emacsPkg}/bin/emacs $out/bin/emacs-unwrapped
''
