# Wrap an emacs derivation to use our config directory
# Returns: pkgs -> configDir -> emacsPkg -> derivation
pkgs: configDir: emacsPkg:
let
  # Preserve PATH from wrapped emacs if it has deps
  pathPrefix = if emacsPkg ? deps then
    "--prefix PATH : ${pkgs.lib.makeBinPath emacsPkg.deps}"
  else "";
in
pkgs.runCommand "hypermodern-emacs-configured" {
  nativeBuildInputs = [ pkgs.makeWrapper ];
  meta.mainProgram = "emacs";
} ''
  mkdir -p $out/bin
  makeWrapper ${emacsPkg}/bin/emacs $out/bin/emacs \
    ${pathPrefix} \
    --add-flags "--init-directory=${configDir} --load ${configDir}/init.el"

  # Wrap emacsclient with same PATH
  ${if emacsPkg ? deps then ''
    makeWrapper ${emacsPkg}/bin/emacsclient $out/bin/emacsclient \
      --prefix PATH : ${pkgs.lib.makeBinPath emacsPkg.deps}
  '' else ''
    ln -s ${emacsPkg}/bin/emacsclient $out/bin/emacsclient
  ''}

  ln -s ${emacsPkg}/bin/emacs $out/bin/emacs-unwrapped
''
