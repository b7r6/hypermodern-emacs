{ inputs, ... }:
{
  perSystem = { pkgs, ... }:
    let
      inherit (inputs) self;
    in
    {
      checks = {
        build = self.lib."mk-hypermodern-emacs" pkgs;
        build-full = self.lib."mk-hypermodern-emacs-with-deps" pkgs;

        test = pkgs.runCommand "hypermodern-emacs-test"
          { nativeBuildInputs = [ (self.lib."mk-hypermodern-emacs" pkgs) ]; }
          ''
            export HOME=$(mktemp -d)
            emacs --batch ${self.lib."test-load-args" self}
            touch $out
          '';

        byte-compile = pkgs.runCommand "hypermodern-emacs-byte-compile"
          { nativeBuildInputs = [ (self.lib."mk-hypermodern-emacs" pkgs) ]; }
          ''
            export HOME=$(mktemp -d)
            ${self.lib."byte-compile-script" self}
            touch $out
          '';
      };
    };
}
