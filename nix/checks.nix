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
            emacs --batch \
              -L ${self} -L ${self}/lib -L ${self}/test \
              -l ert \
              -l early-init \
              -l hypermodern-core -l hypermodern-windows -l hypermodern-navigation \
              -l hypermodern-compile -l hypermodern-languages -l hypermodern-terminal \
              -l hypermodern-remote -l hypermodern-secrets \
              -l hypermodern-core-test -l hypermodern-secrets-test \
              -l hypermodern-compile-test \
              -l hypermodern-languages-test -l hypermodern-remote-test \
              -l hypermodern-terminal-test -l hypermodern-integration-test \
              -l hypermodern-build-test -l hypermodern-wiring-test \
              -l hypermodern-windows-test -l hypermodern-navigation-test \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';

        byte-compile = pkgs.runCommand "hypermodern-emacs-byte-compile"
          { nativeBuildInputs = [ (self.lib."mk-hypermodern-emacs" pkgs) ]; }
          ''
            export HOME=$(mktemp -d)
            workdir=$(mktemp -d)
            cp ${self}/lib/*.el "$workdir/"
            emacs --batch \
              -L "$workdir" \
              --eval "(setq byte-compile-error-on-warn t)" \
              -f batch-byte-compile "$workdir"/*.el
            touch $out
          '';
      };
    };
}
