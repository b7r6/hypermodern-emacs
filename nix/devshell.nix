{ inputs, ... }:
{
  perSystem = { pkgs, ... }:
    let
      inherit (inputs) self;
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          (self.lib."mk-hypermodern-emacs-with-deps" pkgs)
        ] ++ (self.lib."runtime-deps" pkgs);
      };
    };
}
