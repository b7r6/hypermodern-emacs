{ inputs, lib, ... }:
{
  imports = [
    ./packages.nix
    ./apps.nix
    ./checks.nix
    ./devshell.nix
  ];

  flake.lib = import ./lib.nix {
    self = inputs.self;
    inherit lib;
  };

  flake.homeManagerModules.default = import ./home-manager/module.nix {
    self = inputs.self;
    inherit lib;
  };
}
