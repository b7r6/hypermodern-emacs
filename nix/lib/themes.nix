{ self, lib }:
let
  ono = import "${self}/themes/ono-sendai.nix";
  maas = import "${self}/themes/maas.nix";
  all = ono // maas;

  schemes = lib.filterAttrs (_: v: (v ? palette) && (v ? slug) && (v ? name)) all;
in
{
  inherit
    ono
    maas
    all
    schemes
    ;
}
