{ self, lib }:
pkgs:
pkgs.symlinkJoin {
  name = "hypermodern-emacs-libs";
  
  paths = builtins.attrValues (
    builtins.mapAttrs (
      name: _:
      pkgs.writeTextFile {
        inherit name;
        text = builtins.readFile "${self}/lib/${name}";
        destination = "/share/emacs/site-lisp/${name}";
      }
    ) (lib.filterAttrs (n: _: lib.hasSuffix ".el" n) (builtins.readDir "${self}/lib"))
  );
}
