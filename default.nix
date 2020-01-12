{ compiler ? null
, pkgs ? import <nixpkgs> {}
}:

let
  haskellPackages =
    if builtins.isNull compiler
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
  overriddenPackages = haskellPackages.override {
    overrides = self: super: {
      taskwarrior =
        self.callPackage ./taskwarrior.nix {};
    };
  };
in
  overriddenPackages.callCabal2nix "hasknote" ./. {}
