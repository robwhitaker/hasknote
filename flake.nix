{
  description = "A very basic flake";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nixpkgs = {
      url = "github:NixOS/nixpkgs/release-22.11";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-compat, nixpkgs, flake-utils }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;
        overriddenPackages = haskellPackages.override {
          overrides = self: super: {
            hasknote = self.callCabal2nix "hasknote" ./. { };
          };
        };
      in {
        packages.default = overriddenPackages.hasknote;
        devShells.default = overriddenPackages.shellFor {
          packages = p: [ p.hasknote ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hls-splice-plugin
            hls-eval-plugin
          ];
        };
      });
}
