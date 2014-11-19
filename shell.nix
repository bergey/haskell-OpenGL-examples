{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages }:

let 
  hs = haskellPackages.override {
        extension = self: super: rec {
          hsPkg = pkg: version: self.callPackage "/home/bergey/code/nixHaskellVersioned/${pkg}/${version}.nix" {};
          vinylGl = hsPkg "vinyl-gl" "0.2.1";
          linear = hsPkg "linear" "1.15.1";
          currentPkg = self.callPackage ./. {};
      };
    };
    in pkgs.lib.overrideDerivation hs.currentPkg (attrs: {
        buildInputs = [hs.cabalInstall ] ++ attrs.buildInputs;
 })
