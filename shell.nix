{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellngPackages }:

let 
  hs = haskellPackages.override {
        overrides = self: super: rec {
          hsPkg = pkg: version: self.callPackage "/home/bergey/code/nixHaskellVersioned/${pkg}/${version}.nix" {};
          # required, not in Nix
          # version pins
          test-framework-quickcheck2 = pkgs.haskell-ng.lib.dontCheck super.test-framework-quickcheck2;
          singletons = hsPkg "singletons" "1.1";
          th-desugar = hsPkg "th-desugar" "1.5";
          # HEAD packages
          # self
          thisPackage = self.callPackage ./. {};
      };
    };
  in hs.thisPackage.env
