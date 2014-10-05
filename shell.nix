{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages }:

let 
  tmpHaskellPkgs= haskellPackages.override {
        extension = self: super: {
          GLUtil = self.callPackage /home/bergey/code/nixHaskellVersioned/GLUtil/0.8.2.nix {};
          vinylGl = self.callPackage /home/bergey/code/nixHaskellVersioned/vinyl-gl/0.2.1.nix {};
          currentPkg = self.callPackage ./. {};
      };
    };
  in let
     haskellPackages = tmpHaskellPkgs;
     in pkgs.lib.overrideDerivation haskellPackages.currentPkg (attrs: {
       buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })
