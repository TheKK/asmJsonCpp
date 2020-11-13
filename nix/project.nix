{ pkgs ? import ./pkgs.nix {}
}:

let
  musl-pkgs = pkgs.pkgsCross.musl64;

  mkHaskellPkgs = { haskell-nix }:
    haskell-nix.project {
      # 'cleanGit' cleans a source directory based on the files known by git
      src = haskell-nix.haskellLib.cleanGit {
        name = "asmJsonCpp";
        src = ../.;
      };
    };

  haskellPkgs = pkgs.callPackage mkHaskellPkgs {};
  musl-haskellPkgs = musl-pkgs.callPackage mkHaskellPkgs {};

  inherit (haskellPkgs.asmJsonCpp.components.exes)
    asmJsonCpp-exe
    asmJsonCpp-server
  ;

  static-asmJsonCpp-exe = musl-haskellPkgs.asmJsonCpp.components.exes.asmJsonCpp-exe
    .overrideAttrs (
      self: {
        # XXX This remove unused dependencies and binary still being "not stripped".
        dontStrip = false;
      });
  static-asmJsonCpp-server = musl-haskellPkgs.asmJsonCpp.components.exes.asmJsonCpp-server
    .overrideAttrs (
      self: {
        # XXX This remove unused dependencies and binary still being "not stripped".
        dontStrip = false;
      });

in
{
  inherit asmJsonCpp-exe;
  inherit asmJsonCpp-server;
  inherit static-asmJsonCpp-exe;
  inherit static-asmJsonCpp-server;
}
