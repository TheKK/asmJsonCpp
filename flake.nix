{
  description = "AsmJsonCpp";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "haskell-nix/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskell-nix.overlay
          (final: prev: {
            asmJsonCpp = final.haskell-nix.stackProject' {
              src = pkgs.haskell-nix.haskellLib.cleanGit {
                name = "asmJsonCpp";
                src = ./.;
              };
            };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; };
        hsPkgs = pkgs.haskellPackages;

        asmJsonCpp = pkgs.asmJsonCpp;
        asmJsonCpp-flake = asmJsonCpp.flake { };

        # TODO Still not enough and should be fixed in haskell.nix.
        gc-roots = [ asmJsonCpp.stack-nix ];

      in asmJsonCpp-flake // {
        defaultPackage =
          asmJsonCpp-flake.packages."asmJsonCpp:exe:asmJsonCpp-exe";
        defaultApp = asmJsonCpp-flake.apps."asmJsonCpp:exe:asmJsonCpp-exe";
        devShell = asmJsonCpp.shellFor {
          withHoogle = false;
          tools = {
            cabal = "3.2.0.0";
            haskell-language-server = "1.0.0.0";
            ghcid = "0.8.7";
          };
          nativeBuildInputs = [ hsPkgs.hpack pkgs.ormolu ] ++ gc-roots;
          exactDeps = true;
        };
      });
}
