{
  description = "AsmJsonCpp";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = haskell-nix.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages;

        haskellNix = pkgs.haskell-nix.stackProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "asmJsonCpp";
            src = ./.;
          };
        };

        asmJsonCpp-exe = haskellNix.asmJsonCpp.components.exes.asmJsonCpp-exe;
        asmJsonCpp-server =
          haskellNix.asmJsonCpp.components.exes.asmJsonCpp-server;

        asmJsonCpp-gc-root = haskellNix.asmJsonCpp.project.roots;

      in rec {
        packages = flake-utils.lib.flattenTree {
          inherit asmJsonCpp-exe;
          inherit asmJsonCpp-server;
        };

        defaultPackage = packages.asmJsonCpp-exe;

        apps = {
          asmJsonCpp-exe = flake-utils.lib.mkApp {
            drv = packages.asmJsonCpp-exe;
            exePath = "/bin/asmJsonCpp-exe";
          };
          asmJsonCpp-server = flake-utils.lib.mkApp {
            drv = packages.asmJsonCpp-server;
            exePath = "/bin/asmJsonCpp-server";
          };
        };

        defaultApp = apps.asmJsonCpp-exe;

        devShell = haskellNix.shellFor {
          packages = p: [ p.asmJsonCpp ];
          withHoogle = false;
          tools = {
            cabal = "3.2.0.0";
            haskell-language-server = "1.0.0.0";
            ghcid = "0.8.7";
          };
          nativeBuildInputs = [ hsPkgs.hpack pkgs.ormolu asmJsonCpp-gc-root ];
          exactDeps = true;
        };
      });
}
