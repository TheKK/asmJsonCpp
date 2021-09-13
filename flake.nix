{
  description = "AsmJsonCpp";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "haskell-nix/nixpkgs";
    flake-compat = {
      url = "github:edolstra/flake-compat/12c64ca55c1014cdc1b16ed5a804aa8576601ff2";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskell-nix.overlay
          (final: prev: {
            asmJsonCpp = final.haskell-nix.stackProject' {
              src = final.haskell-nix.haskellLib.cleanGit {
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

        musl-asmJsonCpp-flake = asmJsonCpp.projectCross.musl64.flake { };

        static-asmJsonCpp-exe = musl-asmJsonCpp-flake.packages."asmJsonCpp:exe:asmJsonCpp-exe";

        static-asmJsonCpp-server = musl-asmJsonCpp-flake.packages."asmJsonCpp:exe:asmJsonCpp-server"
          .overrideAttrs (
            self: {
              # XXX This remove unused dependencies and binary still being "not stripped".
              dontStrip = false;
            });

        dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "asmJsonCpp-server";
            tag = "latest";
            config = {
              Env = [
                "PORT=8080"
              ];
              Cmd = [ "${static-asmJsonCpp-server}/bin/asmJsonCpp-server" ];
              ExposedPorts = {
                "8080" = {};
              };
            };
          };

        tools = {
            cabal = "3.2.0.0";
            haskell-language-server = "1.0.0.0";
            ghcid = "0.8.7";
        };

        tools-gc-roots =
          let
            toolsDrv = builtins.attrValues (asmJsonCpp.tools tools);
            toolsProjectPlanNix = builtins.map (t: t.project.plan-nix) toolsDrv;
          in toolsDrv ++ toolsProjectPlanNix;
        gc-roots = [ asmJsonCpp.stack-nix asmJsonCpp.roots ] ++ tools-gc-roots;

      in pkgs.lib.recursiveUpdate asmJsonCpp-flake {
        defaultPackage =
          musl-asmJsonCpp-flake.packages."asmJsonCpp:exe:asmJsonCpp-exe";
        defaultApp = musl-asmJsonCpp-flake.apps."asmJsonCpp:exe:asmJsonCpp-exe";
        packages = { inherit dockerImage; static = static-asmJsonCpp-exe; };
        devShell = asmJsonCpp.shellFor {
          withHoogle = false;
          inherit tools;
          nativeBuildInputs = [ hsPkgs.hpack pkgs.ormolu ] ++ gc-roots;
          exactDeps = true;
        };
      });
}
