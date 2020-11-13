{ pkgs ? import ./pkgs.nix {}
}:

with pkgs;

let
  project = import ./project.nix { pkgs = pkgs.pkgsCross.musl64; };
  inherit (project) static-asmJsonCpp-server;

in
dockerTools.buildLayeredImage {
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
}
