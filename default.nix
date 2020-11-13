let
  project = import nix/project.nix {};

in
  project.asmJsonCpp-exe
