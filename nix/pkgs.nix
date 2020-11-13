let
  haskellNix = import ./haskell-nix.nix {};

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;

in

{ # haskell.nix provides access to the nixpkgs pins which are used by our CI,
  # hence you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'.
  pkgs ? haskellNix.sources.nixpkgs-2009
}:

import pkgs nixpkgsArgs
