# setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
# overlays...
{ inputs, system }:

import inputs.nixpkgs {
  overlays = [
    # our local packages.
    (import ./overlay.nix)
    # iohkNix.overlays.crypto provide libsodium-vrf, libblst and libsecp256k1.
    inputs.iohkNix.overlays.crypto
    # haskellNix.overlay can be configured by later overlays, so need to come before them.
    inputs.haskellNix.overlay
    # configure haskell.nix to use iohk-nix crypto librairies.
    inputs.iohkNix.overlays.haskell-nix-crypto
  ];
  inherit system;
  inherit (inputs.haskellNix) config;
}
