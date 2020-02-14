
with import ./. {};

haskell.lib.buildStackProject {
  name = "stack-env";
  buildInputs = [ zlib openssl gmp libffi git haskellPackages.happy ];
  ghc = (import ../shell.nix { inherit pkgs; }).ghc.baseGhc;
  CARDANO_MAINNET_MIRROR =
    "${import ./cardano-mainnet-mirror.nix {inherit pkgs;}}/epochs";
}
