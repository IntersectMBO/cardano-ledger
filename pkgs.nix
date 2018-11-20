let
  nixpkgs = builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/38db6fdfb9672a0206a2042447132094bc05a5ea.tar.gz";
in
import nixpkgs {
  config = {
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc861 = pkgs.haskell.packages.ghc861.override {
            overrides = self: super: {
              basement = self.callHackage "basement" "0.0.8" {};
              memory = pkgs.haskell.lib.doJailbreak (self.callHackage "memory" "0.14.18" {});
              small-steps = self.callCabal2nix "small-steps" ./specs/semantics/hs {};
              cs-ledger = pkgs.haskell.lib.overrideCabal (self.callCabal2nix "cs-ledger" ./specs/ledger/hs {}) (old: {
                enableParallelBuilding = false;
              });
              cs-blockchain = self.callCabal2nix "cs-blockchain" ./specs/chain/hs {};
            };
          };
        };
      };
    };
  };
}
