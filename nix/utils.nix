# Shared values and helpers used across the flake modules.
{ pkgs, lib }: {
  # see the flake `variants` in outputs.nix for alternative compilers
  defaultCompiler = "ghc967";
  fourmoluVersion =
    "0.17.0.0"; # Should match the tag used in .github/workflows/haskell.yml
  cabalGildVersion =
    "1.5.0.1"; # Should match the tag used in .github/workflows/haskell.yml
  nixfmtVersion =
    "0.6.0"; # Should match the tag used in .github/workflows/haskell.yml

  # From the iogx flake template: aggregate every derivation in `hydraJobs`
  # into the single job Hydra reports CI status from.
  makeHydraRequiredJob = hydraJobs:
    let
      cleanJobs =
        lib.filterAttrsRecursive (name: _: name != "recurseForDerivations")
        (removeAttrs hydraJobs [ "required" ]);
    in pkgs.releaseTools.aggregate {
      name = "required";
      meta.description = "All jobs required to pass CI";
      constituents = lib.collect lib.isDerivation cleanJobs;
    };
}
