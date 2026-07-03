# Shared values used across the flake modules.
{
  # see flake `variants` below for alternative compilers
  defaultCompiler = "ghc967";
  fourmoluVersion =
    "0.17.0.0"; # Should match the tag used in .github/workflows/haskell.yml
  cabalGildVersion =
    "1.5.0.1"; # Should match the tag used in .github/workflows/haskell.yml
  nixfmtVersion =
    "0.6.0"; # Should match the tag used in .github/workflows/haskell.yml
}
