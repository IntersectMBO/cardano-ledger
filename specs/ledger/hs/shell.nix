with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    cabal-install
    haskellPackages.ghcid
    haskell.compiler.ghc861
  ];
}
