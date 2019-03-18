{ system ? builtins.currentSystem
, config ? {}
, pkgs ? import (import ../nix/fetch-nixpkgs.nix) { inherit system config; }
, buildTools ? with pkgs; [ git nix gnumake ]
}:

with pkgs.lib;
with pkgs;

let
  cache-s3 = callPackage ./cache-s3.nix {};

  stack-hpc-coveralls = pkgs.haskell.lib.dontCheck
    (haskell.packages.ghc844.callPackage ./stack-hpc-coveralls.nix {});

  stackRebuild = runCommand "stack-rebuild" {} ''
    ${haskellPackages.ghcWithPackages (ps: [ps.turtle ps.safe ps.transformers])}/bin/ghc -o $out ${./rebuild.hs}
  '';

in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath ([ cache-s3 stack gnused gnutar coreutils stack-hpc-coveralls ] ++ buildTools)}
    exec ${stackRebuild} "$@"
  ''
