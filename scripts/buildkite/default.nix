with import ../../lib.nix;
with pkgs;

let

  stack-hpc-coveralls = pkgs.haskell.lib.dontCheck
    (haskellPackages.callPackage ./stack-hpc-coveralls.nix {});

  stackRebuild = runCommand "stack-rebuild" {} ''
    ${haskellPackages.ghcWithPackages (ps: [ps.turtle ps.safe ps.transformers])}/bin/ghc -o $out ${./rebuild.hs}
  '';

  buildTools =
    [ git nix gnumake stack gnused gnutar coreutils stack-hpc-coveralls ];

in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath buildTools}
    exec ${stackRebuild} "$@"
  ''
