{ system ? builtins.currentSystem
, config ? {}
, pkgs ? import ../../pkgs.nix
}:

with pkgs;

stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math amsmath
                      extarrows cleveref semantic
                      polytable lazylist

                      # font libraries `mathpazo` seems to depend on palatino, but it isn't pulled.
                      mathpazo palatino microtype

                      # libraries for marginal notes
                      xargs todonotes

                      # build tools
                      latexmk

                      ;
                  })

                   pkgs.haskellPackages.lhs2tex

                ];
  src = ./.;
  buildPhase = "make";
  installPhase = "mkdir $out; cp *.pdf $out/";

  meta = with lib; {
    description = "An Accounting Model for Cardano";
    license = licenses.bsd3;
    platforms = platforms.linux;
  };
}
