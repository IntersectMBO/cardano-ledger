{ pkgs ? (import ../../lib.nix).pkgs
}:

with pkgs;

stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math amsmath
                      extarrows cleveref

                      # font libraries `mathpazo` seems to depend on palatino, but it isn't pulled.
                      mathpazo palatino microtype

                      # libraries for marginal notes
                      xargs todonotes

                      # drawing
                      pgf

                      # build tools
                      latexmk

                      ;
                  })
                  gitMinimal
                ];
  src = ./.;
  buildPhase = "make";

  meta = with lib; {
    description = "Delegation Design Specification";
    license = licenses.bsd3;
    platforms = platforms.linux;
  };
}
