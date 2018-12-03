with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math amsmath extarrows cleveref semantic xcolor

                      # bclogo and dependencies
                      bclogo mdframed xkeyval etoolbox needspace

                      # font libraries `mathpazo` seems to depend on palatino, but it isn't pulled.
                      mathpazo palatino microtype

                      # libraries for marginal notes
                      xargs todonotes

                      # build tools
                      latexmk

                      # packages for lhs2tex
                      polytable lazylist

                      ;
                  })
                  haskellPackages.lhs2tex
                ];
}
