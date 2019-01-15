with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math amsmath extarrows cleveref semantic xcolor xstring

                      # bclogo and dependencies
                      bclogo mdframed xkeyval etoolbox needspace

                      # font libraries `mathpazo` seems to depend on palatino, but it isn't pulled.
                      mathpazo palatino microtype

                      # libraries for marginal notes
                      xargs todonotes

                      # Tikz
                      pgf

                      # git info
                      gitinfo2

                      # build tools
                      latexmk

                      ;
                  })
                  gitMinimal
                ];
}
