with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math extarrows cleveref semantic tikz-cd xcolor xstring

                      # bclogo and dependencies
                      bclogo mdframed needspace pgf

                      # font libraries `mathpazo` seems to depend on palatino, but it isn't pulled.
                      mathpazo palatino

                      # libraries for marginal notes
                      xargs todonotes

                      # git info
                      gitinfo2

                      # build tools
                      latexmk

                      ;
                  })
                  gitMinimal
                ];
}
