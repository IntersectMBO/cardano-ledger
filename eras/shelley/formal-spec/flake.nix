{
  outputs = inputs: {
    packages =
      builtins.mapAttrs (_: {nixpkgs, ...}: {
        default = with nixpkgs;
          buildLatex {
            name = "shelley-ledger-spec";
            texFiles = ["shelley-ledger"];
            meta = with lib; {
              description = "Shelley Ledger Specification";
              license = licenses.asl20;
              platforms = platforms.linux;
            };
            src = filterLatex ./.;

            texInputs = {
              inherit
                (texlive)
                scheme-small
                # libraries
                
                stmaryrd
                lm-math
                amsmath
                extarrows
                cleveref
                semantic
                xcolor
                xstring
                paralist
                cm-super
                tocloft
                # bclogo and dependencies
                
                bclogo
                mdframed
                xkeyval
                etoolbox
                needspace
                # font libraries `mathpazo` seems to depend on palatino, but it isn't pulled.
                
                mathpazo
                palatino
                microtype
                # libraries for marginal notes
                
                xargs
                todonotes
                # Tikz
                
                pgf
                tikz-cd
                # build tools
                
                latexmk
                # Referencing
                
                zref
                ;
            };
            buildInputs = [gitMinimal];
          };
      })
      (inputs.main or (import ../../../nix/flake-compat.nix).defaultNix).legacyPackages;
  };
}
