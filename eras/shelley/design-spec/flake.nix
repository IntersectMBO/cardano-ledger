{
  outputs = inputs: {
    packages =
      builtins.mapAttrs (_: {nixpkgs, ...}: {
        default = with nixpkgs;
          buildLatex {
            name = "delegation-design-spec";
            texFiles = ["shelley-delegation"];
            meta = with lib; {
              description = "Delegation Design Specification";
              license = licenses.asl20;
              platforms = platforms.linux;
            };
            src = filterLatex ./.;

            texInputs = {
              inherit
                (texlive)
                scheme-small
                # fonts
                
                cm-super
                # libraries
                
                stmaryrd
                lm-math
                amsmath
                extarrows
                cleveref
                titlesec
                # font libraries `mathpazo` seems to depend on palatino, but it isn't pulled.
                
                mathpazo
                palatino
                microtype
                # libraries for marginal notes
                
                xargs
                todonotes
                # drawing
                
                pgf
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
