{
  outputs = inputs: {
    packages =
      builtins.mapAttrs (_: {nixpkgs, ...}: {
        default = with nixpkgs;
          buildLatex {
            name = "alonzo-spec";
            texFiles = ["alonzo-ledger"];
            meta = with lib; {
              description = "Goguen ledger specification";
              license = licenses.asl20;
              platforms = platforms.linux;
            };
            src = filterLatex ./.;

            texInputs = {
              inherit
                (texlive)
                scheme-small
                collection-latexextra
                collection-latexrecommended
                collection-mathscience
                bclogo
                ;
            };
          };
      })
      (inputs.main or (import ../../../nix/flake-compat.nix).defaultNix).legacyPackages;
  };
}
