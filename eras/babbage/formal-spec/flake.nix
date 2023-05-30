{
  outputs = inputs: {
    packages =
      builtins.mapAttrs (_: {nixpkgs, ...}: {
        default = with nixpkgs;
          buildLatex {
            name = "babbage-spec";
            texFiles = ["babbage-ledger"];
            meta = with lib; {
              description = "Babbage ledger specification";
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
