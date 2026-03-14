{
  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with
        import nixpkgs { inherit system; };
      let
        eachCompiler = pkgSet: f:
          lib.attrsets.mapAttrs (_: f) pkgSet.haskell.packages;
        this = compiler:
          haskell.lib.justStaticExecutables (compiler.callPackage ./. { });
        packages = {
          dynamic = eachCompiler pkgs this;
          static = eachCompiler pkgsStatic this;
        };
      in
      {
        packages = packages // {
          default = packages.dynamic.ghc910;
          supported = linkFarm "supported" {
            inherit (packages.dynamic) ghc94 ghc96 ghc98 ghc910 ghc912;
          };
        };
        devShells = lib.attrsets.mapAttrsRecursiveCond
          (as: !(lib.isDerivation as))
          (_: p: p.env or p)
          self.packages.${system};
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://neil-mayhew.cachix.org"
    ];
    extra-trusted-public-keys = [
      "neil-mayhew.cachix.org-1:mxrzBmebKDFyT7RzZom+8uhFochoTk6BL/1UTBU64eY="
    ];
  };
}
