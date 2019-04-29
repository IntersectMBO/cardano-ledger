{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "small-steps"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-chain";
      url = "";
      synopsis = "Small step semantics";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.containers)
          (hsPkgs.free)
          (hsPkgs.hedgehog)
          (hsPkgs.tasty-hunit)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.transformers)
          ];
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.free)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty-hunit)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.sequence)
            (hsPkgs.transformers)
            (hsPkgs.doctest)
            (hsPkgs.small-steps)
            ];
          build-tools = [ ((hsPkgs.buildPackages).doctest-discover) ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "30f2728662e782c50f2da1f66f4b3159e85c8ff1";
      sha256 = "1zaw311hk9n8kr2phja57hrflfalnniib3mz89761qyf38v7n3kp";
      });
    postUnpack = "sourceRoot+=/byron/semantics/executable-spec; echo source root reset to \$sourceRoot";
    }