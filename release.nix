############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-ledger-specs ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-ledger-specs.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-ledger-specs;
  gitrev = cardano-ledger-specs.rev;
};

with pkgs.lib;

let
  testsSupportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  # Recurse through an attrset, returning all test derivations in a list.
  collectTests' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the test derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectTests = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectTests' package)
    ) ds);

  jobs = {
    native = mapTestOn (__trace (__toJSON (packagePlatforms project)) (packagePlatforms project));
  } // (mkRequiredJob (
      # collectTests jobs.native.tests ++
      # collectTests jobs.native.benchmarks ++
      [ jobs.native.byronLedgerSpec.x86_64-linux
        jobs.native.byronChainSpec.x86_64-linux
        jobs.native.semanticsSpec.x86_64-linux
        jobs.native.shelleyLedgerSpec.x86_64-linux
        jobs.native.delegationDesignSpec.x86_64-linux
        jobs.native.nonIntegerCalculations.x86_64-linux
        jobs.native.blocksCDDLSpec.x86_64-linux
      ]
    ))

  # Collect all spec PDFs, without system suffix
  // { inherit (project)
         byronLedgerSpec
         byronChainSpec
         semanticsSpec
         shelleyLedgerSpec
         delegationDesignSpec
         nonIntegerCalculations
         blocksCDDLSpec
       ; };

in jobs
