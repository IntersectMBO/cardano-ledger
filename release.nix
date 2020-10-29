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
  testsSupportedSystems = [ "x86_64-linux" ];
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
      collectTests jobs.native.libs ++
      collectTests jobs.native.exes ++
      collectTests jobs.native.checks.tests ++
      collectTests jobs.native.benchmarks ++
      mapAttrsToList (_: spec: spec.x86_64-linux or null) jobs.native.specs
    ))

  // {
    # Collect all spec PDFs, without system suffix.
    specs = removeAttrs project.specs ["recurseForDerivations"];
    # Compatibility with old names
    byronLedgerSpec = project.specs.byron-ledger;
    byronChainSpec = project.specs.byron-chain;
    semanticsSpec = project.specs.small-step-semantics;
    shelleyLedgerSpec = project.specs.shelley-ledger;
    delegationDesignSpec = project.specs.delegation-design;
    nonIntegerCalculations = project.specs.non-integer-calculations;
    blocksCDDLSpec = project.specs.blocks-cddl;
    # Ensure everything in the shell is cached
    shell = project.shell;
  };

in jobs
