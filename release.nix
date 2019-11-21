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
, projectArgs ? { config = { allowUnfree = false; inHydra = true; }; }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Import IOHK common nix lib
, iohkLib ? import ./nix/iohk-common.nix {}
}:

with (import iohkLib.release-lib) {
  inherit (import ./nix/iohk-common.nix {}) pkgs;

  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-ledger-specs;
  gitrev = cardano-ledger-specs.rev;
};

with pkgs.lib;

let
  testsSupportedSystems = [ "x86_64-linux" ];
  collectTests = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);

  jobs = {
    native = mapTestOn (packagePlatforms project);
  } // (mkRequiredJob (
      # collectTests jobs.native.tests ++
      # collectTests jobs.native.benchmarks ++
      [ jobs.native.byronLedgerSpec.x86_64-linux
        jobs.native.byronChainSpec.x86_64-linux
        jobs.native.semanticsSpec.x86_64-linux
        jobs.native.shelleyLedgerSpec.x86_64-linux
        jobs.native.delegationDesignSpec.x86_64-linux
        jobs.native.nonIntegerCalculations.x86_64-linux
      ]
    ))

  # Collect all spec PDFs, without system suffix
  // { inherit (project)
         byronLedgerSpec
         byronChainSpec
         semanticsSpec
         shelleyLedgerSpec
         delegationDesignSpec
         nonIntegerCalculations; }

  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms { inherit (project) shell; });

in jobs
