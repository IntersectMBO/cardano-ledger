<h1 align="center">Cardano Ledger</h1>

<p align="center">
  <a href="https://buildkite.com/input-output-hk/cardano-ledger-specs">
    <img alt="Build Status" src="https://img.shields.io/buildkite/a94c23758aeb2858869d5e256e466fc78e03a5baf1954cb8cc.svg?style=for-the-badge"/>
  </a>
  <a href="https://coveralls.io/github/input-output-hk/cardano-ledger-specs?branch=master">
    <img alt="Coverage Status" src="https://img.shields.io/coveralls/github/input-output-hk/cardano-ledger-specs.svg?style=for-the-badge"/>
  </a>
</p>

This repository contains the formal specifications, executable models,
and implementations of the Cardano Ledger.

The documents are built in our CI and can be readily accessed using the
following links:

Era | Design Documents | Formal Specification | CDDL
----|------------------|----------------------|-----
Byron | | [Chain Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec "Specification of the Blockchain Layer"), [Ledger Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec "A Formal Specification of the Cardano Ledger") | [CDDL](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/blocksCDDLSpec/latest/download-by-type/doc-pdf/binary)
Shelley | [Design](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec "Design Specification for Delegation and Incentives in Cardano") | [Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/input-output-hk/cardano-ledger-specs/tree/master/shelley/chain-and-ledger/cardano-ledger-shelley-test/cddl-files)
Allegra & Mary | [Multi-Currency](https://eprint.iacr.org/2020/895 "Multi-Currency Ledgers"), [UTXOma](https://iohk.io/en/research/library/papers/utxoma-utxo-with-multi-asset-support/ "UTXOma:UTXO with Multi-Asset Support") | [Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-ma/latest/download-by-type/doc-pdf/shelley-ma "A Formal Specification of the Cardano Ledger with a Native Multi-Asset Implementation") | [CDDL](https://github.com/input-output-hk/cardano-ledger-specs/tree/master/shelley-ma/shelley-ma-test/cddl-files)
Alonzo | [eUTXO](https://iohk.io/en/research/library/papers/the-extended-utxo-model/ "The Extended UTXO Model")| [Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.alonzo-ledger/latest/download-by-type/doc-pdf/alonzo-changes "A Formal Specification of the Cardano Ledger integrating Plutus Core") | [CDDL](https://github.com/input-output-hk/cardano-ledger-specs/tree/master/alonzo/test/cddl-files)

Other Documents:
- [Non-integer calculations specification](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/nonIntegerCalculations/latest/download-by-type/doc-pdf/non-integer-calculations): details on the parts of the Shelley specification that use real numbers.
- [Stake pool ranking specification](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.pool-ranking/latest/download-by-type/doc-pdf/pool-ranking): details for a robust stake pool ranking mechanism.
- [Explanation of the small-step-semantics framework](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/semanticsSpec/latest/download-by-type/doc-pdf/small-step-semantics): a guide to the notation and style used by our ledger rules.

In addition, there is a formalization of the Ledger Specification in Isabelle/HOL which can be found [here](https://github.com/input-output-hk/fm-ledger-formalization).

# Repository structure

The directory structure of this repository is as follows:

- [byron](./byron)
  - [ledger](./byron/ledger)
    - [formal-spec](./byron/ledger/formal-spec)
    - [executable-spec](./byron/ledger/executable-spec)
    - [implementation](./byron/ledger/impl)
  - [chain](./byron/chain)
    - [formal-spec](./byron/chain/formal-spec)
    - [executable-spec](./byron/chain/executable-spec)
  - [cddl](./byron/cddl-spec)
- [shelley](./shelley)
  - [design-spec](./shelley/design-spec)
  - [chain-and-ledger](./shelley/chain-and-ledger) (specs are combined in Shelley era)
    - [formal-spec](./shelley/chain-and-ledger/formal-spec)
    - [implementation](./shelley/chain-and-ledger/executable-spec)
    - [tests](./shelley/chain-and-ledger/cardano-ledger-shelley-test)
    - [dependencies](./shelley/chain-and-ledger/dependencies)
  - [cddl](./shelley/chain-and-ledger/cardano-ledger-shelley-test/cddl-files)
- [Timelocks and Multi-Assets](./shelley-ma)
    - [formal-spec](./shelley-ma/formal-spec)
    - [implementation](./shelley-ma/impl)
    - [tests](./shelley-ma/shelley-ma-test)

## Build tools

For building LaTeX documents we use
[`nix`](https://nixos.org/nix/download.html). Haskell files can be built either
with `nix` or [`cabal`](https://www.haskell.org/cabal/).

When using `nix` it is recommended that you setup the cache, so that it can
reuse built artifacts, reducing the compilation times dramatically:

If you are using [NixOS](https://nixos.org/) add the snippet below to your
`/etc/nixos/configuration.nix`:

```
nix.binaryCaches = [
  "https://cache.nixos.org"
  "https://hydra.iohk.io"
];

nix.binaryCachePublicKeys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
```

If you are using the `nix` package manager next to another operating system put
the following in `/etc/nix/nix.conf` if you have a system-wide `nix`
installation , or in `~/.config/nix/nix.conf` if you have a local installation:

```
substituters        = https://hydra.iohk.io https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

## Building the LaTeX documents and executable specifications

When using `nix` the documents and Haskell code can be readily
built by running:

```shell
nix build
```

The LaTeX documents will be places inside directories named `result*`, e.g.:

```shell
result-2/ledger-spec.pdf
result-3/delegation_design_spec.pdf
result-4/non-integer-calculations.pdf
result-5/small-step-semantics.pdf
result-6/ledger-spec.pdf
result/blockchain-spec.pdf
```


## Building individual LaTeX documents


Change to the latex directory where the latex document is (e.g. `shelley/chain-and-ledger/formal-spec`
for the ledger specification corresponding to the Shelley release, or
`byron/ledger/formal-spec` for the ledger specification corresponding to
the Byron release). Then, build the latex document by running:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch"
```

## Testing the Haskell programs

The tests can be run with cabal.
For example the Shelley tests can be run with:

```shell
cabal test cardano-ledger-shelley-test
```

**Note** that the tests in `cardano-ledger-shelley-test` require two Ruby gems,
[cbor-diag](https://rubygems.org/gems/cbor-diag) and
[cddl](https://rubygems.org/gems/cddl).

It can be helpful to use the `--test-show-details=streaming` option for seeing
the output of the tests while they run:

```shell
cabal test cardano-ledger-shelley-test --test-show-details=streaming
```

### Running Specific Tests

The test suites use [Tasty](https://github.com/feuerbach/tasty),
which allows for running specific tests.
This is done by passing the `-p` flag to the test program, followed by an `awk` pattern.
You can alternatively use the `TASTY_PATTERN` environment variable with a pattern.
For example, the Shelley golden tests can be run with:

```shell
cabal test cardano-ledger-shelley-test --test-options="-p golden"
```

or

```shell
TASTY_PATTERN=golden cabal test cardano-ledger-shelley-test
```

`Tasty` allows for more
[complex patterns](https://github.com/feuerbach/tasty#patterns).
For instance, to run only the Byron update mechanism tests for the ledger
that classify traces, we can pass the
`-p $1 ~ /Ledger/ && $2 ~ /Update/ && $3 ~ /classified/` option.
Here each `$i` refers to a level in the tests names hierarchy.
Passing `-l` to `tasty` will list the available test names.

When testing using `cabal`, pay special attention to escaping the right symbols, e.g.:

```shell
cabal test byron-spec-ledger:test:byron-spec-ledger-test --test-options "-p \"\$1 ~ /Ledger/ && \$2 ~ /Update/ && \$3 ~ /classified/\""
```

### Replaying QuickCheck Failures

When a QuickCheck test fails, the seed which produced the failure is reported.
The failure can be replayed with:

```shell
cabal test cardano-ledger-shelley-test --test-options "--quickcheck-replay=42"
```
(where 42 is an example seed).

### Test Scenarios

Most of the test suites are grouped into test scenarios.
For example, the Shelley test suite contains
`ContinuousIntegration`, `Development`, `Nightly`, and `Fast`,
which can be run with the `--scenario` flag. For example:

```shell
cabal test cardano-ledger-shelley-test --test-options --scenario=Fast
```

### ghcid

We have support for running
[ghcid](https://github.com/ndmitchell/ghcid)
from inside of nix-shell.
Enter nix-shell from the base directory of the repository,
change directories to the cabal package that you wish to check,
then run `ghcid`.

For example:

```shell
nix-shell
cd shelley/chain-and-ledger/executable-spec/
ghcid
```

---

# nix-build Infrastructure

The artifacts in this repository can be built and tested using nix. This is
additionally used by the Hydra CI to test building, including cross-compilation
for other systems.

## To add a new Haskell project

To add a new Haskell project, you should do the following:

1. Create the project in the usual way. It should have an appropriate `.cabal` file.
2. Add the project to the [top-level stack.yaml](./stack.yaml), configuring
   dependencies etc as needed. If your project's configuration deviates too far
   from the [snapshot in
   ``cardano-prelude`](https://github.com/input-output-hk/cardano-prelude/blob/master/snapshot.yaml),
   then you may have to submit a PR there to update that snapshot.
3. At this point, test that your new project builds using `stack build <project_name>`.
4. Run [nix-shell ./nix -A iohkNix.stack-cabal-sync-shell --run scripts/stack-cabal_config_check.sh](./scripts/stack-cabal_config_check.sh)
  script to check and report your change from stack.yaml to cabal.project.
5. Run the [regenerate](./nix/regenerate.sh) script to
   update sha256 checksums in cabal.project.
5. Test that you can build your new project by running the following: `nix build
   -f default.nix libs.<project_name>`. If you have executables, then
   you may also try building these using the `exes.<executable_name>`
   attribute path. A good way to see what's available is to execute `:l
   default.nix` in `nix repl`. This will allow you to explore the potential
   attribute names.
5. If you want your product to be tested by CI, add it to
   [release.nix](./release.nix) using the format specified in that file.

## To add a new LaTeX specification

To add a new LaTeX specification, the easiest way is to copy from one of the
existing specifications. You will want the `Makefile` and `default.nix` (say
from [the Shelley ledger spec](./shelley/chain-and-ledger/formal-spec)).

1. Copy these files into the root of your new LaTeX specification.
2. Modify the `DOCNAME` in the `Makefile`.
3. Update `default.nix` to:
   1. Make sure that the relative path in the first line is pointing to
      (default.nix)[./default.nix]. This is used to pin the
      `nixpkgs` version used to build the LaTeX specifications.
   2. Update the `buildInputs` to add in any LaTeX packages you need in your
      document, and remove any unneeded ones.
   3. Alter the `meta` description field to reflect the nature of this document.
4. Add a link to the package at the bottom of [default.nix](./default.nix),
   following the existing examples.
5. To require that your specification be built in CI, add it at the end of the
   list in [default.nix](./default.nix) following the existing examples.

## Additional documentation

You can find additional documentation on the nix infrastructure used in this
repo in the following places:

- [The haskell.nix user guide](https://github.com/input-output-hk/haskell.nix/blob/documentation/docs/user-guide.md)
- [The nix-tools repository](https://github.com/input-output-hk/nix-tools)
- [The iohk-nix repository](https://github.com/input-output-hk/iohk-nix)

Note that the user guide linked above is incomplete and does not correctly refer
to projects built using `iohk-nix`, as this one is. A certain amount of trial
and error may be required to make substantive changes!

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-ledger-specs/blob/master/LICENSE">
    <img src="https://img.shields.io/github/license/input-output-hk/cardano-ledger-specs.svg?style=for-the-badge"/>
  </a>
</p>

# Contributing

## Code formatting

We use [`editorconfig`](https://editorconfig.org/) to ensure consistency in the format of our
Haskell code. There are editorconfig plugins for several text editors, so make sure that your editor
honors the configuration in [`.editorconfig`](.editorconfig).

Additionally, we use [`ormolu`](https://github.com/tweag/ormolu/) for formatting.
There is a script [here](https://github.com/input-output-hk/cardano-ledger-specs/blob/master/scripts/ormolise.sh)
which uses nix to format the appropriate directories.
