<h1 align="center">Cardano Ledger</h1>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-ledger/actions/workflows/haskell.yml">
    <img alt="GitHub Workflow Status (master)" src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-ledger/Haskell%20CI/master.svg?style=for-the-badge" />
  </a>
</p>

This repository contains the formal specifications, executable models,
and implementations of the Cardano Ledger.

The documents are built in our CI and can be readily accessed using the
following links:

Era | Design Documents | Formal Specification | CDDL
----|------------------|----------------------|-----
Byron | | [Chain Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec "Specification of the Blockchain Layer"), [Ledger Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/input-output-hk/cardano-ledger/tree/master/eras/byron/cddl-spec/byron.cddl), [PDF](https://hydra.iohk.io/job/Cardano/cardano-ledger/blocksCDDLSpec/latest/download-by-type/doc-pdf/binary)
Shelley | [Design](https://hydra.iohk.io/job/Cardano/cardano-ledger/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec "Design Specification for Delegation and Incentives in Cardano") | [Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/input-output-hk/cardano-ledger/tree/master/eras/shelley/test-suite/cddl-files)
Allegra & Mary | [Multi-Currency](https://eprint.iacr.org/2020/895 "Multi-Currency Ledgers"), [UTXOma](https://iohk.io/en/research/library/papers/utxoma-utxo-with-multi-asset-support/ "UTXOma:UTXO with Multi-Asset Support") | [Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger/specs.shelley-ma/latest/download-by-type/doc-pdf/shelley-ma "A Formal Specification of the Cardano Ledger with a Native Multi-Asset Implementation") | [CDDL](https://github.com/input-output-hk/cardano-ledger/tree/master/eras/shelley-ma/test-suite/cddl-files)
Alonzo | [eUTXO](https://iohk.io/en/research/library/papers/the-extended-utxo-model/ "The Extended UTXO Model")| [Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger/specs.alonzo-ledger/latest/download-by-type/doc-pdf/alonzo-changes "A Formal Specification of the Cardano Ledger integrating Plutus Core") | [CDDL](https://github.com/input-output-hk/cardano-ledger/tree/master/eras/alonzo/test-suite/cddl-files)
Babbage | [batch-verification](https://iohk.io/en/research/library/papers/on-uc-secure-range-extension-and-batch-verification-for-ecvrf/ "On UC-Secure Range Extension and Batch Verification for ECVRF"), [CIP-31](https://github.com/cardano-foundation/CIPs/pull/159 "Reference inputs"), [CIP-32](https://github.com/cardano-foundation/CIPs/pull/160 "Inline datums"), [CIP-33](https://github.com/cardano-foundation/CIPs/pull/161 "Reference scripts") | [Spec](https://hydra.iohk.io/job/Cardano/cardano-ledger/specs.babbage-ledger/latest/download-by-type/doc-pdf/babbage-changes "Formal Specification of the Cardano Ledger for the Babbage era") | [CDDL](https://github.com/input-output-hk/cardano-ledger/tree/master/eras/babbage/test-suite/cddl-files)


Other Documents:
- [Non-integer calculations specification](https://hydra.iohk.io/job/Cardano/cardano-ledger/nonIntegerCalculations/latest/download-by-type/doc-pdf/non-integer-calculations): details on the parts of the Shelley specification that use real numbers.
- [Stake pool ranking specification](https://hydra.iohk.io/job/Cardano/cardano-ledger/specs.pool-ranking/latest/download-by-type/doc-pdf/pool-ranking): details for a robust stake pool ranking mechanism.
- [Explanation of the small-step-semantics framework](https://hydra.iohk.io/job/Cardano/cardano-ledger/semanticsSpec/latest/download-by-type/doc-pdf/small-step-semantics): a guide to the notation and style used by our ledger rules.

In addition, there is a formalization of the Ledger Specification in Isabelle/HOL which can be found [here](https://github.com/input-output-hk/fm-ledger-formalization).

Some user documentation is published on [Read the Docs](https://cardano-ledger.readthedocs.io/en/latest)

# Repository structure

The directory structure of this repository is as follows:

- [byron](./eras/byron)
  - [ledger](./eras/byron/ledger)
    - [formal-spec](./eras/byron/ledger/formal-spec)
    - [executable-spec](./eras/byron/ledger/executable-spec)
    - [implementation](./eras/byron/ledger/impl)
  - [chain](./eras/byron/chain)
    - [formal-spec](./eras/byron/chain/formal-spec)
    - [executable-spec](./eras/byron/chain/executable-spec)
  - [cddl](./eras/byron/cddl-spec)
- [shelley](./eras/shelley)
  - [design-spec](./eras/shelley/design-spec)
  - [formal-spec](./eras/shelley/formal-spec)
  - [implementation](./eras/shelley/impl)
  - [tests](./eras/shelley/test-suite)
  - [cddl](./eras/shelley/test-suite/cddl-files)
- [Timelocks and Multi-Assets](./eras/shelley-ma)
    - [formal-spec](./eras/shelley-ma/formal-spec)
    - [implementation](./eras/shelley-ma/impl)
    - [tests](./eras/shelley-ma/test-suite)
- [Smart Contracts](./eras/alonzo)
    - [formal-spec](./eras/alonzo/formal-spec)
    - [implementation](./eras/alonzo/impl)
    - [tests](./eras/alonzo/test-suite)
- [Libraries](./libs)

# Building

It is recommended to use [`nix`](https://nixos.org/nix/download.html) for building everything in this repository.
Haskell files can be built with [`cabal`](https://www.haskell.org/cabal/) inside of a nix shell.

## Nix Cache

When using `nix` it is recommended that you setup the cache, so that it can
reuse built artifacts, reducing the compilation times dramatically:

If you are using [NixOS](https://nixos.org/) add the snippet below to your
`/etc/nixos/configuration.nix`:

```
nix.settings = {
  experimental-features = [ "nix-command" "flakes" ];
  substituters = [
    "https://cache.nixos.org"
    "https://cache.iog.io"
  ];
  trusted-public-keys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
};
```

If you are using the `nix` package manager next to another operating system put
the following in `/etc/nix/nix.conf`:

```
experimental-features = nix-command flakes
substituters        = https://cache.iog.io https://cache.nixos.org/
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


Change to the latex directory where the latex document is (e.g. `eras/shelley/formal-spec`
for the ledger specification corresponding to the Shelley release, or
`eras/byron/ledger/formal-spec` for the ledger specification corresponding to
the Byron release). Then, build the latex document by running:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch"
```

## Updating dependencies

When updating the [pinned hackage index-state](https://github.com/input-output-hk/cardano-ledger/blob/master/cabal.project) (for example, in order to get a new version of a package), it's necessary to make sure that [`hackage.nix` pin](https://github.com/input-output-hk/cardano-ledger/blob/master/nix/sources.json) points to a later date than the index-state, in order to avoid an error like this:
```
error: Unknown index-state 2021-08-08T00:00:00Z, the latest index-state I know about is 2021-08-06T00:00:00Z. You may need to update to a newer hackage.nix.
```

You can update the `sources.json` file using niv:
```
niv update hackage
```

# Submitting an issue

Issues can be filed in the [GitHub Issue tracker](https://github.com/input-output-hk/cardano-ledger/issues).

However, note that this is pre-release software, so we will not usually be providing support.

# Contributing

See [CONTRIBUTING](https://github.com/input-output-hk/cardano-ledger/blob/master/CONTRIBUTING.md).
