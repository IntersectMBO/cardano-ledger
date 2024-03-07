<h1 align="center">Cardano Ledger</h1>

<p align="center">
  <a href="https://input-output-hk.github.io/cardano-engineering-handbook">
    <img alt="CEH" src="https://img.shields.io/badge/policy-Cardano%20Engineering%20Handbook-informational?style=for-the-badge" />
  </a>
  <a href="https://github.com/intersectmbo/cardano-ledger/actions/workflows/haskell.yml">
    <img alt="GitHub Workflow Status (master)" src="https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-ledger/haskell.yml?branch=master&style=for-the-badge" />
  </a>
  <a href="https://cardano-ledger.cardano.intersectmbo.org/">
    <img alt="Haddock (master)" src="https://img.shields.io/badge/documentation-Haddock-yellow?style=for-the-badge" />
  </a>
</p>

This repository contains the formal specifications, executable models,
and implementations of the Cardano Ledger.

The documents are built in our CI and can be readily accessed using the
following links:

Era | Design Documents | Formal Specification | CDDL
----|------------------|----------------------|-----
Byron | | [Chain Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-blockchain.pdf "Specification of the Blockchain Layer"), [Ledger Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-ledger.pdf "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/byron/cddl-spec/byron.cddl), [PDF](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/byron-binary.pdf)
Shelley | [Design](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-delegation.pdf "Design Specification for Delegation and Incentives in Cardano") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf "A Formal Specification of the Cardano Ledger") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/shelley/impl/cddl-files)
Allegra | Same as Mary era below | Same as Mary era below | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/allegra/impl/cddl-files)
Mary | [Multi-Currency](https://eprint.iacr.org/2020/895 "Multi-Currency Ledgers"), [UTXOma](https://iohk.io/en/research/library/papers/utxoma-utxo-with-multi-asset-support/ "UTXOma:UTXO with Multi-Asset Support") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/mary-ledger.pdf "A Formal Specification of the Cardano Ledger with a Native Multi-Asset Implementation") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/mary/impl/cddl-files)
Alonzo | [eUTXO](https://iohk.io/en/research/library/papers/the-extended-utxo-model/ "The Extended UTXO Model")| [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf "A Formal Specification of the Cardano Ledger integrating Plutus Core") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/alonzo/impl/cddl-files)
Babbage | [batch-verification](https://iohk.io/en/research/library/papers/on-uc-secure-range-extension-and-batch-verification-for-ecvrf/ "On UC-Secure Range Extension and Batch Verification for ECVRF"), [CIP-31](https://github.com/cardano-foundation/CIPs/pull/159 "Reference inputs"), [CIP-32](https://github.com/cardano-foundation/CIPs/pull/160 "Inline datums"), [CIP-33](https://github.com/cardano-foundation/CIPs/pull/161 "Reference scripts") | [Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/babbage-ledger.pdf "Formal Specification of the Cardano Ledger for the Babbage era") | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/babbage/impl/cddl-files)
Conway | [CIP-1694](https://github.com/cardano-foundation/CIPs/tree/master/CIP-1694) | [Spec (WIP)](https://github.com/intersectmbo/formal-ledger-specifications) | [CDDL](https://github.com/intersectmbo/cardano-ledger/tree/master/eras/conway/impl/cddl-files)


Other Documents:
- [Non-integer calculations specification](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/non-integer-calculations.pdf): details on the parts of the Shelley specification that use real numbers.
- [Stake pool ranking specification](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/pool-ranking.pdf): details for a robust stake pool ranking mechanism.
- [Explanation of the small-step-semantics framework](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/small-step-semantics.pdf): a guide to the notation and style used by our ledger rules.

In addition, there is a formalization of the Ledger Specification in Isabelle/HOL which can be found [here](https://github.com/input-output-hk/fm-ledger-formalization).

Some user documentation is published on [Read the Docs](https://cardano-ledger.readthedocs.io/en/latest)

Haddock code documentation of the latest master branch is available [here](https://input-output-hk.github.io/cardano-ledger).

# Repository structure

The directory structure of this repository is as follows:

- [Byron](./eras/byron)
  - [ledger](./eras/byron/ledger)
    - [formal-spec](./eras/byron/ledger/formal-spec)
    - [executable-spec](./eras/byron/ledger/executable-spec)
    - [implementation](./eras/byron/ledger/impl)
  - [chain](./eras/byron/chain)
    - [formal-spec](./eras/byron/chain/formal-spec)
    - [executable-spec](./eras/byron/chain/executable-spec)
  - [cddl](./eras/byron/cddl-spec)
- [Shelley](./eras/shelley)
  - [design-spec](./eras/shelley/design-spec)
  - [formal-spec](./eras/shelley/formal-spec)
  - [implementation](./eras/shelley/impl)
  - [tests](./eras/shelley/test-suite)
  - [cddl](./eras/shelley/impl/cddl-files)
- [Allegra - Timelocks](./eras/allegra)
  - [formal-spec](./eras/shelley-ma/formal-spec)
  - [implementation](./eras/allegra/impl)
  - [tests](./eras/shelley-ma/test-suite)
- [Mary - Multi-Assets](./eras/allegra)
  - [formal-spec](./eras/shelley-ma/formal-spec)
  - [implementation](./eras/allegra/impl)
  - [tests](./eras/shelley-ma/test-suite)
- [Alonzo - Smart Contracts](./eras/alonzo)
  - [formal-spec](./eras/alonzo/formal-spec)
  - [implementation](./eras/alonzo/impl)
  - [cddl](./eras/alonzo/impl/cddl-files)
  - [tests](./eras/alonzo/test-suite)
- [Babbage](./eras/babbage)
  - [formal-spec](./eras/babbage/formal-spec)
  - [implementation](./eras/babbage/impl)
  - [cddl](./eras/babbage/impl/cddl-files)
  - [tests](./eras/babbage/test-suite)
- [Conway - Governance](./eras/conway)
  - [formal-spec](./eras/conway/formal-spec)
  - [implementation](./eras/conway/impl)
  - [cddl](./eras/conway/impl/cddl-files)
  - [tests](./eras/conway/impl/test)
- [Libraries](./libs)

# Building

It is recommended to use [`nix`](https://nixos.org/nix/download.html) for building everything in this repository.
Haskell files can be built with [`cabal`](https://www.haskell.org/cabal/) inside of a nix shell.

Make sure you have a recent version of `nix` by following this [guide](https://nixos.org/manual/nix/stable/installation/upgrading.html)

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
nix build .#specs
```

The LaTeX documents will be placed inside a directory named `result`, e.g.:

```shell
result/byron-ledger.pdf
result/shelley-delegation.pdf
result/non-integer-calculations.pdf
result/small-step-semantics.pdf
result/shelley-ledger.pdf
result/byron-blockchain.pdf
```


## Building individual LaTeX documents


Change to the latex directory where the latex document is (e.g. `eras/shelley/formal-spec`
for the ledger specification corresponding to the Shelley release, or
`eras/byron/ledger/formal-spec` for the ledger specification corresponding to
the Byron release). Then, build the latex document by running:

```shell
cd <myLaTexDir>
nix develop --command make
```

For a continuous compilation of the `LaTeX` file run:

```shell
cd <myLaTexDir>
nix develop --command make watch
```

# Testing

Run `cabal test all` to run all tests or `cabal test <package>` to run the tests for a specific package.

Note: The `CARDANO_MAINNET_MIRROR` environment variable can be overriden in `flake.nix` if one desires to run
the Byron tests with a different version of the [mainnet epochs](https://github.com/input-output-hk/cardano-mainnet-mirror/tree/master/epochs).

# Submitting an issue

Issues can be filed in the [GitHub Issue tracker](https://github.com/intersectmbo/cardano-ledger/issues).

However, note that this is pre-release software, so we will not usually be providing support.

# Contributing

See [CONTRIBUTING](https://github.com/intersectmbo/cardano-ledger/blob/master/CONTRIBUTING.md).
