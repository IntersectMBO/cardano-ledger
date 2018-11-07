# Formal and executable specifications for the cardano chain

This directory is organized as follows:

- [`ledger/latex`](ledger/latex) contains the LaTeX specification of Cardano
  ledger semantics.
- [`ledger/hs`](ledger/hs) contains an executable specification of Cardano
  ledger semantics.
- [`chain/latex`](chain/latex) contains the LaTeX specification of Cardano
  chain semantics.
- [`chain/hs`](chain/hs) contains an executable specification of Cardano chain
  semantics.

## Building the documents

To build the `LaTeX` document run:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch"
```

## Building the executable specs
