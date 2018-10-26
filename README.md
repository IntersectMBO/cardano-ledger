# Formal Models for Ledger Rules

Formal and executable specifications for the new features to be introduced by Shelley.

## Building LaTeX documents

Change to the latex directory `fm-ledger-rules/latex/`.

To build the latex document run:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch" 
```

## Testing the Haskell model

Change to the haskell directory `fm-ledger-rules/hs/`.

The tests can be run with stack:

```shell
stack test --pedantic
```

While developing the models, it can be helpful to run ghcid in a separate shell:

```shell
make ghcid
```

or with tests included:

```shell
make ghcid-test
```

---

[![Build Status](https://travis-ci.org/input-output-hk/fm-ledger-rules.svg?branch=master)](https://travis-ci.org/input-output-hk/fm-ledger-rules)
