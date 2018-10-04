# Shelley Ledger Rules

Formal and executable specifications for the new features to be introduced by Shelley.

## Building LaTeX documents

Change to the latex directory for the spec that you wish to build,
such as `shelley-ledger-rules/specs/utxo/latex/`.

To build the latex documents run:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch" 
```

## Testing the Haskell models

Change to the hs directory for the model that you wish to use,
such as `shelley-ledger-rules/specs/utxo/hs/`.

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
