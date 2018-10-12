# Shelley Ledger Rules

Formal and executable specifications for the new features to be introduced by Shelley.

## Building LaTeX documents

Change to the latex directory `shelley-ledger-rules/latex/`.

To build the latex document run:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch" 
```

## Testing the Haskell model

Change to the haskell directory `shelley-ledger-rules/hs/`.

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
