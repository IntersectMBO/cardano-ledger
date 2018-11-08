# Accounting Model

A model to account for every one of the forty-five billion Lovelace in Cardano.

## Building

Insinde the directory with this README, run make:

```shell
make
```

This makes both the `Accounting.pdf` and `accounting/src/Accounting.hs`.

## Testing the Haskell model

Change to the haskell directory `accounting`.

The tests can be run with stack:

```shell
stack test --pedantic
```
