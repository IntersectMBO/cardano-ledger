# Accounting Model

A model to account for every one of the forty-five billion Lovelace in Cardano.

## Building

Insinde the directory with this README, run make:

```shell
make
```

This makes both the `Accounting.pdf` and `accounting/src/Accounting.hs`.

If you prefer using [Nix](https://nixos.org/nix/), the `make` command can be
run as via `nix-build`:

```shell
nix-build
```

For a continuous compilation of the `LaTeX` document run:

```shell
nix-shell --pure --run "make watch"
```

## Testing the Haskell model

Change to the haskell directory `accounting`.

The tests can be run with stack:

```shell
stack test --pedantic
```
