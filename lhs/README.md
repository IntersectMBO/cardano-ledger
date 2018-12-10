# lhs2tex Vending Machine Test

This is a simple experiment to try using the small step semantics with lhs2tex.
It makes two files of importance: Vending.pdf and Vending.hs.

Makefile options:
```
make # build the PDF and HS
```

Run the tests in GHCi:
```
stack ghci
```

```
λ: :l Vending.hs
λ: defaultMain tests
```

If you prefer using [Nix](https://nixos.org/nix/), the `make` command can be
run as via `nix-build`:

```shell
nix-build
```
