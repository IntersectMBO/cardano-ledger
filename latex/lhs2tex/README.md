
# lhs2tex spec demo

You can use the `shell.nix` in this directory to get a shell
with lhs2TeX properly installed.

The `LedgerSpec.lhs` contains a snippet of the spec from which
you can generate both the LaTeX PDF spec, but which you can also
load into GHCi. It is not (currently) executable, but can at
least be typechecked.

Makefile options:
```
make         # build the PDF
make ghci    # load into GHCi
```
