## Building the LaTeX documents and executable specifications

When using `nix` the documents and executable specifications can be readily
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


Change to the latex directory where the latex document is (e.g. `shelley/chain-and-ledger/formal-spec`
for the ledger specification corresponding to the Shelley release, or
`byron/ledger/formal-spec` for the ledger specification corresponding to
the Byron release). Then, build the latex document by running:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch"
```
