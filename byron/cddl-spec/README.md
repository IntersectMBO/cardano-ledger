# CDDL spec

The current binary format is specified in `byron.cddl`. You can use this
to generate (pseudo-)valid blocks:

```shell
nix-shell
cddl byron.cddl generate | diag2pretty.rb > test.pretty
```

By default these are produced in [CBOR Diagnostic
notation](https://tools.ietf.org/html/rfc7049#section-6), so you can use
`diag2pretty.rb` or `diag2cbor.rb` to convert them to a pretty-printed or binary
encoded CBOR format respectively.

The CDDL tools may also be used to validate existing blocks:

```shell
cddl byron.cddl validate test.block
```
