# CDDL spec

The current binary format is specified in `byron.cddl`. You can use this
to generate (pseudo-)valid blocks:

```shellsession
$ cuddle gen -f pretty -r block -o test.pretty byron.cddl
```

This produces output in a pretty-printed format. To produce binary-encoded CBOR output:

```shellsession
$ cuddle gen -f cbor -r block -o test.block byron.cddl
```

The `cuddle` tool may also be used to validate existing blocks:

```shellsession
$ cuddle validate-cbor -r block -c test.block byron.cddl
```

To start a shell with the `cuddle` tool in scope, you can use

```shellsession
$ nix develop
```

It's also possible to use the older Ruby-based tools for CBOR generation and validation,
but they aren't compatible with the Cardano eras after Babbage.

To start a shell with the Ruby tools in scope, you can use:

```shellsession
$ nix shell nixpkgs#{cddl,cbor-diag}
```
