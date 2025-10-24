# tasty-compat

This is a compatibility library to allow Tasty-based test code to be used with
Hspec instead. It defines some of the basic Tasty functions and types in terms
of their Hspec equivalents using the same module names as Tasty. If the Tasty
usage is simple, it's possible just to replace the `tasty*` module names with a
single `tasty-compat` in `build-depends`.

Currently, only the functionality needed by `cardano-ledger` is provided.
