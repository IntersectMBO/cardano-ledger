# Adding a new era to `cardano-ledger`

First you need to crate a new sub-package in the `eras` directory.
It's easiest to just copy the `.cabal` file from the previous era and then make
some changes to that. Change the name of the project to `cardano-ledger-<era>`
and update the description/synopsis.

Next you'll want to add a datatype to represent the new era (e.g. `ConwayEra`).
