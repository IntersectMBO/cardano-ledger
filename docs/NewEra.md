### Adding a new era to `cardano-ledger`

First you need to crate a new sub-package in the `eras` directory.
It's easiest to just copy the `.cabal` file from the previous era and then make
some changes to that. Change the name of the project to `cardano-ledger-<era>`
and update the description/synopsis.

Next you'll want to add a datatype to represent the new era (e.g. `ConwayEra`). 
Then copy over the entire test suite from the previous era and substitute the 
era types for the newly added era type. Once the tests are in place, the type 
checker will guide you to add all the necessary type family and type class 
instances. You might need to make some of the tests era-generic. See which tests
don't yet take an era type annotation and modify those test if possible. The tests
that can be made era-generic should be moved to the `testlib` of the era where 
they were first introduced, and any era-specific tests should reside in the test
suite package of that era.

Add the `Cardano.Ledger.<era>.Core` module and re-export the `Core` module from 
the previous era. Use the `Core` module from the current era whenever you need 
to import anything from the core module. Do the same for 
`Cardano.Ledger.<era>.State`.

It's a good idea to re-use the data types defined in the previous era at first. 
You might need to use `coerce` in a couple of places to change the era parameter 
when translating these types. Also, there might be some constraints that expect 
the era to be exactly the previous era, you will probably be able to generalize
these functions and type class instances to make them compatible with the new era.

For `EraRule` instances use the same rules as in the previous era.

Make sure to update `.github/ci/haskell.yml` so that the job also runs the test 
suite of the newly added era.
