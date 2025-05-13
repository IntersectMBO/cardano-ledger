# Adding a new era to `cardano-ledger`

First you need to crate a new sub-package in the `eras` directory.
It's easiest to just copy the `.cabal` file from the previous era and then make
some changes to that. Change the name of the project to `cardano-ledger-<era>`
and update the description/synopsis.

Next you'll want to add a datatype to represent the new era (e.g. `ConwayEra`). 
Then copy over the entire test suite from the previous era and substitute the 
era types for the newly added era type. Once the tests are in place, the type 
checker will guide you to add all the necessary type family and type class 
instances.

It's a good idea to re-use the data types defined in the previous era at first. 
You might need to use `coerce` in a couple of places to change the era parameter 
when translating these types. Also, there might be some constraints that expect 
the era to be exactly the previous era, you will probably be able to generalize
these functions and type class instances to make them compatible with the new era.
