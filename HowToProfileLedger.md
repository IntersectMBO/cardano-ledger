# How To Profile Ledger
*Tim Sheard*

## Motivation

Profiling the ledger is an intricate dance between `nix`, `cabal`, and `ghc`. This document
describes how I set all this up to profile some of the property tests in the
`cardano-ledger-test` module. I hope this is useful to others who want to profile
other parts of the ledger codebase.

## Background

Profiling is an important tool to analyze performance (time and space) in Haskell code.
I recommend reading through the [GHC users guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).

This is great background, but the guide says almost nothing about how to make it all work
in a repository that uses `nix` and `cabal`. Hence this document

## Summary

We list here a high level description of the steps needed. In further sections we go into
greater detail about each step

1. Decide what you want to profile, and arrange the code so this is possible
2. Choreograph the dance between `nix`, `cabal`, and `ghc`. This has two parts
   1. Adding stuff to files like `cabal.project`, `cabal.project.local`
   2. Passing the right flags to `cabal`, and `ghc`, running the right `nix develop` shell,
3. Recompiling everything so it can be profiled. This takes a very long time (greater than
   30 minutes when I did it)
4. Start up the profiling. I used `cabal test` with just the right command line arguments.
5. Inspect the produced `.prof` file, and decide what to do
6. Repeat steps 3-5, until satisfied.
7. Undo all the changes (from steps 1 and 2) made just for profiling.

## Deciding what to profile
Usually one wants to profile code that appears to be using too many resources (space or time).
Because of the difficulty in getting `nix`, `cabal`, and `ghc` to work together, I have found that
co-opting an existing `Test.hs` file is the way to go. Here are the reasons why this is a good idea

1) Everything is already set up to compile and run the test file by simply typing: `cabal test`
   in the root directory of the module that contains the test.
2) Is is usually quite easy to rename the `main` variable in the test file to something else
   and add your own `main`, that contains the code you want to profile
3) Using a quick check property test, allows for your code to consist of multiple "tests" each
   with a different random input. This means your profiling results are less likely to be biased
   by a bad choice of input.

Here is how I did it. I edited the file `cardano-ledger/libs/cardano-ledger-test/test/Test.hs`
Here is a synopsis of what it originally looked like, and what it looked like after I edited it.

BEFORE
------------------------
```haskell
-- main entry point
main :: IO ()
main = do
  hSetEncoding stdout utf8
  defaultMain tests
```
------------------------

AFTER
-----------------------
```haskell
import Test.Cardano.Ledger.Generic.Properties (adaIsPreservedBabbage)

mainSave :: IO ()
mainSave = do
  hSetEncoding stdout utf8
  defaultMain tests

main :: IO ()
main = defaultMain adaIsPreservedBabbage
```
-----------------------

The variable `adaIsPreservedBabbage` is a property test of type `TestTree` that takes
about 1 minute to run. Similar tests took about 10-20 seconds, so I was interested why
it took so long. So now I have a `main` that runs just this one test. I could see that I
have set things up properly by changing directory to `cardano-ledger/libs/cardano-ledger-test`,
the root directory of the module. This is the directory that contains the `cardano-ledger-test.cabal` file.
So I can simply type: `cabal test`, and the test runs.  Now all I need to do is get it to be profiled.

## Choreographing the dance

We need to tell `nix`, `cabal`, and `ghc` that we want to profile. We do this by changing a few of the
files that build the system, and by passing the right flags to each of the
programs.


First we must add (or change if you already have it) `cabal.project.local` In the root of the
ledger repository. Put this in `cabal.project.local`
```cabal
ignore-project: False
profiling: True
profiling-detail: all-functions
```

The final step is to pass the right flags to `cabal`, `ghc`, and running the right `nix develop` shell. Here is a summary.

1) to start `nix`, we must use
   `nix develop .#profiling`
2) to build with `cabal`, we must use
   `cabal build --enable-profiling`
3) to run the test, we must pass extra flags to `ghc`, so we must use
   `cabal test --test-options="+RTS  -i60 -p"`

## How to build the system for profiling

Be sure you have set up the files `cabal.project.local` and `nix/haskell.nix` as described above.
Exit the `nix` shell, if you are running it. Now change directories to the root of the `cardano-ledger` repository.

Now to start `nix` type
```bash
nix develop .#profiling
```
(or, e.g. `nix develop .#ghc8107.profiling` for alternative compiler)

When the `nix develop` shell completes (this can take a long time, since it must make sure
every file in the ledger is compiled with profiling enabled). This might take a
while. Be patient. Take the dogs for a walk.

If `nix develop .#profiling` fails to give you a nix shell this may be related to a problem
with `plutus-core` that uses template Haskell, and happens to trigger a
[known bug](https://gitlab.haskell.org/ghc/ghc/-/issues/18320) in `ghc`.

The workaround for this is quite convoluted, but if you fail to get a `nix-shell` working there are two things
you may need to do:

1) add the following to `cabal.project.local`:
```cabal
package plutus-core
   ghc-options: -fexternal-interpreter
```
2) uncomment the following in `flake.nix`:
```nix
packages.plutus-core.components.library.ghcOptions = [ "-fexternal-interpreter" ];
```

Now change directories to the root directory of the module that contains your
modified `Test.hs` file, and type
```bash
cabal build --enable-profiling
```
This might also take a while. Take the dogs for second walk.  When this completes
you are ready to start profiling!

## How to run a profile

In the same directory where you did (`cabal build --enable-profiling`) type
```bash
cabal test --test-options="+RTS  -i60 -p"
```
This should take slightly longer than running the test without profiling.
When it is done, there will be a file in this same directory with extension .prof
When I did it, the file was called `cardano-ledger-test.prof`. It is a big file
Here are the first few lines.
```
----------------------------------------------------------------------------------------------------------
Fri May  6 14:02 2022 Time and Allocation Profiling Report  (Final)

	   cardano-ledger-test +RTS -i60 -p -RTS

	total time  =       51.05 secs   (51050 ticks @ 1000 us, 1 processor)
	total alloc = 109,620,447,376 bytes  (excludes profiling overheads)

COST CENTRE               MODULE                                SRC                                                            %time %alloc

showsPrec                 Cardano.Ledger.Alonzo.TxInfo          src/Cardano/Ledger/Alonzo/TxInfo.hs:504:13-16                   10.3   19.9
evalScripts               Cardano.Ledger.Alonzo.PlutusScriptApi src/Cardano/Ledger/Alonzo/PlutusScriptApi.hs:(231,1)-(247,55)    7.5    1.6
evalScripts.endMsg        Cardano.Ledger.Alonzo.PlutusScriptApi src/Cardano/Ledger/Alonzo/PlutusScriptApi.hs:(240,7)-(246,11)    6.5   17.8
show                      Cardano.Ledger.Alonzo.Scripts         src/Cardano/Ledger/Alonzo/Scripts.hs:202:3-74                    6.1   14.8
blake2b_libsodium         Cardano.Crypto.Hash.Blake2b           src/Cardano/Crypto/Hash/Blake2b.hs:(37,1)-(43,104)               2.6    1.0
decodeAddrStateT          Cardano.Ledger.CompactAddress         src/Cardano/Ledger/CompactAddress.hs:(287,1)-(304,40)            2.3    1.2
splitSMGen                System.Random.SplitMix                src/System/Random/SplitMix.hs:(225,1)-(229,31)                   1.4    3.2
explainPlutusFailure.line Cardano.Ledger.Alonzo.TxInfo          src/Cardano/Ledger/Alonzo/TxInfo.hs:(665,13)-(673,19)            1.4    2.3
toBuilder                 Codec.CBOR.Write                      src/Codec/CBOR/Write.hs:(102,1)-(103,57)                         1.3    0.5
genValidatedTxAndInfo     Test.Cardano.Ledger.Generic.TxGen     src/Test/Cardano/Ledger/Generic/TxGen.hs:(759,1)-(947,55)        1.3    0.1
runE                      Data.Coders                           src/Data/Coders.hs:(566,1)-(577,20)                              1.3    0.7
showsPrec                 Cardano.Ledger.Alonzo.TxInfo          src/Cardano/Ledger/Alonzo/TxInfo.hs:467:13-16                    1.2    3.2
hashWith                  Cardano.Crypto.Hash.Class             src/Cardano/Crypto/Hash/Class.hs:(125,1)-(129,13)                1.2    0.5
genKeyHash.\              Test.Cardano.Ledger.Generic.GenState  src/Test/Cardano/Ledger/Generic/GenState.hs:369:37-83            1.1    0.4
serializeEncoding         Cardano.Binary.Serialize              src/Cardano/Binary/Serialize.hs:(61,1)-(67,49)                   0.8    2.1
toLazyByteString          Codec.CBOR.Write                      src/Codec/CBOR/Write.hs:86:1-49                                  0.7    4.9
---------------------------------------------------------------------------------------------------------------------
```

The problem with my test, was that `evalScripts` was inadvertently showing a large data structure
using `tellEvent`. This was added when debugging and never removed. After fixing this, we had much better results.
I hope you experience is just as rewarding.

Don't forget to revert `cabal.project.local`, `nix/haskell.nix`  and your `Test.hs` file to their original state.
