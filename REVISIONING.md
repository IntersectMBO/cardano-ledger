When you attempt to release packages to CHaP, they might cause breakage
on some downstream packages due to lacking upper bounds.
The purpose of this document is to show you how to resolve these issues,
to show you how to release packages to CHaP with revisions.
The first section gives you a [general overview](#general-overview-on-how-to-do-revisions) on the steps that you will need to take.
The other section is a more detailed [step-by-step guide](#step-by-step-example), that will walk you through an exact case in the past,
when we released `cardano-slotting-0.2.0.0` and `cardano-strict-containers-0.1.3.0` from `cardano-base`.
The former caused some breakages downstream and thus required revisioning.
It is recommended to familiarise yourself first with the following documents before attempting to follow the steps below:
  * https://github.com/IntersectMBO/cardano-haskell-packages?tab=readme-ov-file#cardano-haskell-package-repository-chap
  * https://github.com/IntersectMBO/cardano-ledger/blob/master/RELEASING.md

# General overview on how to do revisions

1. In `cardano-haskell-packages`, `for all` the packages `name-of-package` that you want to add to the release of commit `commit-sha` from repo `my-repo`, `do`:
   ```bash
   ./scripts/add-from-github.sh https://github.com/my-repo commit-sha name-of-package
   ```
   Sidenote: you can provide multiple packages as arguments at once
   (i.e: `./scripts/add-from-github.sh https://github.com/IntersectMBO/cardano-base commit-sha name-of-package-1 name-of-package-2`)
   but that will create a single commit.
   One could argue that this violates the atomicity of commits.
2. `Repeat` until there are no errors:
   1. Build `_repo` with:
      ```bash
      nix develop --command bash -c "foliage build -j 0 --write-metadata"
      ```
   2. Try building your local packages with `Nix` or `Cabal` against your locally built CHaP.
   3. `If` the above fails for package `p` with version `v1.v2.v3.v4`,
       `for all` available releases `v'` between `v1.*.*.*` and `v1+1.*.*.*`, `do`:
         * ```bash
           ./scripts/add-revision.sh _repo p v'
           ```
         * Update the version bounds as necessary in the freshly added `.cabal` file `for all` `_sources/p/v'/revisions/`

# Step-by-step example

As mentioned in the introduction, this was a specific case that we had to deal with when we attempted to release `cardano-slotting-0.2.0.0`
and `cardano-strict-containers-0.1.3.0`.
At the time, the most recent commit for `cardano-haskell-packages` was
`4fab1fe2`, so we will checkout that commit in order to exactly simulate the situation that we had.

1. Clone `cardano-haskell-packages` if you haven't done so already, `cd` into it and checkout commit `4fab1fe2`:
   ```bash
   git clone git@github.com:IntersectMBO/cardano-haskell-packages.git
   cd cardano-haskell-packages
   git checkout 4fab1fe2
   ```
2. If you had cloned `cardano-haskell-packages` earlier and built a local version of CHaPs with `foliage` before in `cardano-haskell-packages/_repo/`, then make sure that you delete `_repo` before you continue.
3. Add `cardano-slotting` and `cardano-strict-containers` packages from `cardano-base` at `f11ddc7f87038c16d5a6d7611fb5dda51be9533d`
   (this was the first commit that contained all the changes we wanted to release) by running the following in `cardano-haskell-packages`:
   ```bash
   ./scripts/add-from-github.sh https://github.com/IntersectMBO/cardano-base f11ddc7f cardano-strict-containers
   ./scripts/add-from-github.sh https://github.com/IntersectMBO/cardano-base f11ddc7f cardano-slotting
   ```
4. Build `_repo` with foliage by running:
   ```bash
   nix develop --command bash -c "foliage build -j 0 --write-metadata"
   ```

Next, you will add the actual revisions and test if you can build your packages locally against your local version of CHaPs. You can either build them with `Nix` or `Cabal`.

## Building with `Nix`
1. Try running the following in `cardano-haskell-packages`:
   ```bash
   nix build --override-input CHaP path:/home/user/cardano-haskell-packages/_repo \
   '.#"ghc96/cardano-ledger-allegra/1.3.0.0"' \
   '.#"ghc96/cardano-ledger-alonzo/1.6.0.0"' \
   '.#"ghc96/cardano-ledger-alonzo-test/1.2.0.0"' \
   '.#"ghc96/cardano-ledger-babbage/1.6.0.0"' \
   '.#"ghc96/cardano-ledger-babbage-test/1.2.0.0"' \
   '.#"ghc96/byron-spec-chain/1.0.0.2"' \
   '.#"ghc96/byron-spec-ledger/1.0.0.2"' \
   '.#"ghc96/cardano-ledger-byron/1.0.0.4"' \
   '.#"ghc96/cardano-ledger-byron-test/1.5.0.1"' \
   '.#"ghc96/cardano-crypto-wrapper/1.5.1.1"' \
   '.#"ghc96/cardano-crypto-test/1.5.0.1"' \
   '.#"ghc96/cardano-ledger-conway/1.12.0.0"' \
   '.#"ghc96/cardano-ledger-conway-test/1.2.1.3"' \
   '.#"ghc96/cardano-ledger-mary/1.5.0.0"' \
   '.#"ghc96/cardano-ledger-shelley/1.9.0.0"' \
   '.#"ghc96/cardano-ledger-shelley-test/1.3.0.1"' \
   '.#"ghc96/cardano-ledger-shelley-ma-test/1.2.1.6"' \
   '.#"ghc96/cardano-ledger-api/1.8.0.0"' \
   '.#"ghc96/cardano-ledger-core/1.10.0.0"' \
   '.#"ghc96/cardano-ledger-binary/1.3.0.0"' \
   '.#"ghc96/cardano-protocol-tpraos/1.1.0.0"' \
   '.#"ghc96/non-integral/1.0.0.0"' \
   '.#"ghc96/small-steps/1.0.1.0"' \
   '.#"ghc96/small-steps-test/1.0.1.0"' \
   '.#"ghc96/cardano-data/1.2.0.0"' \
   '.#"ghc96/set-algebra/1.1.0.2"' \
   '.#"ghc96/vector-map/1.1.0.0"'
   ```
   You could also run `nix build --override-input CHaP path:/home/lucsanszky/cardano-haskell-packages/_repo .#allSmokeTestPackages`
   instead (which is also ran by CI), but that will take longer.
2. If all is right, the above should fail for `cardano-ledger-shelley-1.9.0.0` (or another version for that package), so you will have to add a revision for it by running:
   ```bash
   ./scripts/add-revision.sh _repo cardano-ledger-shelley 1.9.0.0
   ```
   If all went well, you should see that there are unstaged changes in `git` for
   `_sources/cardano-ledger-shelley/1.9.0.0/meta.toml` and also some untracked file(s) in `_sources/cardano-ledger-shelley/1.9.0.0/revisions`.
   You will want to edit the `.cabal` file from the untracked files.
   Its name will depend on whether there were some earlier revisions or not.
   For the first revision, its name will be `1.cabal`, for the second it will be `2.cabal`, etc.
   Proceed to edit the `.cabal` file by adding version bounds to the freshly released package that caused the breakage. In our case, you will have to
   replace the `cardano-slotting` dependency with `cardano-slotting <0.2` in the `build-depends` section.
   Note, that it was `cardano-slotting-0.2.0.0` that caused the breakage in `cardano-ledger-shelley-1.9.0.0`, hence the `<0.2` bound.
3. When such a failure happens, you will have to add revisions for all of
   the available versions for that release of the package and update all of the `.cabal` files like above.
   In our case, you will have to repeat `step 2` for all `cardano-ledger-shelley` versions between 1.0 and 2.0 (so >=1.0 and <=2.0).
4. Rebuild `_repo` again with `foliage` and try rebuilding your packages by running:
   ```bash
   nix develop --command bash -c "foliage build -j 0 --write-metadata"
   nix build --override-input CHaP path:/home/user/cardano-haskell-packages/_repo \
   '.#"ghc96/cardano-ledger-allegra/1.3.0.0"' \
   '.#"ghc96/cardano-ledger-alonzo/1.6.0.0"' \
   '.#"ghc96/cardano-ledger-alonzo-test/1.2.0.0"' \
   '.#"ghc96/cardano-ledger-babbage/1.6.0.0"' \
   '.#"ghc96/cardano-ledger-babbage-test/1.2.0.0"' \
   '.#"ghc96/byron-spec-chain/1.0.0.2"' \
   '.#"ghc96/byron-spec-ledger/1.0.0.2"' \
   '.#"ghc96/cardano-ledger-byron/1.0.0.4"' \
   '.#"ghc96/cardano-ledger-byron-test/1.5.0.1"' \
   '.#"ghc96/cardano-crypto-wrapper/1.5.1.1"' \
   '.#"ghc96/cardano-crypto-test/1.5.0.1"' \
   '.#"ghc96/cardano-ledger-conway/1.12.0.0"' \
   '.#"ghc96/cardano-ledger-conway-test/1.2.1.3"' \
   '.#"ghc96/cardano-ledger-mary/1.5.0.0"' \
   '.#"ghc96/cardano-ledger-shelley/1.9.0.0"' \
   '.#"ghc96/cardano-ledger-shelley-test/1.3.0.1"' \
   '.#"ghc96/cardano-ledger-shelley-ma-test/1.2.1.6"' \
   '.#"ghc96/cardano-ledger-api/1.8.0.0"' \
   '.#"ghc96/cardano-ledger-core/1.10.0.0"' \
   '.#"ghc96/cardano-ledger-binary/1.3.0.0"' \
   '.#"ghc96/cardano-protocol-tpraos/1.1.0.0"' \
   '.#"ghc96/non-integral/1.0.0.0"' \
   '.#"ghc96/small-steps/1.0.1.0"' \
   '.#"ghc96/small-steps-test/1.0.1.0"' \
   '.#"ghc96/cardano-data/1.2.0.0"' \
   '.#"ghc96/set-algebra/1.1.0.2"' \
   '.#"ghc96/vector-map/1.1.0.0"'
   ```
   In our case, it should succeed this time. In general however, you might run into some more failures.
   If that is the case, you will have to add further revisions for the failing packages, effectively repeating the steps from `step 2` onwards.

## Building with `Cabal`
For this, you will want to create a minimal `cabal` project. For example, a directory with the following files would do:
  - `cabal.project`
  - `releasing.cabal`

Paste the following in `cabal.project`:
```cabal
-- Give it a different name to avoid cabal confusing it with the
-- real CHaP
repository cardano-haskell-packages-local
  -- Point this to the *built* repository
  url: file:/home/user/cardano-haskell-packages/_repo
  secure: True
  -- You can skip the root-keys field

packages:
  releasing.cabal
-- Add all the packages you want to try building
extra-packages:
  cardano-ledger-allegra-1.3.0.0,
  cardano-ledger-alonzo-1.6.0.0,
  cardano-ledger-alonzo-test-1.2.0.0,
  cardano-ledger-babbage-1.6.0.0,
  cardano-ledger-babbage-test-1.2.0.0,
  byron-spec-chain-1.0.0.2,
  byron-spec-ledger-1.0.0.2,
  cardano-ledger-byron-1.0.0.4,
  cardano-ledger-byron-test-1.5.0.1,
  cardano-crypto-wrapper-1.5.1.1,
  cardano-crypto-test-1.5.0.1,
  cardano-ledger-conway-1.12.0.0,
  cardano-ledger-conway-test-1.2.1.3,
  cardano-ledger-mary-1.5.0.0,
  cardano-ledger-shelley-1.9.0.0,
  cardano-ledger-shelley-test-1.3.0.1,
  cardano-ledger-shelley-ma-test-1.2.1.6,
  cardano-ledger-api-1.8.0.0,
  cardano-ledger-core-1.10.0.0,
  cardano-ledger-binary-1.3.0.0,
  cardano-protocol-tpraos-1.1.0.0,
  non-integral-1.0.0.0,
  small-steps-1.0.1.0,
  small-steps-test-1.0.1.0,
  cardano-data-1.2.0.0,
  set-algebra-1.1.0.2,
  vector-map-1.1.0.0
```

Paste the following in `releasing.cabal`:
```cabal
cabal-version:      3.0
name:               releasing
version:            0.1.0.0
build-type:         Simple
library
    build-depends:    base ^>=4.16.4.0
    default-language: Haskell2010
```
1. Now run the following:
   ```bash
   cabal update
   cabal build \
   cardano-ledger-allegra \
   cardano-ledger-alonzo \
   cardano-ledger-alonzo-test \
   cardano-ledger-babbage \
   cardano-ledger-babbage-test \
   byron-spec-chain \
   byron-spec-ledger \
   cardano-ledger-byron \
   cardano-ledger-byron-test \
   cardano-crypto-wrapper \
   cardano-crypto-test \
   cardano-ledger-conway \
   cardano-ledger-conway-test \
   cardano-ledger-mary \
   cardano-ledger-shelley \
   cardano-ledger-shelley-test \
   cardano-ledger-shelley-ma-test \
   cardano-ledger-api \
   cardano-ledger-core \
   cardano-ledger-binary \
   cardano-protocol-tpraos \
   non-integral \
   small-steps \
   small-steps-test \
   cardano-data \
   set-algebra \
   vector-map
   ```
2. If all is right, the above should fail for `cardano-ledger-shelley-1.9.0.0` (or another version for that package), so you will have to add a revision for it in `cardano-haskell-packages` by running:
   ```bash
   cd /home/user/cardano-haskell-packages
   ./scripts/add-revision.sh _repo cardano-ledger-shelley 1.9.0.0
   ```
   If all went well, you should see that there are unstaged changes in `git` for
   `_sources/cardano-ledger-shelley/1.9.0.0/meta.toml` and also some untracked file(s) in `_sources/cardano-ledger-shelley/1.9.0.0/revisions`.
   You will want to edit the `.cabal` file from the untracked files.
   Its name will depend on whether there were some earlier revisions or not.
   For the first revision, its name will be `1.cabal`, for the second it will be `2.cabal`, etc.
   Proceed to edit the `.cabal` file by adding version bounds to the freshly released package that caused the breakage. In our case, you will have to
   replace the `cardano-slotting` dependency with `cardano-slotting <0.2` in the `build-depends` section.
   Note, that it was `cardano-slotting-0.2.0.0` that caused the breakage in `cardano-ledger-shelley-1.9.0.0`, hence the `<0.2` bound.
3. When such a failure happens, you will have to add revisions for all of
   the available versions for that release of the package and update all of the `.cabal` files like above.
   In our case, you will have to repeat `step 2` for all `cardano-ledger-shelley` versions between 1.0 and 2.0 (so >=1.0 and <=2.0).
4. Rebuild `_repo` again in `cardano-haskell-packages` with `foliage` and try rebuilding your packages by running:
   ```bash
   nix develop --command bash -c "foliage build -j 0 --write-metadata"
   cd /path_to/your/minimal_cabal_project
   cabal update
   cabal build \
   cardano-ledger-allegra \
   cardano-ledger-alonzo \
   cardano-ledger-alonzo-test \
   cardano-ledger-babbage \
   cardano-ledger-babbage-test \
   byron-spec-chain \
   byron-spec-ledger \
   cardano-ledger-byron \
   cardano-ledger-byron-test \
   cardano-crypto-wrapper \
   cardano-crypto-test \
   cardano-ledger-conway \
   cardano-ledger-conway-test \
   cardano-ledger-mary \
   cardano-ledger-shelley \
   cardano-ledger-shelley-test \
   cardano-ledger-shelley-ma-test \
   cardano-ledger-api \
   cardano-ledger-core \
   cardano-ledger-binary \
   cardano-protocol-tpraos \
   non-integral \
   small-steps \
   small-steps-test \
   cardano-data \
   set-algebra \
   vector-map
   ```
   In our case, it should succeed this time. In general however, you might run into some more failures.
   If that is the case, you will have to add further revisions for the failing packages, effectively repeating the steps from `step 2` onwards.
