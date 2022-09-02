{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Strategy for Generic Tests
--   Make the GenState include a Mode of the NewEpochState, modify
--   the ModelNewEpochState to reflect what we generated.
module Test.Cardano.Ledger.Generic.GenState.GenScript
  ( genDatumWithHash,
    genKeyHash,
    genPositiveVal,
    genScript,
    genGenState,
    getBlocksizeMax,
    getCertificateMax,
    getOldUtxoPercent,
    getRefInputsMax,
    getReserves,
    getSlot,
    getSlotDelta,
    getSpendInputsMax,
    getTreasury,
    getUtxoChoicesMax,
    getUtxoElem,
    getUtxoTest,
    getCollInputsMax,
    getNewPoolTest,
    viewGenState,
    initialLedgerState,
    addPoolToInitialState,
    genFreshKeyHash,
    addPoolToModel,
  )
where

import Cardano.Ledger.Alonzo.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Scripts hiding (Mint, Script)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    coerceKeyRole,
    hashKey,
  )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import qualified Cardano.Ledger.Shelley.Scripts as Shelley (MultiSig (..))
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (gets, modify)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import Numeric.Natural
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Generic.Functions
  ( alwaysFalse,
    alwaysTrue,
    primaryLanguage,
  )
import Test.Cardano.Ledger.Generic.GenState.Types
import Test.Cardano.Ledger.Generic.ModelState
  ( ModelNewEpochState (..),
  )
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Tasty.QuickCheck (arbitrary, choose, elements, frequency)

-- =============================================
-- Generators of inter-related items

-- Adds to the gsKeys
genKeyHash :: Reflect era => GenRS era (KeyHash kr (EraCrypto era))
genKeyHash = do
  keyPair <- lift arbitrary
  let keyHash = hashKey $ vKey keyPair
  modify $ \ts@GenState {gsKeys} -> ts {gsKeys = Map.insert keyHash keyPair gsKeys}
  pure $ coerceKeyRole keyHash

-- Adds to the gsDatums
genDatumWithHash :: Era era => GenRS era (DataHash (EraCrypto era), Data era)
genDatumWithHash = do
  datum <- lift arbitrary
  let datumHash = hashData datum
  modify $ \ts@GenState {gsDatums} ->
    ts {gsDatums = Map.insert datumHash datum gsDatums}
  pure (datumHash, datum)

genFreshKeyHash :: Reflect era => GenRS era (KeyHash kr (EraCrypto era))
genFreshKeyHash = go (100 :: Int) -- avoid unlikely chance of generated hash collisions.
  where
    go n
      | n <= 0 = error "Something very unlikely happened"
      | otherwise = do
          avoidKeys <- gets gsAvoidKey
          kh <- genKeyHash
          if coerceKeyRole kh `Set.member` avoidKeys
            then go $ n - 1
            else return kh

addPoolToInitialState ::
  KeyHash 'StakePool (EraCrypto era) ->
  PoolParams (EraCrypto era) ->
  IndividualPoolStake (EraCrypto era) ->
  GenRS era ()
addPoolToInitialState poolId poolparam stake =
  modify
    ( \st ->
        st
          { gsInitialPoolParams = Map.insert poolId poolparam (gsInitialPoolParams st),
            gsInitialPoolDistr = Map.insert poolId stake (gsInitialPoolDistr st)
          }
    )

addPoolToModel :: KeyHash 'StakePool (EraCrypto era) -> PoolParams (EraCrypto era) -> IndividualPoolStake (EraCrypto era) -> GenRS era ()
addPoolToModel poolId poolparam stake =
  modifyModel
    ( \m ->
        m
          { mPoolParams = Map.insert poolId poolparam (mPoolParams m),
            mPoolDistr = Map.insert poolId stake (mPoolDistr m)
          }
    )

-- ===========================================================
-- Generate Era agnostic Scripts

-- Adds to gsScripts and gsPlutusScripts
genScript :: forall era. Reflect era => Proof era -> Tag -> GenRS era (ScriptHash (EraCrypto era))
genScript proof tag = case proof of
  Conway _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Babbage _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Alonzo _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Mary _ -> genTimelockScript proof
  Allegra _ -> genTimelockScript proof
  Shelley _ -> genMultiSigScript proof

-- Adds to gsScripts
genTimelockScript :: forall era. Reflect era => Proof era -> GenRS era (ScriptHash (EraCrypto era))
genTimelockScript proof = do
  vi@(ValidityInterval mBefore mAfter) <- gets gsValidityInterval
  -- We need to limit how deep these timelocks can go, otherwise this generator will
  -- diverge. It also has to stay very shallow because it grows too fast.
  let genNestedTimelock k
        | k > 0 =
            elementsT $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = elementsT nonRecTimelocks
      nonRecTimelocks :: [GenRS era (Timelock era)]
      nonRecTimelocks =
        [ r
          | SJust r <-
              [ requireTimeStart <$> mBefore,
                requireTimeExpire <$> mAfter,
                SJust requireSignature
              ]
        ]
      requireSignature = RequireSignature <$> genKeyHash
      requireAllOf k = do
        n <- lift nonNegativeSingleDigitInt
        RequireAllOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireAnyOf k = do
        n <- lift positiveSingleDigitInt
        RequireAnyOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireMOf k = do
        n <- lift nonNegativeSingleDigitInt
        m <- lift $ choose (0, n)
        RequireMOf m . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireTimeStart (SlotNo validFrom) = do
        minSlotNo <- lift $ choose (minBound, validFrom)
        pure $ RequireTimeStart (SlotNo minSlotNo)
      requireTimeExpire (SlotNo validTill) = do
        maxSlotNo <- lift $ choose (validTill, maxBound)
        pure $ RequireTimeExpire (SlotNo maxSlotNo)
  tlscript <- genNestedTimelock (2 :: Natural)
  let corescript :: Script era
      corescript = case proof of
        Conway _ -> TimelockScript tlscript
        Babbage _ -> TimelockScript tlscript
        Alonzo _ -> TimelockScript tlscript
        Mary _ -> tlscript
        Allegra _ -> tlscript
        Shelley _ -> error "Shelley does not have TimeLock scripts"
  let scriptHash = hashScript @era corescript
      insertOrCreate x Nothing = Just (Set.singleton x)
      insertOrCreate x (Just s) = Just (Set.insert x s)
  modify $ \ts@GenState {gsScripts, gsVI} ->
    ts
      { gsScripts = Map.insert scriptHash corescript gsScripts,
        gsVI = Map.alter (insertOrCreate scriptHash) vi gsVI
      }
  pure scriptHash

-- Adds to gsScripts
genMultiSigScript :: forall era. Reflect era => Proof era -> GenRS era (ScriptHash (EraCrypto era))
genMultiSigScript proof = do
  let genNestedMultiSig k
        | k > 0 =
            elementsT $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = elementsT nonRecTimelocks
      nonRecTimelocks = [requireSignature]
      requireSignature = Shelley.RequireSignature @era <$> genKeyHash
      requireAllOf k = do
        n <- lift nonNegativeSingleDigitInt
        Shelley.RequireAllOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireAnyOf k = do
        n <- lift positiveSingleDigitInt
        Shelley.RequireAnyOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireMOf k = do
        n <- lift nonNegativeSingleDigitInt
        m <- lift $ choose (0, n)
        Shelley.RequireMOf m <$> replicateM n (genNestedMultiSig (k - 1))
  msscript <- genNestedMultiSig (2 :: Natural)
  let corescript :: Script era
      corescript = case proof of
        Shelley _ -> msscript
        _ -> error (show proof ++ " does not have MultiSig scripts")
  let scriptHash = hashScript @era corescript
  modify $ \ts@GenState {gsScripts} -> ts {gsScripts = Map.insert scriptHash corescript gsScripts}
  pure scriptHash

-- Adds to gsPlutusScripts
genPlutusScript ::
  forall era.
  Reflect era =>
  Proof era ->
  Tag ->
  GenRS era (ScriptHash (EraCrypto era))
genPlutusScript proof tag = do
  isValid <- lift $ frequency [(5, pure False), (95, pure True)]
  -- Plutus scripts alwaysSucceeds needs at least numArgs, while
  -- alwaysFails needs exactly numArgs to have the desired affect.
  -- For reasons unknown, this number differs from Alonzo to Babbage
  -- Perhaps because Babbage is using PlutusV2 scripts?
  let numArgs = case (proof, tag) of
        (Conway _, Spend) -> 2
        (Conway _, _) -> 1
        (Babbage _, Spend) -> 2
        (Babbage _, _) -> 1
        (_, Spend) -> 3
        (_, _) -> 2
  -- While using varying number of arguments for alwaysSucceeds we get
  -- varying script hashes, which helps with the fuzziness
  let mlanguage = primaryLanguage proof
  script <-
    if isValid
      then alwaysTrue proof mlanguage . (+ numArgs) <$> lift (elements [0, 1, 2, 3 :: Natural])
      else pure $ alwaysFalse proof mlanguage numArgs

  let corescript :: Script era
      corescript = case proof of
        Alonzo _ -> script
        Babbage _ -> script
        Conway _ -> script
        _ ->
          error
            ( "PlutusScripts are available starting in the Alonzo era. "
                ++ show proof
                ++ " does not support PlutusScripts."
            )
      scriptHash = hashScript @era corescript
  modify $ \ts@GenState {gsPlutusScripts} ->
    ts {gsPlutusScripts = Map.insert (scriptHash, tag) (IsValid isValid, corescript) gsPlutusScripts}
  pure scriptHash
