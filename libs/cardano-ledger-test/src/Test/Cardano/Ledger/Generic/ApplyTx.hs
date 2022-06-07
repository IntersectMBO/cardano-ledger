{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.ApplyTx where

-- computed from testGlobals

import Cardano.Ledger.Alonzo.Tx (IsValid (..), ValidatedTx (..))
import Cardano.Ledger.BaseTypes (TxIx, mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    EraIndependentTxBody,
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val ((<+>), (<->))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Examples.BabbageFeatures (collateralOutputTx, initUTxO)
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), TxField (..), abstractTx, abstractTxBody)
import Test.Cardano.Ledger.Generic.Functions (getBody, getOutputs, getTxOutCoin, keyPoolDeposits, txInBalance)
import Test.Cardano.Ledger.Generic.ModelState
  ( Model,
    ModelNewEpochState (..),
    mNewEpochStateZero,
    pcModelNewEpochState,
  )
import Test.Cardano.Ledger.Generic.PrettyCore (pcCredential, pcTx)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo)

-- ========================================================================

hasValid :: [TxField era] -> Maybe Bool
hasValid [] = Nothing
hasValid (Valid (IsValid b) : _) = Just b
hasValid (_ : fs) = hasValid fs

applyTx :: Reflect era => Proof era -> Int -> SlotNo -> Model era -> Core.Tx era -> Model era
applyTx proof count slot model tx = ans
  where
    transactionEpoch = epochFromSlotNo slot
    modelEpoch = mEL model
    epochAccurateModel = epochBoundary count slot transactionEpoch modelEpoch model
    txbody = (getBody proof tx)
    outputs = getOutputs proof txbody
    fields = abstractTx proof tx
    nextTxIx = mkTxIxPartial (fromIntegral (length outputs)) -- When IsValid is false, ColRet will get this TxIx
    ans = case hasValid fields of
      Nothing -> List.foldl' (applyTxSimple proof count) epochAccurateModel fields
      Just True -> List.foldl' (applyTxSimple proof count) epochAccurateModel fields
      Just False -> List.foldl' (applyTxFail proof count nextTxIx) epochAccurateModel fields

epochBoundary :: Int -> SlotNo -> EpochNo -> EpochNo -> Model era -> Model era
epochBoundary count (SlotNo n) transactionEpoch modelEpoch model =
  if transactionEpoch > modelEpoch
    then model {mEL = trace ("EPOCH count=" ++ show count ++ " slot=" ++ show n ++ " epoch=" ++ show transactionEpoch) transactionEpoch}
    else model

applyTxSimple :: Proof era -> Int -> Model era -> TxField era -> Model era
applyTxSimple proof count model field = case field of
  Body body1 -> applyTxBody proof count model body1
  BodyI fs -> List.foldl' (applyField proof count) model fs
  Witnesses _ -> model
  WitnessesI _ -> model
  AuxData _ -> model
  Valid _ -> model

applyTxBody :: Proof era -> Int -> Model era -> Core.TxBody era -> Model era
applyTxBody proof count model tx = List.foldl' (applyField proof count) model (abstractTxBody proof tx)

applyField :: Proof era -> Int -> Model era -> TxBodyField era -> Model era
applyField proof count model field = case field of
  (Inputs txins) -> model {mUTxO = Map.withoutKeys (mUTxO model) txins}
  (Outputs seqo) -> case Map.lookup count (mIndex model) of
    Nothing -> error ("Output not found phase1: " ++ show (mIndex model))
    Just (TxId hash) -> model {mUTxO = Map.union newstuff (mUTxO model)}
      where
        newstuff = additions hash minBound (toList seqo)
  (Txfee coin) -> model {mFees = coin <+> (mFees model)}
  (Certs seqc) -> List.foldl' (applyCert proof) model (toList seqc)
  (Wdrls (Wdrl m)) -> Map.foldlWithKey' (applyWdrl proof) model m
  _other -> model

applyWdrl :: Proof era -> Model era -> RewardAcnt (Crypto era) -> Coin -> Model era
applyWdrl _proof model (RewardAcnt _network cred) coin =
  model {mRewards = Map.adjust (\c -> c <-> coin) cred (mRewards model)}

applyCert :: Proof era -> Model era -> DCert (Crypto era) -> Model era
applyCert proof model dcert = case dcert of
  (DCertDeleg (RegKey x)) ->
    model
      { mRewards = Map.insert x (Coin 0) (mRewards model),
        mDeposited = mDeposited model <+> keydeposit
      }
    where
      pp = mPParams model
      (keydeposit, _) = keyPoolDeposits proof pp
  (DCertDeleg (DeRegKey x)) -> case Map.lookup x (mRewards model) of
    Nothing -> error . show $ "DeRegKey not in rewards: " <> pcCredential x
    Just (Coin 0) ->
      model
        { mRewards = Map.delete x (mRewards model),
          mDeposited = mDeposited model <-> keydeposit
        }
      where
        pp = mPParams model
        (keydeposit, _) = keyPoolDeposits proof pp
    Just (Coin _n) -> error "DeRegKey with non-zero balance"
  (DCertDeleg (Delegate (Delegation cred hash))) ->
    model {mDelegations = Map.insert cred hash (mDelegations model)}
  (DCertPool (RegPool poolparams)) ->
    model
      { mPoolParams = Map.insert hk poolparams (mPoolParams model),
        mDeposited = mDeposited model <+> pooldeposit
      }
    where
      hk = _poolId poolparams
      pp = mPParams model
      (_, pooldeposit) = keyPoolDeposits proof pp
  (DCertPool (RetirePool keyhash epoch)) ->
    model
      { mRetiring = Map.insert keyhash epoch (mRetiring model),
        mDeposited = mDeposited model <-> pooldeposit
      }
    where
      pp = mPParams model
      (_, pooldeposit) = keyPoolDeposits proof pp
  (DCertGenesis _) -> model
  (DCertMir _) -> model

-- =========================================================
-- What to do if the second phase does not validatate.
-- Process and use Collateral to pay fees

data CollInfo era = CollInfo
  { ciBal :: Coin, -- Balance of all the collateral inputs
    ciRet :: Coin, -- Coin amount of the collateral return output
    ciDelset :: Set (TxIn (Crypto era)), -- The set of inputs to delete from the UTxO
    ciAddmap :: Map (TxIn (Crypto era)) (Core.TxOut era) -- Things to Add to the UTxO
  }

emptyCollInfo :: CollInfo era
emptyCollInfo = CollInfo (Coin 0) (Coin 0) Set.empty Map.empty

-- | Collect information about how to process Collateral, in a second phase failure.
collInfo ::
  (Reflect era, HasCallStack) =>
  Proof era ->
  Int ->
  TxIx ->
  Model era ->
  CollInfo era ->
  TxBodyField era ->
  CollInfo era
collInfo proof count firstTxIx model info field = case field of
  CollateralReturn SNothing -> info
  CollateralReturn (SJust txout) ->
    case Map.lookup count (mIndex model) of
      Nothing -> error ("Output not found phase2: " ++ show (count, mIndex model))
      Just (TxId hash) ->
        info
          { ciRet = getTxOutCoin proof txout,
            ciAddmap = newstuff
          }
        where
          newstuff = additions hash firstTxIx [txout]
  Collateral inputs ->
    info
      { ciDelset = inputs,
        ciBal = txInBalance inputs (mUTxO model)
      }
  _ -> info

updateInfo :: CollInfo era -> Model era -> Model era
updateInfo info m =
  m
    { mUTxO = Map.union (ciAddmap info) (Map.withoutKeys (mUTxO m) (ciDelset info)),
      mFees = mFees m <+> ciBal info <-> ciRet info
    }

applyTxFail :: Reflect era => Proof era -> Int -> TxIx -> Model era -> TxField era -> Model era
applyTxFail proof count nextTxIx model field = case field of
  Body body2 -> updateInfo info model
    where
      info = List.foldl' (collInfo proof count nextTxIx model) emptyCollInfo (abstractTxBody proof body2)
  BodyI fs -> updateInfo info model
    where
      info = List.foldl' (collInfo proof count nextTxIx model) emptyCollInfo fs
  Witnesses _ -> model
  WitnessesI _ -> model
  AuxData _ -> model
  Valid _ -> model

-- =======================================

additions ::
  SafeHash (Crypto era) EraIndependentTxBody ->
  TxIx ->
  [Core.TxOut era] ->
  Map (TxIn (Crypto era)) (Core.TxOut era)
additions bodyhash firstTxIx outputs =
  Map.fromList
    [ (TxIn (TxId bodyhash) idx, out)
      | (out, idx) <- zip outputs [firstTxIx ..]
    ]

-- | This is a template of how we might create unit tests that run both the real STS rules
--   and the model to see that they agree. 'collateralOutputTx' and 'initUTxO' are from
--   the BabbageFeatures.hs unit test file.
go :: IO ()
go = do
  let proof = Babbage Mock
      tx = (collateralOutputTx proof) {isValid = IsValid False}
      allinputs = getAllTxInputs txbody
      txbody = body tx
      doc = pcTx proof tx
      model1 =
        (mNewEpochStateZero @(BabbageEra Mock))
          { mUTxO = Map.restrictKeys (unUTxO (initUTxO proof)) allinputs,
            mCount = 0,
            mFees = Coin 10,
            mIndex = Map.singleton 0 (TxId (hashAnnotated txbody))
          }
      model2 = applyTx proof 0 (SlotNo 0) model1 tx
  print (pcModelNewEpochState proof model1)
  print doc
  print (pcModelNewEpochState proof model2)
