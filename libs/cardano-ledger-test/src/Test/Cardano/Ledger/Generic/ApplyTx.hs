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

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Scripts (CostModels (..), ExUnits (ExUnits))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (RdmrPtr), Redeemers (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), TxIx, mkTxIxPartial, natVersion)
import Cardano.Ledger.Coin (Coin (..), addDeltaCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.API (Credential, KeyRole (Staking))
import Cardano.Ledger.Shelley.Rewards (aggregateRewards)
import Cardano.Ledger.Shelley.TxBody (
  DCert (..),
  DelegCert (..),
  Delegation (..),
  PoolCert (..),
  PoolParams (..),
  RewardAcnt (..),
  Wdrl (..),
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (inject), (<+>), (<->))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Iterate.Exp (dom, (∈))
import Control.Iterate.SetAlgebra (eval)
import Data.Foldable (fold, toList)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  freeCostModelV1,
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
 )
import Test.Cardano.Ledger.Generic.Fields (
  PParamsField (..),
  TxBodyField (..),
  TxField (..),
  TxOutField (..),
  WitnessesField (..),
  abstractTx,
  abstractTxBody,
 )
import Test.Cardano.Ledger.Generic.Functions (
  createRUpdNonPulsing',
  getBody,
  getOutputs,
  txInBalance,
 )
import Test.Cardano.Ledger.Generic.ModelState (
  Model,
  ModelNewEpochState (..),
  mNewEpochStateZero,
  pcModelNewEpochState,
 )
import Test.Cardano.Ledger.Generic.PrettyCore (pcCredential, pcTx)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Scriptic (Scriptic (never))
import Test.Cardano.Ledger.Generic.Updaters (newPParams, newScriptIntegrityHash, newTx, newTxBody, newTxOut)
import Test.Cardano.Ledger.Shelley.Rewards (RewardUpdateOld (deltaFOld), rsOld)
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo)

-- ========================================================================

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls . CostModels $ Map.singleton PlutusV1 freeCostModelV1
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @5) 0
  , KeyDeposit (Coin 2)
  , PoolDeposit (Coin 5)
  , CollateralPercentage 100
  ]

pparams :: EraPParams era => Proof era -> PParams era
pparams pf = newPParams pf defaultPPs

hasValid :: [TxField era] -> Maybe Bool
hasValid [] = Nothing
hasValid (Valid (IsValid b) : _) = Just b
hasValid (_ : fs) = hasValid fs

applyTx :: Reflect era => Proof era -> Int -> SlotNo -> Model era -> Tx era -> Model era
applyTx proof count slot model tx = ans
  where
    transactionEpoch = epochFromSlotNo slot
    modelEpoch = mEL model
    epochAccurateModel = epochBoundary proof transactionEpoch modelEpoch model
    txbody = getBody proof tx
    outputs = getOutputs proof txbody
    fields = abstractTx proof tx
    nextTxIx = mkTxIxPartial (fromIntegral (length outputs)) -- When IsValid is false, ColRet will get this TxIx
    ans = case hasValid fields of
      Nothing -> List.foldl' (applyTxSimple proof count) epochAccurateModel fields
      Just True -> List.foldl' (applyTxSimple proof count) epochAccurateModel fields
      Just False -> List.foldl' (applyTxFail proof count nextTxIx) epochAccurateModel fields

epochBoundary :: forall era. Proof era -> EpochNo -> EpochNo -> Model era -> Model era
epochBoundary proof transactionEpoch modelEpoch model =
  if transactionEpoch > modelEpoch
    then
      applyRUpd ru $
        model
          { mEL = transactionEpoch
          }
    else model
  where
    ru = createRUpdNonPulsing' proof model

applyTxSimple :: EraPParams era => Proof era -> Int -> Model era -> TxField era -> Model era
applyTxSimple proof count model field = case field of
  Body body1 -> applyTxBody proof count model body1
  BodyI fs -> List.foldl' (applyField proof count) model fs
  TxWits _ -> model
  WitnessesI _ -> model
  AuxData _ -> model
  Valid _ -> model

applyTxBody :: EraPParams era => Proof era -> Int -> Model era -> TxBody era -> Model era
applyTxBody proof count model tx = List.foldl' (applyField proof count) model (abstractTxBody proof tx)

applyField :: EraPParams era => Proof era -> Int -> Model era -> TxBodyField era -> Model era
applyField proof count model field = case field of
  (Inputs txins) -> model {mUTxO = Map.withoutKeys (mUTxO model) txins}
  (Outputs seqo) -> case Map.lookup count (mIndex model) of
    Nothing -> error ("Output not found phase1: " ++ show (mIndex model))
    Just (TxId hash) -> model {mUTxO = Map.union newstuff (mUTxO model)}
      where
        newstuff = additions hash minBound (toList seqo)
  (Txfee coin) -> model {mFees = coin <+> (mFees model)}
  (Certs seqc) -> List.foldl' applyCert model (toList seqc)
  (Wdrls (Wdrl m)) -> Map.foldlWithKey' (applyWdrl proof) model m
  _other -> model

applyWdrl :: Proof era -> Model era -> RewardAcnt (EraCrypto era) -> Coin -> Model era
applyWdrl _proof model (RewardAcnt _network cred) coin =
  model {mRewards = Map.adjust (\c -> c <-> coin) cred (mRewards model)}

applyCert :: EraPParams era => Model era -> DCert (EraCrypto era) -> Model era
applyCert model dcert = case dcert of
  (DCertDeleg (RegKey x)) ->
    model
      { mRewards = Map.insert x (Coin 0) (mRewards model)
      , mKeyDeposits = Map.insert x (pp ^. ppKeyDepositL) (mKeyDeposits model)
      , mDeposited = mDeposited model <+> pp ^. ppKeyDepositL
      }
    where
      pp = mPParams model
  (DCertDeleg (DeRegKey x)) -> case Map.lookup x (mRewards model) of
    Nothing -> error ("DeRegKey not in rewards: " <> show (pcCredential x))
    Just (Coin 0) ->
      model
        { mRewards = Map.delete x (mRewards model)
        , mKeyDeposits = Map.delete x (mKeyDeposits model)
        , mDeposited = mDeposited model <-> keyDeposit
        }
      where
        keyDeposit = case Map.lookup x (mKeyDeposits model) of
          Nothing -> mempty
          Just c -> c
    Just (Coin _n) -> error "DeRegKey with non-zero balance"
  (DCertDeleg (Delegate (Delegation cred hash))) ->
    model {mDelegations = Map.insert cred hash (mDelegations model)}
  (DCertPool (RegPool poolparams)) ->
    model
      { mPoolParams = Map.insert hk poolparams (mPoolParams model)
      , mDeposited =
          if Map.member hk (mPoolDeposits model)
            then mDeposited model
            else mDeposited model <+> pp ^. ppPoolDepositL
      , mPoolDeposits -- Only add if it isn't already there
        =
          case Map.lookup hk (mPoolDeposits model) of
            Just _ -> mPoolDeposits model
            Nothing -> Map.insert hk (pp ^. ppPoolDepositL) (mPoolDeposits model)
      }
    where
      hk = ppId poolparams
      pp = mPParams model
  (DCertPool (RetirePool keyhash epoch)) ->
    model
      { mRetiring = Map.insert keyhash epoch (mRetiring model)
      , mDeposited = mDeposited model <-> pp ^. ppPoolDepositL
      }
    where
      pp = mPParams model
  (DCertGenesis _) -> model
  (DCertMir _) -> model

-- =========================================================
-- What to do if the second phase does not validatate.
-- Process and use Collateral to pay fees

data CollInfo era = CollInfo
  { ciBal :: Coin -- Balance of all the collateral inputs
  , ciRet :: Coin -- Coin amount of the collateral return output
  , ciDelset :: Set (TxIn (EraCrypto era)) -- The set of inputs to delete from the UTxO
  , ciAddmap :: Map (TxIn (EraCrypto era)) (TxOut era) -- Things to Add to the UTxO
  }

emptyCollInfo :: CollInfo era
emptyCollInfo = CollInfo (Coin 0) (Coin 0) Set.empty Map.empty

-- | Collect information about how to process Collateral, in a second phase failure.
collInfo ::
  (Reflect era, HasCallStack) =>
  Int ->
  TxIx ->
  Model era ->
  CollInfo era ->
  TxBodyField era ->
  CollInfo era
collInfo count firstTxIx model info field = case field of
  CollateralReturn SNothing -> info
  CollateralReturn (SJust txout) ->
    case Map.lookup count (mIndex model) of
      Nothing -> error ("Output not found phase2: " ++ show (count, mIndex model))
      Just (TxId hash) ->
        info
          { ciRet = txout ^. coinTxOutL
          , ciAddmap = newstuff
          }
        where
          newstuff = additions hash firstTxIx [txout]
  Collateral inputs ->
    info
      { ciDelset = inputs
      , ciBal = txInBalance inputs (mUTxO model)
      }
  _ -> info

updateInfo :: CollInfo era -> Model era -> Model era
updateInfo info m =
  m
    { mUTxO = Map.union (ciAddmap info) (Map.withoutKeys (mUTxO m) (ciDelset info))
    , mFees = mFees m <+> ciBal info <-> ciRet info
    }

applyTxFail :: Reflect era => Proof era -> Int -> TxIx -> Model era -> TxField era -> Model era
applyTxFail proof count nextTxIx model field = case field of
  Body body2 -> updateInfo info model
    where
      info = List.foldl' (collInfo count nextTxIx model) emptyCollInfo (abstractTxBody proof body2)
  BodyI fs -> updateInfo info model
    where
      info = List.foldl' (collInfo count nextTxIx model) emptyCollInfo fs
  TxWits _ -> model
  WitnessesI _ -> model
  AuxData _ -> model
  Valid _ -> model

-- =======================================

additions ::
  SafeHash (EraCrypto era) EraIndependentTxBody ->
  TxIx ->
  [TxOut era] ->
  Map (TxIn (EraCrypto era)) (TxOut era)
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
      tx = (notValidatingTx proof) {isValid = IsValid False}
      allinputs = txbody ^. allInputsTxBodyF
      txbody = body tx
      doc = pcTx proof tx
      model1 =
        (mNewEpochStateZero @(BabbageEra Mock))
          { mUTxO = Map.restrictKeys (unUTxO (initUTxO proof)) allinputs
          , mCount = 0
          , mFees = Coin 10
          , mIndex = Map.singleton 0 (TxId (hashAnnotated txbody))
          }
      model2 = applyTx proof 0 (SlotNo 0) model1 tx
  print (pcModelNewEpochState proof model1)
  print doc
  print (pcModelNewEpochState proof model2)

filterRewards ::
  EraPParams era =>
  PParams era ->
  Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))) ->
  ( Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era)))
  , Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era)))
  )
filterRewards pp rewards =
  if pvMajor (pp ^. ppProtocolVersionL) > natVersion @2
    then (rewards, Map.empty)
    else
      let mp = Map.map Set.deleteFindMin rewards
       in (Map.map (Set.singleton . fst) mp, Map.filter (not . Set.null) $ Map.map snd mp)

filterAllRewards ::
  EraPParams era =>
  Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))) ->
  Model era ->
  ( Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era)))
  , Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era)))
  , Set (Credential 'Staking (EraCrypto era))
  , Coin
  )
filterAllRewards rs' m =
  (registered, eraIgnored, unregistered, totalUnregistered)
  where
    pp = mPParams m
    (regRU, unregRU) =
      Map.partitionWithKey
        (\k _ -> eval (k ∈ dom (mRewards m)))
        rs'
    totalUnregistered = fold $ aggregateRewards (pp ^. ppProtocolVersionL) unregRU
    unregistered = Map.keysSet unregRU

    (registered, eraIgnored) = filterRewards pp regRU

applyRUpd ::
  forall era.
  RewardUpdateOld (EraCrypto era) ->
  Model era ->
  Model era
applyRUpd ru m =
  m
    { mFees = mFees m `addDeltaCoin` deltaFOld @(EraCrypto era) ru
    , mRewards = Map.unionWith (<>) (mRewards m) (rsOld ru)
    }

notValidatingTx ::
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingTx pf =
  newTx
    pf
    [ Body notValidatingBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated notValidatingBody) (someKeys pf)]
        , ScriptWits' [never 0 pf]
        , DataWits' [Data (PV1.I 0)]
        , RdmrWits redeemers
        ]
    ]
  where
    notValidatingBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 2]
        , Collateral' [mkGenesisTxIn 12]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 2995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pparams pf) [PlutusV1] redeemers (mkTxDats (Data (PV1.I 0))))
        ]
    redeemers =
      Redeemers
        ( Map.fromList
            [
              ( RdmrPtr Tag.Spend 0
              , (Data (PV1.I 1), ExUnits 5000 5000)
              )
            ]
        )
