{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Strategy for Generic Tests
--   Make the GenState include a Mode of the NewEpochState, modify
--   the ModelNewEpochState to reflect what we generated.
module Test.Cardano.Ledger.Generic.GenState (
  GenEnv (..),
  GenRS,
  GenState (..),
  GenSize (..),
  PlutusPurposeTag (..),
  plutusPurposeTags,
  mkRedeemers,
  mkRedeemersFromTags,
  mkPlutusPurposePointer,
  mkAlonzoPlutusPurposePointer,
  mkConwayPlutusPurposePointer,
  elementsT, -- TODO move to a utilities module
  frequencyT, -- TODO move to a utilities module
  positiveSingleDigitInt,
  nonNegativeSingleDigitInt,
  genSetElem,
  genMapElem,
  genMapElemWhere,
  genRewardVal,
  genPositiveVal,
  genGenState,
  genGenEnv,
  genValidityInterval,
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
  modifyModel,
  runGenRS,
  ioGenRS,
  small,
  genDatumWithHash,
  genKeyHash,
  genScript,
  genFreshKeyHash,
  genCredential,
  genFreshCredential,
  genFreshRegCred,
  genPool,
  genPoolParams,
  genRewards,
  genNewPool,
  genRetirementHash,
  initStableFields,
  modifyGenStateInitialUtxo,
  modifyGenStateInitialRewards,
  modifyModelCount,
  modifyModelIndex,
  modifyModelUTxO,
  modifyModelMutFee,
) where

import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  Timelock (..),
  ValidityInterval (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo.Scripts hiding (Script)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (EpochInterval (..), Network (Testnet), inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), StakeCredential)
import Cardano.Ledger.Keys (
  KeyHash (..),
  KeyRole (..),
  coerceKeyRole,
  hashKey,
 )
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  LedgerState (..),
  PState (..),
  RewardAccounts,
  smartUTxOState,
  totalObligation,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.TxIn (TxId, TxIn (..))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (join, replicateM, when, zipWithM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), ask, asks, get, gets, modify)
import Control.SetAlgebra (eval, (⨃))
import Data.Default.Class (Default (def))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Word (Word32, Word64)
import Lens.Micro
import Numeric.Natural
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields
import Test.Cardano.Ledger.Generic.Functions (
  alwaysFalse,
  alwaysTrue,
  primaryLanguage,
  protocolVersion,
  txoutFields,
 )
import Test.Cardano.Ledger.Generic.ModelState (
  ModelNewEpochState (..),
  genDelegsZero,
  instantaneousRewardsZero,
  mKeyDeposits,
  mNewEpochStateZero,
  mPoolDeposits,
  pcModelNewEpochState,
 )
import Test.Cardano.Ledger.Generic.PrettyCore (
  PDoc,
  PrettyA (..),
  pcCoin,
  pcCredential,
  pcIndividualPoolStake,
  pcKeyHash,
  pcPoolParams,
  pcTxIn,
  pcTxOut,
  ppInt,
  ppMap,
  ppRecord,
  ppSet,
  ppString,
  ppValidityInterval,
 )
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Updaters (defaultCostModels, newPParams)
import Test.Tasty.QuickCheck (
  Gen,
  Positive (..),
  arbitrary,
  choose,
  chooseInt,
  elements,
  frequency,
  generate,
 )

-- =================================================

-- | Constants that determine how big a GenState is generated.
data GenSize = GenSize
  { treasury :: !Integer
  , reserves :: !Integer
  , startSlot :: !Word64
  , slotDelta :: !(Word64, Word64)
  , blocksizeMax :: !Integer
  , collInputsMax :: !Natural
  , spendInputsMax :: !Int
  , refInputsMax :: !Int
  , utxoChoicesMax :: !Int
  , certificateMax :: !Int
  , withdrawalMax :: !Int
  , oldUtxoPercent :: !Int -- between 0-100, 10 means pick an old UTxO 10% of the time
  , maxStablePools :: !Int
  , invalidScriptFreq :: !Int -- percentage
  }
  deriving (Show)

data GenEnv era = GenEnv
  { gePParams :: !(PParams era)
  , geSize :: !GenSize
  }

data GenState era = GenState
  { gsValidityInterval :: !ValidityInterval
  , gsKeys :: !(Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)))
  , gsScripts :: !(Map (ScriptHash (EraCrypto era)) (Script era))
  , gsPlutusScripts :: !(Map (ScriptHash (EraCrypto era), PlutusPurposeTag) (IsValid, Script era))
  , gsDatums :: !(Map (DataHash (EraCrypto era)) (Data era))
  , gsVI :: !(Map ValidityInterval (Set (ScriptHash (EraCrypto era))))
  , gsModel :: !(ModelNewEpochState era)
  , gsInitialUtxo :: !(Map (TxIn (EraCrypto era)) (TxOut era))
  , gsInitialRewards :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , gsInitialDelegations ::
      !(Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
  , gsInitialPoolParams :: !(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
  , gsInitialPoolDistr ::
      !(Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
  , -- Stable fields are stable from initialization to the end of the generation process
    gsStablePools :: !(Set (KeyHash 'StakePool (EraCrypto era)))
  , gsStableDelegators :: !(Set (StakeCredential (EraCrypto era)))
  , gsAvoidCred :: !(Set (Credential 'Staking (EraCrypto era)))
  , gsAvoidKey :: !(Set (KeyHash 'StakePool (EraCrypto era)))
  , gsProof :: !(Proof era)
  , gsGenEnv :: !(GenEnv era)
  , gsSeedIdx :: !Int
  }

emptyGenState :: Reflect era => Proof era -> GenEnv era -> GenState era
emptyGenState proof genv =
  GenState
    (ValidityInterval SNothing SNothing)
    mempty
    mempty
    mempty
    mempty
    mempty
    (mNewEpochStateZero {mPParams = gePParams genv})
    Map.empty
    Map.empty
    Map.empty
    Map.empty
    Map.empty
    Set.empty
    Set.empty
    Set.empty
    Set.empty
    proof
    genv
    0
{-# NOINLINE emptyGenState #-}

instance Default GenSize where
  def =
    GenSize
      { treasury = 1000000
      , reserves = 1000000
      , startSlot = 0
      , slotDelta = (3, 7)
      , blocksizeMax = 10
      , collInputsMax = 5
      , oldUtxoPercent = 15
      , spendInputsMax = 10
      , refInputsMax = 6
      , utxoChoicesMax = 30
      , certificateMax = 10
      , withdrawalMax = 10
      , maxStablePools = 5
      , invalidScriptFreq = 5
      }

small :: GenSize
small =
  GenSize
    { treasury = 1000000
    , reserves = 1000000
    , startSlot = 0
    , slotDelta = (2, 5)
    , blocksizeMax = 3
    , collInputsMax = 2
    , oldUtxoPercent = 5
    , spendInputsMax = 3
    , refInputsMax = 1
    , utxoChoicesMax = 12
    , certificateMax = 2
    , withdrawalMax = 2
    , maxStablePools = 4
    , invalidScriptFreq = 5
    }

data PlutusPurposeTag
  = Spending
  | Minting
  | Certifying
  | Rewarding
  | Voting
  | Proposing
  deriving (Eq, Ord, Show, Enum, Bounded)

plutusPurposeTags :: Proof era -> [PlutusPurposeTag]
plutusPurposeTags = \case
  Shelley {} -> []
  Allegra {} -> []
  Mary {} -> []
  Alonzo {} -> [Spending .. Rewarding]
  Babbage {} -> [Spending .. Rewarding]
  Conway {} -> [Spending .. Proposing]

mkRedeemers ::
  forall era.
  Proof era ->
  [(PlutusPurpose AsIx era, (Data era, ExUnits))] ->
  Redeemers era
mkRedeemers proof redeemerMap =
  -- Pattern match on proof is needed in order to avoid leacking Ord constraint.
  case proof of
    Shelley {} -> error "No Redeemers"
    Allegra {} -> error "No Redeemers"
    Mary {} -> error "No Redeemers"
    Alonzo {} -> Redeemers $ Map.fromList redeemerMap
    Babbage {} -> Redeemers $ Map.fromList redeemerMap
    Conway {} -> Redeemers $ Map.fromList redeemerMap

mkRedeemersFromTags ::
  forall era.
  Proof era ->
  [((PlutusPurposeTag, Word32), (Data era, ExUnits))] ->
  Redeemers era
mkRedeemersFromTags proof redeemerPointers =
  case proof of
    Shelley {} -> error "No Redeemers"
    Allegra {} -> error "No Redeemers"
    Mary {} -> error "No Redeemers"
    Alonzo {} -> mkRedeemers proof redeemerAssocs
    Babbage {} -> mkRedeemers proof redeemerAssocs
    Conway {} -> mkRedeemers proof redeemerAssocs
  where
    redeemerAssocs :: [(PlutusPurpose AsIx era, (Data era, ExUnits))]
    redeemerAssocs =
      [ (mkPlutusPurposePointer proof tag i, redeemer)
      | ((tag, i), redeemer) <- redeemerPointers
      ]

mkPlutusPurposePointer ::
  Proof era ->
  PlutusPurposeTag ->
  Word32 ->
  PlutusPurpose AsIx era
mkPlutusPurposePointer proof tag i =
  case proof of
    Shelley {} -> error "No PlutusPurpose"
    Allegra {} -> error "No PlutusPurpose"
    Mary {} -> error "No PlutusPurpose"
    Alonzo {} -> mkAlonzoPlutusPurposePointer tag i
    Babbage {} -> mkAlonzoPlutusPurposePointer tag i
    Conway {} -> mkConwayPlutusPurposePointer tag i

mkAlonzoPlutusPurposePointer ::
  forall era.
  Era era =>
  PlutusPurposeTag ->
  Word32 ->
  AlonzoPlutusPurpose AsIx era
mkAlonzoPlutusPurposePointer tag i =
  case tag of
    Spending -> AlonzoSpending (AsIx i)
    Minting -> AlonzoMinting (AsIx i)
    Certifying -> AlonzoCertifying (AsIx i)
    Rewarding -> AlonzoRewarding (AsIx i)
    _ -> error $ "Unsupported tag: " ++ show tag ++ " in era " ++ eraName @era

mkConwayPlutusPurposePointer :: PlutusPurposeTag -> Word32 -> ConwayPlutusPurpose AsIx era
mkConwayPlutusPurposePointer tag i =
  case tag of
    Spending -> ConwaySpending (AsIx i)
    Minting -> ConwayMinting (AsIx i)
    Certifying -> ConwayCertifying (AsIx i)
    Rewarding -> ConwayRewarding (AsIx i)
    Voting -> ConwayVoting (AsIx i)
    Proposing -> ConwayProposing (AsIx i)

-- =====================================================================
-- Accessing information

getSlot :: GenState era -> SlotNo
getSlot = SlotNo . startSlot . geSize . gsGenEnv

getSlotDelta :: GenState era -> (Word64, Word64)
getSlotDelta = slotDelta . geSize . gsGenEnv

getBlocksizeMax :: GenState era -> Integer
getBlocksizeMax = blocksizeMax . geSize . gsGenEnv

getSpendInputsMax :: GenState era -> Int
getSpendInputsMax = spendInputsMax . geSize . gsGenEnv

getRefInputsMax :: GenState era -> Int
getRefInputsMax = refInputsMax . geSize . gsGenEnv

getCertificateMax :: GenState era -> Int
getCertificateMax = certificateMax . geSize . gsGenEnv

getUtxoChoicesMax :: GenState era -> Int
getUtxoChoicesMax = utxoChoicesMax . geSize . gsGenEnv

getCollInputsMax :: GenState era -> Natural
getCollInputsMax = collInputsMax . geSize . gsGenEnv

getOldUtxoPercent :: GenState era -> Int
getOldUtxoPercent x = max 0 (min 100 (oldUtxoPercent (geSize (gsGenEnv x))))

getTreasury :: GenState era -> Coin
getTreasury = Coin . treasury . geSize . gsGenEnv

getReserves :: GenState era -> Coin
getReserves = Coin . reserves . geSize . gsGenEnv

-- ========================================================
-- Modifying fields of the GenState as side effects in GenRS

setVi :: GenState era -> ValidityInterval -> GenState era
setVi gs vi = gs {gsValidityInterval = vi}
{-# NOINLINE setVi #-}

modifyGenStateKeys ::
  ( Map.Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)) ->
    Map.Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))
  ) ->
  GenRS era ()
modifyGenStateKeys f = modify (\x -> x {gsKeys = f (gsKeys x)})

modifyGenStateDatums ::
  (Map.Map (DataHash (EraCrypto era)) (Data era) -> Map.Map (DataHash (EraCrypto era)) (Data era)) ->
  GenRS era ()
modifyGenStateDatums f = modify (\x -> x {gsDatums = f (gsDatums x)})

modifyGenStateVI ::
  ( Map ValidityInterval (Set (ScriptHash (EraCrypto era))) ->
    Map ValidityInterval (Set (ScriptHash (EraCrypto era)))
  ) ->
  GenRS era ()
modifyGenStateVI f = modify (\x -> x {gsVI = f (gsVI x)})

modifyGenStateInitialRewards ::
  ( Map.Map (Credential 'Staking (EraCrypto era)) Coin ->
    Map.Map (Credential 'Staking (EraCrypto era)) Coin
  ) ->
  GenRS era ()
modifyGenStateInitialRewards f = modify $ \st -> st {gsInitialRewards = f (gsInitialRewards st)}

modifyGenStateInitialUtxo ::
  ( Map (TxIn (EraCrypto era)) (TxOut era) ->
    Map (TxIn (EraCrypto era)) (TxOut era)
  ) ->
  GenRS era ()
modifyGenStateInitialUtxo f = modify $ \st -> st {gsInitialUtxo = f (gsInitialUtxo st)}

modifyGenStateAvoidCred ::
  ( Set (Credential 'Staking (EraCrypto era)) ->
    Set (Credential 'Staking (EraCrypto era))
  ) ->
  GenRS era ()
modifyGenStateAvoidCred f = modify (\st -> st {gsAvoidCred = f (gsAvoidCred st)})

modifyGenStateAvoidKey ::
  (Set (KeyHash 'StakePool (EraCrypto era)) -> Set (KeyHash 'StakePool (EraCrypto era))) ->
  GenRS era ()
modifyGenStateAvoidKey f = modify (\s -> s {gsAvoidKey = f (gsAvoidKey s)})

modifyGenStateStablePools ::
  (Set (KeyHash 'StakePool (EraCrypto era)) -> Set (KeyHash 'StakePool (EraCrypto era))) ->
  GenRS era ()
modifyGenStateStablePools f = modify (\gs -> gs {gsStablePools = f (gsStablePools gs)})

modifyGenStateInitialPoolParams ::
  ( Map.Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)) ->
    Map.Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era))
  ) ->
  GenRS era ()
modifyGenStateInitialPoolParams f = modify (\gs -> gs {gsInitialPoolParams = f (gsInitialPoolParams gs)})

modifyGenStateInitialPoolDistr ::
  ( Map.Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)) ->
    Map.Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era))
  ) ->
  GenRS era ()
modifyGenStateInitialPoolDistr f = modify (\gs -> gs {gsInitialPoolDistr = f (gsInitialPoolDistr gs)})

modifyGenStateStableDelegators ::
  (Set (StakeCredential (EraCrypto era)) -> Set (StakeCredential (EraCrypto era))) ->
  GenRS era ()
modifyGenStateStableDelegators f = modify (\gs -> gs {gsStableDelegators = f (gsStableDelegators gs)})

modifyGenStateInitialDelegations ::
  ( Map.Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
    Map.Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
  ) ->
  GenRS era ()
modifyGenStateInitialDelegations f = modify (\gs -> gs {gsInitialDelegations = f (gsInitialDelegations gs)})

modifyGenStateScripts ::
  ( Map.Map (ScriptHash (EraCrypto era)) (Script era) ->
    Map.Map (ScriptHash (EraCrypto era)) (Script era)
  ) ->
  GenRS era ()
modifyGenStateScripts f =
  modify $ \gs -> gs {gsScripts = f (gsScripts gs)}

modifyPlutusScripts ::
  ( Map.Map (ScriptHash (EraCrypto era), PlutusPurposeTag) (IsValid, Script era) ->
    Map.Map (ScriptHash (EraCrypto era), PlutusPurposeTag) (IsValid, Script era)
  ) ->
  GenRS era ()
modifyPlutusScripts f = modify (\gs -> gs {gsPlutusScripts = f (gsPlutusScripts gs)})

-- ===================================================
-- functions that modify individual fields of ModelState

modifyModel :: (ModelNewEpochState era -> ModelNewEpochState era) -> GenRS era ()
modifyModel f = modify (\gstate -> gstate {gsModel = f (gsModel gstate)})

modifyModelDelegations ::
  ( Map.Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
    Map.Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
  ) ->
  GenRS era ()
modifyModelDelegations f = modifyModel (\ms -> ms {mDelegations = f (mDelegations ms)})

modifyModelRewards ::
  ( Map.Map (Credential 'Staking (EraCrypto era)) Coin ->
    Map.Map (Credential 'Staking (EraCrypto era)) Coin
  ) ->
  GenRS era ()
modifyModelRewards f = modifyModel (\ms -> ms {mRewards = f (mRewards ms)})

modifyModelDeposited :: (Coin -> Coin) -> GenRS era ()
modifyModelDeposited f = modifyModel (\ms -> ms {mDeposited = f (mDeposited ms)})

modifyKeyDeposits :: Credential 'Staking (EraCrypto era) -> Coin -> GenRS era ()
modifyKeyDeposits cred c =
  modifyModel (\ms -> ms {mKeyDeposits = Map.insert cred c (mKeyDeposits ms)})

modifyModelPoolParams ::
  ( Map.Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)) ->
    Map.Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era))
  ) ->
  GenRS era ()
modifyModelPoolParams f = modifyModel (\ms -> ms {mPoolParams = f (mPoolParams ms)})

modifyModelPoolDistr ::
  ( Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)) ->
    Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era))
  ) ->
  GenRS era ()
modifyModelPoolDistr f = modifyModel (\ms -> ms {mPoolDistr = f (mPoolDistr ms)})

modifyModelKeyDeposits :: KeyHash 'StakePool (EraCrypto era) -> Coin -> GenRS era ()
modifyModelKeyDeposits kh pooldeposit =
  modifyModel (\ms -> ms {mPoolDeposits = Map.insert kh pooldeposit (mPoolDeposits ms)})

modifyModelCount :: (Int -> Int) -> GenRS era ()
modifyModelCount f = modifyModel (\ms -> ms {mCount = f (mCount ms)})

modifyModelIndex ::
  (Map Int (TxId (EraCrypto era)) -> Map Int (TxId (EraCrypto era))) ->
  GenRS era ()
modifyModelIndex f = modifyModel (\ms -> ms {mIndex = f (mIndex ms)})

modifyModelUTxO ::
  (Map (TxIn (EraCrypto era)) (TxOut era) -> Map (TxIn (EraCrypto era)) (TxOut era)) ->
  GenRS era ()
modifyModelUTxO f = modifyModel (\ms -> ms {mUTxO = f (mUTxO ms)})

modifyModelMutFee ::
  ( Map (TxIn (EraCrypto era)) (TxOut era) ->
    Map (TxIn (EraCrypto era)) (TxOut era)
  ) ->
  GenRS era ()
modifyModelMutFee f = modifyModel (\m -> m {mMutFee = f (mMutFee m)})

-- ==============================================================
-- The Monad

type GenRS era = RWST (GenEnv era) () (GenState era) Gen

genMapElem :: Map k a -> Gen (Maybe (k, a))
genMapElem m
  | n == 0 = pure Nothing
  | otherwise = do
      i <- choose (0, n - 1)
      pure $ Just $ Map.elemAt i m
  where
    n = Map.size m

genSetElem :: Set a -> Gen (Maybe a)
genSetElem m
  | n == 0 = pure Nothing
  | otherwise = do
      i <- choose (0, n - 1)
      pure $ Just $ Set.elemAt i m
  where
    n = Set.size m

-- | Use up to 'tries' attempts to choose a random (k,a) pair from 'm', that meets predicate 'p'
genMapElemWhere :: Map k a -> Int -> (k -> a -> Bool) -> Gen (Maybe (k, a))
genMapElemWhere m tries p
  | tries <= 0 = pure Nothing
  | n == 0 = pure Nothing
  | otherwise = do
      i <- choose (0, n - 1)
      let (k, a) = Map.elemAt i m
      if p k a
        then pure $ Just $ (k, a)
        else genMapElemWhere m (tries - 1) p
  where
    n = Map.size m

elementsT :: (Monad (t Gen), MonadTrans t) => [t Gen b] -> t Gen b
elementsT = join . lift . elements

frequencyT :: (Monad (t Gen), MonadTrans t) => [(Int, t Gen b)] -> t Gen b
frequencyT [] = error ("frequencyT called with empty list")
frequencyT choices = join . lift . frequency . map (pure <$>) $ choices

-- | Gen a positive single digit Int, on a skewed distribution that
--   favors 2,3,4,5 but occasionally gets others
positiveSingleDigitInt :: Gen Int
positiveSingleDigitInt =
  frequency (map f [(1, 1), (5, 2), (4, 3), (4, 4), (3, 5), (2, 6), (1, 7), (1, 8), (1, 9)])
  where
    f (x, y) = (x, pure y)

-- | Gen a non-negative single digit Int, on a skewed distribution that
--   favors 2,3,4,5 but occasionally gets others
nonNegativeSingleDigitInt :: Gen Int
nonNegativeSingleDigitInt =
  frequency (map f [(1, 0), (2, 1), (5, 2), (4, 3), (3, 4), (2, 5), (2, 6), (1, 7), (1, 8), (1, 9)])
  where
    f (x, y) = (x, pure y)

-- | Generate a non-zero value
genPositiveVal :: Val v => Gen v
genPositiveVal = inject . Coin . getPositive <$> arbitrary

-- | Generate a value (which is occaisionally 0) useful in generating Rewards, where we need a
--   few 0's, because we cannot generate a DeReg certificates, without a 0 Reg value.
--   Also used when generating the CollReturn, where an occasional 0 would be nice
genRewardVal :: Val v => Gen v
genRewardVal = frequency [(3, pure mempty), (97, genPositiveVal)]

-- | Test if the Payment part of the Address in the TxOut
--   is valid in the current ValidityInterval. Using the simple rule allowing
--   only (Key or Plutus or MutiSig) locking. Disallowing all Timelock scripts
validTxOut ::
  Proof era ->
  Map (ScriptHash (EraCrypto era)) (Script era) ->
  TxIn (EraCrypto era) ->
  TxOut era ->
  Bool
validTxOut proof m _txin txout = case txoutFields proof txout of
  (Addr _ (KeyHashObj _) _, _, _) -> True
  (Addr _ (ScriptHashObj h) _, _, _) -> case (proof, Map.lookup h m) of
    (Conway, Just (PlutusScript _)) -> True
    (Babbage, Just (PlutusScript _)) -> True
    (Alonzo, Just (PlutusScript _)) -> True
    (Shelley, Just _msig) -> True
    _ -> False
  _bootstrap -> False

-- | Pick a UTxO element where we can use it in a new Tx. Most of the time we generate new
--   elements for each Tx, but once in a while we choose an existing one. We must be carefull
--   that that the Pay credential of the TxOut can run in the curent ValidityInterval
--   A crude but simple way is to insist Pay credential is either Key locked, or locked
--   with Plutus or MultiSig scripts, and return False for any Timelock scripts.
getUtxoElem :: Reflect era => GenRS era (Maybe (TxIn (EraCrypto era), TxOut era))
getUtxoElem = do
  x <- gets (mUTxO . gsModel)
  scriptmap <- gets gsScripts
  lift $ genMapElemWhere x 20 (validTxOut reify scriptmap)

getUtxoTest :: GenRS era (TxIn (EraCrypto era) -> Bool)
getUtxoTest = do
  x <- gets (mUTxO . gsModel)
  pure (`Map.member` x)

-- | To compute deposits we need a function that tells if the KeyHash is a new Pool
--   Compute this function before we do any generation, since such generation
--   may actually add to the mPoolParams, and then the added thing won't appear new.
getNewPoolTest :: GenRS era (KeyHash 'StakePool (EraCrypto era) -> Bool)
getNewPoolTest = do
  poolparams <- gets (mPoolParams . gsModel)
  pure (`Map.member` poolparams)

-- ========================================================================
-- Tools to get started in the Monad

runGenRS ::
  Reflect era =>
  Proof era ->
  GenSize ->
  GenRS era a ->
  Gen (a, GenState era)
runGenRS proof gsize action = do
  genenv <- genGenEnv proof gsize
  (ans, state, ()) <- runRWST action genenv (emptyGenState proof genenv)
  pure (ans, state)

-- | Should not be used in tests, this is a helper function to be used in ghci only!
ioGenRS :: Reflect era => Proof era -> GenSize -> GenRS era ans -> IO (ans, GenState era)
ioGenRS proof gsize action = generate $ runGenRS proof gsize action

-- | Generate a random, well-formed, GenEnv
genGenEnv :: EraPParams era => Proof era -> GenSize -> Gen (GenEnv era)
genGenEnv proof gsize = do
  maxTxExUnits <- arbitrary :: Gen ExUnits
  maxCollateralInputs <- elements [1 .. collInputsMax gsize]
  collateralPercentage <- fromIntegral <$> chooseInt (1, 10000)
  minfeeA <- Coin <$> choose (0, 1000)
  minfeeB <- Coin <$> choose (0, 10000)
  let pp =
        newPParams
          proof
          [ MinfeeA minfeeA
          , MinfeeB minfeeB
          , defaultCostModels proof
          , MaxValSize 1000
          , MaxTxSize $ fromIntegral (maxBound :: Int)
          , MaxTxExUnits maxTxExUnits
          , MaxCollateralInputs maxCollateralInputs
          , CollateralPercentage collateralPercentage
          , ProtocolVersion $ protocolVersion proof
          , PoolDeposit $ Coin 5
          , KeyDeposit $ Coin 2
          , EMax $ EpochInterval 5
          ]
  pure $
    GenEnv
      { gePParams = pp
      , geSize = gsize
      }

genGenState :: Reflect era => Proof era -> GenSize -> Gen (GenState era)
genGenState proof gsize = do
  let slotNo = startSlot gsize
  minSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, slotNo))]
  maxSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (slotNo + 1, maxBound))]
  let vi = ValidityInterval (SlotNo <$> minSlotNo) (SlotNo <$> maxSlotNo)
  env <- genGenEnv proof gsize
  pure (setVi (emptyGenState proof env) vi)

-- | Generate a transaction body validity interval which is close in proximity
--  (less than a stability window) from the current slot.
genValidityInterval :: SlotNo -> Gen ValidityInterval
genValidityInterval (SlotNo s) = do
  let stabilityWindow = 29 -- < 3k/f many slots, where 10k is the epoch length
  start <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, s))]
  end <- frequency [(1, pure SNothing), (4, SJust <$> choose (s + 1, s + stabilityWindow))]
  pure $ ValidityInterval (SlotNo <$> start) (SlotNo <$> end)

-- =================================================================

pcGenState :: forall era. Reflect era => Proof era -> GenState era -> PDoc
pcGenState proof gs =
  ppRecord
    "GenState Summary"
    [ ("ValidityInterval", ppValidityInterval (gsValidityInterval gs))
    , ("Keymap", ppInt (Map.size (gsKeys gs)))
    , ("Scriptmap", ppInt (Map.size (gsScripts gs)))
    , ("PlutusScripts", ppInt (Map.size (gsPlutusScripts gs)))
    , ("Datums", ppInt (Map.size (gsDatums gs)))
    , ("VI-ScriptMap", ppInt (Map.size (gsVI gs)))
    , ("Model", pcModelNewEpochState @era proof (gsModel gs))
    , ("Initial Utxo", ppMap pcTxIn (pcTxOut @era proof) (gsInitialUtxo gs))
    , ("Initial Rewards", ppMap pcCredential pcCoin (gsInitialRewards gs))
    , ("Initial SPoolUView", ppMap pcCredential pcKeyHash (gsInitialDelegations gs))
    , ("Initial PoolParams", ppMap pcKeyHash pcPoolParams (gsInitialPoolParams gs))
    , ("Initial PoolDistr", ppMap pcKeyHash pcIndividualPoolStake (gsInitialPoolDistr gs))
    , ("Stable PoolParams", ppSet pcKeyHash (gsStablePools gs))
    , ("Stable Delegators", ppSet pcCredential (gsStableDelegators gs))
    , ("Previous RegKey", ppSet pcCredential (gsAvoidCred gs))
    , ("GenEnv", ppString "GenEnv ...")
    , ("Proof", ppString (show (gsProof gs)))
    ]

-- | Helper function for development and debugging in ghci
viewGenState :: Reflect era => Proof era -> GenSize -> Bool -> IO ()
viewGenState proof gsize verbose = do
  st <- generate (genGenState proof gsize)
  when verbose $ print (pcGenState proof st)

instance Reflect era => PrettyA (GenState era) where prettyA = pcGenState reify

instance Reflect era => Show (GenState era) where
  show x = show (pcGenState reify x)

-- =====================================================================
-- Build an Initial LedgerState for a Trace from a GenState, after
-- generating a coherent Trace (a sequence of Transactions that can
-- logically be applied one after another)

initialLedgerState :: forall era. Reflect era => GenState era -> LedgerState era
initialLedgerState gstate = LedgerState utxostate dpstate
  where
    umap =
      UM.unify
        (Map.map rdpair (gsInitialRewards gstate))
        Map.empty
        (gsInitialDelegations gstate)
        Map.empty
    utxostate = smartUTxOState pp (UTxO (gsInitialUtxo gstate)) deposited (Coin 0) emptyGovState mempty
    dpstate = CertState def pstate dstate
    dstate =
      DState
        umap
        Map.empty
        genDelegsZero
        instantaneousRewardsZero
    pstate = PState pools Map.empty Map.empty (fmap (const poolDeposit) pools)
    -- In a wellformed LedgerState the deposited equals the obligation
    deposited = totalObligation dpstate (utxostate ^. utxosGovStateL)
    pools = gsInitialPoolParams gstate
    pp = mPParams (gsModel gstate)
    keyDeposit = pp ^. ppKeyDepositL
    !poolDeposit = pp ^. ppPoolDepositL
    rdpair rew = UM.RDPair (UM.compactCoinOrError rew) (UM.compactCoinOrError keyDeposit)

-- =============================================
-- Generators of inter-related items

-- Adds to the gsKeys
genKeyHash :: Reflect era => GenRS era (KeyHash kr (EraCrypto era))
genKeyHash = do
  keyPair <- lift arbitrary
  let keyHash = hashKey $ vKey keyPair
  modifyGenStateKeys (Map.insert keyHash keyPair)
  pure $ coerceKeyRole keyHash

-- Adds to the gsDatums
genDatumWithHash :: Era era => GenRS era (DataHash (EraCrypto era), Data era)
genDatumWithHash = do
  datum <- lift arbitrary
  let datumHash = hashData datum
  modifyGenStateDatums (Map.insert datumHash datum)
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

-- ===========================================================
-- Generate Era agnostic Scripts

-- Adds to gsScripts and gsPlutusScripts
genScript :: Reflect era => Proof era -> PlutusPurposeTag -> GenRS era (ScriptHash (EraCrypto era))
genScript proof tag = case proof of
  Conway -> elementsT [genTimelockScript, genPlutusScript proof tag]
  Babbage -> elementsT [genTimelockScript, genPlutusScript proof tag]
  Alonzo -> elementsT [genTimelockScript, genPlutusScript proof tag]
  Mary -> genTimelockScript
  Allegra -> genTimelockScript
  Shelley -> genMultiSigScript

-- Adds to gsScripts
genTimelockScript ::
  forall era.
  (AllegraEraScript era, Reflect era, NativeScript era ~ Timelock era) =>
  GenRS era (ScriptHash (EraCrypto era))
genTimelockScript = do
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
            [ requireTimeStart <$> mBefore
            , requireTimeExpire <$> mAfter
            , SJust requireSignature
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
  let corescript = fromNativeScript tlscript
  let scriptHash = hashScript @era corescript
      insertOrCreate x Nothing = Just (Set.singleton x)
      insertOrCreate x (Just s) = Just (Set.insert x s)
  modifyGenStateScripts (Map.insert scriptHash corescript)
  modifyGenStateVI (Map.alter (insertOrCreate scriptHash) vi)
  pure scriptHash

-- Adds to gsScripts
genMultiSigScript ::
  forall era.
  (Reflect era, ShelleyEraScript era, NativeScript era ~ MultiSig era) =>
  GenRS era (ScriptHash (EraCrypto era))
genMultiSigScript = do
  let genNestedMultiSig k
        | k > 0 =
            elementsT $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = elementsT nonRecTimelocks
      nonRecTimelocks = [requireSignature]
      requireSignature = RequireSignature @era <$> genKeyHash
      requireAllOf k = do
        n <- lift nonNegativeSingleDigitInt
        RequireAllOf . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
      requireAnyOf k = do
        n <- lift positiveSingleDigitInt
        RequireAnyOf . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
      requireMOf k = do
        n <- lift nonNegativeSingleDigitInt
        m <- lift $ choose (0, n)
        RequireMOf m . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
  msscript <- genNestedMultiSig (2 :: Natural)
  let corescript = fromNativeScript msscript
  let scriptHash = hashScript @era corescript
  modifyGenStateScripts (Map.insert scriptHash corescript)
  pure scriptHash

-- Adds to gsPlutusScripts
genPlutusScript ::
  forall era.
  Reflect era =>
  Proof era ->
  PlutusPurposeTag ->
  GenRS era (ScriptHash (EraCrypto era))
genPlutusScript proof tag = do
  falseFreq <- asks $ invalidScriptFreq . geSize
  isValid <- lift $ frequency [(falseFreq, pure False), (100 - falseFreq, pure True)]
  -- Plutus scripts alwaysSucceeds needs at least numArgs, while
  -- alwaysFails needs exactly numArgs to have the desired affect.
  -- For reasons unknown, this number differs from Alonzo to Babbage
  -- Perhaps because Babbage is using PlutusV2 scripts?
  let numArgs = case (proof, tag) of
        (Conway, Spending) -> 2
        (Conway, _) -> 1
        (Babbage, Spending) -> 2
        (Babbage, _) -> 1
        (_, Spending) -> 3
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
        Alonzo -> script
        Babbage -> script
        Conway -> script
        _ ->
          error
            ( "PlutusScripts are available starting in the Alonzo era. "
                ++ show proof
                ++ " does not support PlutusScripts."
            )
      scriptHash = hashScript @era corescript
  modifyPlutusScripts (Map.insert (scriptHash, tag) (IsValid isValid, corescript))
  pure scriptHash

-- ======================================================================
-- Generators of Transaction components
-- =======================================================================

-- | Generate a credential that can be used for supplied purpose (in case of
-- plutus scripts), while occasionally picking out randomly from previously
-- generated set. Returns the credential
-- Adds to both gsKeys and gsScripts and gsPlutusScript
-- via genKeyHash and genScript
genCredential ::
  forall era kr. Reflect era => PlutusPurposeTag -> GenRS era (Credential kr (EraCrypto era))
genCredential tag =
  frequencyT
    [ (35, KeyHashObj <$> genKeyHash')
    , (35, ScriptHashObj <$> genScript')
    , (10, pickExistingKeyHash)
    , (20, pickExistingScript)
    ]
  where
    genKeyHash' = do
      kh <- genFreshKeyHash -- We need to avoid some key credentials
      case tag of
        Rewarding -> modifyGenStateInitialRewards (Map.insert (KeyHashObj kh) (Coin 0))
        _ -> pure ()
      return $ coerceKeyRole kh
    genScript' = f (100 :: Int)
      where
        f n
          | n <= 0 = error "Failed to generate a fresh script hash"
          | otherwise = do
              sh <- genScript @era reify tag
              initialRewards <- gets gsInitialRewards
              avoidCredentials <- gets gsAvoidCred
              let newcred = ScriptHashObj sh
              if Map.notMember newcred initialRewards && Set.notMember newcred avoidCredentials
                then do
                  case tag of
                    Rewarding -> modifyGenStateInitialRewards (Map.insert newcred (Coin 0))
                    _ -> pure ()
                  return sh
                else f $ n - 1
    pickExistingKeyHash =
      KeyHashObj <$> do
        keysMap <- gsKeys <$> get
        lift (genMapElem keysMap) >>= \case
          Just (k, _) -> pure $ coerceKeyRole k
          Nothing -> genKeyHash'
    pickExistingScript =
      ScriptHashObj
        <$> elementsT [pickExistingPlutusScript, pickExistingTimelockScript]
    pickExistingPlutusScript = do
      plutusScriptsMap <-
        Map.filterWithKey (\(_, t) _ -> t == tag) . gsPlutusScripts <$> get
      lift (genMapElem plutusScriptsMap) >>= \case
        Just ((h, _), _) -> pure h
        Nothing -> genScript reify tag
    pickExistingTimelockScript = do
      -- Only pick one if it matches the
      vi <- gets gsValidityInterval -- current ValidityInterval
      vimap <- gets gsVI
      case Map.lookup vi vimap of
        Nothing -> genScript @era reify tag
        Just s ->
          lift (genSetElem s) >>= \case
            Nothing -> genScript reify tag
            Just hash -> pure hash

-- Return a fresh credential, one that is not a member of the set 'old'.
-- One gets 'tries' chances to generate a fresh one, before an error is raised.
-- This avoids silent infinite loops.
genFreshCredential ::
  forall era kr.
  Reflect era =>
  Int ->
  PlutusPurposeTag ->
  Set (Credential kr (EraCrypto era)) ->
  GenRS era (Credential kr (EraCrypto era))
genFreshCredential 0 _tag _old = error "Ran out of tries in genFreshCredential."
genFreshCredential tries0 tag old = go tries0
  where
    go tries = do
      c <- genCredential tag
      if Set.member c old
        then go (tries - 1)
        else pure c

genFreshRegCred ::
  Reflect era => PlutusPurposeTag -> GenRS era (Credential 'Staking (EraCrypto era))
genFreshRegCred tag = do
  old <- gets (Map.keysSet . gsInitialRewards)
  avoid <- gets gsAvoidCred
  rewards <- gets $ Map.keysSet . mRewards . gsModel
  cred <- genFreshCredential 100 tag $ old <> avoid <> rewards
  modifyGenStateAvoidCred (Set.insert cred)
  pure cred

genPoolParams ::
  Reflect era =>
  KeyHash 'StakePool (EraCrypto era) ->
  GenRS era (PoolParams (EraCrypto era))
genPoolParams ppId = do
  ppVrf <- lift arbitrary
  ppPledge <- lift genPositiveVal
  ppCost <- lift genPositiveVal
  ppMargin <- lift arbitrary
  ppRewardAccount <- RewardAccount Testnet <$> genFreshRegCred Rewarding
  let ppOwners = mempty
  let ppRelays = mempty
  let ppMetadata = SNothing
  pure PoolParams {..}

-- | Generate a 'n' fresh credentials (ones not in the set 'old'). We get 'tries' chances,
--   if it doesn't work in 'tries' attempts then quit with an error. Better to raise an error
--   than go into an infinite loop.
genFreshCredentials ::
  forall era kr.
  Reflect era =>
  Int ->
  Int ->
  PlutusPurposeTag ->
  Set (Credential kr (EraCrypto era)) ->
  [Credential kr (EraCrypto era)] ->
  GenRS era [Credential kr (EraCrypto era)]
genFreshCredentials _n 0 _tag _old _ans = error "Ran out of tries in genFreshCredentials."
genFreshCredentials n0 tries tag old0 ans0 = go n0 old0 ans0
  where
    go 0 _ ans = pure ans
    go n old ans = do
      c <- genFreshCredential tries tag old
      go (n - 1) (Set.insert c old) (c : ans)

-- | Use this function to get a new pool that should not be used in the future transactions
genNewPool ::
  forall era.
  Reflect era =>
  GenRS
    era
    ( KeyHash 'StakePool (EraCrypto era)
    , PoolParams (EraCrypto era)
    , IndividualPoolStake (EraCrypto era)
    )
genNewPool = do
  poolId <- genFreshKeyHash
  poolParam <- genPoolParams poolId
  percent <- lift $ choose (0, 1 :: Float)
  let stake = IndividualPoolStake @(EraCrypto era) (toRational percent) mempty (ppVrf poolParam)
  modifyGenStateAvoidKey (Set.insert (coerceKeyRole poolId))
  pure (poolId, poolParam, stake)

-- | Initialize (or overwrite if they are not empty) the Stable fields. It is
--   intended that this be called just once at the beginning of a trace generation.
initStableFields :: forall era. Reflect era => GenRS era ()
initStableFields = do
  GenEnv {geSize} <- ask
  hashes <- replicateM (maxStablePools geSize) $ do
    pp <- asks gePParams
    (kh, poolParams, ips) <- genNewPool
    modifyGenStateStablePools (Set.insert kh)
    modifyGenStateInitialPoolParams (Map.insert kh poolParams)
    modifyGenStateInitialPoolDistr (Map.insert kh ips)
    modifyModelPoolParams (Map.insert kh poolParams)
    modifyModelKeyDeposits kh (pp ^. ppPoolDepositL)
    return kh

  -- This incantation gets a list of fresh (not previously generated) Credential
  credentials <- replicateM (maxStablePools geSize) $ do
    old' <- gets (Map.keysSet . gsInitialRewards)
    prev <- gets gsAvoidCred
    cred <- genFreshCredential 100 Rewarding (Set.union old' prev)
    modifyGenStateStableDelegators (Set.insert cred)
    modifyGenStateInitialRewards (Map.insert cred (Coin 0))
    return cred
  let f :: Credential 'Staking (EraCrypto era) -> KeyHash 'StakePool (EraCrypto era) -> GenRS era ()
      f cred kh = do
        pp <- asks gePParams
        let keyDeposit = pp ^. ppKeyDepositL
        modifyModelDelegations (Map.insert cred kh)
        modifyModelRewards (Map.insert cred (Coin 0))
        modifyModelDeposited (<+> keyDeposit)
        modifyKeyDeposits cred keyDeposit
        modifyGenStateInitialDelegations (Map.insert cred kh)
  zipWithM_ f credentials hashes

-- =============================================
-- Generators of inter-related items

-- Adds to the rewards of the ModelNewEpochState. This used exclusively to generate Withdrawals, so
-- we mark these as ones to avoid in the future. Especialy when generating DeRegKey.
genRewards :: Reflect era => GenRS era (RewardAccounts (EraCrypto era))
genRewards = do
  wmax <- gets (withdrawalMax . geSize . gsGenEnv)
  n <- lift $ choose (1, wmax)
  -- we need a fresh credential, one that was not previously
  -- generated here, or one that arose from gsAvoidCred (i.e. prev)
  old <- gets (Map.keysSet . gsInitialRewards)
  prev <- gets gsAvoidCred
  credentials <- genFreshCredentials n 100 Rewarding (Set.union old prev) []
  newRewards <- Map.fromList <$> mapM (\x -> (,) x <$> lift genRewardVal) credentials
  modifyModelRewards (\rewards -> eval (rewards ⨃ newRewards)) -- Prefers coins in newrewards
  pp <- asks gePParams
  sequence_ (map (\cred -> modifyKeyDeposits cred (pp ^. ppKeyDepositL)) credentials)
  modifyGenStateInitialRewards (\rewards -> eval (rewards ⨃ newRewards))
  modifyGenStateAvoidCred (Set.union (Set.fromList credentials))
  pure newRewards

genRetirementHash :: forall era. Reflect era => GenRS era (KeyHash 'StakePool (EraCrypto era))
genRetirementHash = do
  m <- gets (mPoolParams . gsModel)
  honestKhs <- gets gsStablePools
  avoidKey <- gets gsAvoidKey
  res <- lift . genMapElemWhere m 10 $ \kh _ ->
    kh `Set.notMember` honestKhs && kh `Set.notMember` avoidKey
  case res of
    Just x -> do
      modifyGenStateAvoidKey (Set.insert (fst x))
      -- if it is retiring, we should probably avoid it in the future
      pure $ fst x
    Nothing -> do
      (poolid, poolparams, stake) <- genNewPool

      -- add the Pool to the initial state
      modifyGenStateInitialPoolParams (Map.insert poolid poolparams)
      modifyGenStateInitialPoolDistr (Map.insert poolid stake)

      -- add the Pool to the Model
      modifyModelPoolParams (Map.insert poolid poolparams)
      modifyModelPoolDistr (Map.insert poolid stake)
      pure poolid

-- Adds to the mPoolParams and the  mPoolDistr of the Model, and the initial set of objects for Traces
genPool ::
  forall era.
  Reflect era =>
  GenRS era (KeyHash 'StakePool (EraCrypto era), PoolParams (EraCrypto era))
genPool = frequencyT [(10, genNew), (90, pickExisting)]
  where
    genNew = do
      (kh, pp, ips) <- genNewPool
      -- add pool to initial state
      modifyGenStateInitialPoolParams (Map.insert kh pp)
      modifyGenStateInitialPoolDistr (Map.insert kh ips)
      -- update the model
      modifyModelPoolParams (Map.insert kh pp)
      return (kh, pp)
    pickExisting = do
      psStakePoolParams <- gets (mPoolParams . gsModel)
      avoidKey <- gets gsAvoidKey
      lift (genMapElemWhere psStakePoolParams 10 (\kh _ -> kh `Set.notMember` avoidKey)) >>= \case
        Nothing -> genNew
        Just (kh, pp) -> pure (kh, pp)
