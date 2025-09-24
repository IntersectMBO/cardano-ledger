{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Api.Tx.Body (
  -- | Building and inspecting transaction outputs
  module Cardano.Ledger.Api.Tx.Out,
  -- | Working with Timelock scripts and Plutus scripts
  module Cardano.Ledger.Api.Scripts,
  EraTxBody (TxBody),
  mkBasicTxBody,
  inputsTxBodyL,
  outputsTxBodyL,
  feeTxBodyL,
  withdrawalsTxBodyL,
  Withdrawals (..),
  auxDataHashTxBodyL,
  TxAuxDataHash (..),
  AuxiliaryDataHash,
  spendableInputsTxBodyF,
  allInputsTxBodyF,
  evalBalanceTxBody,
  txIdTxBody,

  -- * Any Era
  AnyEraTxBody (..),

  -- * Shelley Era
  ShelleyEraTxBody,
  ttlTxBodyL,
  updateTxBodyL,
  certsTxBodyL,

  -- * Allegra Era
  AllegraEraTxBody,
  vldtTxBodyL,
  ValidityInterval (..),
  invalidBeforeL,
  invalidHereAfterL,

  -- * Mary Era
  MaryEraTxBody,
  mintTxBodyL,
  mintValueTxBodyF,
  mintedTxBodyF,

  -- * Alonzo Era
  AlonzoEraTxBody,
  collateralInputsTxBodyL,
  reqSignerHashesTxBodyL,
  scriptIntegrityHashTxBodyL,
  ScriptIntegrityHash,
  networkIdTxBodyL,
  redeemerPointer,
  redeemerPointerInverse,

  -- * Babbage Era
  BabbageEraTxBody,
  sizedOutputsTxBodyL,
  referenceInputsTxBodyL,
  totalCollateralTxBodyL,
  collateralReturnTxBodyL,
  sizedCollateralReturnTxBodyL,
  allSizedOutputsTxBodyF,

  -- * Conway Era
  ConwayEraTxBody,
  votingProceduresTxBodyL,
  VotingProcedure (..),
  VotingProcedures (..),
  proposalProceduresTxBodyL,
  ProposalProcedure (..),
  currentTreasuryValueTxBodyL,
  treasuryDonationTxBodyL,

  -- * Dijstra Era
  DijkstraEraTxBody,
  guardsTxBodyL,

  -- * Upgrade
  binaryUpgradeTxBody,
  upgradeTxBody,
) where

import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.Allegra.Core (AllegraEraTxBody (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), ScriptIntegrityHash)
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Scripts
import Cardano.Ledger.Api.Tx.Cert
import Cardano.Ledger.Api.Tx.Out
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody (..))
import Cardano.Ledger.BaseTypes (Network, SlotNo, StrictMaybe (..), strictMaybeToMaybe)
import Cardano.Ledger.Binary.Decoding (Sized)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Governance (
  ProposalProcedure (..),
  VotingProcedure (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..))
import Cardano.Ledger.Core (
  EraTxBody (..),
  PParams,
  TxAuxDataHash (..),
  Value,
  binaryUpgradeTxBody,
  txIdTxBody,
 )
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), coerceKeyRole)
import Cardano.Ledger.Mary.Core (MaryEraTxBody (..))
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody (..))
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.UTxO (getProducedValue)
import Cardano.Ledger.State (EraUTxO (getConsumedValue), UTxO)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val ((<->))
import Data.OSet.Strict (OSet)
import qualified Data.OSet.Strict as OSet (fromSet)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set (map)
import Lens.Micro (Lens', SimpleGetter, lens, to)

class (EraTxBody era, AnyEraTxOut era, AnyEraTxCert era) => AnyEraTxBody era where
  updateTxBodyG :: SimpleGetter (TxBody era) (Maybe (Maybe (Update era)))
  updateTxBodyG = to (const Nothing)

  vldtTxBodyG :: SimpleGetter (TxBody era) ValidityInterval
  default vldtTxBodyG ::
    AllegraEraTxBody era => SimpleGetter (TxBody era) ValidityInterval
  vldtTxBodyG = vldtTxBodyL

  mintTxBodyG :: SimpleGetter (TxBody era) (Maybe MultiAsset)
  default mintTxBodyG ::
    MaryEraTxBody era => SimpleGetter (TxBody era) (Maybe MultiAsset)
  mintTxBodyG = mintTxBodyL . to Just

  collateralInputsTxBodyG :: SimpleGetter (TxBody era) (Maybe (Set TxIn))
  default collateralInputsTxBodyG ::
    AlonzoEraTxBody era => SimpleGetter (TxBody era) (Maybe (Set TxIn))
  collateralInputsTxBodyG = collateralInputsTxBodyL . to Just

  scriptIntegrityHashTxBodyG ::
    SimpleGetter (TxBody era) (Maybe (Maybe ScriptIntegrityHash))
  default scriptIntegrityHashTxBodyG ::
    AlonzoEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (Maybe ScriptIntegrityHash))
  scriptIntegrityHashTxBodyG = scriptIntegrityHashTxBodyL . to (Just . strictMaybeToMaybe)

  networkIdTxBodyG :: SimpleGetter (TxBody era) (Maybe (Maybe Network))
  default networkIdTxBodyG ::
    AlonzoEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (Maybe Network))
  networkIdTxBodyG = networkIdTxBodyL . to (Just . strictMaybeToMaybe)

  sizedOutputsTxBodyG :: SimpleGetter (TxBody era) (Maybe (StrictSeq (Sized (TxOut era))))
  default sizedOutputsTxBodyG ::
    BabbageEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (StrictSeq (Sized (TxOut era))))
  sizedOutputsTxBodyG = sizedOutputsTxBodyL . to Just

  referenceInputsTxBodyG :: SimpleGetter (TxBody era) (Maybe (Set TxIn))
  default referenceInputsTxBodyG ::
    BabbageEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (Set TxIn))
  referenceInputsTxBodyG = referenceInputsTxBodyL . to Just

  totalCollateralTxBodyG :: SimpleGetter (TxBody era) (Maybe (Maybe Coin))
  default totalCollateralTxBodyG ::
    BabbageEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (Maybe Coin))
  totalCollateralTxBodyG = totalCollateralTxBodyL . to (Just . strictMaybeToMaybe)

  collateralReturnTxBodyG :: SimpleGetter (TxBody era) (Maybe (Maybe (TxOut era)))
  default collateralReturnTxBodyG ::
    BabbageEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (Maybe (TxOut era)))
  collateralReturnTxBodyG = collateralReturnTxBodyL . to (Just . strictMaybeToMaybe)

  sizedCollateralReturnTxBodyG :: SimpleGetter (TxBody era) (Maybe (Maybe (Sized (TxOut era))))
  default sizedCollateralReturnTxBodyG ::
    BabbageEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (Maybe (Sized (TxOut era))))
  sizedCollateralReturnTxBodyG = sizedCollateralReturnTxBodyL . to (Just . strictMaybeToMaybe)

  currentTreasuryValueTxBodyG :: SimpleGetter (TxBody era) (Maybe (Maybe Coin))
  default currentTreasuryValueTxBodyG ::
    ConwayEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (Maybe Coin))
  currentTreasuryValueTxBodyG = currentTreasuryValueTxBodyL . to (Just . strictMaybeToMaybe)

  votingProceduresTxBodyG :: SimpleGetter (TxBody era) (Maybe (VotingProcedures era))
  default votingProceduresTxBodyG ::
    ConwayEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (VotingProcedures era))
  votingProceduresTxBodyG = votingProceduresTxBodyL . to Just

  proposalProceduresTxBodyG :: SimpleGetter (TxBody era) (Maybe (OSet (ProposalProcedure era)))
  default proposalProceduresTxBodyG ::
    ConwayEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (OSet (ProposalProcedure era)))
  proposalProceduresTxBodyG = proposalProceduresTxBodyL . to Just

  treasuryDonationTxBodyG :: SimpleGetter (TxBody era) (Maybe Coin)
  default treasuryDonationTxBodyG :: ConwayEraTxBody era => SimpleGetter (TxBody era) (Maybe Coin)
  treasuryDonationTxBodyG = treasuryDonationTxBodyL . to Just

  guardsTxBodyG :: SimpleGetter (TxBody era) (Maybe (OSet (Credential Guard)))
  default guardsTxBodyG ::
    DijkstraEraTxBody era =>
    SimpleGetter (TxBody era) (Maybe (OSet (Credential Guard)))
  guardsTxBodyG = guardsTxBodyL . to Just

instance AnyEraTxBody ShelleyEra where
  updateTxBodyG = updateTxBodyL . to (Just . strictMaybeToMaybe)
  vldtTxBodyG = ttlTxBodyL . to ttlToValidityInterval
  mintTxBodyG = to (const Nothing)
  collateralInputsTxBodyG = to (const Nothing)
  scriptIntegrityHashTxBodyG = to (const Nothing)
  networkIdTxBodyG = to (const Nothing)
  sizedOutputsTxBodyG = to (const Nothing)
  referenceInputsTxBodyG = to (const Nothing)
  totalCollateralTxBodyG = to (const Nothing)
  collateralReturnTxBodyG = to (const Nothing)
  sizedCollateralReturnTxBodyG = to (const Nothing)
  currentTreasuryValueTxBodyG = to (const Nothing)
  votingProceduresTxBodyG = to (const Nothing)
  proposalProceduresTxBodyG = to (const Nothing)
  treasuryDonationTxBodyG = to (const Nothing)
  guardsTxBodyG = to (const Nothing)

instance AnyEraTxBody AllegraEra where
  updateTxBodyG = updateTxBodyL . to (Just . strictMaybeToMaybe)
  mintTxBodyG = to (const Nothing)
  collateralInputsTxBodyG = to (const Nothing)
  scriptIntegrityHashTxBodyG = to (const Nothing)
  networkIdTxBodyG = to (const Nothing)
  sizedOutputsTxBodyG = to (const Nothing)
  referenceInputsTxBodyG = to (const Nothing)
  totalCollateralTxBodyG = to (const Nothing)
  collateralReturnTxBodyG = to (const Nothing)
  sizedCollateralReturnTxBodyG = to (const Nothing)
  currentTreasuryValueTxBodyG = to (const Nothing)
  votingProceduresTxBodyG = to (const Nothing)
  proposalProceduresTxBodyG = to (const Nothing)
  treasuryDonationTxBodyG = to (const Nothing)
  guardsTxBodyG = to (const Nothing)

instance AnyEraTxBody MaryEra where
  updateTxBodyG = updateTxBodyL . to (Just . strictMaybeToMaybe)
  collateralInputsTxBodyG = to (const Nothing)
  scriptIntegrityHashTxBodyG = to (const Nothing)
  networkIdTxBodyG = to (const Nothing)
  sizedOutputsTxBodyG = to (const Nothing)
  referenceInputsTxBodyG = to (const Nothing)
  totalCollateralTxBodyG = to (const Nothing)
  collateralReturnTxBodyG = to (const Nothing)
  sizedCollateralReturnTxBodyG = to (const Nothing)
  currentTreasuryValueTxBodyG = to (const Nothing)
  votingProceduresTxBodyG = to (const Nothing)
  proposalProceduresTxBodyG = to (const Nothing)
  treasuryDonationTxBodyG = to (const Nothing)
  guardsTxBodyG = to (const Nothing)

instance AnyEraTxBody AlonzoEra where
  updateTxBodyG = updateTxBodyL . to (Just . strictMaybeToMaybe)
  sizedOutputsTxBodyG = to (const Nothing)
  referenceInputsTxBodyG = to (const Nothing)
  totalCollateralTxBodyG = to (const Nothing)
  collateralReturnTxBodyG = to (const Nothing)
  sizedCollateralReturnTxBodyG = to (const Nothing)
  currentTreasuryValueTxBodyG = to (const Nothing)
  votingProceduresTxBodyG = to (const Nothing)
  proposalProceduresTxBodyG = to (const Nothing)
  treasuryDonationTxBodyG = to (const Nothing)
  guardsTxBodyG =
    -- TODO: switch reqSignerHashesTxBodyL to `Guard` from `Witness`, thus getting rid of
    -- coerceKeyRole: https://github.com/IntersectMBO/cardano-ledger/issues/5315
    reqSignerHashesTxBodyG . to (Just . OSet.fromSet . Set.map (KeyHashObj . coerceKeyRole))

instance AnyEraTxBody BabbageEra where
  updateTxBodyG = updateTxBodyL . to (Just . strictMaybeToMaybe)
  currentTreasuryValueTxBodyG = to (const Nothing)
  votingProceduresTxBodyG = to (const Nothing)
  proposalProceduresTxBodyG = to (const Nothing)
  treasuryDonationTxBodyG = to (const Nothing)
  guardsTxBodyG =
    reqSignerHashesTxBodyG . to (Just . OSet.fromSet . Set.map (KeyHashObj . coerceKeyRole))

instance AnyEraTxBody ConwayEra where
  guardsTxBodyG =
    reqSignerHashesTxBodyG . to (Just . OSet.fromSet . Set.map (KeyHashObj . coerceKeyRole))

instance AnyEraTxBody DijkstraEra

-- | Evaluate the difference between the value currently being consumed by a transaction
-- and the total value being produced. This value will be zero for a valid transaction.
--
-- In case when full `Cardano.Ledger.CertState` is available then this can be simplified to:
--
-- > let lookupRefund = lookupDepositDState (certDState dpState)
-- > let isRegPoolId = (`Map.member` psStakePools (certPState dpState))
-- > evalBalanceTxBody pp lookupRefund isRegPoolId utxo txBody
evalBalanceTxBody ::
  EraUTxO era =>
  -- | Current protocol parameters
  PParams era ->
  -- | Lookup current deposit amount for a registered stake credential delegation. This
  -- function must produce valid answer for all of the stake credentials present in any of
  -- the `DeRegKey` delegation certificates in the supplied `TxBody`. In other words,
  -- there is no requirement to know about all of the delegation certificates in the
  -- ledger state, just the ones this transaction cares about.
  (Credential 'Staking -> Maybe Coin) ->
  -- | Lookup current deposit amount for a registered DRep credential. This
  -- function must produce valid answer for all of the DRep credentials present in any of
  -- the `UnRegDRep` certificates in the supplied `TxBody`. In other words,
  -- there is no requirement to know about all of the DRep registrations in the
  -- ledger state, just the ones this transaction cares about.
  (Credential 'DRepRole -> Maybe Coin) ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered. There is no
  -- requirement to answer this question for all stake pool credentials, just for the ones
  -- that have the registration certificates included in the supplied `TxBody`
  (KeyHash 'StakePool -> Bool) ->
  -- | The UTxO relevant to the transaction.
  UTxO era ->
  -- | The transaction being evaluated for balance.
  TxBody era ->
  -- | The difference between what the transaction consumes and what it produces.
  Value era
evalBalanceTxBody pp lookupKeyRefund lookupDRepRefund isRegPoolId utxo txBody =
  getConsumedValue pp lookupKeyRefund lookupDRepRefund utxo txBody
    <-> getProducedValue pp isRegPoolId txBody

-- | Lens to access the 'invalidBefore' field of a 'ValidityInterval' as a 'Maybe SlotNo'.
invalidBeforeL :: Lens' ValidityInterval (Maybe SlotNo)
invalidBeforeL = lens g s
  where
    g :: ValidityInterval -> Maybe SlotNo
    g (ValidityInterval ma _) =
      case ma of
        SNothing -> Nothing
        SJust a -> Just a

    s :: ValidityInterval -> Maybe SlotNo -> ValidityInterval
    s (ValidityInterval _ b) a = ValidityInterval (maybe SNothing SJust a) b

-- | Lens to access the 'invalidHereAfter' field of a 'ValidityInterval' as a 'Maybe SlotNo'.
invalidHereAfterL :: Lens' ValidityInterval (Maybe SlotNo)
invalidHereAfterL = lens g s
  where
    g :: ValidityInterval -> Maybe SlotNo
    g (ValidityInterval _ mb) =
      case mb of
        SNothing -> Nothing
        SJust b -> Just b

    s :: ValidityInterval -> Maybe SlotNo -> ValidityInterval
    s (ValidityInterval ma _) = ValidityInterval ma . maybe SNothing SJust
