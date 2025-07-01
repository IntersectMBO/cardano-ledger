{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Shelley (
  ShelleyResultExamples (..),
  ShelleyLedgerExamples (..),
  defaultShelleyLedgerExamples,
  exampleShelleyLedgerBlock,
  exampleHashHeader,
  exampleTx,
  exampleProposedPParamsUpdates,
  examplePoolDistr,
  exampleNonMyopicRewards,
  exampleNewEpochState,
  exampleLedgerChainDepState,
  exampleCoin,
  exampleTxBodyShelley,
  exampleAuxDataMap,
  exampleTxIns,
  exampleCerts,
  exampleWithdrawals,
  exampleProposedPPUpdates,
  examplePayKey,
  exampleStakeKey,
  exampleKeys,
  exampleAuxiliaryDataShelley,
  examplePoolParams,
  ledgerExamplesShelley,
  testShelleyGenesis,
  -- from Translation
  emptyFromByronTranslationContext,
) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Keys hiding (hashVerKeyVRF)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API hiding (hashVerKeyVRF)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Translation (emptyFromByronTranslationContext)
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader
import Data.Default
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Word (Word64)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Protocol.TPraos.Create (
  AllIssuerKeys (..),
 )

data ShelleyResultExamples era = ShelleyResultExamples
  { srePParams :: PParams era
  , sreProposedPPUpdates :: ProposedPPUpdates era
  , srePoolDistr :: PoolDistr
  , sreNonMyopicRewards ::
      Map
        (Either Coin (Credential 'Staking))
        (Map (KeyHash 'StakePool) Coin)
  , sreShelleyGenesis :: ShelleyGenesis
  }

deriving instance
  ( Eq (PParams era)
  , Eq (PParamsUpdate era)
  , Era era
  ) =>
  Eq (ShelleyResultExamples era)

data ShelleyLedgerExamples era = ShelleyLedgerExamples
  { sleBlock :: Block (BHeader StandardCrypto) era
  , sleHashHeader :: HashHeader
  , sleTx :: Tx era
  , sleApplyTxError :: ApplyTxError era
  , sleRewardsCredentials :: Set (Either Coin (Credential 'Staking))
  , sleResultExamples :: ShelleyResultExamples era
  , sleNewEpochState :: NewEpochState era
  , sleChainDepState :: ChainDepState
  , sleTranslationContext :: TranslationContext era
  }

deriving instance
  ( EraTx era
  , EraGov era
  , Eq (TxSeq era)
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (StashedAVVMAddresses era)
  , Eq (TranslationContext era)
  , Eq (CertState era)
  , Eq (InstantStake era)
  ) =>
  Eq (ShelleyLedgerExamples era)

defaultShelleyLedgerExamples ::
  forall era.
  ( EraSegWits era
  , EraGov era
  , EraStake era
  , EraCertState era
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  , PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era
  , Default (StashedAVVMAddresses era)
  , ProtVerAtMost era 4
  ) =>
  (TxBody era -> [KeyPair 'Witness] -> TxWits era) ->
  (ShelleyTx era -> Tx era) ->
  Value era ->
  TxBody era ->
  TxAuxData era ->
  TranslationContext era ->
  ShelleyLedgerExamples era
defaultShelleyLedgerExamples = undefined

exampleShelleyLedgerBlock ::
  forall era.
  EraSegWits era =>
  Tx era ->
  Block (BHeader StandardCrypto) era
exampleShelleyLedgerBlock = undefined

-- | ShelleyLedgerExamples for Shelley era
ledgerExamplesShelley :: ShelleyLedgerExamples ShelleyEra
ledgerExamplesShelley = undefined

testShelleyGenesis :: ShelleyGenesis
testShelleyGenesis = undefined

exampleHashHeader :: HashHeader
exampleHashHeader = undefined

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
  forall era.
  (TxBody era -> [KeyPair 'Witness] -> TxWits era) ->
  TxBody era ->
  TxAuxData era ->
  ShelleyTx era
exampleTx = undefined

exampleProposedPParamsUpdates ::
  EraPParams era =>
  ProposedPPUpdates era
exampleProposedPParamsUpdates = undefined

examplePoolDistr :: PoolDistr
examplePoolDistr = undefined

exampleNonMyopicRewards ::
  Map
    (Either Coin (Credential 'Staking))
    (Map (KeyHash 'StakePool) Coin)
exampleNonMyopicRewards = undefined

-- | This is probably not a valid ledger. We don't care, we are only
-- interested in serialisation, not validation.
exampleNewEpochState ::
  forall era.
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraCertState era
  , Default (StashedAVVMAddresses era)
  ) =>
  Value era ->
  PParams era ->
  PParams era ->
  NewEpochState era
exampleNewEpochState = undefined

exampleLedgerChainDepState :: Word64 -> ChainDepState
exampleLedgerChainDepState = undefined

exampleCoin :: Coin
exampleCoin = undefined

exampleTxBodyShelley :: TxBody ShelleyEra
exampleTxBodyShelley = undefined

exampleAuxDataMap :: Map Word64 Metadatum
exampleAuxDataMap = undefined

exampleTxIns :: Set TxIn
exampleTxIns = undefined

exampleCerts :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => StrictSeq (TxCert era)
exampleCerts = undefined

exampleWithdrawals :: Withdrawals
exampleWithdrawals = undefined

exampleProposedPPUpdates ::
  EraPParams era =>
  ProposedPPUpdates er
exampleProposedPPUpdates = undefined

examplePayKey :: KeyPair 'Payment
examplePayKey = undefined

exampleStakeKey :: KeyPair 'Staking
exampleStakeKey = undefined

exampleKeys :: AllIssuerKeys StandardCrypto r
exampleKeys = undefined

exampleAuxiliaryDataShelley :: TxAuxData ShelleyEra
exampleAuxiliaryDataShelley = undefined

examplePoolParams :: PoolParams
examplePoolParams = undefined
