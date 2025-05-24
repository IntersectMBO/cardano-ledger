{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Translation where

import Cardano.Ledger.Alonzo.Core hiding (Tx)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.State
import Cardano.Ledger.Alonzo.Tx (IsValid (..), Tx (..))
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Data.Coerce (coerce)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~), (^.))

--------------------------------------------------------------------------------
-- Translation from Mary to Alonzo
--
-- The instances below are needed by the consensus layer. Do not remove any of
-- them without coordinating with consensus.
--
-- Please add auxiliary instances and other declarations at the bottom of this
-- module, not in the list below so that it remains clear which instances the
-- consensus layer needs.
--
-- WARNING: when a translation instance currently uses the default
-- 'TranslationError', i.e., 'Void', it means the consensus layer relies on it
-- being total. Do not change it!
--------------------------------------------------------------------------------

type instance TranslationContext AlonzoEra = AlonzoGenesis

instance TranslateEra AlonzoEra NewEpochState where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes
        , nesBprev = nesBprev nes
        , nesBcur = nesBcur nes
        , nesEs = translateEra' ctxt $ nesEs nes
        , nesRu = nesRu nes
        , nesPd = nesPd nes
        , stashedAVVMAddresses = ()
        }

instance TranslateEra AlonzoEra PParams where
  translateEra (AlonzoGenesisWrapper upgradeArgs) = pure . upgradePParams upgradeArgs

instance TranslateEra AlonzoEra FuturePParams where
  translateEra ctxt = \case
    NoPParamsUpdate -> pure NoPParamsUpdate
    DefinitePParamsUpdate pp -> DefinitePParamsUpdate <$> translateEra ctxt pp
    PotentialPParamsUpdate mpp -> PotentialPParamsUpdate <$> mapM (translateEra ctxt) mpp

instance TranslateEra AlonzoEra Tx where
  type TranslationError AlonzoEra Tx = DecoderError
  translateEra _ctxt tx = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- translateEraThroughCBOR "TxBody" $ tx ^. bodyTxL
    txWits <- translateEraThroughCBOR "TxWits" $ tx ^. witsTxL
    txAuxData <- mapM (translateEraThroughCBOR "TxAuxData") (tx ^. auxDataTxL)
    -- transactions from Mary era always pass script ("phase 2") validation
    let validating = IsValid True
    pure $
      mkBasicTx txBody
        & witsTxL .~ txWits
        & auxDataTxL .~ txAuxData
        & isValidTxL .~ validating

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance TranslateEra AlonzoEra EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esChainAccountState = esChainAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance TranslateEra AlonzoEra ShelleyAccounts where
  translateEra _ = pure . coerce

instance TranslateEra AlonzoEra DState where
  translateEra ctx DState {dsAccounts = accountsShelley, ..} = do
    dsAccounts <- translateEra ctx accountsShelley
    pure DState {..}

instance TranslateEra AlonzoEra CommitteeState where
  translateEra _ CommitteeState {..} = pure CommitteeState {..}

instance TranslateEra AlonzoEra PState where
  translateEra _ PState {..} = pure PState {..}

instance TranslateEra AlonzoEra ShelleyCertState where
  translateEra ctxt ls =
    pure
      ShelleyCertState
        { shelleyCertDState = translateEra' ctxt $ shelleyCertDState ls
        , shelleyCertPState = translateEra' ctxt $ shelleyCertPState ls
        }

instance TranslateEra AlonzoEra LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls
        , lsCertState = translateEra' ctxt $ lsCertState ls
        }

instance TranslateEra AlonzoEra UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { utxosUtxo = translateEra' ctxt $ utxosUtxo us
        , utxosDeposited = utxosDeposited us
        , utxosFees = utxosFees us
        , utxosGovState = translateEra' ctxt $ utxosGovState us
        , utxosInstantStake = translateEra' ctxt $ utxosInstantStake us
        , utxosDonation = utxosDonation us
        }

instance TranslateEra AlonzoEra ShelleyInstantStake where
  translateEra _ = pure . coerce

instance TranslateEra AlonzoEra UTxO where
  translateEra _ctxt utxo =
    return $ UTxO $ upgradeTxOut `Map.map` unUTxO utxo

instance TranslateEra AlonzoEra ShelleyGovState where
  translateEra ctxt ps =
    return
      ShelleyGovState
        { sgsCurProposals = translateEra' ctxt $ sgsCurProposals ps
        , sgsFutureProposals = translateEra' ctxt $ sgsFutureProposals ps
        , sgsCurPParams = translateEra' ctxt $ sgsCurPParams ps
        , sgsPrevPParams = translateEra' ctxt $ sgsPrevPParams ps
        , sgsFuturePParams = translateEra' ctxt $ sgsFuturePParams ps
        }

instance TranslateEra AlonzoEra ProposedPPUpdates where
  translateEra _ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ fmap (upgradePParamsUpdate def) ppup
