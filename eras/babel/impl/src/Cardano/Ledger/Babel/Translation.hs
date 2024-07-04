{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Translation (
  Tx (..),
  addrPtrNormalize,
  translateDatum,
  translateTxOut,
) where

import Cardano.Ledger.Address (addrPtrNormalize)
import Cardano.Ledger.Babel.Core hiding (Tx)
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Genesis (BabelGenesis (..))
import Cardano.Ledger.Babel.Scripts ()
import Cardano.Ledger.Babel.Tx ()
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.CertState (CommitteeState (..))
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  cgsCommitteeL,
  cgsConstitutionL,
  cgsCurPParamsL,
  cgsPrevPParamsL,
  mkEnactState,
  rsEnactStateL,
  setCompleteDRepPulsingState,
 )
import qualified Cardano.Ledger.Core as Core (Tx)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Plutus.Data (translateDatum)
import Cardano.Ledger.Shelley.API (
  CertState (..),
  DState (..),
  EpochState (..),
  NewEpochState (..),
  PState (..),
  StrictMaybe (..),
  UTxOState (..),
  VState (..),
 )
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.LedgerState (
  epochStateGovStateL,
 )
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Lens.Micro

--------------------------------------------------------------------------------
-- Translation from Babbage to Babel
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

type instance TranslationContext (BabelEra c) = BabelGenesis c

instance (Crypto c, ConwayEraGov (BabelEra c)) => TranslateEra (BabelEra c) NewEpochState where
  translateEra ctxt nes = do
    let es = translateEra' ctxt $ nesEs nes
        -- We need to ensure that we have the same initial EnactState in the pulser as
        -- well as in the current EnactState, otherwise in the very first EPOCH rule call
        -- the pulser will reset it.
        ratifyState =
          def
            & rsEnactStateL
            .~ mkEnactState (es ^. epochStateGovStateL)
    pure $
      NewEpochState
        { nesEL = nesEL nes
        , nesBprev = nesBprev nes
        , nesBcur = nesBcur nes
        , nesEs = setCompleteDRepPulsingState def ratifyState es
        , nesRu = nesRu nes
        , nesPd = nesPd nes
        , stashedAVVMAddresses = ()
        }

newtype Tx era = Tx {unTx :: Core.Tx era}

instance Crypto c => TranslateEra (BabelEra c) Tx where
  type TranslationError (BabelEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- translateEraThroughCBOR "TxBody" $ tx ^. bodyTxL
    txWits <- translateEraThroughCBOR "TxWitness" $ tx ^. witsTxL
    auxData <- mapM (translateEraThroughCBOR "AuxData") (tx ^. auxDataTxL)
    let isValidTx = tx ^. isValidTxL
        newTx =
          mkBasicTx txBody
            & witsTxL
            .~ txWits
            & isValidTxL
            .~ isValidTx
            & auxDataTxL
            .~ auxData
    pure $ Tx newTx

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance Crypto c => TranslateEra (BabelEra c) PParams where
  translateEra BabelGenesis {cgUpgradePParams} = pure . upgradePParams cgUpgradePParams

instance Crypto c => TranslateEra (BabelEra c) EpochState where
  translateEra ctxt es =
    pure $
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (BabelEra c) DState where
  translateEra _ DState {..} = pure DState {..}

instance Crypto c => TranslateEra (BabelEra c) CommitteeState where
  translateEra _ CommitteeState {..} = pure CommitteeState {..}

instance Crypto c => TranslateEra (BabelEra c) VState where
  translateEra ctx VState {..} = do
    committeeState <- translateEra ctx vsCommitteeState
    pure VState {vsCommitteeState = committeeState, ..}

instance Crypto c => TranslateEra (BabelEra c) PState where
  translateEra _ PState {..} = pure PState {..}

instance Crypto c => TranslateEra (BabelEra c) CertState where
  translateEra ctxt ls =
    pure
      CertState
        { certDState = translateEra' ctxt $ certDState ls
        , certPState = translateEra' ctxt $ certPState ls
        , certVState = translateEra' ctxt $ certVState ls
        }

instance Crypto c => TranslateEra (BabelEra c) API.LedgerState where
  translateEra babelGenesis ls =
    pure
      API.LedgerState
        { API.lsUTxOState = translateEra' babelGenesis $ API.lsUTxOState ls
        , API.lsCertState = translateEra' babelGenesis $ API.lsCertState ls
        }

translateGovState ::
  Crypto c =>
  TranslationContext (BabelEra c) ->
  GovState (PreviousEra (BabelEra c)) ->
  GovState (BabelEra c)
translateGovState ctxt@BabelGenesis {..} sgov =
  let curPParams = translateEra' ctxt (sgov ^. curPParamsGovStateL)
      prevPParams = translateEra' ctxt (sgov ^. prevPParamsGovStateL)
   in emptyGovState
        & cgsCurPParamsL
        .~ curPParams
        & cgsPrevPParamsL
        .~ prevPParams
        & cgsCommitteeL
        .~ SJust cgCommittee
        & cgsConstitutionL
        .~ cgConstitution

instance Crypto c => TranslateEra (BabelEra c) UTxOState where
  translateEra ctxt us =
    pure
      UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us
        , API.utxosDeposited = API.utxosDeposited us
        , API.utxosFees = API.utxosFees us
        , API.utxosGovState =
            translateGovState ctxt $
              API.utxosGovState us
        , API.utxosStakeDistr = API.utxosStakeDistr us
        , API.utxosDonation = API.utxosDonation us
        }

instance Crypto c => TranslateEra (BabelEra c) API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ upgradeTxOut `Map.map` API.unUTxO utxo

-- | Filter out `TxOut`s with zero Coins and normalize Pointers,
-- while converting `TxOut`s to Babel era.
translateTxOut ::
  Crypto c =>
  TxOut (PreviousEra (BabelEra c)) ->
  TxOut (BabelEra c)
translateTxOut = upgradeTxOut
{-# DEPRECATED translateTxOut "In favor of `upgradeTxOut`" #-}
