{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Translation (
  Tx (..),
  addrPtrNormalize,
  translateDatum,
) where

import Cardano.Ledger.Address (addrPtrNormalize)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.CertState (EraCertState (..))
import Cardano.Ledger.Conway.CertState ()
import Cardano.Ledger.Conway.Core hiding (Tx)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (
  cgsCommitteeL,
  cgsConstitutionL,
  cgsCurPParamsL,
  cgsFuturePParamsL,
  cgsPrevPParamsL,
  mkEnactState,
  rsEnactStateL,
  setCompleteDRepPulsingState,
 )
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.Tx ()
import qualified Cardano.Ledger.Core as Core (Tx)
import Cardano.Ledger.Plutus.Data (translateDatum)
import Cardano.Ledger.Shelley.API (
  DState (..),
  EpochState (..),
  NewEpochState (..),
  PState (..),
  StrictMaybe (..),
 )
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.CertState (ShelleyCertState (..))
import Cardano.Ledger.Shelley.LedgerState (
  epochStateGovStateL,
  lsCertStateL,
 )
import qualified Cardano.Ledger.UMap as UM
import Data.Default (Default (def))
import qualified Data.Map.Strict as Map
import Lens.Micro

--------------------------------------------------------------------------------
-- Translation from Babbage to Conway
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

type instance TranslationContext ConwayEra = ConwayGenesis

instance TranslateEra ConwayEra NewEpochState where
  translateEra ctxt nes = do
    let es = translateEra' ctxt $ nesEs nes
        -- We need to ensure that we have the same initial EnactState in the pulser as
        -- well as in the current EnactState, otherwise in the very first EPOCH rule call
        -- the pulser will reset it.
        ratifyState =
          def
            & rsEnactStateL .~ mkEnactState (es ^. epochStateGovStateL)
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

instance TranslateEra ConwayEra Tx where
  type TranslationError ConwayEra Tx = DecoderError
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
            & witsTxL .~ txWits
            & isValidTxL .~ isValidTx
            & auxDataTxL .~ auxData
    pure $ Tx newTx

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance TranslateEra ConwayEra PParams where
  translateEra ConwayGenesis {cgUpgradePParams} = pure . upgradePParams cgUpgradePParams

instance TranslateEra ConwayEra FuturePParams where
  translateEra ctxt = \case
    NoPParamsUpdate -> pure NoPParamsUpdate
    DefinitePParamsUpdate pp -> DefinitePParamsUpdate <$> translateEra ctxt pp
    PotentialPParamsUpdate mpp -> PotentialPParamsUpdate <$> mapM (translateEra ctxt) mpp

instance TranslateEra ConwayEra EpochState where
  translateEra ctxt es =
    pure $
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance TranslateEra ConwayEra DState where
  translateEra _ DState {dsUnified = umap, ..} = pure DState {dsUnified = umap', ..}
    where
      umap' =
        umap
          { UM.umElems =
              Map.map (\(UM.UMElem rd _ poolId drep) -> UM.UMElem rd mempty poolId drep) (UM.umElems umap)
          , UM.umPtrs = mempty
          }

instance TranslateEra ConwayEra PState where
  translateEra _ PState {..} = pure PState {..}

instance TranslateEra ConwayEra API.LedgerState where
  translateEra conwayGenesis ls =
    pure
      API.LedgerState
        { API.lsUTxOState = translateEra' conwayGenesis $ API.lsUTxOState ls
        , API.lsCertState =
            ShelleyCertState
              { shelleyCertDState = translateEra' conwayGenesis (ls ^. lsCertStateL . certDStateL)
              , shelleyCertPState = translateEra' conwayGenesis (ls ^. lsCertStateL . certPStateL)
              , shelleyCertVState = def
              }
        }

translateGovState ::
  TranslationContext ConwayEra ->
  GovState BabbageEra ->
  GovState ConwayEra
translateGovState ctxt@ConwayGenesis {..} sgov =
  let curPParams = translateEra' ctxt (sgov ^. curPParamsGovStateL)
      prevPParams = translateEra' ctxt (sgov ^. prevPParamsGovStateL)
      futurePParams = translateEra' ctxt (sgov ^. futurePParamsGovStateL)
   in emptyGovState
        & cgsCurPParamsL .~ curPParams
        & cgsPrevPParamsL .~ prevPParams
        & cgsFuturePParamsL .~ futurePParams
        & cgsCommitteeL .~ SJust cgCommittee
        & cgsConstitutionL .~ cgConstitution

instance TranslateEra ConwayEra UtxoState where
  translateEra ctxt us =
    pure
      UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us
        , API.utxosDeposited = API.utxosDeposited us
        , API.utxosFees = API.utxosFees us
        , API.utxosGovState = translateGovState ctxt $ API.utxosGovState us
        , API.utxosInstantStake = ConwayInstantStake . sisCredentialStake $ API.utxosInstantStake us
        , API.utxosDonation = API.utxosDonation us
        }

instance TranslateEra ConwayEra API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ upgradeTxOut `Map.map` API.unUTxO utxo
