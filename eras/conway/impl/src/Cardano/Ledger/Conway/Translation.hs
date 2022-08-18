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

module Cardano.Ledger.Conway.Translation where

import Cardano.Binary (DecoderError)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..), Datum (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.TxOut ()
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era
  ( PreviousEra,
    TranslateEra (..),
    TranslationContext,
    translateEra',
  )
import Cardano.Ledger.Serialization (translateViaCBORAnn)
import Cardano.Ledger.Shelley.API
  ( DPState (..),
    DState (..),
    EpochState (..),
    NewEpochState (..),
    ShelleyGenesis,
    StrictMaybe (..),
  )
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD)
import Cardano.Ledger.ShelleyMA.Timelocks (translateTimelock)
import Data.Coerce

--------------------------------------------------------------------------------
-- Translation from Alonzo to Babbage
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

type instance PreviousEra (ConwayEra c) = BabbageEra c

type instance TranslationContext (ConwayEra c) = ConwayGenesis c

instance Crypto c => TranslateEra (ConwayEra c) NewEpochState where
  translateEra ctxt nes =
    pure $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ nesEs nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes,
          stashedAVVMAddresses = ()
        }

instance Crypto c => TranslateEra (ConwayEra c) ShelleyGenesis where
  translateEra ctxt genesis =
    pure
      API.ShelleyGenesis
        { API.sgSystemStart = API.sgSystemStart genesis,
          API.sgNetworkMagic = API.sgNetworkMagic genesis,
          API.sgNetworkId = API.sgNetworkId genesis,
          API.sgActiveSlotsCoeff = API.sgActiveSlotsCoeff genesis,
          API.sgSecurityParam = API.sgSecurityParam genesis,
          API.sgEpochLength = API.sgEpochLength genesis,
          API.sgSlotsPerKESPeriod = API.sgSlotsPerKESPeriod genesis,
          API.sgMaxKESEvolutions = API.sgMaxKESEvolutions genesis,
          API.sgSlotLength = API.sgSlotLength genesis,
          API.sgUpdateQuorum = API.sgUpdateQuorum genesis,
          API.sgMaxLovelaceSupply = API.sgMaxLovelaceSupply genesis,
          API.sgProtocolParams = translateEra' ctxt (API.sgProtocolParams genesis),
          API.sgGenDelegs = API.sgGenDelegs genesis,
          API.sgInitialFunds = API.sgInitialFunds genesis,
          API.sgStaking = API.sgStaking genesis
        }

newtype Tx era = Tx {unTx :: Core.Tx era}

instance
  ( Crypto c,
    Core.Tx (ConwayEra c) ~ AlonzoTx (ConwayEra c)
  ) =>
  TranslateEra (ConwayEra c) Tx
  where
  type TranslationError (ConwayEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    bdy <- translateViaCBORAnn "txbody" $ Alonzo.body tx
    txwits <- translateViaCBORAnn "txwitness" $ Alonzo.wits tx
    aux <- case Alonzo.auxiliaryData tx of
      SNothing -> pure SNothing
      SJust axd -> SJust <$> translateViaCBORAnn "auxiliarydata" axd
    let validating = Alonzo.isValid tx
    pure $ Tx $ AlonzoTx bdy txwits validating aux

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance (Crypto c, Functor f) => TranslateEra (ConwayEra c) (ShelleyPParamsHKD f)

instance Crypto c => TranslateEra (ConwayEra c) EpochState where
  translateEra ctxt es =
    pure
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translatePParams $ esPrevPp es,
          esPp = translatePParams $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (ConwayEra c) API.LedgerState where
  translateEra ctxt@(ConwayGenesis newGenDelegs) ls =
    pure
      API.LedgerState
        { API.lsUTxOState = translateEra' ctxt $ API.lsUTxOState ls,
          API.lsDPState = updateGenesisKeys $ API.lsDPState ls
        }
    where
      updateGenesisKeys (DPState dstate pstate) = DPState dstate' pstate
        where
          dstate' = dstate {_genDelegs = newGenDelegs}

instance Crypto c => TranslateEra (ConwayEra c) API.UTxOState where
  translateEra ctxt us =
    pure
      API.UTxOState
        { API._utxo = translateEra' ctxt $ API._utxo us,
          API._deposited = API._deposited us,
          API._fees = API._fees us,
          API._ppups = translateEra' ctxt $ API._ppups us,
          API._stakeDistro = API._stakeDistro us
        }

instance Crypto c => TranslateEra (ConwayEra c) API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ translateTxOut <$> API.unUTxO utxo

instance Crypto c => TranslateEra (ConwayEra c) API.PPUPState where
  translateEra ctxt ps =
    pure
      API.PPUPState
        { API.proposals = translateEra' ctxt $ API.proposals ps,
          API.futureProposals = translateEra' ctxt $ API.futureProposals ps
        }

instance Crypto c => TranslateEra (ConwayEra c) API.ProposedPPUpdates where
  translateEra _ctxt (API.ProposedPPUpdates ppup) =
    pure $ API.ProposedPPUpdates $ fmap translatePParams ppup

translateTxOut ::
  Crypto c =>
  Core.TxOut (BabbageEra c) ->
  Core.TxOut (ConwayEra c)
translateTxOut (BabbageTxOut addr value d s) =
  BabbageTxOut addr value (translateDatum d) (translateScript <$> s)

translateDatum :: Datum (BabbageEra c) -> Datum (ConwayEra c)
translateDatum = \case
  NoDatum -> NoDatum
  DatumHash dh -> DatumHash dh
  Datum bd -> Datum (coerce bd)

translateScript :: Crypto c => Core.Script (BabbageEra c) -> Core.Script (ConwayEra c)
translateScript = \case
  TimelockScript ts -> TimelockScript $ translateTimelock ts
  PlutusScript l sbs -> PlutusScript l sbs

translatePParams ::
  forall f c. BabbagePParamsHKD f (BabbageEra c) -> BabbagePParamsHKD f (ConwayEra c)
translatePParams BabbagePParams {..} = BabbagePParams {..}
