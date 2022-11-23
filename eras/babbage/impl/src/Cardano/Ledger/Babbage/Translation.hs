{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Translation where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParamsHKD (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..), Datum (..))
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era
  ( PreviousEra,
    TranslateEra (..),
    TranslationContext,
    translateEra',
  )
import Cardano.Ledger.HKD (HKDFunctor (..))
import Cardano.Ledger.Shelley.API
  ( EpochState (..),
    NewEpochState (..),
    ShelleyGenesis,
    StrictMaybe (..),
  )
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))

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

type instance PreviousEra (BabbageEra c) = AlonzoEra c

type instance TranslationContext (BabbageEra c) = AlonzoGenesis

instance
  (Crypto c) =>
  TranslateEra (BabbageEra c) NewEpochState
  where
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

instance Crypto c => TranslateEra (BabbageEra c) ShelleyGenesis where
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
    Core.Tx (BabbageEra c) ~ AlonzoTx (BabbageEra c)
  ) =>
  TranslateEra (BabbageEra c) Tx
  where
  type TranslationError (BabbageEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- Core.translateEraThroughCBOR "TxBody" $ Alonzo.body tx
    txWits <- Core.translateEraThroughCBOR "TxWitness" $ Alonzo.wits tx
    auxData <- case Alonzo.auxiliaryData tx of
      SNothing -> pure SNothing
      SJust auxData -> SJust <$> Core.translateEraThroughCBOR "AuxData" auxData
    let validating = Alonzo.isValid tx
    pure $ Tx $ AlonzoTx txBody txWits validating auxData

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance (Crypto c, Functor f) => TranslateEra (BabbageEra c) (ShelleyPParamsHKD f)

instance Crypto c => TranslateEra (BabbageEra c) EpochState where
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

instance Crypto c => TranslateEra (BabbageEra c) API.LedgerState where
  translateEra ctxt ls =
    pure
      API.LedgerState
        { API.lsUTxOState = translateEra' ctxt $ API.lsUTxOState ls,
          API.lsDPState = API.lsDPState ls
        }

instance Crypto c => TranslateEra (BabbageEra c) API.UTxOState where
  translateEra ctxt us =
    pure
      API.UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us,
          API.utxosDeposited = API.utxosDeposited us,
          API.utxosFees = API.utxosFees us,
          API.utxosPpups = translateEra' ctxt $ API.utxosPpups us,
          API.utxosStakeDistr = API.utxosStakeDistr us
        }

instance Crypto c => TranslateEra (BabbageEra c) API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ translateTxOut `Map.map` API.unUTxO utxo

instance Crypto c => TranslateEra (BabbageEra c) API.PPUPState where
  translateEra ctxt ps =
    pure
      API.PPUPState
        { API.proposals = translateEra' ctxt $ API.proposals ps,
          API.futureProposals = translateEra' ctxt $ API.futureProposals ps
        }

instance Crypto c => TranslateEra (BabbageEra c) API.ProposedPPUpdates where
  translateEra _ctxt (API.ProposedPPUpdates ppup) =
    pure $ API.ProposedPPUpdates $ fmap translatePParams ppup

translateTxOut ::
  Crypto c =>
  Core.TxOut (AlonzoEra c) ->
  Core.TxOut (BabbageEra c)
translateTxOut (AlonzoTxOut addr value dh) = BabbageTxOut addr value d SNothing
  where
    d = case dh of
      SNothing -> NoDatum
      SJust d' -> DatumHash d'

-- | A word is 8 bytes, so to convert from coinsPerUTxOWord to coinsPerUTxOByte, rounding down.
coinsPerUTxOWordToCoinsPerUTxOByte :: Coin -> Coin
coinsPerUTxOWordToCoinsPerUTxOByte (Coin c) = Coin $ c `div` 8

-- | A word is 8 bytes, so to convert from coinsPerUTxOByte to coinsPerUTxOWord.
coinsPerUTxOByteToCoinsPerUTxOWord :: Coin -> Coin
coinsPerUTxOByteToCoinsPerUTxOWord (Coin c) = Coin $ c * 8

translatePParams ::
  forall f c. HKDFunctor f => AlonzoPParamsHKD f (AlonzoEra c) -> BabbagePParamsHKD f (BabbageEra c)
translatePParams AlonzoPParams {_coinsPerUTxOWord = cpuw, ..} =
  BabbagePParams {_coinsPerUTxOByte = cpub, ..}
  where
    cpub = hkdMap (Proxy :: Proxy f) coinsPerUTxOWordToCoinsPerUTxOByte cpuw
