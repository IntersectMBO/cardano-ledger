{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Translation where

import Cardano.Binary
  ( DecoderError,
    decodeAnnotator,
    fromCBOR,
    serialize,
  )
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era hiding (Crypto)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Shelley.API hiding (Metadata, TxBody)
import Cardano.Ledger.Shelley.Tx (decodeWits)
import Cardano.Ledger.ShelleyMA.AuxiliaryData
  ( MAAuxiliaryData (..),
  )
import qualified Cardano.Ledger.Val as Val
import Control.Monad.Except (throwError)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
-- Translation from Allegra to Mary
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

type instance PreviousEra (MaryEra c) = AllegraEra c

-- | Currently no context is needed to translate from Allegra to Mary.
--
-- Note: if context is needed, please coordinate with consensus, who will have
-- to provide the context in the right place.
type instance TranslationContext (MaryEra c) = ()

instance Crypto c => TranslateEra (MaryEra c) NewEpochState where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ nesEs nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes,
          stashedAVVMAddresses = ()
        }

instance Crypto c => TranslateEra (MaryEra c) ShelleyTx where
  type TranslationError (MaryEra c) ShelleyTx = DecoderError
  translateEra _ctx tx =
    case decodeAnnotator "tx" fromCBOR (serialize tx) of
      Right newTx -> pure newTx
      Left decoderError -> throwError decoderError

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance (Crypto c, Functor f) => TranslateEra (MaryEra c) (ShelleyPParamsHKD f)

instance Crypto c => TranslateEra (MaryEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translateEra' ctxt $ esPrevPp es,
          esPp = translateEra' ctxt $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (MaryEra c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls,
          lsDPState = lsDPState ls
        }

instance Crypto c => TranslateEra (MaryEra c) ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance Crypto c => TranslateEra (MaryEra c) PPUPState where
  translateEra ctxt ps =
    return
      PPUPState
        { proposals = translateEra' ctxt $ proposals ps,
          futureProposals = translateEra' ctxt $ futureProposals ps
        }

instance Crypto c => TranslateEra (MaryEra c) UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { _utxo = translateEra' ctxt $ _utxo us,
          _deposited = _deposited us,
          _fees = _fees us,
          _ppups = translateEra' ctxt $ _ppups us,
          _stakeDistro = _stakeDistro us
        }

instance Crypto c => TranslateEra (MaryEra c) ShelleyTxOut where
  translateEra () (TxOutCompact addr cfval) =
    pure $ TxOutCompact (coerce addr) (translateCompactValue cfval)

instance Crypto c => TranslateEra (MaryEra c) UTxO where
  translateEra ctxt utxo =
    return $ UTxO (translateEra' ctxt <$> unUTxO utxo)

instance Crypto c => TranslateEra (MaryEra c) ShelleyWitnesses where
  type TranslationError (MaryEra c) ShelleyWitnesses = DecoderError
  translateEra _ctx ws =
    case decodeAnnotator "witnessSet" decodeWits (serialize ws) of
      Right new -> pure new
      Left decoderError -> throwError decoderError

instance Crypto c => TranslateEra (MaryEra c) Update where
  translateEra _ (Update pp en) = pure $ Update (coerce pp) en

instance Crypto c => TranslateEra (MaryEra c) MAAuxiliaryData where
  translateEra _ (MAAuxiliaryData md as) =
    pure $ MAAuxiliaryData md as

translateValue :: Crypto c => Coin -> MaryValue c
translateValue = Val.inject

translateCompactValue :: Crypto c => CompactForm Coin -> CompactForm (MaryValue c)
translateCompactValue =
  fromMaybe (error msg) . toCompact . translateValue . fromCompact
  where
    msg = "impossible error: compact coin is out of range"
