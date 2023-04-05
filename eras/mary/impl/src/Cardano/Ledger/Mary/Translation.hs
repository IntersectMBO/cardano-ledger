{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Translation where

import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Scripts (Timelock, translateTimelock)
import Cardano.Ledger.Mary.Tx ()
import Cardano.Ledger.Mary.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Shelley.API hiding (Metadata)
import qualified Cardano.Ledger.Val as Val
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

instance Crypto c => TranslateEra (MaryEra c) NewEpochState where
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

instance (Crypto c, EraTx (MaryEra c)) => TranslateEra (MaryEra c) ShelleyTx where
  type TranslationError (MaryEra c) ShelleyTx = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTx"

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance Crypto c => TranslateEra (MaryEra c) PParams

instance Crypto c => TranslateEra (MaryEra c) PParamsUpdate

instance Crypto c => TranslateEra (MaryEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esPrevPp = translateEra' ctxt $ esPrevPp es
        , esPp = translateEra' ctxt $ esPp es
        , esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (MaryEra c) DState where
  translateEra _ DState {..} = pure DState {..}

instance Crypto c => TranslateEra (MaryEra c) VState where
  translateEra _ VState {..} = pure VState {..}

instance Crypto c => TranslateEra (MaryEra c) PState where
  translateEra _ PState {..} = pure PState {..}

instance Crypto c => TranslateEra (MaryEra c) CertState where
  translateEra ctxt ls =
    pure
      CertState
        { certDState = translateEra' ctxt $ certDState ls
        , certPState = translateEra' ctxt $ certPState ls
        , certVState = translateEra' ctxt $ certVState ls
        }

instance Crypto c => TranslateEra (MaryEra c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls
        , lsCertState = translateEra' ctxt $ lsCertState ls
        }

instance Crypto c => TranslateEra (MaryEra c) ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance Crypto c => TranslateEra (MaryEra c) ShelleyPPUPState where
  translateEra ctxt ps =
    return
      ShelleyPPUPState
        { proposals = translateEra' ctxt $ proposals ps
        , futureProposals = translateEra' ctxt $ futureProposals ps
        }

instance Crypto c => TranslateEra (MaryEra c) UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { utxosUtxo = translateEra' ctxt $ utxosUtxo us
        , utxosDeposited = utxosDeposited us
        , utxosFees = utxosFees us
        , utxosGovernance = translateEra' ctxt $ utxosGovernance us
        , utxosStakeDistr = utxosStakeDistr us
        }

instance Crypto c => TranslateEra (MaryEra c) ShelleyTxOut where
  translateEra () (TxOutCompact addr cfval) =
    pure $ TxOutCompact (coerce addr) (translateCompactValue cfval)

instance Crypto c => TranslateEra (MaryEra c) UTxO where
  translateEra ctxt utxo =
    return $ UTxO (translateEra' ctxt `Map.map` unUTxO utxo)

instance Crypto c => TranslateEra (MaryEra c) ShelleyTxWits where
  type TranslationError (MaryEra c) ShelleyTxWits = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTxWits"

instance Crypto c => TranslateEra (MaryEra c) Update where
  translateEra _ (Update pp en) = pure $ Update (coerce pp) en

instance Crypto c => TranslateEra (MaryEra c) Timelock where
  translateEra _ = pure . translateTimelock

instance Crypto c => TranslateEra (MaryEra c) AllegraTxAuxData where
  translateEra ctx (AllegraTxAuxData md as) =
    pure $ AllegraTxAuxData md $ translateEra' ctx <$> as

translateValue :: Crypto c => Coin -> MaryValue c
translateValue = Val.inject

translateCompactValue :: Crypto c => CompactForm Coin -> CompactForm (MaryValue c)
translateCompactValue =
  fromMaybe (error msg) . toCompact . translateValue . fromCompact
  where
    msg = "impossible error: compact coin is out of range"
