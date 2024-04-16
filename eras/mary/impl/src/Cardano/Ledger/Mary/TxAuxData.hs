{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxAuxData (
  module Cardano.Ledger.Allegra.TxAuxData,
)
where

import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..), metadataAllegraTxAuxDataL)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Core (EraTxAuxData (..), upgradeScript)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Scripts ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.TxAuxData (validMetadatum)
import Control.DeepSeq (deepseq)

-- =======================================

instance Crypto c => EraTxAuxData (MaryEra c) where
  type TxAuxData (MaryEra c) = AllegraTxAuxData (MaryEra c)

  mkBasicTxAuxData = AllegraTxAuxData mempty mempty

  metadataTxAuxDataL = metadataAllegraTxAuxDataL

  upgradeTxAuxData (AllegraTxAuxData md scripts) = AllegraTxAuxData md $ upgradeScript <$> scripts

  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md

  hashTxAuxData aux = AuxiliaryDataHash (hashAnnotated aux)
