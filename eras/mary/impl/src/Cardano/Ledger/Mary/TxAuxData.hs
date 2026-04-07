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
) where

import Cardano.Ledger.Allegra.TxAuxData (
  AllegraEraTxAuxData (..),
  AllegraTxAuxData (..),
  metadataAllegraTxAuxDataL,
  nativeScriptsAllegraTxAuxDataL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Scripts ()
import Control.DeepSeq (deepseq)

-- =======================================

instance EraTxAuxData MaryEra where
  type TxAuxData MaryEra = AllegraTxAuxData MaryEra

  mkBasicTxAuxData = AllegraTxAuxData mempty mempty

  metadataTxAuxDataL = metadataAllegraTxAuxDataL

  validateTxAuxData _ (AllegraTxAuxData _ as) = as `deepseq` True

instance AllegraEraTxAuxData MaryEra where
  nativeScriptsTxAuxDataL = nativeScriptsAllegraTxAuxDataL
