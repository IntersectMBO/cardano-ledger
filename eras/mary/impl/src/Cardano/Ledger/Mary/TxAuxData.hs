{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxAuxData (
  module Cardano.Ledger.Allegra.TxAuxData,
)
where

import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Core (EraTxAuxData (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Scripts ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.TxAuxData (validMetadatum)
import Control.DeepSeq (deepseq)

-- =======================================

instance Crypto c => EraTxAuxData (MaryEra c) where
  type TxAuxData (MaryEra c) = AllegraTxAuxData (MaryEra c)
  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md
  hashTxAuxData aux = AuxiliaryDataHash (hashAnnotated aux)
