{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.BlockBody () where

import Cardano.Ledger.Core (EraBlockBody (..))
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Tx ()
import Cardano.Ledger.Shelley.BlockBody (
  ShelleyBlockBody,
  mkBasicBlockBodyShelley,
  shelleyBlockBodyHash,
  txSeqBlockBodyShelleyL,
 )

instance EraBlockBody MaryEra where
  type BlockBody MaryEra = ShelleyBlockBody MaryEra
  mkBasicBlockBody = mkBasicBlockBodyShelley
  txSeqBlockBodyL = txSeqBlockBodyShelleyL
  hashBlockBody = shelleyBlockBodyHash
  numSegComponents = 3
