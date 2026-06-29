{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.BlockBody () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Tx ()
import Cardano.Ledger.Core (EraBlockBody (..))
import Cardano.Ledger.Shelley.BlockBody (
  ShelleyBlockBody,
  mkBasicBlockBodyShelley,
  shelleyBlockBodyHash,
  txSeqBlockBodyShelleyL,
 )

instance EraBlockBody AllegraEra where
  type BlockBody AllegraEra = ShelleyBlockBody AllegraEra
  mkBasicBlockBody = mkBasicBlockBodyShelley
  txSeqBlockBodyL = txSeqBlockBodyShelleyL
  hashBlockBody = shelleyBlockBodyHash
  numSegComponents = 3
