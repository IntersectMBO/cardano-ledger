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
import Cardano.Ledger.Shelley.BlockBody (ShelleyBlockBody (..))

instance EraBlockBody AllegraEra where
  type BlockBody AllegraEra = ShelleyBlockBody AllegraEra
  mkBasicBlockBody = mkBasicBlockBody
  txSeqBlockBodyL = txSeqBlockBodyL
  hashBlockBody = hashBlockBody
  numSegComponents = 3
