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
  ShelleyBlockBody (ShelleyBlockBody),
  shelleyBlockBodyHash,
  shelleyBlockBodyTxs,
 )
import Lens.Micro

instance EraBlockBody AllegraEra where
  type BlockBody AllegraEra = ShelleyBlockBody AllegraEra
  mkBasicBlockBody = ShelleyBlockBody mempty
  txSeqBlockBodyL = lens shelleyBlockBodyTxs (\_ s -> ShelleyBlockBody s)
  hashBlockBody = shelleyBlockBodyHash
  numSegComponents = 3
