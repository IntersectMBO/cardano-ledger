{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is usually imported only for its instances.
module Cardano.Ledger.ShelleyMA.Scripts (Timelock (..)) where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import qualified Cardano.Ledger.Shelley as Shelley
import Cardano.Ledger.ShelleyMA (MaryOrAllegra, ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (..),
    ValidityInterval,
    hashTimelockScript,
    validateTimelock,
  )
import Data.Typeable (Typeable)
import GHC.Records
import Shelley.Spec.Ledger.Tx
  ( ValidateScript (..),
  )

-- ====================================================================

type instance
  Core.Script (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    Timelock (ShelleyMAEra ma c)

instance
  ( CryptoClass.Crypto c,
    Typeable ma,
    Shelley.TxBodyConstraints (ShelleyMAEra ma c),
    (HasField "vldt" (Core.TxBody (ShelleyMAEra ma c)) ValidityInterval)
  ) =>
  ValidateScript (ShelleyMAEra ma c)
  where
  validateScript s tx = validateTimelock s tx

  hashScript s = hashTimelockScript s
