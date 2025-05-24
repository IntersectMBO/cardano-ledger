{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Binary.Annotator (
  module Test.Cardano.Ledger.Alonzo.Binary.Annotator,
) where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody
import Cardano.Ledger.Binary
import Test.Cardano.Ledger.Alonzo.Binary.Annotator

deriving newtype instance DecCBOR (TxBody BabbageEra)
