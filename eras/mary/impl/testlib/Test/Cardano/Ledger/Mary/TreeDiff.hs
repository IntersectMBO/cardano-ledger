{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.TreeDiff (
  module Test.Cardano.Ledger.Allegra.TreeDiff,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
import Test.Cardano.Ledger.Allegra.TreeDiff

-- Value
instance ToExpr CompactValue

instance ToExpr MaryValue

instance ToExpr MultiAsset

instance ToExpr PolicyID

instance ToExpr AssetName where
  toExpr an = App "AssetName" [toExpr (assetNameToTextAsHex an)]

deriving newtype instance ToExpr (CompactForm MaryValue)

instance ToExpr (TxBody MaryEra)
