{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Annotator (

) where

import Cardano.Ledger.Binary (DecCBOR)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))

deriving newtype instance DecCBOR (TxBody DijkstraEra)
