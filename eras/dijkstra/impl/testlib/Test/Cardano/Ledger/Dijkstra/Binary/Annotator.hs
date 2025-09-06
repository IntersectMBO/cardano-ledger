{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Annotator (

) where

import Cardano.Ledger.Binary (DecCBOR)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Test.Cardano.Ledger.Conway.Binary.Annotator ()

deriving newtype instance DecCBOR (TxBody DijkstraEra)

deriving newtype instance DecCBOR (Tx DijkstraEra)
