{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Annotator (

) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.Tx (DijkstraTx, Tx (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Cardano.Ledger.MemoBytes (decodeMemoized)
import Data.Typeable (Typeable)
import Test.Cardano.Ledger.Conway.Binary.Annotator ()

deriving newtype instance Typeable l => DecCBOR (TxBody l DijkstraEra)

instance Era era => DecCBOR (DijkstraNativeScriptRaw era) where
  decCBOR = decode $ Summands "DijkstraNativeScriptRaw" $ \case
    0 -> SumD DijkstraRequireSignature <! From
    1 -> SumD DijkstraRequireAllOf <! From
    2 -> SumD DijkstraRequireAnyOf <! From
    3 -> SumD DijkstraRequireMOf <! From <! From
    4 -> SumD DijkstraTimeStart <! From
    5 -> SumD DijkstraTimeExpire <! From
    6 -> SumD DijkstraRequireGuard <! From
    n -> Invalid n

instance Era era => DecCBOR (DijkstraNativeScript era) where
  decCBOR = MkDijkstraNativeScript <$> decodeMemoized decCBOR

instance Typeable l => DecCBOR (DijkstraTx l DijkstraEra) where
  decCBOR = undefined

deriving newtype instance Typeable l => DecCBOR (Tx l DijkstraEra)
