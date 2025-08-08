{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Compatibility (spec) where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBOR (..),
  decodeFullAnnotator,
  serialize,
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  AlonzoEraTxBody (..),
  TranslateEra (..),
  eraProtVerHigh,
  eraProtVerLow,
 )
import Cardano.Ledger.Core (EraTx (..), EraTxBody (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Control.Monad (replicateM)
import Control.Monad.Trans.Except (runExcept)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common (describe, it)
import Test.Cardano.Ledger.Dijkstra.ImpTest (ImpTestM, freshKeyHash)
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()
import Test.Cardano.Ledger.Imp.Common (Spec, expectRightDeep, shouldBeRightExpr, withImpInit)

spec :: Spec
spec = withImpInit $ do
  describe "Dijkstra backwards compatibility" $ do
    it "Can deserialize ConwayTxBody with requiredSignerHashes" $ do
      reqSignerHashes <- replicateM 5 $ freshKeyHash @_ @_ @_ @(ImpTestM DijkstraEra)
      let
        tx =
          mkBasicTx @ConwayEra mkBasicTxBody
            & bodyTxL . reqSignerHashesTxBodyL
              .~ Set.fromList reqSignerHashes
        bs = serialize (eraProtVerHigh @ConwayEra) $ encCBOR tx
      dijkstraTx <- expectRightDeep . runExcept $ translateEra @DijkstraEra (DijkstraGenesis undefined) tx
      let
        decodeRes =
          first show $
            decodeFullAnnotator
              (eraProtVerLow @DijkstraEra)
              "DijkstraTx"
              (decCBOR @(Annotator (Tx DijkstraEra)))
              bs
      decodeRes `shouldBeRightExpr` dijkstraTx
