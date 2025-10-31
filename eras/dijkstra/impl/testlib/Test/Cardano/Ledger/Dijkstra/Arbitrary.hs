{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Arbitrary () where

import Cardano.Ledger.Allegra.Scripts (
#if __GLASGOW_HASKELL__ >= 914
  data RequireTimeExpire,
  data RequireTimeStart,
#else
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
#endif
 )
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (DijkstraPParams, UpgradeDijkstraPParams)
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.Transition (TransitionConfig (..))
import Cardano.Ledger.Dijkstra.Tx (DijkstraTx (..), Tx (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Shelley.Scripts (
#if __GLASGOW_HASKELL__ >= 914
  data RequireSignature,
#else
  pattern RequireSignature,
#endif
 )
import Data.Functor.Identity (Identity)
import Data.Typeable (Typeable)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Allegra.Arbitrary (maxTimelockDepth)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary (sizedNativeScriptGens)

instance Arbitrary (DijkstraPParams Identity DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraPParams StrictMaybe DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary (TxBody TopTx DijkstraEra) where
  arbitrary =
    DijkstraTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (UpgradeDijkstraPParams Identity DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary DijkstraGenesis where
  arbitrary = genericArbitraryU

instance Arbitrary (TransitionConfig DijkstraEra) where
  arbitrary = DijkstraTransitionConfig <$> arbitrary <*> arbitrary

instance
  (forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b)) =>
  Arbitrary (DijkstraPlutusPurpose f DijkstraEra)
  where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraNativeScript DijkstraEra) where
  arbitrary = sizedDijkstraNativeScript maxTimelockDepth

sizedDijkstraNativeScript ::
  DijkstraEraScript era =>
  Int ->
  Gen (NativeScript era)
sizedDijkstraNativeScript 0 = RequireSignature <$> arbitrary
sizedDijkstraNativeScript n =
  oneof $
    sizedNativeScriptGens n
      <> [ RequireTimeStart <$> arbitrary
         , RequireTimeExpire <$> arbitrary
         , RequireGuard <$> arbitrary
         ]

instance (Arbitrary (TxBody l DijkstraEra), Typeable l) => Arbitrary (Tx l DijkstraEra) where
  arbitrary =
    fmap MkDijkstraTx . withSTxBothLevels @l $ \case
      STopTx -> DijkstraTx <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      SSubTx -> DijkstraSubTx <$> arbitrary <*> arbitrary <*> arbitrary

instance Era era => Arbitrary (DijkstraTxCert era) where
  arbitrary =
    oneof
      [ DijkstraTxCertDeleg <$> arbitrary
      , DijkstraTxCertPool <$> arbitrary
      , DijkstraTxCertGov <$> arbitrary
      ]

instance Arbitrary DijkstraDelegCert where
  arbitrary = DijkstraRegDelegCert <$> arbitrary <*> arbitrary <*> arbitrary
