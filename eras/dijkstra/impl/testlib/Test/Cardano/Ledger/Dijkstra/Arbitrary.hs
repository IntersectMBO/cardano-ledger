{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Arbitrary () where

import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.BaseTypes (PerasCert (..), StrictMaybe)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (DijkstraPParams, UpgradeDijkstraPParams)
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.Transition (TransitionConfig (..))
import Cardano.Ledger.Dijkstra.Tx (DijkstraTx (..), Tx (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError)
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireSignature,
 )
import Data.Functor.Identity (Identity)
import qualified Data.OMap.Strict as OMap
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

instance Arbitrary (TxBody SubTx DijkstraEra) where
  arbitrary =
    DijkstraSubTxBody
      <$> arbitrary
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
      <*> arbitrary

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
      <*> (choose (0, 4) >>= \n -> OMap.fromFoldable <$> vectorOf n arbitrary)

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
      -- Per CIP-0167, Dijkstra transactions should always have isValid = True
      -- The isValid flag is omitted in serialization and defaults to True
      STopTx -> DijkstraTx <$> arbitrary <*> arbitrary <*> pure (IsValid True) <*> arbitrary
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

instance
  ( EraPParams era
  , Arbitrary (PlutusPurpose AsItem era)
  , Arbitrary (PlutusPurpose AsIx era)
  , Arbitrary (PParamsHKD Identity era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (TxCert era)
  , Arbitrary (TxOut era)
  ) =>
  Arbitrary (DijkstraContextError era)
  where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Arbitrary (DijkstraBbodyPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "UTXOW" era))
  , Arbitrary (PredicateFailure (EraRule "CERTS" era))
  , Arbitrary (PredicateFailure (EraRule "GOV" era))
  , Arbitrary (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  Arbitrary (DijkstraLedgerPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( EraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Arbitrary (DijkstraUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "UTXO" era))
  , Arbitrary (TxCert era)
  , Arbitrary (PlutusPurpose AsItem era)
  , Arbitrary (PlutusPurpose AsIx era)
  ) =>
  Arbitrary (DijkstraUtxowPredFailure era)
  where
  arbitrary = genericArbitraryU

instance Era era => Arbitrary (DijkstraGovCertPredFailure era) where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Arbitrary (DijkstraGovPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  Arbitrary (PredicateFailure (EraRule "SUBLEDGER" era)) =>
  Arbitrary (DijkstraSubLedgersPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  Arbitrary (PredicateFailure (EraRule "SUBGOV" era)) =>
  Arbitrary (DijkstraSubLedgerPredFailure era)
  where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraSubGovPredFailure era) where
  arbitrary = pure DijkstraSubGovPredFailure

instance
  Arbitrary (PredicateFailure (EraRule "SUBUTXOS" era)) =>
  Arbitrary (DijkstraSubUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraSubUtxosPredFailure era) where
  arbitrary = pure DijkstraSubUtxosPredFailure

instance Arbitrary (DijkstraSubUtxowPredFailure era) where
  arbitrary = pure DijkstraSubUtxowPredFailure

instance Arbitrary PerasCert where
  arbitrary = pure PerasCert

instance
  ( EraBlockBody era
  , AlonzoEraTx era
  , Arbitrary (Tx TopTx era)
  , SafeToHash (TxWits era)
  ) =>
  Arbitrary (DijkstraBlockBody era)
  where
  arbitrary = DijkstraBlockBody <$> arbitrary <*> arbitrary
