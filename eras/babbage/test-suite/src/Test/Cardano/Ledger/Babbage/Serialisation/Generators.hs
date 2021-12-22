{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -fno-warn-unused-imports #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Generators where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Data (AuxiliaryData (..), Data (..))
import Cardano.Ledger.Babbage.Language
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Rules.Utxo (UtxoPredicateFailure (..))
import Cardano.Ledger.Babbage.Rules.Utxos (TagMismatchDescription (..), UtxosPredicateFailure (..))
import Cardano.Ledger.Babbage.Rules.Utxow (BabbagePredFail (..))
import Cardano.Ledger.Babbage.Scripts
  ( CostModel (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    Tag (..),
  )
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody
  ( BabbageBody,
    Datum (..),
    TxBody (..),
    TxOut (..),
  )
import Cardano.Ledger.Babbage.TxInfo (FailureDescription (..), ScriptResult (..))
import Cardano.Ledger.Babbage.TxWitness
import Cardano.Ledger.Compactible (toCompact)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Shelley.CompactAddr (compactAddr)
import Cardano.Ledger.Shelley.Constraints (UsesScript, UsesValue)
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import Data.Text (pack)
import Numeric.Natural (Natural)
import Plutus.V1.Ledger.Api (defaultCostModelParams)
import qualified PlutusTx as Plutus
import Test.Cardano.Ledger.Babbage.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Value era)
  ) =>
  Arbitrary (TxOut era)
  where
  arbitrary =
    TxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( UsesValue era,
    Mock (Crypto era),
    BabbageBody (era),
    Arbitrary (Core.Value era)
  ) =>
  Arbitrary (TxBody era)
  where
  arbitrary =
    TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMintValues
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (Data era) where
  arbitrary = Data <$> arbitrary

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair x y = do a <- x; b <- y; pure (a, b)

instance Arbitrary Plutus.Data where
  arbitrary = resize 5 (sized gendata)
    where
      gendata n
        | n > 0 =
          oneof
            [ (Plutus.I <$> arbitrary),
              (Plutus.B <$> arbitrary),
              (Plutus.Map <$> listOf (genPair (gendata (n `div` 2)) (gendata (n `div` 2)))),
              ( Plutus.Constr <$> fmap fromIntegral (arbitrary :: Gen Natural)
                  <*> listOf (gendata (n `div` 2))
              ),
              (Plutus.List <$> listOf (gendata (n `div` 2)))
            ]
      gendata _ = oneof [Plutus.I <$> arbitrary, Plutus.B <$> arbitrary]

instance
  ( UsesScript era,
    Ord (Core.Script era),
    Core.Script era ~ Script era,
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (AuxiliaryData era)
  where
  arbitrary = AuxiliaryData <$> arbitrary <*> arbitrary

instance Arbitrary Tag where
  arbitrary = elements [Spend, Mint, Cert, Rewrd]

instance Arbitrary RdmrPtr where
  arbitrary = RdmrPtr <$> arbitrary <*> arbitrary

instance Arbitrary ExUnits where
  arbitrary = ExUnits <$> genUnit <*> genUnit
    where
      genUnit = fromIntegral <$> choose (0, maxBound :: Int64)

instance (Era era) => Arbitrary (Redeemers era) where
  arbitrary = Redeemers <$> arbitrary

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Script era),
    Script era ~ Core.Script era,
    ValidateScript era,
    UsesScript era
  ) =>
  Arbitrary (TxWitness era)
  where
  arbitrary =
    TxWitness
      <$> arbitrary
      <*> arbitrary
      <*> genScripts
      <*> genData
      <*> arbitrary

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList ((\x -> (f x, x)) <$> xs)

genScripts ::
  forall era.
  ( Core.Script era ~ Script era,
    ValidateScript era,
    Arbitrary (Script era)
  ) =>
  Gen (Map (ScriptHash (Crypto era)) (Core.Script era))
genScripts = keyBy (hashScript @era) <$> (arbitrary :: Gen [Core.Script era])

genData :: forall era. Era era => Gen (TxDats era)
genData = TxDats <$> keyBy hashData <$> arbitrary

deriving newtype instance Arbitrary IsValid

instance Mock c => Arbitrary (ValidatedTx (BabbageEra c)) where
  arbitrary =
    ValidatedTx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (Script (BabbageEra c)) where
  arbitrary = do
    lang <- arbitrary -- The language is not present in the Script serialization
    frequency
      [ (1, pure (alwaysSucceeds lang 1)),
        (1, pure (alwaysFails lang 1)),
        (10, TimelockScript <$> arbitrary)
      ]

-- ==========================
--

instance Arbitrary Language where
  arbitrary = elements (Set.toList nonNativeLanguages)

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary

instance Arbitrary CostModel where
  arbitrary = CostModel <$> traverse (const arbitrary) dcmp
    where
      dcmp = fromMaybe (error "Corrupt default cost model") defaultCostModelParams

instance Arbitrary (PParams era) where
  arbitrary =
    PParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (PParamsUpdate era) where
  arbitrary =
    PParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary FailureDescription where
  arbitrary =
    oneof
      [ (OnePhaseFailure . pack) <$> arbitrary,
        PlutusFailure <$> (pack <$> arbitrary) <*> arbitrary
      ]

instance Arbitrary ScriptResult where
  arbitrary =
    oneof [pure Passes, Fails <$> arbitrary]

instance Arbitrary TagMismatchDescription where
  arbitrary =
    oneof [pure PassedUnexpectedly, FailedUnexpectedly <$> arbitrary]

instance Mock c => Arbitrary (UtxosPredicateFailure (BabbageEra c)) where
  arbitrary =
    oneof
      [ ValidationTagMismatch <$> arbitrary <*> arbitrary,
        UpdateFailure <$> arbitrary
      ]

instance Mock c => Arbitrary (UtxoPredicateFailure (BabbageEra c)) where
  arbitrary =
    oneof
      [ (BadInputsUTxO) <$> arbitrary,
        OutsideValidityIntervalUTxO <$> arbitrary <*> arbitrary,
        MaxTxSizeUTxO <$> arbitrary <*> arbitrary,
        pure InputSetEmptyUTxO,
        FeeTooSmallUTxO <$> arbitrary <*> arbitrary,
        (ValueNotConservedUTxO) <$> arbitrary <*> arbitrary,
        (OutputTooSmallUTxO) <$> arbitrary,
        (UtxosFailure) <$> arbitrary,
        (WrongNetwork) <$> arbitrary <*> arbitrary,
        (WrongNetworkWithdrawal) <$> arbitrary <*> arbitrary,
        (OutputBootAddrAttrsTooBig) <$> arbitrary,
        pure TriesToForgeADA,
        (OutputTooBigUTxO) <$> arbitrary,
        InsufficientCollateral <$> arbitrary <*> arbitrary,
        ScriptsNotPaidUTxO <$> arbitrary,
        ExUnitsTooBigUTxO <$> arbitrary <*> arbitrary,
        CollateralContainsNonADA <$> arbitrary
      ]

instance Mock c => Arbitrary (BabbagePredFail (BabbageEra c)) where
  arbitrary =
    oneof
      [ WrappedShelleyEraFailure <$> arbitrary,
        MissingRedeemers <$> arbitrary,
        MissingRequiredDatums <$> arbitrary <*> arbitrary,
        PPViewHashesDontMatch <$> arbitrary <*> arbitrary
      ]

instance Mock c => Arbitrary (ScriptPurpose c) where
  arbitrary =
    oneof
      [ Minting <$> arbitrary,
        Spending <$> arbitrary,
        Rewarding <$> arbitrary,
        Certifying <$> arbitrary
      ]

instance Mock c => Arbitrary (ScriptIntegrity (BabbageEra c)) where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> genData
      <*> (Set.singleton <$> (getLanguageView <$> arbitrary <*> arbitrary))

instance
  Mock (Crypto era) =>
  Arbitrary (Datum era)
  where
  arbitrary =
    oneof
      [ pure NoDatum,
        DatumHash <$> arbitrary,
        Datum <$> arbitrary
      ]
