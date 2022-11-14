{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Generators where

import Cardano.Binary (ToCBOR (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data
  ( AlonzoTxAuxData (..),
    AuxiliaryDataHash,
    BinaryData,
    Data (..),
    Datum (..),
    dataToBinaryData,
  )
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.PParams
  ( AlonzoPParams,
    AlonzoPParamsHKD (AlonzoPParams),
    AlonzoPParamsUpdate,
    getLanguageView,
  )
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxoPredFailure (..),
    AlonzoUtxosPredFailure (..),
    AlonzoUtxowPredFailure (..),
    FailureDescription (..),
    TagMismatchDescription (..),
  )
import Cardano.Ledger.Alonzo.Scripts
  ( AlonzoScript (..),
    CostModel,
    CostModels (..),
    ExUnits (..),
    Prices (..),
    Tag (..),
    mkCostModel,
  )
import Cardano.Ledger.Alonzo.Tx
  ( AlonzoTx (AlonzoTx),
    AlonzoTxBody (..),
    IsValid (..),
    ScriptIntegrity (..),
    ScriptPurpose (..),
    hashData,
  )
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..), ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.BaseTypes (Network, StrictMaybe (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Keys (KeyHash)
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (DecodeNonNegative, EncodeMint (..), Val)
import Cardano.Slotting.Slot (SlotNo)
import Codec.CBOR.Term (Term (..))
import Control.State.Transition (PredicateFailure)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Twiddle
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Records (HasField)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen (costModelParamsCount)
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck

instance Era era => Arbitrary (Data era) where
  arbitrary = Data <$> arbitrary

instance Era era => Arbitrary (BinaryData era) where
  arbitrary = dataToBinaryData <$> arbitrary

instance Arbitrary PV1.Data where
  arbitrary = resize 5 (sized gendata)
    where
      gendata n
        | n > 0 =
            oneof
              [ PV1.I <$> arbitrary,
                PV1.B <$> arbitrary,
                PV1.Map <$> listOf ((,) <$> gendata (n `div` 2) <*> gendata (n `div` 2)),
                PV1.Constr
                  <$> fmap fromIntegral (arbitrary :: Gen Natural)
                  <*> listOf (gendata (n `div` 2)),
                PV1.List <$> listOf (gendata (n `div` 2))
              ]
      gendata _ = oneof [PV1.I <$> arbitrary, PV1.B <$> arbitrary]

instance
  ( Script era ~ AlonzoScript era,
    Arbitrary (Script era),
    Era era
  ) =>
  Arbitrary (AlonzoTxAuxData era)
  where
  arbitrary = AlonzoTxAuxData <$> arbitrary <*> arbitrary

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
  ( Mock (EraCrypto era),
    Arbitrary (Script era),
    AlonzoScript era ~ Script era,
    EraScript era
  ) =>
  Arbitrary (AlonzoTxWits era)
  where
  arbitrary =
    AlonzoTxWits
      <$> arbitrary
      <*> arbitrary
      <*> genScripts
      <*> genData
      <*> arbitrary

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList ((\x -> (f x, x)) <$> xs)

genScripts ::
  forall era.
  ( Script era ~ AlonzoScript era,
    EraScript era,
    Arbitrary (AlonzoScript era)
  ) =>
  Gen (Map (ScriptHash (EraCrypto era)) (Script era))
genScripts = keyBy (hashScript @era) <$> (arbitrary :: Gen [Script era])

genData :: forall era. Era era => Gen (TxDats era)
genData = TxDats . keyBy hashData <$> arbitrary

instance
  ( EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Value era)
  ) =>
  Arbitrary (AlonzoTxOut era)
  where
  arbitrary =
    AlonzoTxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  (EraTxOut era, ToCBOR (PParamsUpdate era), Arbitrary (TxOut era), Mock (EraCrypto era)) =>
  Arbitrary (AlonzoTxBody era)
  where
  arbitrary =
    AlonzoTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMintValues @(EraCrypto era)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary IsValid

instance
  ( Arbitrary (TxBody era),
    Arbitrary (TxWits era),
    Arbitrary (TxAuxData era)
  ) =>
  Arbitrary (AlonzoTx era)
  where
  arbitrary =
    AlonzoTx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Era era, Mock (EraCrypto era)) => Arbitrary (AlonzoScript era) where
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
  arbitrary = elements nonNativeLanguages

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary

genCM :: Language -> Gen CostModel
genCM lang = do
  newParamValues <- (vectorOf (costModelParamsCount lang) (arbitrary :: Gen Integer))
  pure $ fromRight (error "Corrupt cost model") (mkCostModel lang newParamValues)

genCostModel :: Language -> Gen (Language, CostModel)
genCostModel lang = (,) lang <$> genCM lang

instance Arbitrary CostModel where
  arbitrary = snd <$> (elements nonNativeLanguages >>= genCostModel)

instance Arbitrary CostModels where
  arbitrary = CostModels . Map.fromList <$> (sublistOf nonNativeLanguages >>= mapM genCostModel)

instance Arbitrary (AlonzoPParams era) where
  arbitrary =
    AlonzoPParams
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

instance Arbitrary (AlonzoPParamsUpdate era) where
  arbitrary =
    AlonzoPParams
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
  arbitrary = PlutusFailure <$> (pack <$> arbitrary) <*> arbitrary

instance Arbitrary TagMismatchDescription where
  arbitrary =
    oneof [pure PassedUnexpectedly, FailedUnexpectedly <$> ((:|) <$> arbitrary <*> arbitrary)]

instance
  (Era era, Mock (EraCrypto era), Arbitrary (PredicateFailure (EraRule "PPUP" era))) =>
  Arbitrary (AlonzoUtxosPredFailure era)
  where
  arbitrary =
    oneof
      [ ValidationTagMismatch <$> arbitrary <*> arbitrary,
        UpdateFailure <$> arbitrary
      ]

instance
  ( EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Value era),
    Arbitrary (TxOut era),
    Arbitrary (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Arbitrary (AlonzoUtxoPredFailure era)
  where
  arbitrary =
    oneof
      [ BadInputsUTxO <$> arbitrary,
        OutsideValidityIntervalUTxO <$> arbitrary <*> arbitrary,
        MaxTxSizeUTxO <$> arbitrary <*> arbitrary,
        pure InputSetEmptyUTxO,
        FeeTooSmallUTxO <$> arbitrary <*> arbitrary,
        ValueNotConservedUTxO <$> arbitrary <*> arbitrary,
        OutputTooSmallUTxO <$> arbitrary,
        UtxosFailure <$> arbitrary,
        WrongNetwork <$> arbitrary <*> arbitrary,
        WrongNetworkWithdrawal <$> arbitrary <*> arbitrary,
        OutputBootAddrAttrsTooBig <$> arbitrary,
        pure TriesToForgeADA,
        OutputTooBigUTxO <$> arbitrary,
        InsufficientCollateral <$> arbitrary <*> arbitrary,
        ScriptsNotPaidUTxO <$> arbitrary,
        ExUnitsTooBigUTxO <$> arbitrary <*> arbitrary,
        CollateralContainsNonADA <$> arbitrary
      ]

instance
  ( Era era,
    Mock (EraCrypto era),
    Arbitrary (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Arbitrary (AlonzoUtxowPredFailure era)
  where
  arbitrary =
    oneof
      [ ShelleyInAlonzoUtxowPredFailure <$> arbitrary,
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

instance
  ( Era era,
    Mock (EraCrypto era),
    Arbitrary (PParams era),
    HasField "_costmdls" (PParams era) CostModels
  ) =>
  Arbitrary (ScriptIntegrity era)
  where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> genData
      -- FIXME: why singleton? We should generate empty as well as many value sets
      <*> (Set.singleton <$> (getLanguageView @era <$> arbitrary <*> arbitrary))

instance
  (Mock (EraCrypto era), Era era) =>
  Arbitrary (Datum era)
  where
  arbitrary =
    oneof
      [ pure NoDatum,
        DatumHash <$> arbitrary,
        Datum . dataToBinaryData <$> arbitrary
      ]

instance (Era era, Val (Value era), DecodeNonNegative (Value era)) => Twiddle (AlonzoTxOut era) where
  twiddle = twiddle . toTerm

instance Twiddle SlotNo where
  twiddle = twiddle . toTerm

instance C.Crypto c => Twiddle (DCert c) where
  twiddle = twiddle . toTerm

instance C.Crypto c => Twiddle (Wdrl c) where
  twiddle = twiddle . toTerm

instance C.Crypto c => Twiddle (AuxiliaryDataHash c) where
  twiddle = twiddle . toTerm

emptyOrNothing ::
  forall t a c.
  ( Foldable t,
    Twiddle (t Void),
    Monoid (t Void),
    Twiddle (t a)
  ) =>
  AlonzoTxBody (AlonzoEra c) ->
  (AlonzoTxBody (AlonzoEra c) -> t a) ->
  Gen (Maybe Term)
emptyOrNothing txBody f =
  if null $ f txBody
    then
      oneof
        [ Just <$> twiddle @(t Void) mempty,
          pure Nothing
        ]
    else Just <$> twiddle (f txBody)

twiddleStrictMaybe :: Twiddle a => StrictMaybe a -> Gen (Maybe Term)
twiddleStrictMaybe SNothing = pure Nothing
twiddleStrictMaybe (SJust x) = Just <$> twiddle x

instance C.Crypto c => Twiddle (Update (AlonzoEra c)) where
  twiddle = twiddle . toTerm

instance C.Crypto c => Twiddle (MultiAsset c) where
  twiddle = twiddle . encodingToTerm . encodeMint

instance C.Crypto c => Twiddle (ScriptIntegrityHash c) where
  twiddle = twiddle . toTerm

instance (C.Crypto c, Typeable t) => Twiddle (KeyHash t c) where
  twiddle = twiddle . toTerm

instance Twiddle Network where
  twiddle = twiddle . toTerm

instance C.Crypto c => Twiddle (TxIn c) where
  twiddle = twiddle . toTerm

instance Twiddle Coin where
  twiddle = twiddle . toTerm

instance C.Crypto c => Twiddle (AlonzoTxBody (AlonzoEra c)) where
  twiddle txBody = do
    inputs' <- twiddle $ atbInputs txBody
    outputs' <- twiddle $ atbOutputs txBody
    fee' <- twiddle $ atbTxFee txBody
    -- Empty collateral can be represented by empty set or the
    -- value can be omitted entirely
    ttl' <- twiddleStrictMaybe . invalidHereafter $ atbValidityInterval txBody
    cert' <- emptyOrNothing txBody atbCerts
    wdrls' <- twiddle $ atbWdrls txBody
    update' <- twiddleStrictMaybe $ atbUpdate txBody
    auxDataHash' <- twiddleStrictMaybe $ atbAuxDataHash txBody
    validityStart' <- twiddleStrictMaybe . invalidBefore $ atbValidityInterval txBody
    mint' <- twiddle $ atbMint txBody
    scriptDataHash' <- twiddleStrictMaybe $ atbScriptIntegrityHash txBody
    collateral' <- emptyOrNothing txBody atbCollateral
    requiredSigners' <- emptyOrNothing txBody atbReqSignerHashes
    networkId' <- twiddleStrictMaybe $ atbTxNetworkId txBody
    mp <- elements [TMap, TMapI]
    let fields =
          [ (TInt 0, inputs'),
            (TInt 1, outputs'),
            (TInt 2, fee')
          ]
            <> catMaybes
              [ (TInt 3,) <$> ttl',
                (TInt 4,) <$> cert',
                (TInt 5,) <$> Just wdrls',
                (TInt 6,) <$> update',
                (TInt 7,) <$> auxDataHash',
                (TInt 8,) <$> validityStart',
                (TInt 9,) <$> Just mint',
                (TInt 11,) <$> scriptDataHash',
                (TInt 13,) <$> collateral',
                (TInt 14,) <$> requiredSigners',
                (TInt 15,) <$> networkId'
              ]
    fields' <- shuffle fields
    pure $ mp fields'

instance Typeable c => Twiddle (AlonzoScript (AlonzoEra c)) where
  twiddle = twiddle . toTerm

instance Typeable c => Twiddle (Data (AlonzoEra c)) where
  twiddle = twiddle . toTerm

instance Typeable c => Twiddle (BinaryData (AlonzoEra c)) where
  twiddle = twiddle . toTerm
