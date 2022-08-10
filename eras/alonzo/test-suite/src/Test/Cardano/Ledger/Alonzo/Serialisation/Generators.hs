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
import Cardano.Ledger.Alonzo.Data
  ( AlonzoAuxiliaryData (..),
    BinaryData,
    Data (..),
    Datum (..),
    dataToBinaryData,
  )
import Cardano.Ledger.Alonzo.Language (Language (..), nonNativeLanguages)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (AlonzoAuxiliaryData (..), BinaryData, Data (..), dataToBinaryData, AuxiliaryDataHash)
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
    CostModels (..),
    ExUnits (..),
    Prices (..),
    Tag (..),
    mkCostModel,
  )
import Cardano.Ledger.Alonzo.Tx
  ( AlonzoTx (AlonzoTx),
    AlonzoTxBody (AlonzoTxBody),
    CostModel,
    IsValid (..),
    ScriptIntegrity (..),
    ScriptPurpose (..),
    hashData,
  )
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Alonzo.TxWitness
import Cardano.Ledger.BaseTypes (StrictMaybe (..), Network)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Control.State.Transition (PredicateFailure)
import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (DecodeNonNegative, Val, EncodeMint (..))
import Cardano.Slotting.Slot (SlotNo)
import Codec.CBOR.Term (Term (..), encodeTerm)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import GHC.Records (HasField)
import Data.Twiddle
import Data.Void (Void)
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V2.Ledger.Api as PV2
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.Keys (KeyHash)
import Data.Typeable (Typeable)
import Debug.Trace (traceM)
import Codec.CBOR.Write (toLazyByteString)

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
              PV1.Constr <$> fmap fromIntegral (arbitrary :: Gen Natural)
                <*> listOf (gendata (n `div` 2)),
              PV1.List <$> listOf (gendata (n `div` 2))
            ]
      gendata _ = oneof [PV1.I <$> arbitrary, PV1.B <$> arbitrary]

instance
  ( Script era ~ AlonzoScript era,
    Arbitrary (Script era),
    Era era
  ) =>
  Arbitrary (AlonzoAuxiliaryData era)
  where
  arbitrary =
    frequency
      [ (9, AlonzoAuxiliaryData <$> arbitrary <*> arbitrary),
        (1, pure $ AlonzoAuxiliaryData mempty mempty)
      ]

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
  (EraTxOut era, ToCBOR (PParamsUpdate era), Arbitrary (Value era), Mock (EraCrypto era)) =>
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
  ( EraTxBody era,
    EraScript era,
    Mock (EraCrypto era),
    Script era ~ AlonzoScript era,
    Arbitrary (TxBody era),
    Arbitrary (AuxiliaryData era)
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

mkNullCostModel :: Set Text -> Map Text Integer
mkNullCostModel = Map.fromSet (const 0)

genCM :: Language -> Set Text -> Gen CostModel
genCM lang costModelParamNames = do
  newCMPs <- traverse (const arbitrary) (mkNullCostModel costModelParamNames)
  either (error "Corrupt cost model") pure $ mkCostModel lang newCMPs

genCostModel :: Language -> Gen (Language, CostModel)
genCostModel PlutusV1 = (PlutusV1,) <$> genCM PlutusV1 PV1.costModelParamNames
genCostModel PlutusV2 = (PlutusV2,) <$> genCM PlutusV2 PV2.costModelParamNames

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

instance (Era era, Val (Value era), DecodeNonNegative (Value era)) => Twiddle (AlonzoTxOut era) where
  twiddle = twiddle . toTerm

instance Twiddle SlotNo where
  twiddle = twiddle . toTerm

instance C.Crypto crypto => Twiddle (DCert crypto) where
  twiddle = twiddle . toTerm

instance C.Crypto crypto => Twiddle (Wdrl crypto) where
  twiddle = twiddle . toTerm

instance C.Crypto crypto => Twiddle (AuxiliaryDataHash crypto) where
  twiddle = twiddle . toTerm

emptyOrNothing ::
  forall t a crypto.
  ( Foldable t,
    Twiddle (t Void),
    Monoid (t Void),
    Twiddle (t a)
  ) =>
  AlonzoTxBody (AlonzoEra crypto) ->
  ( AlonzoTxBody (AlonzoEra crypto) ->
    t a
  ) ->
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

instance C.Crypto crypto => Twiddle (Update (AlonzoEra crypto)) where
  twiddle = twiddle . toTerm

instance C.Crypto crypto => Twiddle (MultiAsset crypto) where
  twiddle = twiddle . encodingToTerm . encodeMint

instance C.Crypto crypto => Twiddle (ScriptIntegrityHash crypto) where
  twiddle = twiddle . toTerm

instance (C.Crypto crypto, Typeable t) => Twiddle (KeyHash t crypto) where
  twiddle = twiddle . toTerm

instance Twiddle Network where
  twiddle = twiddle . toTerm

instance C.Crypto c => Twiddle (TxIn c) where
  twiddle = twiddle . toTerm

instance Twiddle Coin where
  twiddle = twiddle . toTerm

instance C.Crypto crypto => Twiddle (AlonzoTxBody (AlonzoEra crypto)) where
  twiddle txBody = do
    inputs' <- twiddle $ inputs txBody
    traceM $ "inputs': " <> (show  . toLazyByteString $ encodeTerm inputs')
    outputs' <- twiddle $ outputs txBody
    fee' <- twiddle $ txfee txBody
    -- Empty collateral can be represented by empty set or the
    -- value can be omitted entirely
    ttl' <- twiddleStrictMaybe . invalidHereafter $ txvldt txBody
    cert' <- emptyOrNothing txBody txcerts
    wdrls' <- twiddle $ txwdrls txBody
    update' <- twiddleStrictMaybe $ txUpdates txBody
    auxDataHash' <- twiddleStrictMaybe $ adHash txBody
    validityStart' <- twiddleStrictMaybe . invalidBefore $ txvldt txBody
    mint' <- twiddle $ mint txBody
    scriptDataHash' <- twiddleStrictMaybe $ scriptIntegrityHash txBody 
    collateral' <- emptyOrNothing txBody collateral
    requiredSigners' <- emptyOrNothing txBody reqSignerHashes
    networkId' <- twiddleStrictMaybe $ txnetworkid txBody
    mp <- elements [TMap, TMapI]
    pure . mp $
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
