{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxWits (
  keyBy,
  decodeWits,
  ShelleyTxWits (
    ShelleyTxWits,
    addrWits,
    bootWits,
    scriptWits
  ),
  ShelleyTxWitsRaw,
  scriptShelleyTxWitsL,
  addrShelleyTxWitsL,
  bootAddrShelleyTxWitsL,
  addrWits',
  shelleyEqTxWitsRaw,
  mapTraverseableDecoderA,

  -- * Re-exports
  WitVKey (..),
)
where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  Decoder,
  EncCBOR (encCBOR),
  decodeList,
 )
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain (ToCBOR (..))
import Cardano.Ledger.Core (
  Era (EraCrypto),
  EraScript (Script),
  EraTxWits (..),
  ScriptHash,
  hashScript,
 )
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..), witVKeyHash)
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  Memoized (..),
  getMemoRawType,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts ()
import Cardano.Ledger.Shelley.TxAuxData ()
import Control.DeepSeq (NFData)
import Data.Functor.Classes (Eq1 (liftEq))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map (fromElems)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records ()
import Lens.Micro (Lens', (^.))
import NoThunks.Class (NoThunks (..))

data ShelleyTxWitsRaw era = ShelleyTxWitsRaw
  { addrWits' :: !(Set (WitVKey 'Witness (EraCrypto era)))
  , scriptWits' :: !(Map (ScriptHash (EraCrypto era)) (Script era))
  , bootWits' :: !(Set (BootstrapWitness (EraCrypto era)))
  }
  deriving (Generic)

deriving instance EraScript era => Show (ShelleyTxWitsRaw era)

deriving instance EraScript era => Eq (ShelleyTxWitsRaw era)

instance
  ( Era era
  , NFData (Script era)
  , NFData (WitVKey 'Witness (EraCrypto era))
  , NFData (BootstrapWitness (EraCrypto era))
  ) =>
  NFData (ShelleyTxWitsRaw era)

instance EraScript era => NoThunks (ShelleyTxWitsRaw era)

newtype ShelleyTxWits era = ShelleyTxWitsConstr (MemoBytes ShelleyTxWitsRaw era)
  deriving (Generic)
  deriving newtype (SafeToHash, Plain.ToCBOR)

instance Memoized ShelleyTxWits where
  type RawType ShelleyTxWits = ShelleyTxWitsRaw

deriving newtype instance EraScript era => Eq (ShelleyTxWits era)

deriving newtype instance EraScript era => Show (ShelleyTxWits era)

instance
  ( Era era
  , NFData (Script era)
  , NFData (WitVKey 'Witness (EraCrypto era))
  , NFData (BootstrapWitness (EraCrypto era))
  ) =>
  NFData (ShelleyTxWits era)

instance EraScript era => NoThunks (ShelleyTxWits era)

-- =======================================================
-- Accessors
-- =======================================================

-- | Addresses witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
addrShelleyTxWitsL ::
  EraScript era => Lens' (ShelleyTxWits era) (Set (WitVKey 'Witness (EraCrypto era)))
addrShelleyTxWitsL =
  lensMemoRawType addrWits' $ \witsRaw aw -> witsRaw {addrWits' = aw}
{-# INLINEABLE addrShelleyTxWitsL #-}

-- | Bootstrap Addresses witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
bootAddrShelleyTxWitsL ::
  EraScript era =>
  Lens' (ShelleyTxWits era) (Set (BootstrapWitness (EraCrypto era)))
bootAddrShelleyTxWitsL =
  lensMemoRawType bootWits' $ \witsRaw bw -> witsRaw {bootWits' = bw}
{-# INLINEABLE bootAddrShelleyTxWitsL #-}

-- | Script witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
scriptShelleyTxWitsL ::
  EraScript era =>
  Lens' (ShelleyTxWits era) (Map (ScriptHash (EraCrypto era)) (Script era))
scriptShelleyTxWitsL =
  lensMemoRawType scriptWits' $ \witsRaw sw -> witsRaw {scriptWits' = sw}
{-# INLINEABLE scriptShelleyTxWitsL #-}

instance Crypto c => EraTxWits (ShelleyEra c) where
  {-# SPECIALIZE instance EraTxWits (ShelleyEra StandardCrypto) #-}

  type TxWits (ShelleyEra c) = ShelleyTxWits (ShelleyEra c)

  mkBasicTxWits = mempty

  addrTxWitsL = addrShelleyTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrShelleyTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptShelleyTxWitsL
  {-# INLINE scriptTxWitsL #-}

  upgradeTxWits =
    error
      "Calling this function will cause a compilation error, since there is no TxWits instance for ByronEra"

instance (TxWits era ~ ShelleyTxWits era, EraTxWits era) => EqRaw (ShelleyTxWits era) where
  eqRaw = shelleyEqTxWitsRaw

instance (Era era, EncCBOR (Script era)) => EncCBOR (ShelleyTxWitsRaw era) where
  encCBOR (ShelleyTxWitsRaw vkeys scripts boots) =
    encode $
      Keyed ShelleyTxWitsRaw
        !> Omit null (Key 0 $ To vkeys)
        !> Omit null (Key 1 $ E (encCBOR . Map.elems) scripts)
        !> Omit null (Key 2 $ To boots)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (ShelleyTxWits era)

instance EraScript era => Semigroup (ShelleyTxWits era) where
  (ShelleyTxWits a b c) <> y | Set.null a && Map.null b && Set.null c = y
  y <> (ShelleyTxWits a b c) | Set.null a && Map.null b && Set.null c = y
  (ShelleyTxWits a b c) <> (ShelleyTxWits a' b' c') = ShelleyTxWits (a <> a') (b <> b') (c <> c')

instance EraScript era => Monoid (ShelleyTxWits era) where
  mempty = ShelleyTxWits mempty mempty mempty

pattern ShelleyTxWits ::
  forall era.
  EraScript era =>
  Set (WitVKey 'Witness (EraCrypto era)) ->
  Map (ScriptHash (EraCrypto era)) (Script era) ->
  Set (BootstrapWitness (EraCrypto era)) ->
  ShelleyTxWits era
pattern ShelleyTxWits {addrWits, scriptWits, bootWits} <-
  (getMemoRawType -> ShelleyTxWitsRaw addrWits scriptWits bootWits)
  where
    ShelleyTxWits awits scriptWitMap bootstrapWits =
      mkMemoized $ ShelleyTxWitsRaw awits scriptWitMap bootstrapWits

{-# COMPLETE ShelleyTxWits #-}

shelleyEqTxWitsRaw :: EraTxWits era => TxWits era -> TxWits era -> Bool
shelleyEqTxWitsRaw txWits1 txWits2 =
  liftEq eqRaw (txWits1 ^. addrTxWitsL) (txWits2 ^. addrTxWitsL)
    && liftEq eqRaw (txWits1 ^. scriptTxWitsL) (txWits2 ^. scriptTxWitsL)
    && liftEq eqRaw (txWits1 ^. bootAddrTxWitsL) (txWits2 ^. bootAddrTxWitsL)

instance EraScript era => DecCBOR (Annotator (ShelleyTxWitsRaw era)) where
  decCBOR = decodeWits

deriving via
  (Mem ShelleyTxWitsRaw era)
  instance
    EraScript era => DecCBOR (Annotator (ShelleyTxWits era))

-- | This type is only used to preserve the old buggy behavior where signature
-- was ignored in the `Ord` instance for `WitVKey`s.
newtype IgnoreSigOrd kr c = IgnoreSigOrd {unIgnoreSigOrd :: WitVKey kr c}
  deriving (Eq)

instance (Typeable kr, Crypto c) => Ord (IgnoreSigOrd kr c) where
  compare (IgnoreSigOrd w1) (IgnoreSigOrd w2) = compare (witVKeyHash w1) (witVKeyHash w2)

decodeWits ::
  forall era s.
  EraScript era =>
  Decoder s (Annotator (ShelleyTxWitsRaw era))
decodeWits =
  decode $
    SparseKeyed
      "ShelleyTxWitsRaw"
      (pure emptyWitnessSet)
      witField
      []
  where
    emptyWitnessSet = ShelleyTxWitsRaw mempty mempty mempty
    witField :: Word -> Field (Annotator (ShelleyTxWitsRaw era))
    witField 0 =
      fieldAA
        (\x wits -> wits {addrWits' = x})
        ( D $
            mapTraverseableDecoderA
              (decodeList decCBOR)
              (Set.map unIgnoreSigOrd . Set.fromList . fmap IgnoreSigOrd)
        )
    witField 1 =
      fieldAA
        (\x wits -> wits {scriptWits' = x})
        ( D $
            mapTraverseableDecoderA
              (decodeList decCBOR)
              (Map.fromElems (hashScript @era))
        )
    witField 2 =
      fieldAA
        (\x wits -> wits {bootWits' = x})
        (D $ mapTraverseableDecoderA (decodeList decCBOR) Set.fromList)
    witField n = fieldAA (\_ wits -> wits) (Invalid n)

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs
{-# DEPRECATED keyBy "In favor of `Data.MapExtras.fromElems`" #-}

mapTraverseableDecoderA ::
  Traversable f =>
  Decoder s (f (Annotator a)) ->
  (f a -> m b) ->
  Decoder s (Annotator (m b))
mapTraverseableDecoderA decList transformList = fmap transformList . sequence <$> decList
