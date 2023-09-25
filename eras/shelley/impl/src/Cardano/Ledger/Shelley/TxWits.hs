{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
  WitnessSetHKD (
    txWitsBytes
  ),
  scriptShelleyTxWitsL,
  addrShelleyTxWitsL,
  bootAddrShelleyTxWitsL,
  addrWits',
  prettyWitnessSetParts,
  shelleyEqTxWitsRaw,

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
  decodeMapContents,
  decodeWord,
  encodeMapLen,
  encodeWord,
  invalidKey,
  serialize,
  withSlice,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain (ToCBOR (..), encodePreEncoded)
import Cardano.Ledger.Core (
  Era (EraCrypto),
  EraScript (Script, hashScript),
  EraTxWits (..),
  ScriptHash,
  eraProtVerLow,
 )
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..), witVKeyHash)
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts ()
import Cardano.Ledger.Shelley.TxAuxData ()
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold)
import Data.Functor.Classes (Eq1 (liftEq))
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records ()
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

data WitnessSetHKD f era = WitnessSet'
  { addrWits' :: !(HKD f (Set (WitVKey 'Witness (EraCrypto era))))
  , scriptWits' :: !(HKD f (Map (ScriptHash (EraCrypto era)) (Script era)))
  , bootWits' :: !(HKD f (Set (BootstrapWitness (EraCrypto era))))
  , txWitsBytes :: BSL.ByteString
  }

deriving instance EraScript era => Show (WitnessSetHKD Identity era)

deriving instance EraScript era => Eq (WitnessSetHKD Identity era)

deriving instance Era era => Generic (WitnessSetHKD Identity era)

instance
  ( Era era
  , NFData (Script era)
  , NFData (WitVKey 'Witness (EraCrypto era))
  , NFData (BootstrapWitness (EraCrypto era))
  ) =>
  NFData (WitnessSetHKD Identity era)

deriving via
  AllowThunksIn
    '[ "txWitsBytes"
     ]
    (WitnessSetHKD Identity era)
  instance
    (Era era, NoThunks (Script era)) =>
    (NoThunks (WitnessSetHKD Identity era))

newtype ShelleyTxWits era = ShelleyTxWitsConstr (WitnessSetHKD Identity era)

deriving newtype instance EraScript era => Eq (ShelleyTxWits era)

deriving newtype instance EraScript era => Show (ShelleyTxWits era)

deriving newtype instance Era era => Generic (ShelleyTxWits era)

instance (Era era, ToExpr (Script era)) => ToExpr (ShelleyTxWits era)

instance
  ( Era era
  , NFData (Script era)
  , NFData (WitVKey 'Witness (EraCrypto era))
  , NFData (BootstrapWitness (EraCrypto era))
  ) =>
  NFData (ShelleyTxWits era)

instance (Era era, NoThunks (Script era)) => NoThunks (ShelleyTxWits era)

-- =======================================================
-- Accessors
-- =======================================================

-- | Addresses witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
addrShelleyTxWitsL ::
  EraScript era => Lens' (ShelleyTxWits era) (Set (WitVKey 'Witness (EraCrypto era)))
addrShelleyTxWitsL =
  lens
    (\(ShelleyTxWitsConstr w) -> addrWits' w)
    (\w s -> w {addrWits = s})
{-# INLINEABLE addrShelleyTxWitsL #-}

-- | Bootstrap Addresses witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
bootAddrShelleyTxWitsL ::
  EraScript era =>
  Lens' (ShelleyTxWits era) (Set (BootstrapWitness (EraCrypto era)))
bootAddrShelleyTxWitsL =
  lens
    (\(ShelleyTxWitsConstr w) -> bootWits' w)
    (\w s -> w {bootWits = s})
{-# INLINEABLE bootAddrShelleyTxWitsL #-}

-- | Script witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
scriptShelleyTxWitsL ::
  EraScript era =>
  Lens' (ShelleyTxWits era) (Map (ScriptHash (EraCrypto era)) (Script era))
scriptShelleyTxWitsL =
  lens
    (\(ShelleyTxWitsConstr w) -> scriptWits' w)
    (\w s -> w {scriptWits = s})
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

  upgradeTxWits = error "Calling this function will cause a compilation error, since there is no TxWits instance for ByronEra"

instance (TxWits era ~ ShelleyTxWits era, EraTxWits era) => EqRaw (ShelleyTxWits era) where
  eqRaw = shelleyEqTxWitsRaw

instance Era era => Plain.ToCBOR (ShelleyTxWits era) where
  toCBOR (ShelleyTxWitsConstr w) = Plain.encodePreEncoded $ BSL.toStrict $ txWitsBytes w

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
  ShelleyTxWitsConstr (WitnessSet' addrWits scriptWits bootWits _)
  where
    ShelleyTxWits awits scriptWitMap bootstrapWits =
      let encodeIndexedMaybe ix x =
            if null x then Nothing else Just $ encodeWord ix <> encCBOR x
          l =
            catMaybes
              [ encodeIndexedMaybe 0 awits
              , encodeIndexedMaybe 1 (Map.elems scriptWitMap)
              , encodeIndexedMaybe 2 bootstrapWits
              ]
          n = fromIntegral $ length l
          witsBytes = serialize (eraProtVerLow @era) $ encodeMapLen n <> fold l
       in ShelleyTxWitsConstr
            ( WitnessSet'
                { addrWits' = awits
                , scriptWits' = scriptWitMap
                , bootWits' = bootstrapWits
                , txWitsBytes = witsBytes
                }
            )

{-# COMPLETE ShelleyTxWits #-}

shelleyEqTxWitsRaw :: EraTxWits era => TxWits era -> TxWits era -> Bool
shelleyEqTxWitsRaw txWits1 txWits2 =
  liftEq eqRaw (txWits1 ^. addrTxWitsL) (txWits2 ^. addrTxWitsL)
    && liftEq eqRaw (txWits1 ^. scriptTxWitsL) (txWits2 ^. scriptTxWitsL)
    && liftEq eqRaw (txWits1 ^. bootAddrTxWitsL) (txWits2 ^. bootAddrTxWitsL)

instance SafeToHash (ShelleyTxWits era) where
  originalBytes (ShelleyTxWitsConstr w) = BSL.toStrict $ txWitsBytes w

-- | Exports the relevant parts from a (WintessSetHKD Identity era) for
--     use by the pretty printer without all the horrible constraints.
--     Uses the non-exported WitnessSet' constructor.
prettyWitnessSetParts ::
  ShelleyTxWits era ->
  ( Set (WitVKey 'Witness (EraCrypto era))
  , Map (ScriptHash (EraCrypto era)) (Script era)
  , Set (BootstrapWitness (EraCrypto era))
  )
prettyWitnessSetParts (ShelleyTxWitsConstr (WitnessSet' a b c _)) = (a, b, c)

instance EraScript era => DecCBOR (Annotator (ShelleyTxWits era)) where
  decCBOR = decodeWits

-- | This type is only used to preserve the old buggy behavior where signature
-- was ignored in the `Ord` instance for `WitVKey`s.
newtype IgnoreSigOrd kr c = IgnoreSigOrd {unIgnoreSigOrd :: WitVKey kr c}
  deriving (Eq)

instance (Typeable kr, Crypto c) => Ord (IgnoreSigOrd kr c) where
  compare (IgnoreSigOrd w1) (IgnoreSigOrd w2) = compare (witVKeyHash w1) (witVKeyHash w2)

decodeWits :: forall era s. EraScript era => Decoder s (Annotator (ShelleyTxWits era))
decodeWits = do
  (mapParts, annBytes) <-
    withSlice $
      decodeMapContents $
        decodeWord >>= \case
          0 ->
            decodeList decCBOR >>= \x ->
              pure
                ( \ws ->
                    ws
                      { addrWits' =
                          Set.map unIgnoreSigOrd . Set.fromList . fmap IgnoreSigOrd <$> sequence x
                      }
                )
          1 ->
            decodeList decCBOR >>= \x ->
              pure (\ws -> ws {scriptWits' = keyBy (hashScript @era) <$> sequence x})
          2 ->
            decodeList decCBOR >>= \x ->
              pure (\ws -> ws {bootWits' = Set.fromList <$> sequence x})
          k -> invalidKey k
  let witSet = foldr ($) emptyWitnessSetHKD mapParts
      emptyWitnessSetHKD :: WitnessSetHKD Annotator era
      emptyWitnessSetHKD =
        WitnessSet'
          { addrWits' = pure mempty
          , scriptWits' = pure mempty
          , bootWits' = pure mempty
          , txWitsBytes = mempty
          }
  pure $ ShelleyTxWitsConstr <$> (WitnessSet' <$> addrWits' witSet <*> scriptWits' witSet <*> bootWits' witSet <*> annBytes)

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs
