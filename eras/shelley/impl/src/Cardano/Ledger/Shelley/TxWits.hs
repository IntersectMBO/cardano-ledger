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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxWits
  ( decodeWits,
    ShelleyTxWits,
    WitnessSetHKD
      ( ShelleyTxWits,
        addrWits,
        bootWits,
        scriptWits,
        txWitsBytes
      ),
    scriptShelleyWitsL,
    addrShelleyWitsL,
    bootAddrShelleyWitsL,
    addrWits',
    prettyWitnessSetParts,

    -- * Re-exports
    WitVKey (..),
  )
where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    decodeWord,
    encodeMapLen,
    encodePreEncoded,
    encodeWord,
    serializeEncoding,
    withSlice,
  )
import Cardano.Ledger.Core
  ( Era (EraCrypto),
    EraScript (Script, hashScript),
    EraTxWits (..),
    ScriptHash,
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto, StandardCrypto)
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..), witVKeyHash)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Metadata ()
import Cardano.Ledger.Shelley.Scripts ()
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import Data.Coders
  ( Annotator,
    Decoder,
    decodeList,
    decodeMapContents,
    encodeFoldable,
    invalidKey,
  )
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records ()
import Lens.Micro (Lens', lens)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

data WitnessSetHKD f era = WitnessSet'
  { addrWits' :: !(HKD f (Set (WitVKey 'Witness (EraCrypto era)))),
    scriptWits' :: !(HKD f (Map (ScriptHash (EraCrypto era)) (Script era))),
    bootWits' :: !(HKD f (Set (BootstrapWitness (EraCrypto era)))),
    txWitsBytes :: BSL.ByteString
  }

deriving instance EraScript era => Show (WitnessSetHKD Identity era)

deriving instance EraScript era => Eq (WitnessSetHKD Identity era)

deriving instance Era era => Generic (WitnessSetHKD Identity era)

instance
  ( Era era,
    NFData (Script era),
    NFData (WitVKey 'Witness (EraCrypto era)),
    NFData (BootstrapWitness (EraCrypto era))
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

type ShelleyTxWits = WitnessSetHKD Identity

-- | Addresses witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
addrShelleyWitsL ::
  EraTxWits era => Lens' (ShelleyTxWits era) (Set (WitVKey 'Witness (EraCrypto era)))
addrShelleyWitsL = lens addrWits' (\w s -> w {addrWits = s})
{-# INLINEABLE addrShelleyWitsL #-}

-- | Bootstrap Addresses witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
bootAddrShelleyWitsL ::
  EraTxWits era => Lens' (ShelleyTxWits era) (Set (BootstrapWitness (EraCrypto era)))
bootAddrShelleyWitsL = lens bootWits' (\w s -> w {bootWits = s})
{-# INLINEABLE bootAddrShelleyWitsL #-}

-- | Script witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
scriptShelleyWitsL ::
  EraTxWits era => Lens' (ShelleyTxWits era) (Map (ScriptHash (EraCrypto era)) (Script era))
scriptShelleyWitsL = lens scriptWits' (\w s -> w {scriptWits = s})
{-# INLINEABLE scriptShelleyWitsL #-}

instance CC.Crypto crypto => EraTxWits (ShelleyEra crypto) where
  {-# SPECIALIZE instance EraTxWits (ShelleyEra CC.StandardCrypto) #-}

  type TxWits (ShelleyEra crypto) = ShelleyTxWits (ShelleyEra crypto)

  mkBasicTxWits = mempty

  addrWitsL = addrShelleyWitsL
  {-# INLINE addrWitsL #-}

  bootAddrWitsL = bootAddrShelleyWitsL
  {-# INLINE bootAddrWitsL #-}

  scriptWitsL = scriptShelleyWitsL
  {-# INLINE scriptWitsL #-}

instance Era era => ToCBOR (WitnessSetHKD Identity era) where
  toCBOR = encodePreEncoded . BSL.toStrict . txWitsBytes

instance EraScript era => Semigroup (WitnessSetHKD Identity era) where
  (WitnessSet' a b c _) <> y | Set.null a && Map.null b && Set.null c = y
  y <> (WitnessSet' a b c _) | Set.null a && Map.null b && Set.null c = y
  (ShelleyTxWits a b c) <> (ShelleyTxWits a' b' c') =
    ShelleyTxWits (a <> a') (b <> b') (c <> c')

instance EraScript era => Monoid (WitnessSetHKD Identity era) where
  mempty = ShelleyTxWits mempty mempty mempty

pattern ShelleyTxWits ::
  EraScript era =>
  Set (WitVKey 'Witness (EraCrypto era)) ->
  Map (ScriptHash (EraCrypto era)) (Script era) ->
  Set (BootstrapWitness (EraCrypto era)) ->
  ShelleyTxWits era
pattern ShelleyTxWits {addrWits, scriptWits, bootWits} <-
  WitnessSet' addrWits scriptWits bootWits _
  where
    ShelleyTxWits awits scriptWitMap bootstrapWits =
      let encodeMapElement ix enc x =
            if null x then Nothing else Just (encodeWord ix <> enc x)
          l =
            catMaybes
              [ encodeMapElement 0 encodeFoldable awits,
                encodeMapElement 1 encodeFoldable scriptWitMap,
                encodeMapElement 2 encodeFoldable bootstrapWits
              ]
          n = fromIntegral $ length l
          witsBytes = serializeEncoding $ encodeMapLen n <> fold l
       in WitnessSet'
            { addrWits' = awits,
              scriptWits' = scriptWitMap,
              bootWits' = bootstrapWits,
              txWitsBytes = witsBytes
            }

{-# COMPLETE ShelleyTxWits #-}

instance SafeToHash (WitnessSetHKD Identity era) where
  originalBytes = BSL.toStrict . txWitsBytes

-- | Exports the relevant parts from a (WintessSetHKD Identity era) for
--     use by the pretty printer without all the horrible constraints.
--     Uses the non-exported WitnessSet' constructor.
prettyWitnessSetParts ::
  WitnessSetHKD Identity era ->
  ( Set (WitVKey 'Witness (EraCrypto era)),
    Map (ScriptHash (EraCrypto era)) (Core.Script era),
    Set (BootstrapWitness (EraCrypto era))
  )
prettyWitnessSetParts (WitnessSet' a b c _) = (a, b, c)

instance EraScript era => FromCBOR (Annotator (WitnessSetHKD Identity era)) where
  fromCBOR = decodeWits

-- | This type is only used to preserve the old buggy behavior where signature
-- was ignored in the `Ord` instance for `WitVKey`s.
newtype IgnoreSigOrd kr crypto = IgnoreSigOrd {unIgnoreSigOrd :: WitVKey kr crypto}
  deriving (Eq)

instance (Typeable kr, CC.Crypto crypto) => Ord (IgnoreSigOrd kr crypto) where
  compare (IgnoreSigOrd w1) (IgnoreSigOrd w2) = compare (witVKeyHash w1) (witVKeyHash w2)

decodeWits :: forall era s. EraScript era => Decoder s (Annotator (ShelleyTxWits era))
decodeWits = do
  (mapParts, annBytes) <-
    withSlice $
      decodeMapContents $
        decodeWord >>= \case
          0 ->
            decodeList fromCBOR >>= \x ->
              pure
                ( \ws ->
                    ws
                      { addrWits' =
                          Set.map unIgnoreSigOrd . Set.fromList . fmap IgnoreSigOrd <$> sequence x
                      }
                )
          1 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {scriptWits' = keyBy (hashScript @era) <$> sequence x})
          2 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {bootWits' = Set.fromList <$> sequence x})
          k -> invalidKey k
  let witSet = foldr ($) emptyWitnessSetHKD mapParts
      emptyWitnessSetHKD :: WitnessSetHKD Annotator era
      emptyWitnessSetHKD =
        WitnessSet'
          { addrWits' = pure mempty,
            scriptWits' = pure mempty,
            bootWits' = pure mempty,
            txWitsBytes = mempty
          }
  pure $
    WitnessSet'
      <$> addrWits' witSet
      <*> scriptWits' witSet
      <*> bootWits' witSet
      <*> annBytes

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs
