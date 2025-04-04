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
  ShelleyTxWits (
    ShelleyTxWits,
    addrWits,
    bootWits,
    scriptWits,
    ..
  ),
  ShelleyTxWitsRaw (..),
  scriptShelleyTxWitsL,
  addrShelleyTxWitsL,
  bootAddrShelleyTxWitsL,
  shelleyEqTxWitsRaw,

  -- * Re-exports
  WitVKey (..),
)
where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  decodeList,
 )
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain (ToCBOR (..))
import Cardano.Ledger.Core (
  Era,
  EraScript (Script),
  EraTxWits (..),
  ScriptHash,
  hashScript,
 )
import Cardano.Ledger.Hashes (SafeToHash (..))
import Cardano.Ledger.Keys (BootstrapWitness, KeyRole (Witness), WitVKey (..))
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  MemoBytes,
  Memoized (..),
  decodeMemoized,
  getMemoRawType,
  lensMemoRawType,
  mkMemoizedEra,
 )
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
import GHC.Generics (Generic)
import GHC.Records ()
import Lens.Micro (Lens', (^.))
import NoThunks.Class (NoThunks (..))

data ShelleyTxWitsRaw era = ShelleyTxWitsRaw
  { addrWits' :: !(Set (WitVKey 'Witness))
  , scriptWits' :: !(Map ScriptHash (Script era))
  , bootWits' :: !(Set BootstrapWitness)
  }
  deriving (Generic)

deriving instance EraScript era => Show (ShelleyTxWitsRaw era)

deriving instance EraScript era => Eq (ShelleyTxWitsRaw era)

instance
  ( Era era
  , NFData (Script era)
  , NFData (WitVKey 'Witness)
  , NFData BootstrapWitness
  ) =>
  NFData (ShelleyTxWitsRaw era)

instance EraScript era => NoThunks (ShelleyTxWitsRaw era)

newtype ShelleyTxWits era = ShelleyTxWitsConstr (MemoBytes (ShelleyTxWitsRaw era))
  deriving (Generic)
  deriving newtype (SafeToHash, Plain.ToCBOR)

instance Memoized (ShelleyTxWits era) where
  type RawType (ShelleyTxWits era) = ShelleyTxWitsRaw era

deriving newtype instance EraScript era => Eq (ShelleyTxWits era)

deriving newtype instance EraScript era => Show (ShelleyTxWits era)

instance
  ( Era era
  , NFData (Script era)
  , NFData (WitVKey 'Witness)
  , NFData BootstrapWitness
  ) =>
  NFData (ShelleyTxWits era)

instance EraScript era => NoThunks (ShelleyTxWits era)

instance (EraScript era, DecCBOR (Script era)) => DecCBOR (ShelleyTxWits era) where
  decCBOR = ShelleyTxWitsConstr <$> decodeMemoized decCBOR

-- =======================================================
-- Accessors
-- =======================================================

-- | Addresses witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
addrShelleyTxWitsL ::
  EraScript era => Lens' (ShelleyTxWits era) (Set (WitVKey 'Witness))
addrShelleyTxWitsL =
  lensMemoRawType @ShelleyEra addrWits' $ \witsRaw aw -> witsRaw {addrWits' = aw}
{-# INLINEABLE addrShelleyTxWitsL #-}

-- | Bootstrap Addresses witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
bootAddrShelleyTxWitsL ::
  EraScript era =>
  Lens' (ShelleyTxWits era) (Set BootstrapWitness)
bootAddrShelleyTxWitsL =
  lensMemoRawType @ShelleyEra bootWits' $ \witsRaw bw -> witsRaw {bootWits' = bw}
{-# INLINEABLE bootAddrShelleyTxWitsL #-}

-- | Script witness setter and getter for `ShelleyTxWits`. The
-- setter does update memoized binary representation.
scriptShelleyTxWitsL ::
  EraScript era =>
  Lens' (ShelleyTxWits era) (Map ScriptHash (Script era))
scriptShelleyTxWitsL =
  lensMemoRawType @ShelleyEra scriptWits' $
    \witsRaw sw -> witsRaw {scriptWits' = sw}
{-# INLINEABLE scriptShelleyTxWitsL #-}

instance EraTxWits ShelleyEra where
  type TxWits ShelleyEra = ShelleyTxWits ShelleyEra

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
  Set (WitVKey 'Witness) ->
  Map ScriptHash (Script era) ->
  Set BootstrapWitness ->
  ShelleyTxWits era
pattern ShelleyTxWits {addrWits, scriptWits, bootWits} <-
  (getMemoRawType -> ShelleyTxWitsRaw addrWits scriptWits bootWits)
  where
    ShelleyTxWits awits scriptWitMap bootstrapWits =
      mkMemoizedEra @era $ ShelleyTxWitsRaw awits scriptWitMap bootstrapWits

{-# COMPLETE ShelleyTxWits #-}

shelleyEqTxWitsRaw :: EraTxWits era => TxWits era -> TxWits era -> Bool
shelleyEqTxWitsRaw txWits1 txWits2 =
  txWits1 ^. addrTxWitsL == txWits2 ^. addrTxWitsL
    && liftEq eqRaw (txWits1 ^. scriptTxWitsL) (txWits2 ^. scriptTxWitsL)
    && txWits1 ^. bootAddrTxWitsL == txWits2 ^. bootAddrTxWitsL

instance (EraScript era, DecCBOR (Script era)) => DecCBOR (ShelleyTxWitsRaw era) where
  decCBOR =
    decode $
      SparseKeyed
        "ShelleyTxWits"
        (ShelleyTxWitsRaw mempty mempty mempty)
        witField
        []
    where
      witField :: Word -> Field (ShelleyTxWitsRaw era)
      witField 0 = field (\x wits -> wits {addrWits' = x}) From
      witField 1 =
        field
          (\x wits -> wits {scriptWits' = x})
          (D $ Map.fromElems (hashScript @era) <$> decodeList decCBOR)
      witField 2 = field (\x wits -> wits {bootWits' = x}) From
      witField n = invalidField n
