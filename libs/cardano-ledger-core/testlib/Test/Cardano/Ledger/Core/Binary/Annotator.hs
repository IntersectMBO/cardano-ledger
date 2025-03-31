{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Core.Binary.Annotator (
  decoderEquivalenceEraSpec,
  decoderEquivalenceCoreEraTypesSpec,
  Mem,
  module Test.Cardano.Ledger.Binary.Annotator,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Block
import Cardano.Ledger.Core
import Cardano.Ledger.Keys
import Cardano.Ledger.MemoBytes (MemoBytes, mkMemoBytes)
import Cardano.Ledger.Plutus
import Data.Typeable
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Binary.Annotator
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

-- | Useful when deriving DecCBOR(Annotator T)
-- deriving via (Mem T) instance DecCBOR (Annotator T)
type Mem t = Annotator (MemoBytes t)

instance
  (Typeable t, DecCBOR (Annotator t)) =>
  DecCBOR (Annotator (MemoBytes t))
  where
  decCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice decCBOR
    pure (Annotator (\fullbytes -> mkMemoBytes (getT fullbytes) (getBytes fullbytes)))

instance DecCBOR (Annotator BootstrapWitness) where
  decCBOR = pure <$> decCBOR

instance Typeable kr => DecCBOR (Annotator (WitVKey kr)) where
  decCBOR = pure <$> decCBOR

instance
  ( EraSegWits era
  , DecCBOR (Annotator h)
  , DecCBOR (Annotator (TxSeq era))
  , Typeable h
  ) =>
  DecCBOR (Annotator (Block h era))
  where
  decCBOR = decodeRecordNamed "Block" (const blockSize) $ do
    header <- decCBOR
    txns <- decCBOR
    pure $ Block <$> header <*> txns
    where
      blockSize = 1 + fromIntegral (numSegComponents @era)

instance DecCBOR (Annotator PlutusBinary) where
  decCBOR = pure <$> decCBOR

instance Typeable era => DecCBOR (Annotator (PlutusData era)) where
  decCBOR = pure <$> decCBOR

instance Era era => DecCBOR (Annotator (Data era)) where
  decCBOR = fmap DataConstr <$> decCBOR

decoderEquivalenceEraSpec ::
  forall era t.
  ( Era era
  , Eq t
  , ToCBOR t
  , DecCBOR (Annotator t)
  , Arbitrary t
  , Show t
  ) =>
  Spec
decoderEquivalenceEraSpec = decoderEquivalenceSpec @t (eraProtVerLow @era) (eraProtVerHigh @era)

decoderEquivalenceCoreEraTypesSpec ::
  forall era.
  ( EraTx era
  , Arbitrary (Tx era)
  , Arbitrary (TxBody era)
  , Arbitrary (TxWits era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (Script era)
  , DecCBOR (Annotator (Tx era))
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (Script era))
  , HasCallStack
  ) =>
  Spec
decoderEquivalenceCoreEraTypesSpec =
  describe "DecCBOR instances equivalence" $ do
    decoderEquivalenceEraSpec @era @(Data era)
    decoderEquivalenceEraSpec @era @(Script era)
    decoderEquivalenceEraSpec @era @(TxAuxData era)
    decoderEquivalenceEraSpec @era @(TxWits era)
    decoderEquivalenceEraSpec @era @(TxBody era)
    decoderEquivalenceEraSpec @era @(Tx era)
