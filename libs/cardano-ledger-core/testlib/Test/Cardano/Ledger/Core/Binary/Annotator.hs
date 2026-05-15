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
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

-- leios: DecCBOR (Block h era) instance is now defined in Cardano.Ledger.Block
-- to handle the Body ADT (BodyInline | BodyCertificate).

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
  , Arbitrary (Tx TopTx era)
  , Arbitrary (TxBody TopTx era)
  , Arbitrary (TxWits era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (Script era)
  , HasCallStack
  ) =>
  Spec
decoderEquivalenceCoreEraTypesSpec =
  describe "DecCBOR instances equivalence" $ do
    decoderEquivalenceEraSpec @era @(Data era)
    decoderEquivalenceEraSpec @era @(Script era)
    decoderEquivalenceEraSpec @era @(TxAuxData era)
    decoderEquivalenceEraSpec @era @(TxWits era)
    decoderEquivalenceEraSpec @era @(TxBody TopTx era)
    decoderEquivalenceEraSpec @era @(Tx TopTx era)
