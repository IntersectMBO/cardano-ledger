{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.Cddl (
  cddlEraSpec,
  postBabbageCddlSpec,
) where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Binary (Annotator, DecCBOR, EncCBOR)
import Cardano.Ledger.Core
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert (OCert)
import Test.Cardano.Ledger.Binary.Cddl (
  CddlData,
  cddlDecoderEquivalenceSpec,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Common

cddlBlockSpec ::
  forall era c bh bhbody.
  ( Era era
  , Eq (bh c)
  , Show (bh c)
  , DecCBOR (bh c)
  , EncCBOR (bh c)
  , DecCBOR (Annotator (bh c))
  , Eq (bhbody c)
  , Show (bhbody c)
  , DecCBOR (bhbody c)
  , EncCBOR (bhbody c)
  ) =>
  SpecWith CddlData
cddlBlockSpec = do
  let v = eraProtVerLow @era
  cddlRoundTripCborSpec @(bh c) v "header"
  cddlRoundTripAnnCborSpec @(bh c) v "header"
  cddlRoundTripCborSpec @(bhbody c) v "header_body"
  describe "DecCBOR instances equivalence via CDDL" $ do
    cddlDecoderEquivalenceSpec @(bh c) v "header"

-- To be used in Consensus with the appropriate new header types
praosBlockCddlSpec ::
  forall era c bh bhbody.
  ( Era era
  , AtLeastEra BabbageEra era
  , Eq (bh c)
  , Show (bh c)
  , DecCBOR (bh c)
  , EncCBOR (bh c)
  , DecCBOR (Annotator (bh c))
  , Eq (bhbody c)
  , Show (bhbody c)
  , DecCBOR (bhbody c)
  , EncCBOR (bhbody c)
  ) =>
  SpecWith CddlData
praosBlockCddlSpec = do
  let v = eraProtVerLow @era
  cddlBlockSpec @era @c @bh @bhbody
  cddlRoundTripCborSpec @(OCert StandardCrypto) v "operational_cert"
