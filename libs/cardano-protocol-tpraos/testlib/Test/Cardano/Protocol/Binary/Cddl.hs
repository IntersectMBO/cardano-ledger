{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.Cddl (
  huddleBlockSpec,
  praosBlockHuddleSpec,
) where

import Cardano.Ledger.Binary (Annotator, DecCBOR, EncCBOR)
import Cardano.Ledger.Core
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert (OCert)
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common

huddleBlockSpec ::
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
  SpecWith (CTreeRoot MonoReferenced)
huddleBlockSpec = do
  let v = eraProtVerLow @era
  huddleRoundTripAnnCborSpec @(bh c) v "header"
  huddleRoundTripCborSpec @(bh c) v "header"
  huddleRoundTripCborSpec @(bhbody c) v "header_body"
  describe "DecCBOR instances equivalence via CDDL - Huddle" $ do
    huddleDecoderEquivalenceSpec @(bh c) v "header"

-- To be used in Consensus with the appropriate new header types
praosBlockHuddleSpec ::
  forall era c bh bhbody.
  ( Era era
  , AtLeastEra "Babbage" era
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
  SpecWith (CTreeRoot MonoReferenced)
praosBlockHuddleSpec = do
  let v = eraProtVerLow @era
  huddleBlockSpec @era @c @bh @bhbody
  huddleRoundTripCborSpec @(OCert StandardCrypto) v "operational_cert"
  where
    _atLeastBabbage = atLeastEra @"Babbage" @era
