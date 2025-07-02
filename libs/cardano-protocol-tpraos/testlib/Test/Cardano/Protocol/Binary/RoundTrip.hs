{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.RoundTrip (roundTripBlockSpec) where

import Cardano.Ledger.Binary (Annotator, DecCBOR, EncCBOR)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Data.Typeable
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.Annotator ()
import Test.Cardano.Ledger.Core.Binary.RoundTrip
import Test.Cardano.Protocol.Binary.Annotator ()
import Test.Cardano.Protocol.TPraos.Arbitrary ()

roundTripBlockSpec ::
  forall h era.
  ( Eq h
  , Show h
  , DecCBOR h
  , DecCBOR (Annotator h)
  , EncCBOR h
  , EraBlockBody era
  , Arbitrary (Block h era)
  , DecCBOR (BlockBody era)
  ) =>
  Spec
roundTripBlockSpec =
  prop (show (typeRep $ Proxy @(Block h era))) $ do
    withMaxSuccess 3 $ do
      conjoin
        [ roundTripEraExpectation @era @(Block h era)
        , roundTripAnnEraExpectation @era @(Block h era)
        ]
