{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary (
  decoderEquivalenceSpec,
  decoderEquivalenceExpectation,
  decoderEquivalenceProp,
) where

import Cardano.Ledger.Binary
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import Test.Cardano.Ledger.Binary.Annotator (decodeFullAnnotator)
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripAnnExpectation)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (label)

-- | Generates arbitrary values, encodes them, and verifies that
-- decoding with `DecCBOR (Annotator)` produces the same result as decoding with `DecCBOR`.
decoderEquivalenceSpec ::
  forall t.
  ( Eq t
  , ToCBOR t
  , DecCBOR (Annotator t)
  , Arbitrary t
  , Show t
  ) =>
  Version ->
  Version ->
  Spec
decoderEquivalenceSpec fromVersion toVersion =
  let lbl = show (typeRep $ Proxy @t)
   in prop lbl (decoderEquivalenceProp @t fromVersion toVersion)

decoderEquivalenceProp ::
  forall t.
  ( Eq t
  , ToCBOR t
  , DecCBOR (Annotator t)
  , Show t
  ) =>
  Version ->
  Version ->
  t ->
  Property
decoderEquivalenceProp fromVersion toVersion t =
  property $
    forM_ [fromVersion .. toVersion] $ \version ->
      embedTripAnnExpectation version version shouldBe t

decoderEquivalenceExpectation ::
  forall t.
  ( Eq t
  , DecCBOR t
  , DecCBOR (Annotator t)
  , Show t
  ) =>
  Version ->
  BSL.ByteString ->
  Expectation
decoderEquivalenceExpectation version bs = do
  let decAnn = decodeFullAnnotator @t version (T.pack (show (typeRep $ Proxy @t))) decCBOR bs
      dec = decodeFull @t version bs
  case (decAnn, dec) of
    -- we only check in case of successful deserialisation,
    -- because some arbitrary instances generate data that fails serialisation for some protocols
    -- (for example, TxDats in Conway)
    (Right _, Right _) -> decAnn `shouldBe` dec
    (Left _, Left _) -> pure ()
    _ ->
      expectationFailure $
        "Decoding result: " ++ show dec ++ " did not match the one via Annotator: " ++ show decAnn
