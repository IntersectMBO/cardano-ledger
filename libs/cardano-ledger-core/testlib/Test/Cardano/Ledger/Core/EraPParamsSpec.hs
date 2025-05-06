{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Core.EraPParamsSpec (spec) where

import Cardano.Ledger.Core
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec ::
  forall era.
  ( EraPParams era
  , Arbitrary (PParams era)
  , Arbitrary (PParamsUpdate era)
  , HasCallStack
  ) =>
  Spec
spec = describe "EraPParams" $ do
  prop "PParams JSON equivalence" $ \pp -> do
    let js1 = Aeson.toJSON pp
    let js2 = Aeson.object . jsonPairsPParams @era $ pp
    js1 `shouldBe` js2

    let decoded = Aeson.parseMaybe (fromJsonPParams @era) js1
    Just pp `shouldBe` decoded

  prop "PParamsUpdate JSON equivalence" $ \ppu -> do
    let js1 = Aeson.toJSON ppu
    let js2 = Aeson.object . jsonPairsPParamsUpdate @era $ ppu

    js1 `shouldBe` js2
