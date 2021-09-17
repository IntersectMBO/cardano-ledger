{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Generators.Bootstrap
  ( genBootstrapAddress,
    genSignature,
  )
where

import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Ledger.Address
  ( BootstrapAddress (..),
  )
import Data.Maybe (fromJust)
import qualified Test.Cardano.Chain.Common.Gen as Byron
import Test.Cardano.Prelude (genBytes)
import Test.QuickCheck (Gen)
import Test.QuickCheck.Hedgehog (hedgehog)

genSignature :: forall a b. (DSIGN.DSIGNAlgorithm a) => Gen (DSIGN.SignedDSIGN a b)
genSignature =
  DSIGN.SignedDSIGN
    . fromJust
    . DSIGN.rawDeserialiseSigDSIGN
    <$> hedgehog (genBytes . fromIntegral $ DSIGN.sizeSigDSIGN ([] @a))

genBootstrapAddress :: Gen (BootstrapAddress crypto)
genBootstrapAddress = BootstrapAddress <$> hedgehog Byron.genAddress
