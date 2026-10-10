{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.Arbitrary (
  genVersion,
) where

import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import Cardano.Crypto.Leios (LeiosCert, LeiosCommittee (..), LeiosSeat (..))
import Cardano.Ledger.Binary.Version
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import GHC.Stack
import Test.Cardano.Base.Arbitrary ()
import Test.Cardano.Binary.Arbitrary ()
import Test.Cardano.Crypto.Leios.Gen (genLeiosCert, genLeiosSigningKey)
import Test.Data.VMap.Arbitrary ()
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary Version where
  arbitrary = genVersion minBound maxBound

genVersion :: HasCallStack => Version -> Version -> Gen Version
genVersion minVersion maxVersion =
  genVersion32 (getVersion32 minVersion) (getVersion32 maxVersion)
  where
    genVersion32 minVersion32 maxVersion32 = do
      v32 <- choose (minVersion32, maxVersion32)
      case mkVersion32 v32 of
        Nothing -> error $ "Impossible: Invalid version generated: " ++ show v32
        Just v -> pure v

instance Arbitrary LeiosCert where
  arbitrary = genLeiosCert

instance Arbitrary LeiosSeat where
  arbitrary = do
    weight <- arbitrary
    vkey <-
      oneof
        [ SJust . deriveVerKeyDSIGN <$> genLeiosSigningKey
        , pure SNothing
        ]
    pure (LeiosSeat weight vkey)

instance Arbitrary LeiosCommittee where
  arbitrary = UnsafeLeiosCommittee . V.fromList <$> arbitrary
