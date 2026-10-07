{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Dijkstra.Foreign (spec) where

import Cardano.Crypto.Util (bytesToNatural)
import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (..))
import Cardano.Ledger.Binary (FixedSizeCodec (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState, esSnapshotsL, nesEsL)
import Cardano.Ledger.State (
  BlsKey (..),
  LeiosCommittee (..),
  LeiosSeat (..),
  ssLeiosCommitteeL,
  ssStakeSetL,
 )
import Data.Default (def)
import Data.Either (isLeft)
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~))
import qualified MAlonzo.Code.Ledger.Core.Foreign.API as Agda
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Dijkstra
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance (SpecTranslate (..), externalFunctions, runSpecTransM)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra ()
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = describe "Foreign interface premises" $ do
  prop "accepts an independently generated matching proof" $ \(BlsKey key proof) ->
    Agda.extIsValidPoP externalFunctions (encodeInteger key) (encodeInteger proof) === True
  prop "rejects a proof for a different verification key" $ \(BlsKey key _) (BlsKey otherKey proof) ->
    key
      /= otherKey
        ==> Agda.extIsValidPoP externalFunctions (encodeInteger key) (encodeInteger proof)
        === False
  it "rejects malformed natural-number encodings" $ do
    Agda.extIsValidPoP externalFunctions (-1) 0 `shouldBe` False
    Agda.extIsValidPoP externalFunctions 0 (-1) `shouldBe` False
    Agda.extIsValidPoP externalFunctions 0 0 `shouldBe` False
    Agda.extIsValidPoP externalFunctions (256 ^ (96 :: Int)) 0 `shouldBe` False
    Agda.extIsValidPoP externalFunctions 0 (256 ^ (48 :: Int)) `shouldBe` False

  describe "Leios committee representation boundary" $ do
    it "translates an empty committee" $
      case runSpecTransM Testnet (toSpecRep @DijkstraEra (def :: NewEpochState DijkstraEra)) of
        Left err -> expectationFailure (show err)
        Right model -> Dijkstra.nesLeiosCommittee model `shouldBe` []
    it "rejects a committee whose pool IDs are absent rather than inventing seat identity" $ do
      let state =
            (def :: NewEpochState DijkstraEra)
              & nesEsL
                . esSnapshotsL
                . ssStakeSetL
                . ssLeiosCommitteeL
                .~ UnsafeLeiosCommittee (fromList [LeiosSeat 0 SNothing])
      runSpecTransM Testnet (toSpecRep @DijkstraEra state) `shouldSatisfy` isLeft

encodeInteger :: FixedSizeCodec a => a -> Integer
encodeInteger = toInteger . bytesToNatural . rawEncodeFixedSized
