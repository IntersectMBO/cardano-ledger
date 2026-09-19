{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.State.LeiosCommitteeSpec (spec) where

import Cardano.Crypto.DSIGN (createPossessionProofDSIGN, deriveVerKeyDSIGN, genKeyDSIGN)
import Cardano.Crypto.DSIGN.BLS12381.Internal (minSigPoPDST)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.BaseTypes (EpochInterval (..), StrictMaybe (..), addEpochInterval)
import Cardano.Ledger.Coin (CompactForm (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.State (
  BlsKey (..),
  BlsKeyState (..),
  LeiosCandidate (..),
  LeiosSeat (..),
  emptyLeiosCommittee,
  leiosCommitteeSeats,
  selectLeiosCommittee,
 )
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Ratio ((%))
import qualified Data.Vector as V
import qualified Data.Vector.Strict as VS
import Data.Word (Word16, Word32, Word64)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)

-- | A candidate whose ranking stake and seat weight both track @stake@, so a
-- test can read the seating order straight off the seat weights.
candidateWith :: Int -> Word64 -> StrictMaybe BlsKeyState -> LeiosCandidate
candidateWith poolId stake =
  LeiosCandidate (mkKeyHash poolId) (CompactCoin stake) (fromIntegral stake % 1)

-- | A valid voting key derived from a seed, carrying its own proof of possession.
validKey :: Int -> BlsKey
validKey i =
  BlsKey
    { blsPubKey = deriveVerKeyDSIGN signKey
    , blsPossessionProof = createPossessionProofDSIGN minSigPoPDST signKey
    }
  where
    signKey = genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 (fromIntegral i)))

-- | A valid voting key stamped with its registration epoch.
keyState :: Int -> EpochNo -> BlsKeyState
keyState i registeredIn = BlsKeyState (validKey i) registeredIn

-- | Seat the committee at an epoch far past any registration and with an
-- effectively unbounded key age, so only the stake ordering and proof of
-- possession matter — the aging cutoff is exercised separately below.
seatsOf :: Word16 -> [LeiosCandidate] -> [LeiosSeat]
seatsOf committeeSize =
  VS.toList
    . leiosCommitteeSeats
    . selectLeiosCommittee (EpochNo 0) (EpochInterval maxBound) committeeSize
    . V.fromList

spec :: Spec
spec = describe "selectLeiosCommittee" $ do
  prop "a committee size of zero produces no committee" $
    \(stakes :: [Word64]) ->
      let candidates = [candidateWith i s SNothing | (i, s) <- zip [0 ..] stakes]
       in selectLeiosCommittee (EpochNo 0) (EpochInterval maxBound) 0 (V.fromList candidates)
            === emptyLeiosCommittee

  prop "seats exactly min(committeeSize, number of pools) pools" $
    \(stakes :: [Word64]) (committeeSize :: Word16) ->
      let candidates = [candidateWith i s SNothing | (i, s) <- zip [0 ..] stakes]
       in length (seatsOf committeeSize candidates)
            === min (fromIntegral committeeSize) (length candidates)

  prop "seats pools in descending stake" $
    \(stakes :: [Word64]) ->
      let candidates = [candidateWith i s SNothing | (i, s) <- zip [0 ..] stakes]
          weights = map seatWeight (seatsOf (fromIntegral (length candidates)) candidates)
       in weights === sortOn Down weights
            & counterexample ("seat weights not descending: " <> show weights)

  prop "seats the highest-stake pools" $
    \(stakes :: [Word64]) (committeeSize :: Word16) ->
      let candidates = [candidateWith i s SNothing | (i, s) <- zip [0 ..] stakes]
          seated = map seatWeight (seatsOf committeeSize candidates)
          topWeights = take (length seated) (sortOn Down [fromIntegral s % 1 | s <- stakes])
       in sortOn Down seated === topWeights
            & counterexample ("an excluded pool outweighs a seated one, seated: " <> show seated)

  prop "equal stakes are seated by ascending pool id" $
    \(Positive poolCount) ->
      let n = 1 + poolCount `mod` 8
          -- One valid key per pool, so a seat can be traced back to the pool
          -- holding it: the committee itself records no pool identity. Ordered by
          -- pool id, which is what the tie-break among equal stakes seats them by.
          keyed =
            sortOn
              (lcPoolId . fst)
              [(candidateWith i 1 (SJust (keyState i (EpochNo 0))), validKey i) | i <- [1 .. n]]
          candidates = map fst keyed
          seated = [vk | LeiosSeat _ (SJust vk) <- seatsOf (fromIntegral n) candidates]
       in seated === [blsPubKey key | (_, key) <- keyed]

  prop "a pool with no registered key gets a keyless seat" $
    \(poolId :: Int) ->
      map seatVKey (seatsOf 1 [candidateWith poolId 1 SNothing]) === [SNothing]

  it "a key whose proof of possession does not verify is seated keyless" $
    -- A key built from one seed carrying another seed's proof of possession.
    let mismatched = (validKey 1) {blsPossessionProof = blsPossessionProof (validKey 2)}
        candidate = candidateWith 0 1 (SJust (BlsKeyState mismatched (EpochNo 0)))
     in map seatVKey (seatsOf 1 [candidate]) `shouldBe` [SNothing]

  prop "a key is seated while honoured and keyless once it ages out" $
    \(poolId :: Int) (registeredIn :: EpochNo) (Positive ageWord) ->
      let maxKeyAge = EpochInterval (ageWord :: Word32)
          candidate = candidateWith poolId 1 (SJust (keyState 7 registeredIn))
          keysAt e =
            map seatVKey
              . VS.toList
              . leiosCommitteeSeats
              $ selectLeiosCommittee e maxKeyAge 1 (V.fromList [candidate])
          boundary = addEpochInterval registeredIn maxKeyAge
       in conjoin
            [ keysAt registeredIn === [SJust (blsPubKey (validKey 7))]
                & counterexample "not honoured in its registration epoch"
            , keysAt boundary === [SNothing]
                & counterexample "still honoured at the expiry boundary"
            ]
