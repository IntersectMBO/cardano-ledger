{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Utils (
  unsafeBoundRational,
  epsilonMaybeEq,
  testGlobals,
  mkDummySafeHash,
  txInAt,
)
where

import Cardano.Ledger.BaseTypes (
  EpochSize (..),
  Globals (..),
  Network (..),
  mkActiveSlotCoeff,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.SafeHash (SafeHash, unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxIn, mkTxInPartial)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Data (Proxy)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Rational (unsafeBoundRational)

-- | This test for equality takes into account magnitude of the arguments
epsilonMaybeEq ::
  -- | Epsilon, a maximum tolerated error. Sign is ignored.
  Rational ->
  -- | Expected result.
  Rational ->
  -- | Tested value.
  Rational ->
  Property
epsilonMaybeEq epsilon x y =
  counterexample
    ( concat
        [show x, " /= ", show y, " (Tolerance: ", show diff, " > ", show n, ")"]
    )
    (classify True "Exactly" (x === y) .||. diff <= n)
  where
    (absx, absy) = (abs x, abs y)
    n = epsilon * (1 + max absx absy)
    diff = abs (y - x)

testGlobals :: Globals
testGlobals =
  Globals
    { epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
    , slotsPerKESPeriod = 20
    , stabilityWindow = 33
    , randomnessStabilisationWindow = 33
    , securityParameter = 10
    , maxKESEvo = 10
    , quorum = 5
    , maxMajorPV = maxBound
    , maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , activeSlotCoeff = mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , networkId = Testnet
    , systemStart = SystemStart $ posixSecondsToUTCTime 0
    }

mkDummySafeHash :: forall c a. Crypto c => Proxy c -> Int -> SafeHash c a
mkDummySafeHash _ = unsafeMakeSafeHash . mkDummyHash @(HASH c)

txInAt :: (HasCallStack, Integral i, EraTx era) => i -> Tx era -> TxIn (EraCrypto era)
txInAt index tx =
  let txid = txIdTx tx
   in mkTxInPartial txid (toInteger index)
