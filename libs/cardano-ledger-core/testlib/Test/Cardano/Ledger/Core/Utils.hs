{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Utils (
  unsafeBoundRational,
  testGlobals,
  mkDummySafeHash,
  txInAt,
) where

import Cardano.Ledger.BaseTypes (
  EpochSize (..),
  Globals (..),
  Network (..),
  knownNonZeroBounded,
  mkActiveSlotCoeff,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxIn, mkTxInPartial)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Rational (unsafeBoundRational)

testGlobals :: Globals
testGlobals =
  Globals
    { epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
    , slotsPerKESPeriod = 20
    , stabilityWindow = 33
    , randomnessStabilisationWindow = 33
    , securityParameter = knownNonZeroBounded @10
    , maxKESEvo = 10
    , quorum = 5
    , maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , activeSlotCoeff = mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , networkId = Testnet
    , systemStart = SystemStart $ posixSecondsToUTCTime 0
    }

mkDummySafeHash :: forall a. Int -> SafeHash a
mkDummySafeHash = unsafeMakeSafeHash . mkDummyHash @HASH

txInAt :: (HasCallStack, EraTx era) => Int -> Tx era -> TxIn
txInAt index tx =
  let txId = txIdTx tx
   in mkTxInPartial txId (toInteger index)
