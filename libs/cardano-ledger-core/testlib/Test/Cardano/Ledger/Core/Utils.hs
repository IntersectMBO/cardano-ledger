{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Utils (
  unsafeBoundRational,
  testGlobals,
  mkDummySafeHash,
  mkDummyTxId,
  txInAt,
  nextMajorProtVer,
  nextMinorProtVer,
) where

import Cardano.Ledger.BaseTypes (
  EpochSize (..),
  Globals (..),
  Network (..),
  ProtVer (..),
  knownNonZeroBounded,
  mkActiveSlotCoeff,
  succVersion,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxId (..), TxIn, mkTxInPartial)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad.Trans.Fail.String (errorFail)
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

mkDummyTxId :: Int -> TxId
mkDummyTxId idx = TxId (mkDummySafeHash idx)

txInAt :: (HasCallStack, EraTx era) => Int -> Tx era -> TxIn
txInAt index tx =
  let txId = txIdTx tx
   in mkTxInPartial txId (toInteger index)

-- | A legal ProtVer that moves to the next major Version. Throws an error when already at the
-- latest possible major version
nextMajorProtVer :: HasCallStack => ProtVer -> ProtVer
nextMajorProtVer (ProtVer majorVersion _) = errorFail $ do
  nextMajorVersion <- succVersion majorVersion
  pure $ ProtVer nextMajorVersion 0

-- | A legal ProtVer that differs in the minor Version
nextMinorProtVer :: ProtVer -> ProtVer
nextMinorProtVer protVer = protVer {pvMinor = pvMinor protVer + 1}
