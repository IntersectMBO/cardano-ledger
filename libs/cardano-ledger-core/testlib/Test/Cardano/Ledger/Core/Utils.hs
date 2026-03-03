{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Utils (
  unsafeBoundRational,
  testGlobals,
  mkDummySafeHash,
  txInAt,
  mkActiveStake,
) where

import Cardano.Ledger.BaseTypes (
  EpochSize (..),
  Globals (..),
  Network (..),
  knownNonZeroBounded,
  mkActiveSlotCoeff,
  nonZero,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (toCompactPartial)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.State (ActiveStake (..), StakeWithDelegation (..))
import Cardano.Ledger.TxIn (TxIn, mkTxInPartial)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.VMap as VMap
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

txInAt :: (HasCallStack, EraTx era) => Int -> Tx l era -> TxIn
txInAt index tx =
  let txId = txIdTx tx
   in mkTxInPartial txId (toInteger index)

-- | Construct an @ActiveStake@ from separate maps of stake and delegations.
-- Only credentials present in both maps with non-zero stake are included.
mkActiveStake ::
  Map (Credential Staking) Coin ->
  Map (Credential Staking) (KeyHash StakePool) ->
  ActiveStake
mkActiveStake stakeMap delegsMap =
  ActiveStake
    $ VMap.fromMap
    $ Map.mapMaybe
      (\(c, d) -> flip StakeWithDelegation d <$> nonZero (toCompactPartial c))
    $ Map.intersectionWith (,) stakeMap delegsMap
