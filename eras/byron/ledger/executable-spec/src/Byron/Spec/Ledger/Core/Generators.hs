-- | Generators for the 'Ledger.Core' values.
module Byron.Spec.Ledger.Core.Generators
  ( vkGen,
    vkgenesisGen,
    addrGen,
    slotGen,
    epochGen,
    blockCountGen,
    k,
    kForNumberOfEpochs,
  )
where

import Byron.Spec.Ledger.Core
  ( Addr (Addr),
    BlockCount (BlockCount),
    Epoch (Epoch),
    Owner (Owner),
    Slot (Slot),
    VKey (VKey),
    VKeyGenesis (VKeyGenesis),
  )
import Byron.Spec.Ledger.GlobalParams (slotsPerEpochToK)
import Data.Word (Word64)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

vkGen :: Gen VKey
vkGen = VKey . Owner <$> Gen.integral (Range.linear 0 100)

vkgenesisGen :: Gen VKeyGenesis
vkgenesisGen = VKeyGenesis <$> vkGen

addrGen :: Gen Addr
addrGen = Addr <$> vkGen

-- | Generates a slot within the given bound
slotGen :: Word64 -> Word64 -> Gen Slot
slotGen lower upper =
  Slot <$> Gen.word64 (Range.linear lower upper)

-- | Generates an epoch within the given bound
epochGen :: Word64 -> Word64 -> Gen Epoch
epochGen lower upper =
  Epoch <$> Gen.word64 (Range.linear lower upper)

-- | Generates a block count within the given bound
blockCountGen :: Word64 -> Word64 -> Gen BlockCount
blockCountGen lower upper =
  BlockCount <$> Gen.word64 (Range.linear lower upper)

-- | Generate a chain stability parameter value (@k@) using the given chain length and desired
-- number of epochs.
k ::
  -- | Chain length
  Word64 ->
  -- | Maximum number of epochs
  Word64 ->
  Gen BlockCount
k chainLength maxNumberOfEpochs =
  kForNumberOfEpochs chainLength <$> numberOfEpochsGen
  where
    numberOfEpochsGen :: Gen Word64
    numberOfEpochsGen =
      Gen.frequency
        [ (9, Gen.integral $ Range.linear 1 (maxNumberOfEpochs `max` 1)),
          (1, pure 1)
        ]

-- | Given a chain length, determine the @k@ value that will split the chain length into the desired
-- number of epochs.
--
-- We have that:
--
-- > chainLength = slotsPerEpoch k * numberOfEpochs
-- > = { algebra }
-- > chainLength / numberOfEpochs = slotsPerEpoch k
-- > = { 'slotsPerEpochtoK' is the inverse of 'slotsPerEpoch'; algebra }
-- > slotsPerEpochToK (chainLength / numberOfEpochs) = k
--
-- So the resulting @k@ value will be directly proportional to the @chainLength@ and inversely
-- proportional to the chosen @numberOfEpochs@.
--
-- The minimum value for @k@ will be 1. In particular, this will be the value
-- returned when the number of epochs is greater or equal than @chainLength@.
kForNumberOfEpochs ::
  -- | Chain length
  Word64 ->
  -- | Desired number of epochs
  Word64 ->
  BlockCount
kForNumberOfEpochs chainLength numberOfEpochs =
  max 1 (slotsPerEpochToK slotsPerEpoch)
  where
    slotsPerEpoch :: Word64
    slotsPerEpoch = round $ fromIntegral chainLength / (fromIntegral numberOfEpochs :: Double)
