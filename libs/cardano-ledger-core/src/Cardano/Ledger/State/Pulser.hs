{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Ledger.State.Pulser where

data PulserState era = PulserState
  { pulserPrepState :: !(PulserPrepState era)
  , pulserWorkItems :: !(Vector (Pulse era))
  , pulserUpcomingWork :: !(Either (Pulse era) Int)
  -- ^ Work item that needs to be forced at the next iteration of the pulser. Most often it will be
  -- the index intex into the vector of thunks of work, but the very last item of work is the fold of all
  -- of the results.
  }

class Monoid (Pulse era) => EraPulser era where
  -- | Individual item of work that will be pulsed.
  type Pulse era = (r :: Type) | r -> era

  -- | This is the final outcome of the pulser
  type PulserResult era = (r :: Type) | r -> era

  -- | This is the state that Pulser starts with and the one that was used to define all of the work
  -- pulses
  type PulserPrepState era = (r :: Type) | r -> era

  computePulserResult :: Globals -> PulserPrepState era -> Pulse era -> PulserResult era

  mkFreshPulser :: PulserPrepState era -> PulserState era

  mkPrepState :: Globals -> NewEpochState era -> PulserPrepState era

numPulses :: Globals -> Int
numPulses Globals {securityParameter} =
  -- two extra pulses are reserved for mconcat of pulses and converting final pulse into the result
  (knownNonZero @4 `mulNonZero` toIntegerNonZero securityParameter) - 2

relativeSlotToPulserIndex :: Globals -> SlotNo -> Int
relativeSlotToPulserIndex Globals {activeSlotCoeff} (SlotNo slotNo) =
  floor (toRational slotNo * unboundRational (unActiveSlotVal activeSlotCoeff))

forcePulseAtIx :: Vector (Pulse era) -> Int -> m ()
forcePulseAtIx v i = pulseWork (V.index v i)

pulseWork :: a -> m ()
pulseWork a = a `seq` pure () -- TODO: convert to `par` and delay it

-- | This operation will indicate a failure if the supplied SlotNo is not from the supplied EpochNo,
-- but it will not fail itself and instead as a failsafe it will simply return the final result
pulseEither ::
  (Monad m, HasCallStack) =>
  Globals ->
  EpochNo ->
  SlotNo ->
  PulserState era ->
  Either (Maybe String, PulserResult era) (PulserState era)
pulseEither globals epochNo slotNo pulserState@(PulserState {pulserWorkItems,pulserUpcomingWork})
  | firstSlotNoOfEpoch > slotNo =
      let errMsg =
            "This should not be possible. Supplied a slot number "
              ++ show slotNo
              ++ " that is smaller than the first slot number "
              ++ show firstSlotNoOfEpoch
              ++ " of the epoch "
              ++ show epochNo
       in Left
            (Just errMsg, computeFinalState globals (pulserWorkingState pulserState) (fold pulserWorkItems))
  | otherwise = do
      let relativeSlotNo = binOpEpochNo (-) slotNo firstSlotNoOfEpoch
          ixToForce = relativeSlotToPulserIndex globals relativeSlotNo
      -- This is where we stop when all pulses are finally done
      case pulserUpcomingWork of
        Left finalPulse ->
          -- it is ok for the pulser result to be lazy and get a final pulse, but there is also
          -- nothing wrong if it is forced right away
          let pulserReult = computePulserResult globals (pulserWorkingState pulserState) finalPulse
          pulseWork pulserResult
          pure (Nothing, pulserResult)
        Right curIx -> do
          -- it is possible that curIx is zero, so we want to force all items since zero until
          -- ixToForce
          forM_ [curIx .. min (V.length pulserWorkItems - 1) ixToForce] (forcePulseAtIx pulserWorkItems)
          if ixToForce >= V.length pulserWorkItems
            then do
              let finalPulse = fold pulserWorkItems
              pulseWork finalPulse
              Right $ pulserState {pulserUpcomingWork = Left finalPulse}
            else
              Right $ pulserState {pulserUpcomingWork  = Right ixToForce}
  where
    firstSlotNoOfEpoch = epochInfoFirst epochInfo epochNo
    epochInfo = epochInfoPure globals
