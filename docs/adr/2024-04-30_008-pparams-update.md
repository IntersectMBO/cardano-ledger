---
slug: 8
title: |
  8. Update of protocol parameters
authors: [Alexey Kuleshevich]
tags: [Accepted]
---

## Status

Accepted

## Context

We need an unified approach for predicting and updating protocol parameters.

Outline.

### Shelley through Babbage:

1. During the first 8 stability windows PPUP update rule can add proposals in transactions
   that are signed by genesis keys. Besides keeping the proposals we also now keep
   potential protocol parameters in the future pparms, whenever quorum of genesis votes is
   reached. Potential values for new PParams can change if genesis change their votes
   during this period, therefore they cannot yet be considered stable.
2. The very first tick that happens during the last two stability windows before the end
   of the epoch solidifies the proposed PParams, thus ensuring they will be applied at the
   epoch boundary. (See `solidifyNextEpochPParams`)
3. At the epoch boundary during the NEWPP rule new PParams are applied. All of the votes
   that where potentially submitted during the last two stability windows are converted to
   current votes, which will be treated in the same way as in the step 1. This also resets

### Conway era forward

1. ParameterChange or HardForkInitiation proposal is submitted and votes are collected.
2. At the epoch boundary all of the votes and proposals are snapshotted.
3. Pulser starts the work on every TICK figuring out the stake distribution for DReps,
   calculating the votes and ratifying the proposals. Whenever ParameterChange or
   HardForkInitiation gets ratified in the RATIFY rule, the new values are immediately
   applied to the future enact state by the ENACT rule. Therefore we have future PParams
   at the latest two stability windows before the end of the epoch.
4. During the first 8 stability windows on every tick we also lazily update future PParams.
5. Just as in Shelley era the very first tick that happens during the last two stability
   windows before the end of the epoch solidifies the proposed PParams. (See
   `solidifyNextEpochPParams`). Unlike previous eras, in Conway this step is safe to do at
   any point during the first 8 stability windows, because they are considered stable as
   soon as we enter new epoch, however they are expensive to compute during that period,
   that is why we solidify them only when we are pretty confident that the DRep pulser is
   complete and RATIFY with ENACT rules got a chance to run.
6. At the epoch boundary we apply the new PParams that where solidified in the previous
   step and reset the future pparams to `NoPParamsUpdate`, thus making it ready for the
   next epoch. The important part here is that we do not use the values from the Enact
   state directly, but we take the futurePParams as the source of truth. This allows us to
   correctly update the PParams not only using the voting process of Conway era, but
   allows us to apply the PParams update from Babbage era.

### Forcecast

It is very important that the TICKF does the same steps as the TICK and EPOCH (for Conway)
and NEWPP (for Shelley) rules. In particular same solidification and rotation of pparams
process as in the stepsabove should happen for forcasting to work correctly.

## Decision


## Consequences

