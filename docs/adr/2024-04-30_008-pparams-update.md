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

Outline of the new approach.

### Shelley through Babbage:

1. Genesis key holders submit votes with proposals for new `PParams`. Depending on the
   timeing within the epoch those votes will either go into the current or the future
   proposals bucket.
2. During the first `4k/f` slots in the the PPUP rule PParamUpdate proposals added to
   current proposals. Besides keeping the proposals we also now keep potential protocol
   parameters in the future pparms, but only quorum of genesis votes is reached. Potential
   values for new `PParams` can change if genesis key holders change their votes during
   this period, therefore they cannot yet be considered stable.
3. The very first `TICK` that happens during the last two stability windows before the end
   of the epoch we solidify the proposed `PParams`, thus ensuring they will be applied at
   the next epoch boundary. (See `solidifyNextEpochPParams`)
4. At the epoch boundary during the `NEWPP` rule, instead of counting the votes on the
   proposals, we just looked up in the next `PParams` that were decided earlier and apply
   them. All of the future votes that where potentially submitted before during the last
   two stability windows of past epoch are converted to current votes, which will be
   treated in the same way as if they were submitted in the 2nd step. This also resets the
   future PParams for the next epoch.

### Conway era forward

1. Either `ParameterChange` or `HardForkInitiation` proposal is submitted and votes are
   collected.
2. At the epoch boundary all of the votes and proposals are snapshotted.
3. Pulser starts the work on every `TICK` figuring out the stake distribution for `DReps`,
   calculating the votes and ratifying the proposals. Whenever `ParameterChange` or
   `HardForkInitiation` gets ratified in the `RATIFY` rule, the new values are immediately
   applied to the future enact state by the `ENACT` rule. Therefore we have future
   `PParams` at the latest two stability windows before the end of the epoch.
4. During the first `4k/f` slots on every tick we also lazily update future `PParams`.
5. Just as in Shelley era the very first `TICK` that happens during the last two stability
   windows (`6k/f`) before the end of the epoch we solidify the proposed `PParams`. (See
   `solidifyNextEpochPParams`). Unlike previous eras, in Conway this step is safe to do at
   any point during the initial part of the epoch, because they are considered stable as
   soon as we enter new epoch, however they are expensive to compute during that period,
   that is why we solidify them only when we are pretty confident that the DRep pulser is
   done and `RATIFY` with `ENACT` rules got a chance to be executed.
6. At the epoch boundary we apply the new PParams that where solidified in the previous
   step and reset the future pparams, thus making it ready for the next epoch. The
   important part here is that we do not use the values from the Enact state directly, but
   we take the futurePParams as the source of truth. This allows us to correctly update
   the `PParams` not only using the voting process of Conway era, but allows us to apply
   the `PParams` update from Babbage era.

### Forecast

It is very important that the TICKF rule does the same steps as the TICK and EPOCH (for
Conway) or NEWPP (for pre Conway) rules. In particular same solidification and rotation of
pparams process as in the stepsabove should happen for forcasting to work correctly.

## Decision

New approach to update has been implemented.

## Consequences

* We have a unified approach to update PParams throughout all eras starting with Shelley,
  thus making HFC combinator much more robust and correct.
* We remove duplicate logic from consensus that used to count up the genesis key holder votes.
* We solve a problem where HFC is triggered in previous era, while TICK happens in the new
  era after translation. Which previously caused protocol version not being updated
  correctly in Conway, since protocol parameter update mechanism was vastly different from
  the one in Babbage.
* We finish implementation of HardForkInitiation into a new era.
