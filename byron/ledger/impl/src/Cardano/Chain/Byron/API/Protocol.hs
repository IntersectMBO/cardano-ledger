module Cardano.Chain.Byron.API.Protocol
  ( previewDelegationMap,
  )
where

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as D.Iface
import qualified Cardano.Chain.Slotting as CC
import Cardano.Prelude

-- | Preview the delegation map at a slot assuming no new delegations are
-- | scheduled.
previewDelegationMap ::
  CC.SlotNumber ->
  CC.ChainValidationState ->
  Delegation.Map
previewDelegationMap slot cvs =
  let ds = D.Iface.activateDelegations slot $ CC.cvsDelegationState cvs
   in D.Iface.delegationMap ds
