{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.CDDL (
  namespaceSymbolFromText,
  knownNamespaceKeySizes,
  knownNamespaces,
) where

import Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.Blocks.V0.CDDL as Blocks
import Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0.CDDL as EntitiesAccounts
import Cardano.Ledger.CanonicalState.Namespace.EntitiesCommittee.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesCommittee.V0.CDDL as EntitiesCommittee
import Cardano.Ledger.CanonicalState.Namespace.EntitiesDReps.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesDReps.V0.CDDL as EntitiesDReps
import Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.V0.CDDL as EntitiesStakePools
import Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.VRFKeyHashes.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.VRFKeyHashes.V0.CDDL as EntitiesStakePoolsVRFKeyHashes
import Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0.CDDL as GovCommittee
import Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0.CDDL as GovConstitution
import Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0.CDDL as GovPParams
import Cardano.Ledger.CanonicalState.Namespace.GovProposals.Roots.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.GovProposals.Roots.V0.CDDL as GovProposalsRoots
import Cardano.Ledger.CanonicalState.Namespace.GovProposals.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.GovProposals.V0.CDDL as GovProposals
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.UTxO.V0.CDDL as UTxO
import Cardano.SCLS.NamespaceKey as Spec
import Cardano.SCLS.NamespaceSymbol (
  KnownSpec (..),
  SomeNamespaceSymbol (..),
  mkNamespaceSymbol,
  toString,
 )
import Codec.CBOR.Cuddle.Huddle (Huddle, HuddleItem (HIRule), Rule, collectFromInit)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (symbolVal)

-- | Lookup a namespace symbol from its text representation.
-- | Returns 'Nothing' if the namespace is not known.
namespaceSymbolFromText :: Text -> Maybe SomeNamespaceSymbol
namespaceSymbolFromText t =
  find (\ns -> T.pack (toString ns) == t) knownNamespaces

instance KnownSpec "utxo/v0" where
  namespaceSpec _ = mkDefinition UTxO.record_entry

instance KnownSpec "blocks/v0" where
  namespaceSpec _ = mkDefinition Blocks.record_entry

instance KnownSpec "entities/accounts/v0" where
  namespaceSpec _ = mkDefinition EntitiesAccounts.record_entry

instance KnownSpec "entities/committee/v0" where
  namespaceSpec _ = mkDefinition EntitiesCommittee.record_entry

instance KnownSpec "entities/dreps/v0" where
  namespaceSpec _ = mkDefinition EntitiesDReps.record_entry

instance KnownSpec "entities/stake_pools/v0" where
  namespaceSpec _ = mkDefinition EntitiesStakePools.record_entry

instance KnownSpec "entities/stake_pools/vrf_key_hashes/v0" where
  namespaceSpec _ = mkDefinition EntitiesStakePoolsVRFKeyHashes.record_entry

instance KnownSpec "gov/committee/v0" where
  namespaceSpec _ = mkDefinition GovCommittee.record_entry

instance KnownSpec "gov/constitution/v0" where
  namespaceSpec _ = mkDefinition GovConstitution.record_entry

instance KnownSpec "gov/pparams/v0" where
  namespaceSpec _ = mkDefinition GovPParams.record_entry

instance KnownSpec "gov/proposals/v0" where
  namespaceSpec _ = mkDefinition GovProposals.record_entry

instance KnownSpec "gov/proposals/roots/v0" where
  namespaceSpec _ = mkDefinition GovProposalsRoots.record_entry

mkDefinition :: Rule -> Huddle
mkDefinition r = collectFromInit [HIRule r]

knownNamespaceKeySizes :: Map String Int
knownNamespaceKeySizes =
  Map.fromList $
    map
      (\(SomeNamespaceSymbol (p :: proxy ns)) -> (symbolVal p, Spec.namespaceKeySize @ns))
      knownNamespaces

knownNamespaces :: [SomeNamespaceSymbol]
knownNamespaces =
  [ mkNamespaceSymbol @"utxo/v0"
  , mkNamespaceSymbol @"blocks/v0"
  , mkNamespaceSymbol @"entities/accounts/v0"
  , mkNamespaceSymbol @"entities/committee/v0"
  , mkNamespaceSymbol @"entities/dreps/v0"
  , mkNamespaceSymbol @"entities/stake_pools/v0"
  , mkNamespaceSymbol @"entities/stake_pools/vrf_key_hashes/v0"
  , mkNamespaceSymbol @"gov/committee/v0"
  , mkNamespaceSymbol @"gov/constitution/v0"
  , mkNamespaceSymbol @"gov/pparams/v0"
  , mkNamespaceSymbol @"gov/proposals/v0"
  , mkNamespaceSymbol @"gov/proposals/roots/v0"
  ]
