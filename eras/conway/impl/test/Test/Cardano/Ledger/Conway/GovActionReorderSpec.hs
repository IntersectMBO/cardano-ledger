{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.GovActionReorderSpec (spec) where

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (GovActionState (..))
import Cardano.Ledger.Conway.Rules (actionPriority, reorderActions)
import Data.Foldable (Foldable (..))
import Data.List (sort, sortOn)
import qualified Data.Sequence.Strict as Seq
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common (Spec, describe, prop, shuffle)
import Test.Cardano.Ledger.Conway.Arbitrary (
  genGovActionStateFromAction,
  govActionGenerators,
 )

spec :: Spec
spec =
  describe "Conway governance actions reordering" $ do
    prop "preserves length when reordered" $
      \actions -> Seq.length actions == Seq.length (reorderActions @Conway actions)
    prop "sorts by priority" $
      \actions ->
        sort (toList (actionPriority . gasAction @Conway <$> actions))
          == toList (actionPriority . gasAction <$> reorderActions actions)
    prop "same priority actions are not rearranged" $
      \a as ->
        let filterPrio b = actionPriority (gasAction a) == actionPriority (gasAction b)
         in filter filterPrio (toList $ reorderActions @Conway (a Seq.:<| as))
              == filter filterPrio (toList $ reorderActions (a Seq.:<| as))
    prop "orders actions correctly" $ do
      actionsList <-
        traverse
          (>>= genGovActionStateFromAction)
          (govActionGenerators @Conway)
      let sortedActions = sortOn (actionPriority . gasAction) actionsList
      shuffledActions <- shuffle actionsList
      pure $
        Seq.fromList sortedActions
          == reorderActions (Seq.fromList shuffledActions)
