{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.GovActionReorderSpec (spec) where

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (GovActionState (..))
import Cardano.Ledger.Conway.Rules (actionPriority, reorderActions)
import Data.Foldable (Foldable (..))
import Data.List (sort, sortOn)
import qualified Data.Sequence.Strict as Seq
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()

spec :: Spec
spec =
  describe "Conway governance actions reordering" $ do
    prop "preserves length when reordered" $
      \(actions :: Seq.StrictSeq (GovActionState Conway)) ->
        Seq.length actions `shouldBe` Seq.length (reorderActions @Conway actions)
    prop "sorts by priority" $
      \(actions :: Seq.StrictSeq (GovActionState Conway)) ->
        sort (toList (actionPriority . gasAction @Conway <$> actions))
          `shouldBe` toList (actionPriority . gasAction <$> reorderActions actions)
    prop "same priority actions are not rearranged" $
      \(a :: GovActionState Conway) (as :: Seq.StrictSeq (GovActionState Conway)) ->
        let filterPrio b = actionPriority (gasAction a) == actionPriority (gasAction b)
         in filter filterPrio (toList $ reorderActions @Conway (a Seq.:<| as))
              `shouldBe` filter filterPrio (toList $ reorderActions (a Seq.:<| as))
    prop "orders actions correctly" $
      \(actionsList :: [GovActionState Conway]) -> do
        let sortedActions = sortOn (actionPriority . gasAction) actionsList
        Seq.fromList sortedActions `shouldBe` reorderActions (Seq.fromList actionsList)
