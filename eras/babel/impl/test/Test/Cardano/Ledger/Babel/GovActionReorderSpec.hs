{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babel.GovActionReorderSpec (spec) where

import Cardano.Ledger.Babel (Babel)
import Cardano.Ledger.Conway.Governance (
  GovActionState (..),
  actionPriority,
  gasAction,
  reorderActions,
 )
import Data.Foldable (Foldable (..))
import Data.List (sort)
import qualified Data.Sequence.Strict as Seq
import Test.Cardano.Ledger.Babel.Arbitrary (ShuffledGovActionStates (..))
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "Babel governance actions reordering" $ do
    prop "preserves length when reordered" $
      \(actions :: Seq.StrictSeq (GovActionState Babel)) ->
        Seq.length actions `shouldBe` Seq.length (reorderActions @Babel actions)
    prop "sorts by priority" $
      \(actions :: Seq.StrictSeq (GovActionState Babel)) ->
        sort (toList (actionPriority . gasAction @Babel <$> actions))
          `shouldBe` toList (actionPriority . gasAction <$> reorderActions actions)
    prop "same priority actions are not rearranged" $
      \(a :: GovActionState Babel) (as :: Seq.StrictSeq (GovActionState Babel)) ->
        let filterPrio b = actionPriority (gasAction a) == actionPriority (gasAction b)
         in filter filterPrio (toList $ reorderActions @Babel (a Seq.:<| as))
              `shouldBe` filter filterPrio (toList $ reorderActions (a Seq.:<| as))
    prop "orders actions correctly with shuffles" $
      \(ShuffledGovActionStates gass shuffledGass :: ShuffledGovActionStates Babel) -> do
        reorderActions (Seq.fromList gass) `shouldBe` reorderActions (Seq.fromList shuffledGass)
