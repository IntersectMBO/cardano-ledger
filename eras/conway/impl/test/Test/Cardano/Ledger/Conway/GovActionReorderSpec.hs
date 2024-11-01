{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.GovActionReorderSpec (spec) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  GovActionState (..),
  actionPriority,
  gasAction,
  reorderActions,
 )
import Data.Foldable (Foldable (..))
import Data.List (sort)
import qualified Data.Sequence.Strict as Seq
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary (ShuffledGovActionStates (..))

spec :: Spec
spec =
  describe "Conway governance actions reordering" $ do
    prop "preserves length when reordered" $
      \(actions :: Seq.StrictSeq (GovActionState ConwayEra)) ->
        Seq.length actions `shouldBe` Seq.length (reorderActions @ConwayEra actions)
    prop "sorts by priority" $
      \(actions :: Seq.StrictSeq (GovActionState ConwayEra)) ->
        sort (toList (actionPriority . gasAction @ConwayEra <$> actions))
          `shouldBe` toList (actionPriority . gasAction <$> reorderActions actions)
    prop "same priority actions are not rearranged" $
      \(a :: GovActionState ConwayEra) (as :: Seq.StrictSeq (GovActionState ConwayEra)) ->
        let filterPrio b = actionPriority (gasAction a) == actionPriority (gasAction b)
         in filter filterPrio (toList $ reorderActions @ConwayEra (a Seq.:<| as))
              `shouldBe` filter filterPrio (toList $ reorderActions (a Seq.:<| as))
    prop "orders actions correctly with shuffles" $
      \(ShuffledGovActionStates gass shuffledGass :: ShuffledGovActionStates ConwayEra) -> do
        reorderActions (Seq.fromList gass) `shouldBe` reorderActions (Seq.fromList shuffledGass)
