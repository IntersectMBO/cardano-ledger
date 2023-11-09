{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.GovSnapshot (
  govSnapshotProps,
) where

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (
  GovActionState (..),
  fromGovActionStateSeq,
  isConsistent_,
  snapshotActions,
  snapshotAddVote,
  snapshotIds,
  snapshotInsertGovAction,
  snapshotRemoveIds,
 )
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Conway.Arbitrary (uniqueIdGovActions)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), testProperty)

uniq :: Ord a => [a] -> Bool
uniq x = length x == Set.size (Set.fromList x)

govSnapshotProps :: TestTree
govSnapshotProps =
  testGroup
    "Proposals"
    [ testProperty "Generator is consistent" (isConsistent_ @Conway)
    , testProperty "Adding action preserves consistency" $ do
        as <- uniqueIdGovActions @Conway
        case as of
          x :<| xs ->
            pure $
              isConsistent_ (snapshotInsertGovAction x $ fromGovActionStateSeq xs)
          _ -> pure True
    , testProperty "Removing action preserves consistency" $ do
        as <- uniqueIdGovActions @Conway
        pure $ case as of
          xs@(GovActionState {gasId} :<| _) ->
            isConsistent_ $ fst $ snapshotRemoveIds (Set.singleton gasId) $ fromGovActionStateSeq xs
          _ -> True
    , testProperty "Adding vote preserves consistency" $ do
        as <- uniqueIdGovActions @Conway
        voter <- arbitrary
        vote <- arbitrary
        case as of
          xs@(GovActionState {gasId} :<| _) ->
            pure $
              isConsistent_ (snapshotAddVote voter vote gasId $ fromGovActionStateSeq xs)
          _ -> pure True
    , testProperty "IDs are unique" $ uniq . toList . snapshotIds @Conway
    , testProperty "Order is preserved when inserting" $ do
        as <- uniqueIdGovActions @Conway
        let snapshot = foldl' (flip snapshotInsertGovAction) def as
        pure $ as == snapshotActions snapshot
    ]
