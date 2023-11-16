{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.GovSnapshot (
  govProposalsProps,
) where

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (
  GovActionState (..),
  fromGovActionStateSeq,
  isConsistent_,
  -- proposalsActions,
  -- proposalsAddProposal,
  proposalsAddVote,
  proposalsIds,
  proposalsRemoveIds,
 )
import Data.Foldable (Foldable (..))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Conway.Arbitrary (uniqueIdGovActions)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), testProperty)

uniq :: Ord a => [a] -> Bool
uniq x = length x == Set.size (Set.fromList x)

govProposalsProps :: TestTree
govProposalsProps =
  testGroup
    "Proposals"
    [ testProperty "Generator is consistent" (isConsistent_ @Conway)
    , -- , testProperty "Adding action preserves consistency" $ do -- TODO: @aniketd rewrite these tests
      --     as <- uniqueIdGovActions @Conway
      --     case as of
      --       x :<| xs ->
      --         case proposalsAddProposal x def def $ fromGovActionStateSeq xs of
      --           Nothing -> pure False
      --           Just (_, newProposals) -> pure $ isConsistent_ newProposals
      --       _ -> pure True
      testProperty "Removing action preserves consistency" $ do
        as <- uniqueIdGovActions @Conway
        pure $ case as of
          xs@(GovActionState {gasId} :<| _) ->
            isConsistent_ $ fst $ proposalsRemoveIds (Set.singleton gasId) $ fromGovActionStateSeq xs
          _ -> True
    , testProperty "Adding vote preserves consistency" $ do
        as <- uniqueIdGovActions @Conway
        voter <- arbitrary
        vote <- arbitrary
        case as of
          xs@(GovActionState {gasId} :<| _) ->
            pure $
              isConsistent_ (proposalsAddVote voter vote gasId $ fromGovActionStateSeq xs)
          _ -> pure True
    , testProperty "IDs are unique" $ uniq . toList . proposalsIds @Conway
    -- , testProperty "Order is preserved when inserting" $ do
    --     as <- uniqueIdGovActions @Conway
    --     let proposals =
    --           foldl'
    --             ( \acc p ->
    --                 case proposalsAddProposal p def def acc of
    --                   Nothing -> acc
    --                   Just (_, newPs) -> newPs
    --             )
    --             def
    --             as
    --     pure $ as == proposalsActions proposals
    ]
