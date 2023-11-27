module Test.Cardano.Ledger.Api.TreeDiff where

-- State/Query/CommitteeMembersState
instance ToExpr MemberStatus

instance ToExpr (HotCredAuthStatus c)

instance ToExpr NextEpochChange

instance ToExpr (CommitteeMemberState c)

instance ToExpr (CommitteeMembersState c)
