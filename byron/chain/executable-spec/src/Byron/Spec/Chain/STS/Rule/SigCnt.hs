{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Byron.Spec.Chain.STS.Rule.SigCnt where

import Byron.Spec.Ledger.Core hiding ((|>))
import Byron.Spec.Ledger.Update hiding (NotADelegate)
import Control.Arrow ((|||))
import Control.State.Transition
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Data (Data, Typeable)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Word (Word8)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lens.Micro ((^.))

data SIGCNT deriving (Data, Typeable)

-- | These `PredicateFailure`s are all throwable.
data SigcntPredicateFailure
  = -- | The given genesis key issued too many blocks.
    TooManyIssuedBlocks VKeyGenesis
  | -- | The key signing the block is not a delegate of a genesis key.
    NotADelegate
  deriving (Eq, Show, Data, Typeable)

instance STS SIGCNT where
  type
    Environment SIGCNT =
      ( PParams,
        Bimap VKeyGenesis VKey,
        BlockCount -- Chain stability parameter
      )

  type State SIGCNT = Seq VKeyGenesis

  type Signal SIGCNT = VKey
  type PredicateFailure SIGCNT = SigcntPredicateFailure

  initialRules = []

  transitionRules =
    [ do
        TRC ((pps, dms, k), sgs, vk) <- judgmentContext
        let t' = pps ^. bkSgnCntT
        case Bimap.lookupR vk dms of
          Just vkG -> do
            let sgs' = S.drop (S.length sgs + 1 - (fromIntegral . unBlockCount $ k)) (sgs |> vkG)
                nrSignedBks = fromIntegral (S.length (S.filter (== vkG) sgs'))
            nrSignedBks <= fromIntegral (unBlockCount k) * t' ?! TooManyIssuedBlocks vkG
            pure $! sgs'
          Nothing -> do
            failBecause NotADelegate
            pure $! sgs -- TODO: this is a quite inconvenient encoding for this transition system!
    ]

----------------------------------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------------------------------

-- | Generate an issuer that can still issue blocks according to the @SIGCNT@ rule. The issuers are
-- taken from the range of the delegation map passed as parameter.
--
-- This generator will throw an error if no suitable issuer can be found, which means that the block
-- production halted.
issuer ::
  Environment SIGCNT ->
  State SIGCNT ->
  Gen VKey
issuer (pps, dms, k) sgs =
  if null validIssuers
    then
      error $
        "No valid issuers!" ++ "\n"
          ++ "k = "
          ++ show k
          ++ "\n"
          ++ "keys = "
          ++ show (Bimap.elems dms)
          ++ "\n"
          ++ "sgs = "
          ++ show sgs
          ++ "\n"
          ++ "length sgs = "
          ++ show (length sgs)
          ++ "\n"
          ++ "_bkSgnCntT = "
          ++ show (_bkSgnCntT pps)
          ++ "\n"
          ++ "dms = "
          ++ show dms
    else Gen.element validIssuers
  where
    validIssuers =
      filter canIssueABlock anyIssuer
    anyIssuer = Bimap.elems dms
    canIssueABlock :: VKey -> Bool
    canIssueABlock vk =
      (const False ||| const True) $ applySTS @SIGCNT (TRC ((pps, dms, k), sgs, vk))

-- | Generate a signature count threshold given a chain stability parameter @k@ and number of
-- genesis keys @ngk@.
--
-- This threshold must allow that all the (honest) genesis keys can issue enough blocks to fill the
-- rolling window of @k@. If this is not possible, then the block production will halt since there
-- will not be valid issuers. So the threshold must make it possible to find an integer @n@ such
-- that:
--
-- > n <= k * t
--
-- and
--
-- > k < ngk * n
-- > = { algebra }
-- > k / ngk < n
--
-- We know there must be an integer in the interval
--
-- > (k/ngk, k/ngk + 1]
--
-- So to satisfy the requirements above, we can pick a @t@ such that:
--
-- > k/ngk + 1 <= k * t
-- > = { algebra }
-- > 1/ngk + 1/k <= t
--
-- To pick a value for @t@ we vary the proportion of honest keys.
sigCntT :: BlockCount -> Word8 -> Gen Double
sigCntT (BlockCount k) ngk =
  Gen.double (Range.constant lower upper)
  where
    lower = 1 / (fromIntegral ngk * 0.7) + 1 / fromIntegral k
    upper = 1 / (fromIntegral ngk * 0.5) + 1 / fromIntegral k
