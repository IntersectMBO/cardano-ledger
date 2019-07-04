{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.SigCnt where


import           Data.Word

import           Control.Arrow ((|||))
import           Control.Lens ((^.))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

import           Control.State.Transition

import           Ledger.Core hiding ((|>))
import           Ledger.Update hiding (NotADelegate)

data SIGCNT

instance STS SIGCNT where
  type Environment SIGCNT
    = ( PParams
      , Bimap VKeyGenesis VKey
      , BlockCount -- Chain stability parameter
      )

  type State SIGCNT = Seq VKeyGenesis

  type Signal SIGCNT = VKey

  data PredicateFailure SIGCNT
    = TooManyIssuedBlocks VKeyGenesis -- The given genesis key issued too many blocks.
    | NotADelegate
    -- ^ The key signing the block is not a delegate of a genesis key.

    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((pps, dms, k), sgs, vk) <- judgmentContext
        let
          t' = pps ^. bkSgnCntT
        case Bimap.lookupR vk dms of
          Just vkG -> do
            let sgs' = S.drop (S.length sgs + 1 - (fromIntegral . unBlockCount $ k)) (sgs |> vkG)
                nrSignedBks = fromIntegral (S.length (S.filter (==vkG) sgs'))
            nrSignedBks <= fromIntegral (unBlockCount k) * t' ?! TooManyIssuedBlocks vkG
            pure $! sgs'
          Nothing -> do
            failBecause NotADelegate
            pure $! sgs -- TODO: this is a quite inconvenient encoding for this transition system!
    ]

-- | Generate an issuer that can still issue blocks according to the @SIGCNT@ rule. The issuers are
-- taken from the range of the delegation map passed as parameter.
--
-- This generator can fail if no suitable issuer can be found.
genIssuer
  :: Environment SIGCNT
  -> State SIGCNT
  -> Word8 -- TODO: remove
  -> Gen VKey
genIssuer (pps, dms, k) sgs ngk =
  if null validIssuers
  then error $ "No valid issuers!" ++ "\n"
             ++ "k = " ++ show k ++ "\n"
             ++ "ngk = " ++ show ngk ++ "\n"
             ++ "keys = " ++ show (Bimap.elems dms) ++ "\n"
             ++ "sgs = " ++ show sgs ++ "\n"
             ++ "length sgs = " ++ show (length sgs) ++ "\n"
             ++ "_bkSgnCntT = " ++ show (_bkSgnCntT pps) ++ "\n"
             ++ "dms = " ++ show dms
  else Gen.element validIssuers
  where
    validIssuers =
      filter canIssueABlock anyIssuer
    anyIssuer = Bimap.elems dms
    canIssueABlock :: VKey -> Bool
    canIssueABlock vk =
      (const False ||| const True) $ applySTS @SIGCNT (TRC ((pps, dms, k), sgs, vk))
