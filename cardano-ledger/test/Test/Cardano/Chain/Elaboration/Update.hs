{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Elaboration.Update
  ( elaboratePParams
  )
where

import Cardano.Prelude

import qualified Cardano.Chain.Common as Concrete
import qualified Cardano.Chain.Slotting as Concrete
import qualified Cardano.Chain.Update as Concrete

import qualified Ledger.Update as Abstract

import Test.Cardano.Chain.Genesis.Dummy (dummyProtocolParameters)

elaboratePParams :: Abstract.PParams -> Concrete.ProtocolParameters
elaboratePParams pps = Concrete.ProtocolParameters
  { Concrete.ppScriptVersion      = fromIntegral $ Abstract._scriptVersion pps
  , Concrete.ppSlotDuration       = Concrete.ppSlotDuration dummyProtocolParameters
  , Concrete.ppMaxBlockSize       = 748 * Abstract._maxBkSz pps
  , Concrete.ppMaxHeaderSize      = 95 * Abstract._maxHdrSz pps
  , Concrete.ppMaxTxSize          = 4096 * Abstract._maxTxSz pps
  , Concrete.ppMaxProposalSize    = 0
  , Concrete.ppMpcThd             = Concrete.mkKnownLovelacePortion @0
  , Concrete.ppHeavyDelThd        = Concrete.mkKnownLovelacePortion @0
  , Concrete.ppUpdateVoteThd      = Concrete.mkKnownLovelacePortion @0
  , Concrete.ppUpdateProposalThd  = Concrete.mkKnownLovelacePortion @0
  , Concrete.ppUpdateProposalTTL  = 0
  , Concrete.ppSoftforkRule       = Concrete.SoftforkRule
    (Concrete.mkKnownLovelacePortion @0)
    (Concrete.mkKnownLovelacePortion @0)
    (Concrete.mkKnownLovelacePortion @0)
  , Concrete.ppTxFeePolicy        = Concrete.TxFeePolicyTxSizeLinear
    (Concrete.TxSizeLinear
      (intToLovelace (Abstract._factorA pps))
      (intToLovelace (Abstract._factorB pps))
    )
  , Concrete.ppUnlockStakeEpoch   = Concrete.EpochNumber maxBound
  }
 where
  intToLovelace :: Int -> Concrete.Lovelace
  intToLovelace x =
    case Concrete.mkLovelace (fromIntegral x) of
    Left err -> panic $ "intToLovelace: " <> show err
    Right l -> l
