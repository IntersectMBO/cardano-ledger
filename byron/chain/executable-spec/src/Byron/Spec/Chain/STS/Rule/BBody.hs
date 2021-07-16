{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Byron.Spec.Chain.STS.Rule.BBody where

import Byron.Spec.Chain.STS.Block
import Byron.Spec.Chain.STS.Rule.Bupi
import Byron.Spec.Ledger.Core (BlockCount, Epoch, hash)
import Byron.Spec.Ledger.Delegation
  ( DELEG,
    DIState,
    DSEnv (DSEnv),
    _dIStateDelegationMap,
    _dSEnvAllowedDelegators,
    _dSEnvEpoch,
    _dSEnvK,
    _dSEnvSlot,
  )
import Byron.Spec.Ledger.STS.UTXO (UTxOEnv (UTxOEnv, pps, utxo0), UTxOState)
import Byron.Spec.Ledger.STS.UTXOWS (UTXOWS)
import Byron.Spec.Ledger.UTxO (UTxO)
import Byron.Spec.Ledger.Update (PParams, UPIState, maxBkSz)
import Control.State.Transition
  ( Embed,
    Environment,
    STS (..),
    Signal,
    State,
    TRC (TRC),
    initialRules,
    judgmentContext,
    trans,
    transitionRules,
    wrapFailed,
    (?!),
  )
import Data.Bimap (keys)
import Data.Data (Data, Typeable)
import Data.Set (fromList)
import Data.Word (Word8)
import Lens.Micro ((^.))

data BBODY deriving (Data, Typeable)

-- | These `PredicateFailure`s are all throwable.
data BbodyPredicateFailure
  = InvalidBlockSize
  | InvalidUtxoHash
  | InvalidDelegationHash
  | InvalidUpdateProposalHash
  | BUPIFailure (PredicateFailure BUPI)
  | DelegationFailure (PredicateFailure DELEG)
  | UTXOWSFailure (PredicateFailure UTXOWS)
  deriving (Eq, Show, Data, Typeable)

instance STS BBODY where
  type
    Environment BBODY =
      ( PParams,
        Epoch,
        UTxO,
        Word8,
        BlockCount -- Chain stability parameter
      )

  type
    State BBODY =
      ( UTxOState,
        DIState,
        UPIState
      )

  type Signal BBODY = Block

  type PredicateFailure BBODY = BbodyPredicateFailure

  initialRules = []

  transitionRules =
    [ do
        TRC ((ppsVal, e_n, utxoGenesis, ngk, k), (utxoSt, ds, us), b) <- judgmentContext
        let bMax = ppsVal ^. maxBkSz
        bSize b <= bMax ?! InvalidBlockSize
        let bh = b ^. bHeader
        hash (b ^. bBody ^. bUtxo) == bh ^. bhUtxoHash ?! InvalidUtxoHash
        hash (b ^. bBody ^. bDCerts) == bh ^. bhDlgHash ?! InvalidDelegationHash
        hash (bUpdPayload b) == bh ^. bhUpdHash ?! InvalidUpdateProposalHash

        us' <-
          trans @BUPI $
            TRC
              ( (bh ^. bhSlot, _dIStateDelegationMap ds, k, ngk),
                us,
                (b ^. bBody ^. bUpdProp, b ^. bBody ^. bUpdVotes, bEndorsment b)
              )
        ds' <-
          trans @DELEG $
            TRC
              ( ( DSEnv
                    { _dSEnvAllowedDelegators =
                        (fromList . keys . _dIStateDelegationMap) ds,
                      _dSEnvEpoch = e_n,
                      _dSEnvSlot = bh ^. bhSlot,
                      _dSEnvK = k
                    }
                ),
                ds,
                b ^. bBody ^. bDCerts
              )
        utxoSt' <-
          trans @UTXOWS $
            TRC
              (UTxOEnv {utxo0 = utxoGenesis, pps = ppsVal}, utxoSt, b ^. bBody ^. bUtxo)

        return $! (utxoSt', ds', us')
    ]

instance Embed BUPI BBODY where
  wrapFailed = BUPIFailure

instance Embed DELEG BBODY where
  wrapFailed = DelegationFailure

instance Embed UTXOWS BBODY where
  wrapFailed = UTXOWSFailure
