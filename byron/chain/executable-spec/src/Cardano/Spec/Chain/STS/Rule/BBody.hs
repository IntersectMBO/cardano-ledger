{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Spec.Chain.STS.Rule.BBody where

import Cardano.Spec.Chain.STS.Rule.Bupi
import Control.Lens ((^.))
import Data.Bimap (keys)
import Data.Set (fromList)

import Cardano.Ledger.Spec.STS.UTXO (UTxOEnv(UTxOEnv, pps, utxo0), UTxOState)
import Cardano.Ledger.Spec.STS.UTXOWS (UTXOWS)
import Control.State.Transition
  ( Embed
  , Environment
  , PredicateFailure
  , STS
  , Signal
  , State
  , TRC(TRC)
  , (?!)
  , initialRules
  , judgmentContext
  , trans
  , transitionRules
  , wrapFailed
  )
import Ledger.Core
  (Epoch, hash)
import Ledger.Delegation
  ( DELEG
  , DIState
  , _dIStateDelegationMap
  , DSEnv(DSEnv)
  , _dSEnvAllowedDelegators
  , _dSEnvEpoch
  , _dSEnvSlot
  , _dSEnvK
  )
import Ledger.Update (PParams, maxBkSz, UPIState, stableAfter)
import Ledger.UTxO (UTxO)

import Cardano.Spec.Chain.STS.Block

data BBODY

instance STS BBODY where
  type Environment BBODY =
    ( PParams
    , Epoch
    , UTxO
    )

  type State BBODY =
    ( UTxOState
    , DIState
    , UPIState
    )

  type Signal BBODY = Block

  data PredicateFailure BBODY
    = InvalidBlockSize
    | InvalidUtxoHash
    | InvalidDelegationHash
    | InvalidUpdateProposalHash
    | BUPIFailure (PredicateFailure BUPI)
    | DelegationFailure (PredicateFailure DELEG)
    | UTXOWSFailure (PredicateFailure UTXOWS)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((ppsVal, e_n, utxoGenesis), (utxoSt, ds, us), b) <- judgmentContext
        let bMax = ppsVal ^. maxBkSz
        bSize b <= bMax ?! InvalidBlockSize
        let bh = b ^. bHeader
        hash (b ^. bBody ^. bUtxo)   == bh ^. bhUtxoHash ?! InvalidUtxoHash
        hash (b ^. bBody ^. bDCerts) == bh ^. bhDlgHash  ?! InvalidDelegationHash
        hash (bUpdPayload b)         == bh ^. bhUpdHash  ?! InvalidUpdateProposalHash

        us' <- trans @BUPI $ TRC (
            (bh ^. bhSlot, _dIStateDelegationMap ds, ppsVal ^. stableAfter)
          , us
          , (b ^. bBody ^. bUpdProp, b ^. bBody ^. bUpdVotes, bEndorsment b) )
        ds' <- trans @DELEG $ TRC
          ( ( DSEnv
                { _dSEnvAllowedDelegators =
                    (fromList . keys . _dIStateDelegationMap) ds
                , _dSEnvEpoch = e_n
                , _dSEnvSlot = bh ^. bhSlot
                , _dSEnvK = ppsVal ^. stableAfter
                }
            )
          , ds
          , b ^. bBody ^. bDCerts
          )
        utxoSt' <- trans @UTXOWS $ TRC
          ( UTxOEnv {utxo0 = utxoGenesis, pps = ppsVal}, utxoSt, b ^. bBody ^. bUtxo )

        return $! (utxoSt', ds', us')
    ]

instance Embed BUPI BBODY where
  wrapFailed = BUPIFailure

instance Embed DELEG BBODY where
  wrapFailed = DelegationFailure

instance Embed UTXOWS BBODY where
  wrapFailed = UTXOWSFailure
