{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Conway.PParams where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Constrained
import Data.Word (Word16)
import Test.Cardano.Ledger.Constrained.Conway.Instances (EraPP, IsConwayUniv, PParamSubset (..))

-- | A Spec for PParamSubset, which we will lift to a Spec on PParams
pparamSubsetSpec :: IsConwayUniv fn => Specification fn PParamSubset
pparamSubsetSpec = constrained $ \pparamsubset ->
  match pparamsubset $
    \feeA
     feeB
     bbsize
     txsize
     bhsize
     kdep
     pdep
     emax
     _nopt
     _a0
     _rho
     _tau
     _decentral
     _protocolVersion
     _minval
     poolCost
     -- Alonzo
     _coinsPerWord
     costmod
     _prices
     _maxTxEx
     _maBlockEx
     valsize
     col
     _maxColSize
     -- Babbage
     coinsperbyte
     -- Conway
     _poolThresh
     _drepThresh
     _commMinSize
     commMaxTerm
     govactlife
     actdep
     drepdep
     _drepAct
     _minFeePerByte ->
        [ assert $ bbsize <=. bhsize + txsize
        , assert $ bhsize <=. (lit (fromIntegral (maxBound @Word16)))
        , assert $ (lit 25000) <=. txsize
        , assert $ (lit 1000) <=. bbsize
        , assert $ valsize /=. lit 0
        , assert $ col /=. lit 0
        , assert $ costmod ==. lit mempty
        , assert $ commMaxTerm /=. lit (EpochInterval 0)
        , assert $ govactlife /=. lit (EpochInterval 0)
        , assert $ pdep /=. lit (Coin 0)
        , assert $ kdep /=. lit (Coin 0)
        , assert $ actdep /=. lit (Coin 0)
        , assert $ drepdep /=. lit (Coin 0)
        , assert $ lit (EpochInterval 0) <. emax
        , assert $ lit (Coin 0) <. coinsperbyte
        , assert $ lit (Coin 0) <. feeA
        , assert $ feeB <. feeA
        , assert $ lit (Coin 10) <. poolCost
        ]

pparamsSpec :: (EraPP era, IsConwayUniv fn) => Specification fn (PParams era)
pparamsSpec =
  constrained $ \pp ->
    match pp $ \ppsub -> [satisfies ppsub pparamSubsetSpec]
