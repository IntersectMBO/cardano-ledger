{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Cardano.Ledger.Conway.Rules where

import Control.State.Transition.Extended

import Test.QuickCheck

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Rules.Cert
import Cardano.Ledger.Conway.Rules.VDel

import Cardano.Ledger.Shelley.API

import Test.Cardano.Ledger.Shelley.Utils
import Test.Cardano.Ledger.Generic.Proof

import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Preds.Universes
import Test.Cardano.Ledger.Constrained.Preds.Certs
import Test.Cardano.Ledger.Constrained.Preds.CertState
import Test.Cardano.Ledger.Constrained.Preds.PParams
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.LedgerTests
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Tests (checkPredicates)

-- TODO: this currently doesn't work because of some issue
-- with the `CertState` predicates
prop_CERT :: Property
prop_CERT =
  forAllShrink arbitrary shrink             $ \ slot ->
  forAll (genPtr slot)                      $ \ ptr ->
  forAllShrink pParamsGen pParamsShrink     $ \ (unPParams -> pParams) ->
  forAllShrink acctStateGen acctStateShrink $ \ acctState ->
  forAllShrink certGen certShrink           $ \ cert ->
  -- TODO: should be genTxCerts?
  forAllShrink arbitrary shrink   $ \ txCert -> runShelleyBase $ do
    let delplEnv = DelplEnv slot ptr pParams acctState
    res <- applySTS @(ConwayCERT Conway) $ TRC (delplEnv, cert, txCert)
    pure $ case res of
      Left _pfailure -> property False
      Right cert'    -> validCert cert'
  where
    -- General setup
    proof     = Conway Standard
    gen       = genFromConstraints proof standardOrderInfo
    shr r p t = shrinkFromConstraints @Conway r standardOrderInfo p t

    -- PParams
    pParamsT      = idTarget $ pparams proof
    pParamsPs     = pParamsPreds proof
    pParamsGen    = gen pParamsPs pParamsT
    pParamsShrink = shr (PParamsR proof) pParamsPs pParamsT

    -- AccountState
    acctStatePreds  = [ Random treasury
                      , Random reserves
                      ]
    acctStateGen    = gen acctStatePreds accountStateT
    acctStateShrink = shr AccountStateR acctStatePreds accountStateT

    -- CertState
    certPreds = concat [ universePreds proof
                       , vstatePreds proof
                       , pstatePreds proof
                       , certStatePreds proof
                       ]
    certGen    = gen certPreds certstateT
    certShrink = shr CertStateR certPreds certstateT

    validCert cert = checkPredicates (certsPreds proof)
                                     (saturateEnv (unTarget CertStateR certstateT cert) (certsPreds proof))

prop_VDEL :: Property
prop_VDEL =
  forAllShrink pParamsGen pParamsShrink $ \ (unPParams -> pParams) ->
  forAllShrink vStateGen  vStateShrink  $ \ vState  ->
  -- TODO: these are the wrong generators
  forAllShrink arbitrary  shrink        $ \ comCert ->
  runShelleyBase $ do
    res <- applySTS @(ConwayVDEL Conway) $ TRC (pParams, vState, comCert)
    pure $ case res of
      Left _pfailure -> property False
      Right vState'  -> validVState vState'
  where
    -- General setup
    proof     = Conway Standard
    gen       = genFromConstraints proof standardOrderInfo
    shr r p t = shrinkFromConstraints @Conway r standardOrderInfo p t

    -- PParams
    pParamsT      = idTarget $ pparams proof
    pParamsPs     = pParamsPreds proof
    pParamsGen    = gen pParamsPs pParamsT
    pParamsShrink = shr (PParamsR proof) pParamsPs pParamsT

    -- VState
    -- TODO: this creates some weird `can't solve for Set... <- credsUniv` issue that
    -- should be fixable somehow. The issue is that for some reason the `Subset dreps voteUniv`
    -- constraint in `vstatePreds` is solved before the `voteUniv <- coerce credsUniv` constraint
    -- in `universePreds`. This seems wrong!
    vStatePs  = concat [ universePreds proof
                       , vstatePreds proof
                       ]
    vStateGen = gen vStatePs vstateT
    vStateShrink = shr VStateR vStatePs vstateT

    validVState vstate = checkPredicates vStatePs (saturateEnv (unTarget VStateR vstateT vstate) vStatePs)
