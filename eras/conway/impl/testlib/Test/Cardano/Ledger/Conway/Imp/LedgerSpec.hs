{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.LedgerSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (..), maxRefScriptSizePerTx)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.SafeHash (originalBytesSize)
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsWithDatum,
  alwaysSucceedsNoDatum,
 )

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = do
  it "TxRefScriptsSizeTooBig" $ do
    -- we use here the largest script we currently have as many times as necessary to
    -- trigger the predicate failure
    Just plutusScript <- pure $ mkPlutusScript @era $ alwaysFailsWithDatum SPlutusV3
    let script :: Script era
        script = fromPlutusScript plutusScript
        size = originalBytesSize script
        n = maxRefScriptSizePerTx `div` size + 1
    txIns <- replicateM n (produceRefScript script)
    let tx :: Tx era
        tx = mkBasicTx (mkBasicTxBody & referenceInputsTxBodyL .~ Set.fromList txIns)
    submitFailingTx
      tx
      [ injectFailure $ ConwayTxRefScriptsSizeTooBig (size * n) maxRefScriptSizePerTx
      ]

  it "Withdraw from delegated and non-delegated staking key" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    kh <- freshKeyHash
    let cred = KeyHashObj kh
    ra <- registerStakeCredential cred
    submitAndExpireProposalToMakeReward cred
    reward <- lookupReward cred

    let tx = mkBasicTx $ mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, reward)]

    pv <- getProtVer
    if HF.bootstrapPhase pv
      then submitTx_ tx
      else
        submitFailingTx
          tx
          [injectFailure $ ConwayWdrlNotDelegatedToDRep [kh]]
    _ <- delegateToDRep cred (Coin 1_000_000) DRepAlwaysAbstain
    submitTx_ $
      mkBasicTx $
        mkBasicTxBody
          & withdrawalsTxBodyL
            .~ Withdrawals
              [(ra, if HF.bootstrapPhase pv then mempty else reward)]

  it "Withdraw from delegated and non-delegated staking script" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    let scriptHash = hashPlutusScript $ alwaysSucceedsNoDatum SPlutusV3
    let cred = ScriptHashObj scriptHash
    ra <- registerStakeCredential cred
    submitAndExpireProposalToMakeReward cred
    reward <- lookupReward cred

    submitTx_ $
      mkBasicTx $
        mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, reward)]

    _ <- delegateToDRep cred (Coin 1_000_000) DRepAlwaysAbstain
    submitTx_ $
      mkBasicTx $
        mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, mempty)]
