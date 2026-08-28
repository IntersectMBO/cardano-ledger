{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.CertSpec (spec) where

import Cardano.Crypto.DSIGN (
  DSIGNAggregatable (createPossessionProofDSIGN),
  deriveVerKeyDSIGN,
 )
import Cardano.Crypto.DSIGN.BLS12381.Internal (minSigPoPDST)
import Cardano.Crypto.Leios (LeiosSigningKey)
import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (Voter (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxCert (pattern RegBlsKeyTxCert)
import Cardano.Ledger.Shelley.LedgerState (nesPdL)
import Cardano.Ledger.State (
  BlsKey (..),
  IndividualPoolStake (..),
  unPoolDistr,
 )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Lens.Micro.Mtl ((%=))
import Test.Cardano.Crypto.Leios.Gen (genLeiosSigningKey)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "CERT" $ do
  it "An expired voting key drops from the pool distribution" $ do
    -- Shrink the KES setup so `maxKeyAgeEpochs` derives to 4 epochs.
    impGlobalsL %= \g -> g {maxKESEvo = 2, slotsPerKESPeriod = 4320}
    (spoKh, _, _) <- setupPoolWithStake $ Coin 3_000_000_000
    blsSk :: LeiosSigningKey <- liftGen genLeiosSigningKey
    let blsKey = BlsKey (deriveVerKeyDSIGN blsSk) (createPossessionProofDSIGN minSigPoPDST blsSk)
        poolBlsKeyInDistr = do
          distr <- getsNES nesPdL
          pure $ individualPoolStakeBls <$> Map.lookup spoKh (unPoolDistr distr)
    submitTx_ $
      mkBasicTx (mkBasicTxBody & certsTxBodyL .~ SSeq.fromList [RegBlsKeyTxCert spoKh blsKey])
    -- A key registered in epoch e enters the mark snapshot at the boundary to
    -- e+1 and the active distribution at e+2; it ages out of the distribution
    -- that becomes active at e+4.
    passNEpochs 2
    impAnn "key active two epochs after registration" $
      poolBlsKeyInDistr `shouldReturn` Just (SJust blsKey)
    passNEpochs 1
    impAnn "key still honoured in its last epoch" $
      poolBlsKeyInDistr `shouldReturn` Just (SJust blsKey)
    passNEpochs 1
    impAnn "key aged out, stake and seat remain" $
      poolBlsKeyInDistr `shouldReturn` Just SNothing

  xit "Subtransaction consumes correct refund after keyDeposit is changed" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    _ <- registerStakeCredential stakingCred

    initialKeyDeposit <- getsPParams ppKeyDepositL
    impAnn "Change key deposit" $ do
      (dRep, _, _) <- setupSingleDRep 100_000_000
      ccHotCreds <- registerInitialCommittee
      let newKeyDeposit = initialKeyDeposit <> initialKeyDeposit
      ppChangeId <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuKeyDepositL .~ SJust newKeyDeposit
      submitYesVote_ (DRepVoter dRep) ppChangeId
      submitYesVoteCCs_ ccHotCreds ppChangeId
      getsPParams ppKeyDepositL `shouldReturn` initialKeyDeposit
      passNEpochs 2
      getsPParams ppKeyDepositL `shouldReturn` newKeyDeposit

    impAnn "Unregister staking credential" $ do
      expectStakeCredRegistered stakingCred
      let
        deRegCert = UnRegDepositTxCert stakingCred initialKeyDeposit
        subTransaction =
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ SSeq.singleton deRegCert
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton subTransaction
      expectStakeCredNotRegistered stakingCred

  xit "Multiple subtransactions cannot get the same refund" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    _ <- registerStakeCredential stakingCred
    keyDeposit <- getsPParams ppKeyDepositL
    value1 <- arbitrary
    (_, addr1) <- freshKeyAddr
    input1 <- sendCoinTo addr1 value1
    value2 <- arbitrary
    (_, addr2) <- freshKeyAddr
    input2 <- sendCoinTo addr2 value2
    let
      subTx1 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton input1
          & bodyTxL . certsTxBodyL .~ SSeq.singleton (UnRegDepositTxCert stakingCred keyDeposit)
      subTx2 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton input2
          & bodyTxL . certsTxBodyL .~ SSeq.singleton (UnRegDepositTxCert stakingCred keyDeposit)
      tx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable [subTx1, subTx2]
    submitFailingTx tx . NE.singleton $ error "TODO: predicate failure not yet implemented"
