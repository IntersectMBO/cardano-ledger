{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.ReceivingAccountingSpec (
  spec,
  accountingSpec,
  transactionBudgetSpec,
  tokenCollateralSpec,
) where

import Cardano.Ledger.Address (Addr (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (Mismatch (..), Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import qualified Cardano.Ledger.Dijkstra.Rules as Dijkstra
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  OrdExUnits (..),
  Plutus,
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.State (EraUTxO (..), UTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro
import qualified PlutusLedgerApi.V1 as P
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysFailsNoDatum, alwaysSucceedsNoDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "CIP-160 Receiving accounting" $ do
  accountingSpec @era
  tokenCollateralSpec @era

accountingSpec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
accountingSpec = describe "Fees, integrity and execution budgets" $ do
  feeIntegritySpec @era
  transactionBudgetSpec @era
  blockBudgetSpec @era

-- | Fee and integrity callbacks are abstract in the executable formal model.
-- These cases therefore require the concrete ledger's pricing and hashes.
feeIntegritySpec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
feeIntegritySpec = do
  it "includes Receiving execution units in the minimum fee" $ do
    fixed <- fixupTx =<< receivingTx (alwaysSucceedsNoDatum SPlutusV4)
    pp <- getsPParams id
    utxo <- getUTxO
    -- Both budgets use the same CBOR integer widths, so the difference comes
    -- from execution prices rather than a shorter witness encoding.
    let lowerBudget =
          fixed
            & witsTxL . rdmrsTxWitsL . unRedeemersL
              %~ fmap (\(datum, _) -> (datum, ExUnits 4_000_000 1_000_000_000))
    getMinFeeTxUtxo pp fixed utxo `shouldSatisfy` (> getMinFeeTxUtxo pp lowerBudget utxo)

  it "rejects a Receiving transaction whose fee is removed after balancing" $ do
    fixed <- fixupTx =<< receivingTx (alwaysSucceedsNoDatum SPlutusV4)
    let fee = fixed ^. bodyTxL . feeTxBodyL
        changeIndex = SSeq.length (fixed ^. bodyTxL . outputsTxBodyL) - 1
        underpaid =
          fixed
            & bodyTxL . feeTxBodyL .~ Coin 0
            & bodyTxL . outputsTxBodyL . ix changeIndex . coinTxOutL %~ (<> fee)
    bad <- rederiveAddrTxWits underpaid
    pp <- getsPParams id
    utxo <- getUTxO
    before <- getUTxO
    withNoFixup $
      submitFailingTx
        bad
        [injectFailure $ Dijkstra.FeeTooSmallUTxO $ Mismatch (Coin 0) (getMinFeeTxUtxo pp bad utxo)]
    getUTxO >>= (`shouldBe` before)

  it "binds the Receiving redeemer and budget into the script integrity hash" $ do
    fixed <- fixupTx =<< receivingTx (alwaysSucceedsNoDatum SPlutusV4)
    bad <-
      rederiveAddrTxWits $
        fixed & witsTxL . rdmrsTxWitsL . unRedeemersL %~ fmap (\(_, units) -> (Data (P.I 1), units))
    expected <- computeScriptIntegrityHash bad
    integrity <- impComputeScriptIntegrity bad
    withNoFixup $
      submitFailingTx
        bad
        [ injectFailure $
            Dijkstra.ScriptIntegrityHashMismatch
              (Mismatch (bad ^. bodyTxL . scriptIntegrityHashTxBodyL) expected)
              (originalBytes <$> integrity)
        ]

transactionBudgetSpec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
transactionBudgetSpec =
  it "includes Receiving in the transaction execution-unit limit" $ do
    tx <- receivingTx (alwaysSucceedsNoDatum SPlutusV4)
    let limit = ExUnits 4_000_000 2_000_000_000
        supplied = ExUnits 5_000_000 2_000_000_000
    modifyPParams $ ppMaxTxExUnitsL .~ limit
    submitFailingTx
      tx
      [injectFailure $ Alonzo.ExUnitsTooBigUTxO $ Mismatch (OrdExUnits supplied) (OrdExUnits limit)]

-- | This is a BBODY assertion, beyond the current LEDGER conformance hook.
blockBudgetSpec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
blockBudgetSpec =
  it "includes Receiving in the block execution-unit limit" $ do
    first <- receivingTx (alwaysSucceedsNoDatum SPlutusV4)
    second <- receivingTx (alwaysSucceedsNoDatum SPlutusV4)
    let limit = ExUnits 9_000_000 4_000_000_000
        supplied = ExUnits 10_000_000 4_000_000_000
    modifyPParams $ ppMaxBlockExUnitsL .~ limit
    withTxsInFailingBlock
      (submitTx_ first >> submitTx_ second)
      [injectFailure $ Dijkstra.TooManyExUnits $ Mismatch (OrdExUnits supplied) (OrdExUnits limit)]

-- | The executable conformance model projects values to Coin. These two ledger
-- tests complement its collateral equation with concrete native-asset algebra.
tokenCollateralSpec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
tokenCollateralSpec = describe "Native assets in Receiving collateral" $ do
  it "returns every native asset while collecting only ADA collateral on Receiving failure" $ do
    (tx, collateral, returned) <- tokenCollateralTx
    failed <- submitPhase2Invalid tx
    UTxO final <- getUTxO
    Map.member collateral final `shouldBe` False
    let ordinaryCount = SSeq.length (failed ^. bodyTxL . outputsTxBodyL)
    fmap (^. valueTxOutL) (Map.lookup (txInAt ordinaryCount failed) final) `shouldBe` Just returned
    forM_ [0 .. ordinaryCount - 1] $ \outputIndex -> Map.member (txInAt outputIndex failed) final `shouldBe` False

  it "rejects Receiving collateral whose return omits a native asset" $ do
    (tx, collateral, _) <- tokenCollateralTx
    collateralValue <- (^. valueTxOutL) <$> impGetUTxO collateral
    let omitAsset out = out & valueTxOutL .~ MaryValue (out ^. coinTxOutL) mempty
    before <- getUTxO
    submitFailingTx
      (tx & isPhase2ValidTxL .~ Phase2Invalid & bodyTxL . collateralReturnTxBodyL %~ fmap omitAsset)
      [injectFailure $ Dijkstra.CollateralContainsNonADA collateralValue]
    getUTxO >>= (`shouldBe` before)

receivingTx :: forall era. DijkstraEraImp era => Plutus 'PlutusV4 -> ImpTestM era (Tx TopTx era)
receivingTx plutus = do
  script <- fromPlutusScript <$> mkPlutusScript plutus
  referenceAddress <- freshKeyAddrNoPtr_
  referenceTx <-
    submitTx $
      mkBasicTx $
        mkBasicTxBody
          & outputsTxBodyL
            .~ [mkCoinTxOut referenceAddress (Coin 3_000_000) & referenceScriptTxOutL .~ SJust script]
  collateral <- makeCollateralInput
  let protected = AddrProtected Testnet (ScriptHashObj (hashPlutusScript plutus)) StakeRefNull
  pure $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL .~ [mkCoinTxOut protected (Coin 3_000_000)]
      & bodyTxL . referenceInputsTxBodyL .~ [txInAt 0 referenceTx]
      & bodyTxL . collateralInputsTxBodyL .~ [collateral]
      & witsTxL . rdmrsTxWitsL . unRedeemersL
        .~ Map.singleton (ReceivingPurpose (AsIx 0)) (Data (P.I 0), ExUnits 5_000_000 2_000_000_000)

tokenCollateralTx :: forall era. DijkstraEraImp era => ImpTestM era (Tx TopTx era, TxIn, Value era)
tokenCollateralTx = do
  policy <- impAddNativeScript (RequireAllOf [])
  address <- freshKeyAddrNoPtr_
  let asset = MultiAsset (Map.singleton (PolicyID policy) (Map.singleton (AssetName "collateral") 1))
      returned = MaryValue (Coin 10_000_000) asset
  minted <-
    submitTx $
      mkBasicTx $
        mkBasicTxBody
          & mintTxBodyL .~ asset
          & outputsTxBodyL .~ [mkBasicTxOut address (MaryValue (Coin 20_000_000) asset)]
  tx <- receivingTx (alwaysFailsNoDatum SPlutusV4)
  let collateral = txInAt 0 minted
  pure
    ( tx
        & bodyTxL . collateralInputsTxBodyL .~ [collateral]
        & bodyTxL . collateralReturnTxBodyL .~ SJust (mkBasicTxOut address returned)
        & bodyTxL . totalCollateralTxBodyL .~ SJust (Coin 10_000_000)
    , collateral
    , returned
    )
