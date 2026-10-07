{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (Mismatch (..), Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import qualified Cardano.Ledger.Dijkstra.Rules as Dijkstra
import Cardano.Ledger.Keys (asWitness, witVKeyHash)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  Language (..),
  OrdExUnits (..),
  Plutus,
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.State (EraUTxO (..), UTxO (..), utxoG)
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
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
  mempoolSpec @era

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
        underpaid =
          fixed
            & bodyTxL . feeTxBodyL .~ Coin 0
            & bodyTxL . outputsTxBodyL %~ \case
              SSeq.Empty -> SSeq.Empty
              rest SSeq.:|> change -> rest SSeq.:|> (change & coinTxOutL @era %~ (<> fee))
    bad <- rederiveAddrTxWits underpaid
    pp <- getsPParams id
    utxo <- getUTxO
    beforeUtxo <- getUTxO
    withNoFixup $
      submitFailingTx
        bad
        [injectFailure $ Dijkstra.FeeTooSmallUTxO $ Mismatch (Coin 0) (getMinFeeTxUtxo pp bad utxo)]
    getUTxO >>= (`shouldBe` beforeUtxo)

  it "binds the Receiving redeemer and budget into the script integrity hash" $ do
    fixed <- fixupTx =<< receivingTx (alwaysSucceedsNoDatum SPlutusV4)
    let mutations :: [(Data era, ExUnits) -> (Data era, ExUnits)]
        mutations =
          [ \(_, units) -> (Data @era (P.I 1), units)
          , \(datum, _) -> (datum, ExUnits 4_999_999 2_000_000_000)
          ]
    forM_ mutations $ \mutate -> do
      bad <- rederiveAddrTxWits $ fixed & witsTxL . rdmrsTxWitsL . unRedeemersL %~ fmap mutate
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
    let twoOutputs =
          tx
            & bodyTxL . outputsTxBodyL %~ \case
              SSeq.Empty -> SSeq.Empty
              firstOutput SSeq.:<| _ -> SSeq.fromList [firstOutput, firstOutput]
        eachBudget = ExUnits 3_000_000 1_000_000_000
        authored =
          Map.fromList
            [ (ReceivingPurpose (AsIx 0), (Data @era (P.I 2), eachBudget))
            , (ReceivingPurpose (AsIx 1), (Data @era (P.I 4), eachBudget))
            ]
    fixed <- fixupTx (twoOutputs & witsTxL . rdmrsTxWitsL . unRedeemersL .~ authored)
    fixed ^. witsTxL . rdmrsTxWitsL . unRedeemersL `shouldBe` authored
    let limit = ExUnits 4_000_000 2_000_000_000
        supplied = ExUnits 6_000_000 2_000_000_000
    getTotalExUnits fixed `shouldBe` supplied
    modifyPParams $ ppMaxTxExUnitsL .~ limit
    withNoFixup $
      submitFailingTx
        fixed
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
    forM_ ([0 .. ordinaryCount - 1] :: [Int]) $ \outputIndex -> Map.member (txInAt outputIndex failed) final `shouldBe` False

  it "rejects Receiving collateral whose return omits a native asset" $ do
    (tx, collateral, _) <- tokenCollateralTx
    collateralValue <- (^. valueTxOutL) <$> impGetUTxO collateral
    let omitAsset out = out & valueTxOutL .~ MaryValue (out ^. coinTxOutL) mempty
    beforeUtxo <- getUTxO
    submitFailingTx
      (tx & isPhase2ValidTxL .~ Phase2Invalid & bodyTxL . collateralReturnTxBodyL %~ fmap omitAsset)
      [injectFailure $ Dijkstra.CollateralContainsNonADA collateralValue]
    getUTxO >>= (`shouldBe` beforeUtxo)

-- | Mempool admission invokes the concrete ledger transition; its returned
-- state is checked independently of the Imp state's confirmed transactions.
mempoolSpec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
mempoolSpec = describe "Receiving mempool admission" $ do
  it "admits a witnessed Receiving transaction and produces its protected output" $ do
    fixed <- fixupTx =<< receivingTx (alwaysSucceedsNoDatum SPlutusV4)
    beforeUtxo <- getUTxO
    (state, _) <- withNoFixup $ expectRight =<< trySubmitMempoolTx fixed
    let UTxO entries = state ^. utxoG
    Map.lookup (txInAt 0 fixed) entries `shouldBe` SSeq.lookup 0 (fixed ^. bodyTxL . outputsTxBodyL)
    getUTxO >>= (`shouldBe` beforeUtxo)

  it "rejects a Receiving destination key's missing witness at mempool admission" $ do
    key <- freshKeyHash @Payment
    let protected = AddrProtected Testnet (KeyHashObj key) StakeRefNull
    fixed <-
      fixupTx $ mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [mkCoinTxOut protected (Coin 3_000_000)]
    let bad = fixed & witsTxL . addrTxWitsL %~ Set.filter ((/= asWitness key) . witVKeyHash)
    beforeUtxo <- getUTxO
    withNoFixup $
      submitFailingMempoolTx
        bad
        [ Dijkstra.LedgerFailure $
            injectFailure $
              Dijkstra.MissingVKeyWitnessesUTXOW (NES.singleton (asWitness key))
        ]
    getUTxO >>= (`shouldBe` beforeUtxo)

receivingTx :: forall era. DijkstraEraImp era => Plutus 'PlutusV4 -> ImpTestM era (Tx TopTx era)
receivingTx plutus = do
  script <- fromPlutusScript <$> mkPlutusScript plutus
  referenceAddress <- freshKeyAddrNoPtr_
  pp <- getsPParams id
  let referenceOutput =
        ensureMinCoinTxOut pp $
          mkCoinTxOut referenceAddress (Coin 3_000_000) & referenceScriptTxOutL .~ SJust script
  referenceTx <-
    submitTx $
      mkBasicTx $
        mkBasicTxBody
          & outputsTxBodyL
            .~ [referenceOutput]
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
