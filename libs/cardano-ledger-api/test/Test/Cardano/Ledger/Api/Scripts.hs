{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Scripts (spec) where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx
import Cardano.Ledger.BaseTypes (Globals (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (TxLevel (..), emptyPParams)
import Cardano.Ledger.Dijkstra.Core (subTransactionsTxBodyL)
import Cardano.Ledger.Plutus (ExUnits (..))
import Data.Aeson (object, toJSON, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import Lens.Micro
import Test.Cardano.Ledger.Api.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (testGlobals)

spec :: Spec
spec = describe "Receiving purpose API" $ do
  it "projects Receiving in Dijkstra and distinguishes Guarding" $ do
    let receiving = ReceivingPurpose (AsIx 4) :: PlutusPurpose AsIx DijkstraEra
        guarding = GuardingPurpose (AsIx 4) :: PlutusPurpose AsIx DijkstraEra
    toJSON receiving
      `shouldBe` object ["kind" .= ("DijkstraReceiving" :: String), "value" .= object ["index" .= (4 :: Int)]]
    anyEraToReceivingPurpose @DijkstraEra receiving `shouldBe` Just (AsIx 4)
    anyEraToGuardingPurpose @DijkstraEra receiving `shouldBe` Nothing
    anyEraToReceivingPurpose @DijkstraEra guarding `shouldBe` Nothing
    case receiving of
      AnyEraReceivingPurpose index -> index `shouldBe` AsIx 4
      _ -> expectationFailure "Public AnyEra receiving projection did not match"
  it "returns absent for Conway rather than fabricating a purpose" $ do
    let purpose = SpendingPurpose (AsIx 4) :: PlutusPurpose AsIx ConwayEra
    anyEraToReceivingPurpose @ConwayEra purpose `shouldBe` Nothing
  it "uses existing public pointer interfaces for empty receiving domains" $ do
    let pointer = ReceivingPurpose (AsIx 0) :: PlutusPurpose AsIx DijkstraEra
    redeemerPointerInverse (mkBasicTxBody @DijkstraEra @TopTx) pointer `shouldBe` SNothing

  prop "retains equal redeemer pointers from the parent and distinct children" $ \redeemerData -> do
    let pointer = ReceivingPurpose (AsIx 0) :: PlutusPurpose AsIx DijkstraEra
        attachRedeemer :: Tx l DijkstraEra -> Tx l DijkstraEra
        attachRedeemer tx = tx & witsTxL . rdmrsTxWitsL . unRedeemersL .~ Map.singleton pointer (redeemerData, ExUnits 0 0)
        child1 =
          attachRedeemer $ mkBasicTx (mkBasicTxBody @DijkstraEra @SubTx & treasuryDonationTxBodyL .~ Coin 1)
        child2 =
          attachRedeemer $ mkBasicTx (mkBasicTxBody @DijkstraEra @SubTx & treasuryDonationTxBodyL .~ Coin 2)
        batch =
          attachRedeemer $
            mkBasicTx
              (mkBasicTxBody @DijkstraEra @TopTx & subTransactionsTxBodyL .~ OMap.fromFoldable [child1, child2])
        report =
          evalDijkstraTxExUnits
            (emptyPParams @DijkstraEra)
            batch
            mempty
            (epochInfo testGlobals)
            (systemStart testGlobals)
        unknown = Left (RedeemerPointsToUnknownScriptHash pointer)
    Map.lookup (SNothing, pointer) report `shouldBe` Just unknown
    Map.lookup (SJust (txIdTx child1), pointer) report `shouldBe` Just unknown
    Map.lookup (SJust (txIdTx child2), pointer) report `shouldBe` Just unknown
    Map.size report `shouldBe` 3
