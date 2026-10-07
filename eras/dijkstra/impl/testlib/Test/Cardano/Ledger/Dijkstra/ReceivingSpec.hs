{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.ReceivingSpec (spec) where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Binary (decodeFull, serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxBody (
  receivingKeyHashes,
  receivingScriptHashes,
  receivingScriptTargets,
 )
import Cardano.Ledger.Dijkstra.UTxO (getDijkstraScriptsNeeded, getDijkstraWitsVKeyNeeded)
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Plutus (ExUnits (..))
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Word (Word32)
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()

spec :: Spec
spec = describe "Receiving" $ do
  describe "Purpose encoding" $ do
    let version = eraProtVerLow @DijkstraEra
        pointer = DijkstraReceiving (AsIx 3) :: DijkstraPlutusPurpose AsIx DijkstraEra
        item = DijkstraReceiving (AsItem 3) :: DijkstraPlutusPurpose AsItem DijkstraEra
        pointerBytes = BSL.pack [0x82, 0x07, 0x03]
        itemBytes = pointerBytes
    it "uses tag 7 without changing Guarding tag 6" $ do
      serialize version pointer `shouldBe` pointerBytes
      serialize version (DijkstraGuarding (AsIx 3) :: DijkstraPlutusPurpose AsIx DijkstraEra)
        `shouldBe` BSL.pack [0x82, 0x06, 0x03]
      decodeFull version pointerBytes `shouldBe` Right pointer
    it "encodes the original output index item using fixed bytes" $ do
      serialize version item `shouldBe` itemBytes
      decodeFull version itemBytes `shouldBe` Right item
    it "rejects receiving in Conway and rejects adjacent unknown tags" $ do
      decodeFull @(ConwayPlutusPurpose AsIx ConwayEra) (eraProtVerLow @ConwayEra) pointerBytes
        `shouldSatisfy` isLeft
      decodeFull @(DijkstraPlutusPurpose AsIx DijkstraEra) version (BSL.pack [0x82, 0x08, 0x03])
        `shouldSatisfy` isLeft
    it "hoists the resolved view to the existing index and item views" $ do
      let resolved = DijkstraReceiving (AsIxItem 3 3) :: DijkstraPlutusPurpose AsIxItem DijkstraEra
      hoistPlutusPurpose (\(AsIxItem purposeIndex _) -> AsIx purposeIndex) resolved `shouldBe` pointer
      hoistPlutusPurpose (\(AsIxItem _ sh) -> AsItem sh) resolved `shouldBe` item
    prop "preserves all upgraded Conway purpose bytes" $ \(purpose :: ConwayPlutusPurpose AsIx ConwayEra) ->
      serialize version (upgradePlutusPurposeAsIx @DijkstraEra purpose)
        `shouldBe` serialize (eraProtVerLow @ConwayEra) purpose
    prop "JSON item round trip" $ roundTripAesonProperty @(DijkstraPlutusPurpose AsItem DijkstraEra)
    prop "JSON index round trip" $ roundTripAesonProperty @(DijkstraPlutusPurpose AsIx DijkstraEra)
    prop "JSON index/item round trip" $
      roundTripAesonProperty @(DijkstraPlutusPurpose AsIxItem DijkstraEra)

  describe "Body-local targets" $ do
    let key = mkKeyHash 7
        protected sh stake = mkCoinTxOut @DijkstraEra (AddrProtected Testnet (ScriptHashObj sh) stake) (Coin 2)
        keyOut = mkCoinTxOut @DijkstraEra (AddrProtected Testnet (KeyHashObj key) StakeRefNull) (Coin 2)
        ordinary sh = mkCoinTxOut @DijkstraEra (Addr Testnet (ScriptHashObj sh) StakeRefNull) (Coin 2)
        outputs =
          SSeq.fromList
            [ protected highHash StakeRefNull
            , keyOut
            , protected lowHash StakeRefNull
            , protected highHash (StakeRefBase (KeyHashObj (mkKeyHash 9)))
            , ordinary otherHash
            , protected highHash StakeRefNull
            ]
        body = mkBasicTxBody @DijkstraEra @TopTx & outputsTxBodyL .~ outputs
        child =
          mkBasicTxBody @DijkstraEra @SubTx
            & outputsTxBodyL .~ SSeq.singleton (protected highHash StakeRefNull)
    it "retains output order and duplicates while excluding keys and unprotected outputs" $ do
      lowHash `shouldSatisfy` (< highHash)
      Set.toAscList (receivingScriptHashes body) `shouldBe` [lowHash, highHash]
      receivingKeyHashes body `shouldBe` Set.singleton key
      body ^. outputsTxBodyL `shouldBe` outputs
    it "uses original output indices for discovery and pointer inverses" $ do
      receivingScriptTargets body `shouldBe` [(0, highHash), (2, lowHash), (3, highHash), (5, highHash)]
      getDijkstraScriptsNeeded mempty body
        `shouldBe` AlonzoScriptsNeeded
          [ (ReceivingPurpose (AsIxItem 0 0), highHash)
          , (ReceivingPurpose (AsIxItem 2 2), lowHash)
          , (ReceivingPurpose (AsIxItem 3 3), highHash)
          , (ReceivingPurpose (AsIxItem 5 5), highHash)
          ]
      forM_ ([0, 2, 3, 5] :: [Word32]) $ \outputIndex -> do
        redeemerPointer body (ReceivingPurpose (AsItem outputIndex))
          `shouldBe` SJust (ReceivingPurpose (AsIx outputIndex))
        redeemerPointerInverse body (ReceivingPurpose (AsIx outputIndex))
          `shouldBe` SJust (ReceivingPurpose (AsIxItem outputIndex outputIndex))
      forM_ ([1, 4, 6, maxBound] :: [Word32]) $ \outputIndex -> do
        redeemerPointer body (ReceivingPurpose (AsItem outputIndex)) `shouldBe` SNothing
        redeemerPointerInverse body (ReceivingPurpose (AsIx outputIndex)) `shouldBe` SNothing
      redeemerPointerInverse body (ReceivingPurpose (AsIx maxBound)) `shouldBe` SNothing
    it "does not compress key, ordinary, native or duplicate output positions" $ do
      let nativeScript = RequireAllOf mempty :: NativeScript DijkstraEra
          nativeHash = hashScript @DijkstraEra (fromNativeScript nativeScript)
          mixedBody =
            mkBasicTxBody @DijkstraEra @TopTx
              & outputsTxBodyL
                .~ SSeq.fromList
                  [ protected highHash StakeRefNull
                  , keyOut
                  , protected nativeHash StakeRefNull
                  , ordinary lowHash
                  , protected highHash StakeRefNull
                  ]
      serialize (eraProtVerLow @DijkstraEra) nativeScript `shouldBe` BSL.pack [0x82, 0x01, 0x80]
      receivingScriptTargets mixedBody `shouldBe` [(0, highHash), (2, nativeHash), (4, highHash)]
      forM_ ([0, 2, 4] :: [Word32]) $ \outputIndex -> do
        redeemerPointer mixedBody (ReceivingPurpose (AsItem outputIndex))
          `shouldBe` SJust (ReceivingPurpose (AsIx outputIndex))
        redeemerPointerInverse mixedBody (ReceivingPurpose (AsIx outputIndex))
          `shouldBe` SJust (ReceivingPurpose (AsIxItem outputIndex outputIndex))
      forM_ ([1, 3] :: [Word32]) $ \outputIndex ->
        redeemerPointer mixedBody (ReceivingPurpose (AsItem outputIndex)) `shouldBe` SNothing
    it "has independent parent and child domains" $ do
      redeemerPointer child (ReceivingPurpose (AsItem 0))
        `shouldBe` SJust (ReceivingPurpose (AsIx 0))
      receivingScriptTargets child `shouldBe` [(0, highHash)]
      redeemerPointer body (ReceivingPurpose (AsItem 3)) `shouldBe` SJust (ReceivingPurpose (AsIx 3))
      redeemerPointer child (ReceivingPurpose (AsItem 3)) `shouldBe` SNothing
      receivingScriptHashes (mkBasicTxBody @DijkstraEra @TopTx) `shouldBe` Set.empty
    it "excludes collateral returns" $ do
      receivingScriptHashes
        ( body
            & outputsTxBodyL .~ mempty
            & collateralReturnTxBodyL .~ SJust (protected otherHash StakeRefNull)
        )
        `shouldBe` Set.empty
    it "requires protected payment keys once, without adding guards" $ do
      getDijkstraWitsVKeyNeeded mempty body `shouldBe` Set.singleton (asWitness key)
      body ^. guardsTxBodyL `shouldBe` mempty
    prop "keeps pointers local to output positions under permutations and duplication" $ \(hashes :: [ScriptHash]) -> do
      let mkBody xs =
            mkBasicTxBody @DijkstraEra @TopTx
              & outputsTxBodyL .~ SSeq.fromList [protected sh StakeRefNull | sh <- xs]
          authored = mkBody hashes
          reversed = mkBody (reverse hashes)
      receivingScriptHashes authored `shouldBe` receivingScriptHashes reversed
      receivingScriptTargets authored `shouldBe` zip [0 ..] hashes
      receivingScriptTargets reversed `shouldBe` zip [0 ..] (reverse hashes)
      forM_ (zip ([0 ..] :: [Word32]) hashes) $ \(outputIndex, _) -> do
        forM_ [authored, reversed] $ \localBody -> do
          redeemerPointer localBody (ReceivingPurpose (AsItem outputIndex))
            `shouldBe` SJust (ReceivingPurpose (AsIx outputIndex))
          redeemerPointerInverse localBody (ReceivingPurpose (AsIx outputIndex))
            `shouldBe` SJust (ReceivingPurpose (AsIxItem outputIndex outputIndex))

  prop "aggregates Receiving budgets over the parent and all children" $ \redeemerData -> do
    let pointer = ReceivingPurpose (AsIx 0)
        attach budget tx = tx & witsTxL . rdmrsTxWitsL . unRedeemersL .~ Map.singleton pointer (redeemerData, budget)
        child1 =
          attach (ExUnits 2 20) $
            mkBasicTx (mkBasicTxBody @DijkstraEra @SubTx & treasuryDonationTxBodyL .~ Coin 1)
        child2 =
          attach (ExUnits 3 30) $
            mkBasicTx (mkBasicTxBody @DijkstraEra @SubTx & treasuryDonationTxBodyL .~ Coin 2)
        batch =
          mkBasicTx
            (mkBasicTxBody @DijkstraEra @TopTx & subTransactionsTxBodyL .~ OMap.fromFoldable [child1, child2])
            & witsTxL . rdmrsTxWitsL . unRedeemersL
              .~ Map.fromList
                [ (pointer, (redeemerData, ExUnits 1 10))
                , (ReceivingPurpose (AsIx 2), (redeemerData, ExUnits 4 40))
                ]
    getTotalExUnits batch `shouldBe` ExUnits 10 100

-- Deliberately authored independently of the ledger encoder.
lowHash, highHash, otherHash :: ScriptHash
lowHash = ScriptHash (fromJust (Hash.hashFromBytes (BS.replicate 28 0)))
highHash = ScriptHash (fromJust (Hash.hashFromBytes (BS.replicate 28 1)))
otherHash = ScriptHash (fromJust (Hash.hashFromBytes (BS.replicate 28 2)))
