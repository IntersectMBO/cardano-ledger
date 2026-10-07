{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.ReceivingSpec (spec) where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address (Addr (..))
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
import Cardano.Ledger.Dijkstra.TxBody (receivingKeyHashes, receivingScriptHashes)
import Cardano.Ledger.Dijkstra.UTxO (getDijkstraScriptsNeeded, getDijkstraWitsVKeyNeeded)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Plutus (ExUnits (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
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
        item = DijkstraReceiving (AsItem lowHash) :: DijkstraPlutusPurpose AsItem DijkstraEra
        pointerBytes = BSL.pack [0x82, 0x07, 0x03]
        itemBytes = BSL.pack ([0x82, 0x07, 0x58, 0x1c] <> replicate 28 0)
    it "uses tag 7 without changing Guarding tag 6" $ do
      serialize version pointer `shouldBe` pointerBytes
      serialize version (DijkstraGuarding (AsIx 3) :: DijkstraPlutusPurpose AsIx DijkstraEra)
        `shouldBe` BSL.pack [0x82, 0x06, 0x03]
      decodeFull version pointerBytes `shouldBe` Right pointer
    it "encodes the hash item using fixed bytes" $ do
      serialize version item `shouldBe` itemBytes
      decodeFull version itemBytes `shouldBe` Right item
    it "rejects receiving in Conway and rejects adjacent unknown tags" $ do
      decodeFull @(ConwayPlutusPurpose AsIx ConwayEra) (eraProtVerLow @ConwayEra) pointerBytes
        `shouldSatisfy` isLeft
      decodeFull @(DijkstraPlutusPurpose AsIx DijkstraEra) version (BSL.pack [0x82, 0x08, 0x03])
        `shouldSatisfy` isLeft
    it "hoists the resolved view to the existing index and item views" $ do
      let resolved = DijkstraReceiving (AsIxItem 3 lowHash) :: DijkstraPlutusPurpose AsIxItem DijkstraEra
      hoistPlutusPurpose (\(AsIxItem ix _) -> AsIx ix) resolved `shouldBe` pointer
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
            ]
        body = mkBasicTxBody @DijkstraEra @TopTx & outputsTxBodyL .~ outputs
        child =
          mkBasicTxBody @DijkstraEra @SubTx
            & outputsTxBodyL .~ SSeq.singleton (protected highHash StakeRefNull)
    it "sorts by canonical hash bytes, groups differing stakes and excludes keys/unprotected outputs" $ do
      lowHash `shouldSatisfy` (< highHash)
      Set.toAscList (receivingScriptHashes body) `shouldBe` [lowHash, highHash]
      receivingKeyHashes body `shouldBe` Set.singleton key
      body ^. outputsTxBodyL `shouldBe` outputs
    it "uses one canonical domain for discovery and pointer inverses" $ do
      getDijkstraScriptsNeeded mempty body
        `shouldBe` AlonzoScriptsNeeded
          [ (ReceivingPurpose (AsIxItem 0 lowHash), lowHash)
          , (ReceivingPurpose (AsIxItem 1 highHash), highHash)
          ]
      redeemerPointer body (ReceivingPurpose (AsItem highHash))
        `shouldBe` SJust (ReceivingPurpose (AsIx 1))
      redeemerPointerInverse body (ReceivingPurpose (AsIx 1))
        `shouldBe` SJust (ReceivingPurpose (AsIxItem 1 highHash))
      redeemerPointer body (ReceivingPurpose (AsItem otherHash)) `shouldBe` SNothing
      redeemerPointerInverse body (ReceivingPurpose (AsIx 2)) `shouldBe` SNothing
      redeemerPointerInverse body (ReceivingPurpose (AsIx maxBound)) `shouldBe` SNothing
    it "retains a native hash before the Plutus fixture hash in the Receiving domain" $ do
      let nativeScript = RequireAllOf mempty :: NativeScript DijkstraEra
          nativeHash = hashScript @DijkstraEra (fromNativeScript nativeScript)
          fixedNativeHash =
            ScriptHash
              ( fromJust
                  ( Hash.hashFromBytes
                      ( BS.pack
                          [ 0xd4
                          , 0x41
                          , 0x22
                          , 0x75
                          , 0x53
                          , 0xa0
                          , 0xf1
                          , 0xa9
                          , 0x65
                          , 0xfe
                          , 0xe7
                          , 0xd6
                          , 0x0a
                          , 0x0f
                          , 0x72
                          , 0x4b
                          , 0x36
                          , 0x8d
                          , 0xd1
                          , 0xbd
                          , 0xdb
                          , 0xc2
                          , 0x08
                          , 0x73
                          , 0x0f
                          , 0xcc
                          , 0xeb
                          , 0xcf
                          ]
                      )
                  )
              )
          fixedPlutusHash =
            ScriptHash
              ( fromJust
                  ( Hash.hashFromBytes
                      ( BS.pack
                          [ 0xdf
                          , 0x3c
                          , 0x23
                          , 0x78
                          , 0x6b
                          , 0xd8
                          , 0x47
                          , 0x3d
                          , 0xcf
                          , 0x09
                          , 0x9b
                          , 0x4b
                          , 0x34
                          , 0xb9
                          , 0xd0
                          , 0x49
                          , 0x44
                          , 0x70
                          , 0x1b
                          , 0x65
                          , 0xd3
                          , 0x69
                          , 0xa5
                          , 0x30
                          , 0x52
                          , 0xa2
                          , 0x81
                          , 0x9c
                          ]
                      )
                  )
              )
          mixedBody =
            mkBasicTxBody @DijkstraEra @TopTx
              & outputsTxBodyL
                .~ SSeq.fromList [protected fixedPlutusHash StakeRefNull, protected nativeHash StakeRefNull, keyOut]
      serialize (eraProtVerLow @DijkstraEra) nativeScript `shouldBe` BSL.pack [0x82, 0x01, 0x80]
      nativeHash `shouldBe` fixedNativeHash
      redeemerPointer mixedBody (ReceivingPurpose (AsItem fixedPlutusHash))
        `shouldBe` SJust (ReceivingPurpose (AsIx 1))
      redeemerPointerInverse mixedBody (ReceivingPurpose (AsIx 0))
        `shouldBe` SJust (ReceivingPurpose (AsIxItem 0 nativeHash))
    it "has independent parent and child domains" $ do
      redeemerPointer child (ReceivingPurpose (AsItem highHash))
        `shouldBe` SJust (ReceivingPurpose (AsIx 0))
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
    prop "preserves canonical domains and inverse pairs under output permutations" $ \(hashes :: [ScriptHash]) -> do
      let mkBody xs =
            mkBasicTxBody @DijkstraEra @TopTx
              & outputsTxBodyL .~ SSeq.fromList [protected sh StakeRefNull | sh <- xs]
          authored = mkBody hashes
          reversed = mkBody (reverse hashes)
      receivingScriptHashes authored `shouldBe` receivingScriptHashes reversed
      forM_ (zip [0 ..] (Set.toAscList (receivingScriptHashes authored))) $ \(idx, sh) -> do
        redeemerPointer authored (ReceivingPurpose (AsItem sh))
          `shouldBe` SJust (ReceivingPurpose (AsIx idx))
        redeemerPointerInverse reversed (ReceivingPurpose (AsIx idx))
          `shouldBe` SJust (ReceivingPurpose (AsIxItem idx sh))

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
          attach (ExUnits 1 10) $
            mkBasicTx
              (mkBasicTxBody @DijkstraEra @TopTx & subTransactionsTxBodyL .~ OMap.fromFoldable [child1, child2])
    getTotalExUnits batch `shouldBe` ExUnits 6 60

-- Deliberately authored independently of the ledger encoder and Set ordering.
lowHash, highHash, otherHash :: ScriptHash
lowHash = ScriptHash (fromJust (Hash.hashFromBytes (BS.replicate 28 0)))
highHash = ScriptHash (fromJust (Hash.hashFromBytes (BS.replicate 28 1)))
otherHash = ScriptHash (fromJust (Hash.hashFromBytes (BS.replicate 28 2)))
