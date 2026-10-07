{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.ProtectedAddressSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  decNoShareCBOR,
  decodeFullDecoder,
  encodeMemPack,
  natVersion,
  serialize,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway.State (
  ConwayInstantStake,
  addConwayInstantStake,
  deleteConwayInstantStake,
 )
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxBody (receivingKeyHashes, receivingScriptHashes)
import Cardano.Ledger.Plutus (Datum (..))
import Cardano.Ledger.Shelley.UTxO (
  ShelleyScriptsNeeded (..),
  getShelleyScriptsNeeded,
  getShelleyWitsVKeyNeededNoGov,
 )
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxIn, mkTxInPartial)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)
import Test.Cardano.Ledger.Dijkstra.Arbitrary (
  genProtectedAddr,
  genProtectedCompactAddr,
  shrinkProtectedAddr,
 )
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()

spec :: Spec
spec = describe "Protected address storage and consumers" $ do
  let payment = KeyHashObj (mkKeyHash 11)
      stake = StakeRefBase (KeyHashObj (mkKeyHash 12))
      ordinary = Addr Testnet payment stake
      protected = AddrProtected Testnet payment stake
      ordinaryOut = mkCoinTxOut @DijkstraEra ordinary (Coin 20)
      protectedOut = mkCoinTxOut @DijkstraEra protected (Coin 20)
      version = eraProtVerLow @DijkstraEra
  prop "generates supported protected addresses with lossless compact identity" $
    forAll genProtectedAddr $ \address -> do
      fmap (\(protection, _, _, _) -> protection) (shelleyAddressView address) `shouldBe` Just Protected
      decompactAddr (compactAddr address) `shouldBe` address
      case address of
        AddrProtected _ _ (StakeRefPtr _) -> expectationFailure "Generated a protected pointer"
        AddrProtected _ _ _ -> pure ()
        _ -> expectationFailure "Generated an unprotected address"
  prop "generates protected compact addresses without widening ordinary Arbitrary" $
    forAll genProtectedCompactAddr $ \address ->
      fmap (\(protection, _, _, _) -> protection) (shelleyAddressView (decompactAddr address))
        `shouldBe` Just Protected
  prop "shrinking preserves Receiving targets and supported protected bytes" $
    forAll genProtectedAddr $ \address -> do
      let body destination =
            mkBasicTxBody @DijkstraEra @TopTx
              & outputsTxBodyL .~ SSeq.singleton (mkCoinTxOut destination (Coin 20))
      forM_ (shrinkProtectedAddr address) $ \smaller -> do
        receivingKeyHashes (body smaller) `shouldBe` receivingKeyHashes (body address)
        receivingScriptHashes (body smaller) `shouldBe` receivingScriptHashes (body address)
        getNetwork smaller `shouldBe` getNetwork address
        protectAddress smaller `shouldBe` Right smaller
        decompactAddr (compactAddr smaller) `shouldBe` smaller
  it "uses the compact fallback while ordinary ADA-only base outputs retain Addr28" $ do
    ordinaryOut ^. addrEitherTxOutL `shouldBe` Left ordinary
    protectedOut ^. addrEitherTxOutL `shouldBe` Right (compactAddr protected)
    (protectedOut & coinTxOutL .~ Coin 21) ^. addrTxOutL `shouldBe` protected
  it "accounts for actual bytes in minimum coin and transaction identity" $ do
    let pp = emptyPParams @DijkstraEra & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 4310)
        body out = mkBasicTxBody @DijkstraEra @TopTx & outputsTxBodyL .~ SSeq.singleton out
    BSL.length (serialize version protectedOut) `shouldBe` BSL.length (serialize version ordinaryOut)
    getMinCoinTxOut pp protectedOut `shouldBe` getMinCoinTxOut pp ordinaryOut
    txIdTxBody (body protectedOut) `shouldNotBe` txIdTxBody (body ordinaryOut)
  prop "preserves identity through construction, every field lens and both state encodings" $
    \(datum :: Datum DijkstraEra) (refScript :: StrictMaybe (Script DijkstraEra)) (value :: Value DijkstraEra) paymentCred stakingCred -> do
      forM_ [Testnet, Mainnet] $ \network ->
        forM_ [StakeRefNull, StakeRefBase stakingCred] $ \stakeRef -> do
          let address = AddrProtected network paymentCred stakeRef
              out = BabbageTxOut address value datum refScript :: TxOut DijkstraEra
              sameAddress updated = updated ^. addrTxOutL `shouldBe` address
          out ^. addrEitherTxOutL `shouldBe` Right (compactAddr address)
          sameAddress (out & valueTxOutL .~ value)
          sameAddress (out & coinTxOutL .~ Coin 24)
          sameAddress (out & datumTxOutL .~ datum)
          sameAddress (out & dataHashTxOutL .~ SNothing)
          sameAddress (out & referenceScriptTxOutL .~ refScript)
          sameAddress (out & addrEitherTxOutL .~ Right (compactAddr address))
          decodeFullDecoder version "transaction TxOut" decCBOR (serialize version out) `shouldBe` Right out
          -- A restored state does not inherit transaction protocol gating.
          decodeFullDecoder (natVersion @7) "stored CBOR TxOut" decNoShareCBOR (serialize version out)
            `shouldBe` Right out
          decodeFullDecoder
            (natVersion @7)
            "stored MemPack TxOut"
            decNoShareCBOR
            (serialize version (encodeMemPack out))
            `shouldBe` Right out
  it "saves and restores a seeded UTxO with exact protected identity" $
    withSystemTempFile "protected-utxo.cbor" $ \path handle -> do
      let input = mkTxInPartial (txIdTx (mkBasicTx (mkBasicTxBody @DijkstraEra @TopTx))) 0
          state = UTxO (Map.singleton input protectedOut)
      BSL.hPut handle (serialize version state)
      hClose handle
      bytes <- BSL.readFile path
      decodeFullDecoder (natVersion @7) "stored UTxO" decNoShareCBOR bytes `shouldBe` Right state
  prop "adds and removes the same base stake as an ordinary output" $ \(input :: TxIn) -> do
    let singleton out = UTxO (Map.singleton input out)
        emptyStake = mempty :: ConwayInstantStake DijkstraEra
        ordinaryStake = addConwayInstantStake (singleton ordinaryOut) emptyStake
        protectedStake = addConwayInstantStake (singleton protectedOut) emptyStake
    protectedStake `shouldBe` ordinaryStake
    deleteConwayInstantStake (singleton protectedOut) protectedStake `shouldBe` emptyStake
    addConwayInstantStake
      (singleton (mkCoinTxOut @DijkstraEra (AddrProtected Testnet payment StakeRefNull) (Coin 20)))
      emptyStake
      `shouldBe` emptyStake
  prop "retains ordinary script and key spending obligations in seeded state" $
    \(input :: TxIn) scriptHash -> do
      let scriptOut protection = mkCoinTxOut @DijkstraEra (protection Testnet (ScriptHashObj scriptHash) stake) (Coin 20)
          singleton out = UTxO (Map.singleton input out)
          body = mkBasicTxBody @DijkstraEra @TopTx & inputsTxBodyL .~ Set.singleton input
          collateralBody = mkBasicTxBody @DijkstraEra @TopTx & collateralInputsTxBodyL .~ Set.singleton input
      getShelleyScriptsNeeded (singleton (scriptOut AddrProtected)) body
        `shouldBe` ShelleyScriptsNeeded (Set.singleton scriptHash)
      getShelleyScriptsNeeded (singleton (scriptOut Addr)) body
        `shouldBe` ShelleyScriptsNeeded (Set.singleton scriptHash)
      getShelleyWitsVKeyNeededNoGov (singleton protectedOut) body
        `shouldBe` getShelleyWitsVKeyNeededNoGov (singleton ordinaryOut) body
      getShelleyWitsVKeyNeededNoGov (singleton protectedOut) collateralBody
        `shouldBe` getShelleyWitsVKeyNeededNoGov (singleton ordinaryOut) collateralBody
