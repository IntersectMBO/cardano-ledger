{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TransactionInteropSpec (spec) where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Binary (serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxBody (
  receivingKeyHashes,
  receivingScriptHashes,
  receivingScriptTargets,
 )
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Plutus (Data (Data), ExUnits (..), PlutusData (..), transScriptHash)
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict', withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Word (Word32)
import Lens.Micro
import Numeric (readHex)
import Paths_cardano_ledger_dijkstra (getDataFileName)
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkKeyPairWithSeed, mkWitnessVKey)
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()

-- These literal bytes were produced independently by cbor2 6.1.3 and
-- cryptography 41.0.7, without importing or executing ledger code. Constructing
-- typed ledger values before comparison avoids relying on memoized decode bytes.
-- The fixtures establish codec/signing agreement, not transaction admission.
spec :: Spec
spec = describe "Independent full-transaction serialization" $ beforeAll loadVectors $ do
  let version = eraProtVerLow @DijkstraEra
      sender = mkKeyPairWithSeed (BS.pack [0 .. 31]) :: KeyPair Payment
      recipient = mkKeyPairWithSeed (BS.pack [32 .. 63]) :: KeyPair Payment
      keyHash = hashKey (vKey recipient)
      native = fromNativeScript (RequireAllOf mempty :: NativeScript DijkstraEra)
      nativeHash = hashScript @DijkstraEra native
      input =
        mkTxInPartial (TxId (unsafeMakeSafeHash (fromJust (Hash.hashFromBytes (BS.replicate 32 0x11))))) 0
      receivingOutput payment coin = mkCoinTxOut @DijkstraEra (AddrProtected Testnet payment StakeRefNull) (Coin coin)
      bodyWith outputs =
        mkBasicTxBody @DijkstraEra @TopTx
          & inputsTxBodyL
            .~ Set.singleton input
          & outputsTxBodyL
            .~ SSeq.fromList outputs
          & feeTxBodyL
            .~ Coin 10
      nativeBody =
        bodyWith
          [ receivingOutput (ScriptHashObj nativeHash) 5
          , receivingOutput (KeyHashObj keyHash) 7
          , receivingOutput (ScriptHashObj nativeHash) 8
          ]
      bodyHash = unTxId (txIdTxBody nativeBody)
      unsigned = mkBasicTx nativeBody
      partial =
        unsigned
          & witsTxL
            . scriptTxWitsL
            .~ Map.singleton nativeHash native
          & witsTxL
            . addrTxWitsL
            .~ Set.singleton (mkWitnessVKey bodyHash sender)
      signed =
        partial
          & witsTxL
            . addrTxWitsL
            %~ Set.insert (mkWitnessVKey bodyHash recipient)
      lowHash = ScriptHash (fromJust (Hash.hashFromBytes (BS.replicate 28 0)))
      highHash = ScriptHash (fromJust (Hash.hashFromBytes (BS.replicate 28 0xff)))
      redeemerBody =
        bodyWith
          [ receivingOutput (ScriptHashObj highHash) 5
          , receivingOutput (KeyHashObj keyHash) 7
          , receivingOutput (ScriptHashObj lowHash) 8
          , receivingOutput (ScriptHashObj highHash) 9
          , receivingOutput (ScriptHashObj highHash) 5
          ]
      redeemerTx =
        mkBasicTx redeemerBody
          & witsTxL
            . rdmrsTxWitsL
            . unRedeemersL
            .~ Map.fromList
              [ (ReceivingPurpose (AsIx 0), (Data (P.I 1), ExUnits 100 200))
              , (ReceivingPurpose (AsIx 2), (Data (P.I 2), ExUnits 300 400))
              , (ReceivingPurpose (AsIx 3), (Data (P.I 3), ExUnits 500 600))
              , (ReceivingPurpose (AsIx 4), (Data (P.I 3), ExUnits 700 800))
              ]
  it "agrees on the body bytes and body hash" $ \(vectors, _) -> do
    let nativeVector = getVector "signed-native-and-key-receiving" vectors
        redeemerVector = getVector "receiving-redeemer-map-codec-only" vectors
    serialize version nativeBody `shouldBe` hex (vectorBody nativeVector)
    Hash.hashToBytes (extractHash bodyHash) `shouldBe` BSL.toStrict (hex (vectorTxId nativeVector))
    serialize version redeemerBody `shouldBe` hex (vectorBody redeemerVector)
    Hash.hashToBytes (extractHash (unTxId (txIdTxBody redeemerBody)))
      `shouldBe` BSL.toStrict (hex (vectorTxId redeemerVector))
  it "agrees on unsigned, partial and two-key signed transaction bytes" $ \(vectors, _) -> do
    hashKey (vKey sender) `shouldNotBe` keyHash
    serialize version unsigned
      `shouldBe` hex (vectorTransaction (getVector "native-body-before-witness-assembly" vectors))
    serialize version partial
      `shouldBe` hex (vectorTransaction (getVector "native-body-with-spending-signature" vectors))
    serialize version signed
      `shouldBe` hex (vectorTransaction (getVector "signed-native-and-key-receiving" vectors))
    txIdTxBody (signed ^. bodyTxL) `shouldBe` txIdTxBody (unsigned ^. bodyTxL)
    receivingKeyHashes nativeBody `shouldBe` Set.singleton keyHash
  it "agrees on tag 7 bytes and raw pointers for differing and byte-identical outputs" $ \(vectors, _) -> do
    forM_
      [ (nativeBody, getVector "signed-native-and-key-receiving" vectors)
      , (redeemerBody, getVector "receiving-redeemer-map-codec-only" vectors)
      ]
      $ \(body, referenceVector) -> do
        receivingScriptTargets body
          `shouldBe` [ (pointerOutputIndex pointer, scriptHashFromHex $ pointerScriptHash pointer)
                     | pointer <- vectorPointers referenceVector
                     ]
        forM_ (vectorPointers referenceVector) $ \pointer -> do
          let outputIndex = pointerOutputIndex pointer
          pointerBytes pointer `shouldBe` [7, outputIndex]
          redeemerPointer body (ReceivingPurpose (AsItem outputIndex))
            `shouldBe` SJust (ReceivingPurpose (AsIx outputIndex))
          redeemerPointerInverse body (ReceivingPurpose (AsIx outputIndex))
            `shouldBe` SJust (ReceivingPurpose (AsIxItem outputIndex outputIndex))
    serialize version redeemerTx
      `shouldBe` hex (vectorTransaction (getVector "receiving-redeemer-map-codec-only" vectors))
    receivingScriptHashes redeemerBody `shouldBe` Set.fromList [lowHash, highHash]
    receivingScriptTargets redeemerBody
      `shouldBe` [(0, highHash), (2, lowHash), (3, highHash), (4, highHash)]
    SSeq.lookup 0 (redeemerBody ^. outputsTxBodyL)
      `shouldBe` SSeq.lookup 4 (redeemerBody ^. outputsTxBodyL)
    redeemerPointer redeemerBody (ReceivingPurpose (AsItem 1)) `shouldBe` SNothing

  it "agrees with independent V4 Data ASTs for two-field tag 7 purpose and exact resolved output" $ \(_, dataVectors) -> do
    map dataOutputIndex dataVectors `shouldBe` [0, 2, 3, 4]
    forM_ dataVectors $ \referenceVector -> do
      let outputIndex = dataOutputIndex referenceVector
          scriptHash = scriptHashFromHex (dataScriptHash referenceVector)
          purpose = PV4.Receiving (transScriptHash scriptHash) (toInteger outputIndex)
          resolvedOutput =
            PV4.TxOut
              (PV4.AddressProtected (PV4.ScriptCredential $ transScriptHash scriptHash) Nothing)
              (PV4.lovelaceValue $ PV4.Lovelace $ dataOutputCoin referenceVector)
              PV4.NoOutputDatum
              Nothing
          scriptInfo = PV4.ReceivingScript (toInteger outputIndex) resolvedOutput
          PlutusData purposeData = dataPurpose referenceVector
          PlutusData scriptInfoData = dataScriptInfo referenceVector
      PV4.toData purpose `shouldBe` purposeData
      PV4.fromData purposeData `shouldBe` Just purpose
      PV4.toData scriptInfo `shouldBe` scriptInfoData
      PV4.fromData scriptInfoData `shouldBe` Just scriptInfo
      -- The old one-field purpose and zero-field Receiving context must not decode as the new schema.
      PV4.fromData (P.Constr 7 [P.B $ BSL.toStrict $ hex $ dataScriptHash referenceVector])
        `shouldBe` (Nothing :: Maybe PV4.ScriptPurpose)
      PV4.fromData (P.Constr 7 [])
        `shouldBe` (Nothing :: Maybe PV4.ScriptInfo)

scriptHashFromHex :: String -> ScriptHash
scriptHashFromHex hashHex = ScriptHash (fromJust (Hash.hashFromBytes (BSL.toStrict (hex hashHex))))

hex :: String -> BSL.ByteString
hex = BSL.pack . go
  where
    go [] = []
    go (a : b : rest) = case readHex [a, b] of
      [(byte, "")] -> byte : go rest
      _ -> error "Invalid fixed hexadecimal vector"
    go _ = error "Odd-length fixed hexadecimal vector"

data Vector = Vector
  { vectorName :: String
  , vectorBody :: String
  , vectorTransaction :: String
  , vectorTxId :: String
  , vectorPointers :: [ReferencePointer]
  }

instance FromJSON Vector where
  parseJSON = withObject "reference transaction vector" $ \o ->
    Vector
      <$> o .: "name"
      <*> o .: "body_hex"
      <*> o .: "transaction_hex"
      <*> o .: "txid"
      <*> o .: "receiving_pointers"

data ReferencePointer = ReferencePointer
  { pointerOutputIndex :: Word32
  , pointerScriptHash :: String
  , pointerBytes :: [Word32]
  }

instance FromJSON ReferencePointer where
  parseJSON = withObject "reference Receiving pointer" $ \o ->
    ReferencePointer <$> o .: "output_index" <*> o .: "script_hash" <*> o .: "pointer"

data DataVector = DataVector
  { dataOutputIndex :: Word32
  , dataScriptHash :: String
  , dataOutputCoin :: Integer
  , dataPurpose :: PlutusData DijkstraEra
  , dataScriptInfo :: PlutusData DijkstraEra
  }

instance FromJSON DataVector where
  parseJSON = withObject "reference V4 Data vector" $ \o ->
    DataVector
      <$> o .: "output_index"
      <*> o .: "script_hash"
      <*> o .: "output_coin"
      <*> o .: "receiving_purpose"
      <*> o .: "receiving_script_info"

data Vectors = Vectors [Vector] [DataVector]

instance FromJSON Vectors where
  parseJSON = withObject "reference transaction vectors" $ \o ->
    Vectors <$> o .: "vectors" <*> o .: "v4_data_vectors"

loadVectors :: IO ([Vector], [DataVector])
loadVectors = do
  path <- getDataFileName "golden/receiving-interop.json"
  decoded <- eitherDecodeFileStrict' path
  case decoded of
    Left err -> fail err
    Right (Vectors vectors dataVectors) -> pure (vectors, dataVectors)

getVector :: String -> [Vector] -> Vector
getVector name vectors = case find ((== name) . vectorName) vectors of
  Just referenceVector -> referenceVector
  Nothing -> error ("Missing reference transaction vector: " <> name)
