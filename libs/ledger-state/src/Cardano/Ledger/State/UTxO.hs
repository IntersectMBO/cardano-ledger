{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans -funbox-strict-fields #-}

module Cardano.Ledger.State.UTxO where

--import Cardano.Prelude (heapWords)
import Numeric.Natural
import qualified Cardano.Address as A
import qualified Cardano.Address.Style.Byron as AB
import qualified Cardano.Address.Style.Icarus as AI
import qualified Cardano.Address.Style.Shelley as AS
import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing as Byron
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Hashes as Hashes
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Shelley
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.UTxO
import Conduit
import Data.Aeson as Aeson
import Data.Aeson.Parser as Aeson
import Data.Aeson.Types as Aeson
import Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import qualified Data.ByteString.Base16 as B16
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as C
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import HeapSize

type C = StandardCrypto

type CurrentEra = AlonzoEra C

instance FromJSON (UTxO CurrentEra) where
  parseJSON = fmap UTxO . parseJSON

instance ToJSON (TxIn C) where
  toJSON = toJSON . txInToText

instance ToJSONKey (TxIn C) where
  toJSONKey = toJSONKeyText txInToText

instance FromJSON (TxIn C) where
  parseJSON = withText "TxIn" parseTxIn

instance FromJSONKey (TxIn C) where
  fromJSONKey = FromJSONKeyTextParser parseTxIn

txInToText :: TxIn C -> T.Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToTextAsHex (SafeHash.extractHash txidHash)
    <> T.pack "#"
    <> T.pack (show ix)

parseTxIn :: T.Text -> Aeson.Parser (TxIn C)
parseTxIn txt = do
  let hashByteSize = sizeHash (Proxy :: Proxy (HASH C))
      (hashBase16, rest) = T.splitAt (fromIntegral hashByteSize * 2) txt
  case T.uncons rest of
    Nothing -> fail "Unexpected end of input hash"
    Just ('#', ixTxt) -> do
      ix <- case T.decimal ixTxt of
        Left err -> fail $ "Failed to parse TxIn index: " <> err
        Right (ix, "") -> pure ix
        Right (_, leftOver) -> fail $ "Unexpected leftovers: " <> show leftOver
      h <- parseBase16Hash hashBase16
      pure $ TxIn (TxId (SafeHash.unsafeMakeSafeHash h)) ix
    Just _ -> fail $ "Expected `#`, but got: " <> T.unpack rest

parseBase16Hash :: forall h a m. (MonadFail m, HashAlgorithm h) => T.Text -> m (Hash h a)
parseBase16Hash hashTxt16 =
  case hashFromTextAsHex hashTxt16 of
    Nothing ->
      fail $
        "Can't parse as (Hash "
          <> hashAlgorithmName (Proxy :: Proxy h)
          <> "): "
          <> show hashTxt16
    Just h -> pure h

newtype A = A {unA :: Addr C}
  deriving (Show)

parseA :: MonadFail m => T.Text -> m A
parseA txt =
  case A.fromBech32 txt of
    Nothing
      | Right a <- B16.decode $ T.encodeUtf8 txt ->
        case AS.eitherInspectAddress Nothing $ A.unsafeMkAddress a of
          Left err -> fail $ "Base16 encoded, but invalid address: " ++ show err
          Right ia -> A <$> inspectedAddressToAddr ia
    Nothing -> fail $ "Not Bech32 or Base16 encoded: " <> show txt
    Just addr ->
      case AS.eitherInspectAddress Nothing addr of
        Left err -> fail $ show err
        Right ia -> A <$> inspectedAddressToAddr ia

instance FromJSON A where
  parseJSON = withText "Address" parseA

inspectedAddressToAddr :: MonadFail m => AS.InspectAddress -> m (Addr C)
inspectedAddressToAddr =
  \case
    AS.InspectAddressShelley ai@AS.AddressInfo {..} ->
      let mkHash ::
            (MonadFail m, HashAlgorithm h) =>
            String ->
            ByteString ->
            m (Hash h a)
          mkHash name =
            let msg = "Hash is of invalid length: " ++ name ++ ": " ++ show ai
             in maybe (fail msg) pure . hashFromBytes
          ni
            | infoNetworkTag == A.NetworkTag 0 = pure Testnet
            | infoNetworkTag == A.NetworkTag 1 = pure Mainnet
            | otherwise = fail $ "Who knows this magic: " ++ show infoNetworkTag
          pc
            | Just skh <- infoSpendingKeyHash =
              KeyHashObj . Keys.KeyHash <$> mkHash "Paying:KeyHashObj" skh
            | Just sh <- infoScriptHash =
              ScriptHashObj . Hashes.ScriptHash
                <$> mkHash "Paying:ScriptHashObj" sh
            | otherwise = fail $ "reward account?: " <> show ai
          sr
            | Just skh <- infoStakeKeyHash,
              Just AS.ByValue <- infoStakeReference =
              StakeRefBase . KeyHashObj . Keys.KeyHash
                <$> mkHash "Staking:KeyHashObj" skh
            | Just ssh <- infoStakeScriptHash,
              Just AS.ByValue <- infoStakeReference =
              StakeRefBase . ScriptHashObj . Hashes.ScriptHash
                <$> mkHash "Staking:ScriptHashObj" ssh
            | Just (AS.ByPointer (A.ChainPointer slotNum txIx outIx)) <-
                infoStakeReference =
              pure $
                StakeRefPtr $
                  Ptr
                    (fromIntegral slotNum)
                    (fromIntegral txIx)
                    (fromIntegral outIx)
            | otherwise = pure StakeRefNull
       in Addr <$> ni <*> pc <*> sr
    AS.InspectAddressByron ai@AB.AddressInfo {..} -> do
      let addrAttributes =
            Byron.Attributes
              (Byron.AddrAttributes Nothing Byron.NetworkMainOrStage)
              (Byron.UnparsedFields mempty)
          addrType = Byron.ATRedeem --infoNetworkTag
      addrRoot <- mkByronHash "ByronAddress" ai infoAddressRoot
      pure $
        AddrBootstrap $
          BootstrapAddress $ Byron.Address addrRoot addrAttributes addrType
    AS.InspectAddressIcarus ai@AI.AddressInfo {..} -> do
      let addrAttributes =
            Byron.Attributes
              (Byron.AddrAttributes Nothing Byron.NetworkMainOrStage)
              (Byron.UnparsedFields mempty)
          addrType = Byron.ATRedeem --infoNetworkTag
      addrRoot <- mkByronHash "IcarusAddress" ai infoAddressRoot
      pure $
        AddrBootstrap $
          BootstrapAddress $ Byron.Address addrRoot addrAttributes addrType
  where
    mkByronHash ::
      (MonadFail m, Byron.HashAlgorithm h, Show b) =>
      String ->
      b ->
      ByteString ->
      m (Byron.AbstractHash h a)
    mkByronHash name b =
      let msg = "Hash is invalid: " ++ name ++ ": " ++ show b
       in maybe (fail msg) pure . Byron.abstractHashFromBytes

instance FromJSON (Alonzo.TxOut CurrentEra) where
  parseJSON = withObject "TxOut" $ \obj -> do
    A address <- obj .: "address"
    v <- obj .: "value"
    mDataHashTxt <- obj .:? "datahash"
    mDataHash <- mapM parseBase16Hash mDataHashTxt
    pure $ Alonzo.TxOut address v (SafeHash.unsafeMakeSafeHash <$> maybeToStrictMaybe mDataHash)

instance ToJSON (Mary.Value C) where
  toJSON (Mary.Value l ps) =
    object ["lovelace" .= toJSON l, "policies" .= toJSON ps]

instance FromJSON (Mary.Value C) where
  parseJSON = withObject "Value" $ \obj -> do
    lovelace <- obj .: "lovelace" -- There are Icarus addresses without any lovelace!?
    policies <- obj .:? "policies" .!= mempty
    pure $ Mary.Value lovelace policies

instance ToJSON (Mary.PolicyID C) where
  toJSON (Mary.PolicyID (Shelley.ScriptHash h)) = String (hashToTextAsHex h)

instance FromJSON (Mary.PolicyID C) where
  parseJSON = withText "PolicyID" $ \txt ->
    Mary.PolicyID . Shelley.ScriptHash <$> parseBase16Hash txt

instance ToJSONKey (Mary.PolicyID C) where
  toJSONKey = toJSONKeyText (\(Mary.PolicyID (Shelley.ScriptHash h)) -> hashToTextAsHex h)

instance FromJSONKey (Mary.PolicyID C) where
  fromJSONKey =
    FromJSONKeyTextParser (fmap (Mary.PolicyID . Shelley.ScriptHash) . parseBase16Hash)

instance ToJSON Mary.AssetName where
  toJSON = String . T.decodeLatin1 . B16.encode . Mary.assetName

instance FromJSON Mary.AssetName where
  parseJSON = withText "AssetName" parseAssetName

parseAssetName :: MonadFail m => T.Text -> m Mary.AssetName
parseAssetName = either fail (pure . Mary.AssetName) . B16.decode . T.encodeUtf8

instance ToJSONKey Mary.AssetName where
  toJSONKey = toJSONKeyText (T.decodeLatin1 . B16.encode . Mary.assetName)

instance FromJSONKey Mary.AssetName where
  fromJSONKey = FromJSONKeyTextParser parseAssetName

instance ToJSON Shelley.AccountState where
  toJSON (Shelley.AccountState tr rs) =
    object
      [ "treasury" .= tr,
        "reserves" .= rs
      ]

instance FromJSON Shelley.AccountState where
  parseJSON = withObject "AccountState" $ \obj -> do
    treasury <- obj .: "treasury"
    reserves <- obj .: "reserves"
    pure $ Shelley.AccountState treasury reserves

--- ===================================================================================

parseUTxO :: ConduitM ByteString (TxIn C, Alonzo.TxOut CurrentEra) IO ()
parseUTxO = conduitParserEither entryParser .| C.mapMaybeM (parseTxOutReport)
  where
    parseTxOutReport = \case
      Left err -> Nothing <$ print err
      Right (k, o) ->
        case parseTxOut o of
          Left err -> Nothing <$ putStrLn (err ++ ": " ++ show (k, o))
          Right v -> pure $ Just v

parseTxOut :: (T.Text, Aeson.Value) -> Either String (TxIn C, Alonzo.TxOut CurrentEra)
parseTxOut (txInTxt, txOutVal) = do
  txIn <- parseEither parseTxIn txInTxt
  txOut <- parseEither parseJSON txOutVal
  pure (txIn, txOut)


prefixParser :: Atto.Parser ()
prefixParser = skipSpace <* Atto8.char '{' <* skipSpace

entryParser :: Atto.Parser (T.Text, Aeson.Value)
entryParser = do
  k <- (jstring Atto.<?> "key") <* skipSpace <* (Atto8.char ':' Atto.<?> "':'") <* skipSpace
  v <- json <* skipSpace <* Atto8.satisfy (\w -> w == ',' || w == '}') Atto.<?> "',' or '}'"
  (k, v) <$ skipSpace

skipSpace :: Atto.Parser ()
skipSpace = Atto.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09


--- Loading

consumeUTxO :: FilePath -> ConduitT (TxIn C, Alonzo.TxOut CurrentEra) Void IO b -> IO b
consumeUTxO fp sink = do
  withSourceFile fp $ \c -> runConduit $ c .| (sinkParser prefixParser *> parseUTxO) .| sink

loadUTxO_ :: FilePath -> IO ()
loadUTxO_ fp = (consumeUTxO fp lengthC :: IO Int) >>= print

foldlUTxO :: FilePath -> (a -> (TxIn C, Alonzo.TxOut CurrentEra) -> a) -> a -> IO a
foldlUTxO fp f = consumeUTxO fp . foldlC f

loadUTxO :: FilePath -> IO (Map.Map (TxIn C) (Alonzo.TxOut CurrentEra))
loadUTxO fp = do
  utxo <- foldlUTxO fp (\m (k, v) -> Map.insert k v m) mempty
  -- print $ heapWords utxo
  -- let ws = heapWords utxo
  -- putStrLn $ "cardano-prelude - HeapWords: " ++ show ws ++ " NumBytes: " ++ show (ws * 8)
  -- sz <- runHeapsize 100 (recursiveSize utxo)
  -- putStrLn $ "heapsize - NumBytes: " ++ show sz
  pure utxo

loadUTxOn :: FilePath -> IO (Map.Map (TxId C) (IntMap.IntMap (Alonzo.TxOut CurrentEra)))
loadUTxOn fp =
  foldlUTxO fp nestedInsert mempty
  where
    nestedInsert !m (TxIn txId txIx, !v) =
      Map.insertWith (<>) txId (IntMap.singleton (fromIntegral txIx) v) m

totalADA :: Map.Map (TxIn C) (Alonzo.TxOut CurrentEra) -> Mary.Value C
totalADA = foldMap (\(Alonzo.TxOut _ v _) -> v)

collectStats :: FilePath -> IO ()
collectStats fp =
  foldlUTxO fp collect (emptyUniques, initStats) >>= uncurry reportStats
  where
    collect ::
         (UTxOUniques, UTxOStats)
      -> (TxIn C, Alonzo.TxOut CurrentEra)
      -> (UTxOUniques, UTxOStats)
    collect (u@UTxOUniques {..}, s@UTxOStats {..}) (TxIn txId txIx, Alonzo.TxOut addr _val _datum) =
      let u' = u {txIds = Set.insert txId txIds, txIxs = Set.insert txIx txIxs}
          s' = s {statsTotalTxOuts = statsTotalTxOuts + 1}
          updateStakingStats sr (su, ss) =
            case sr of
              StakeRefNull ->
                (su, ss {stateTotalStakeNulls = stateTotalStakeNulls + 1})
              StakeRefPtr ptr ->
                ( su {stakePtrs = Set.insert ptr stakePtrs}
                , ss {statsTotalStakePtrs = statsTotalStakePtrs + 1})
              StakeRefBase a
                | KeyHashObj kh <- a ->
                  ( su {stakeKeys = Set.insert kh stakeKeys}
                  , ss {statsTotalStakeKeys = statsTotalStakeKeys + 1})
                | ScriptHashObj sh <- a ->
                  ( su {stakeScripts = Set.insert sh stakeScripts}
                  , ss {statsTotalStakeScripts = statsTotalStakeScripts + 1})
       in case addr of
            AddrBootstrap _ ->
              (u', s' {statsByronTxOuts = statsByronTxOuts + 1})
            Addr _ni pc sr
              | KeyHashObj kh <- pc ->
                updateStakingStats
                  sr
                  ( u' {paymentKeys = Set.insert kh paymentKeys}
                  , s' {statsTotalPaymentKeys = statsTotalPaymentKeys + 1})
              | ScriptHashObj kh <- pc ->
                updateStakingStats
                  sr
                  ( u' {paymentScripts = Set.insert kh paymentScripts}
                  , s' {statsTotalPaymentScripts = statsTotalPaymentScripts + 1})

reportStats :: UTxOUniques -> UTxOStats -> IO ()
reportStats UTxOUniques {..} UTxOStats {..} = do
  let showPercent x y
        | y == 0 = "0"
        | otherwise =
          case ((1000 * x) `div` y) `quotRem` 10 of
            (q, r) ->
              show x <> ", " <> show q <> "." <> show r <> "% of total"
  putStrLn $
    unlines
      [ "Total TxOuts = " <> show statsTotalTxOuts,
        "Byron TxOuts = " <> showPercent statsByronTxOuts statsTotalTxOuts,
        "Unique TxIds = " <> showPercent (Set.size txIds) statsTotalTxOuts,
        "Unique TxIxs = " <> showPercent (Set.size txIxs) statsTotalTxOuts,
        "Shelley Total Payment Keys = " <> show statsTotalPaymentKeys,
        "Shelley Unique Payment Keys = " <> showPercent (Set.size paymentKeys) statsTotalPaymentKeys,
        "Shelley Total Payment Scripts = " <> show statsTotalPaymentScripts,
        "Shelley Unique Payment Scripts = "
          <> showPercent (Set.size paymentScripts) statsTotalPaymentScripts,
        "Shelley Total Stake Keys = " <> show statsTotalStakeKeys,
        "Shelley Unique Stake Keys = " <> showPercent (Set.size stakeKeys) statsTotalStakeKeys,
        "Shelley Total Stake Scripts = " <> show statsTotalStakeScripts,
        "Shelley Unique Stake Scripts = "
          <> showPercent (Set.size stakeScripts) statsTotalStakeScripts,
        "Shelley Total Stake Ptrs = " <> show statsTotalStakePtrs,
        "Shelley Unique Stake Ptrs = " <> showPercent (Set.size stakePtrs) statsTotalStakePtrs
      ]

data UTxOUniques = UTxOUniques
  { paymentKeys :: !(Set.Set (Shelley.KeyHash 'Shelley.Payment C)),
    paymentScripts :: !(Set.Set (Shelley.ScriptHash C)),
    stakeKeys :: !(Set.Set (Shelley.KeyHash 'Shelley.Staking C)),
    stakeScripts :: !(Set.Set (Shelley.ScriptHash C)),
    stakePtrs :: !(Set.Set Shelley.Ptr),
    scripts :: !(Set.Set (Shelley.ScriptHash C)),
    txIds :: !(Set.Set (TxId C)),
    txIxs :: !(Set.Set Natural)
  }

emptyUniques :: UTxOUniques
emptyUniques = UTxOUniques mempty mempty mempty mempty mempty mempty mempty mempty

data UTxOStats = UTxOStats
  { statsTotalTxOuts :: !Int,
    statsByronTxOuts :: !Int,
    statsTotalPaymentKeys :: !Int,
    statsTotalPaymentScripts :: !Int,
    statsTotalStakeKeys :: !Int,
    statsTotalStakeScripts :: !Int,
    statsTotalStakePtrs :: !Int,
    stateTotalStakeNulls :: !Int
  }
  deriving (Show)

initStats :: UTxOStats
initStats = UTxOStats 0 0 0 0 0 0 0 0

-- newtype UTxO era = UTxO (Map (TxId (Crypto era)) (IntMap (TxOut era)))
