{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -funbox-strict-fields #-}

module Cardano.Ledger.State.UTxO where

import Cardano.Prelude (HeapWords(..))
import qualified Cardano.Crypto.Hash.Class as HS
import qualified Data.Hashable as HM
import qualified Data.HashMap.Strict as HM
import Cardano.Ledger.TxIn
import qualified Cardano.Address as A
import qualified Cardano.Address.Style.Byron as AB
import qualified Cardano.Address.Style.Icarus as AI
import qualified Cardano.Address.Style.Shelley as AS
import Cardano.Binary (FromCBOR (..))
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hash.Class as H
import qualified Cardano.Crypto.Hashing as Byron
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Data
import Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Hashes as Hashes
import qualified Cardano.Ledger.Keys as Keys
import Cardano.Ledger.Mary.Value
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards
import Cardano.Protocol.TPraos (individualPoolStakeVrf)
import Codec.CBOR.Read (deserialiseFromBytes)
import Conduit
import Control.Exception (throwIO)
import Control.Iterate.SetAlgebra (range)
import Control.Monad
import Data.Aeson as Aeson
import Data.Aeson.Parser as Aeson
import Data.Aeson.Types as Aeson
import Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as C
import Data.Foldable as F
import Data.Functor
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Typeable
import Numeric.Natural
import Prettyprinter
import Text.Printf
import Data.Compact.KeyMap as KeyMap hiding (Stat)

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
  H.hashToTextAsHex (SafeHash.extractHash txidHash)
    <> T.pack "#"
    <> T.pack (show ix)

parseTxIn :: T.Text -> Aeson.Parser (TxIn C)
parseTxIn txt = do
  let hashByteSize = H.sizeHash (Proxy :: Proxy (HASH C))
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

parseBase16Hash :: forall h a m. (MonadFail m, H.HashAlgorithm h) => T.Text -> m (H.Hash h a)
parseBase16Hash hashTxt16 =
  case H.hashFromTextAsHex hashTxt16 of
    Nothing ->
      fail $
        "Can't parse as (Hash "
          <> H.hashAlgorithmName (Proxy :: Proxy h)
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
            (MonadFail m, H.HashAlgorithm h) =>
            String ->
            BS.ByteString ->
            m (H.Hash h a)
          mkHash name =
            let msg = "Hash is of invalid length: " ++ name ++ ": " ++ show ai
             in maybe (fail msg) pure . H.hashFromBytes
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
      BS.ByteString ->
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
  toJSON (Mary.PolicyID (ScriptHash h)) = String (H.hashToTextAsHex h)

instance FromJSON (Mary.PolicyID C) where
  parseJSON = withText "PolicyID" $ \txt ->
    Mary.PolicyID . ScriptHash <$> parseBase16Hash txt

instance ToJSONKey (Mary.PolicyID C) where
  toJSONKey = toJSONKeyText (\(Mary.PolicyID (ScriptHash h)) -> H.hashToTextAsHex h)

instance FromJSONKey (Mary.PolicyID C) where
  fromJSONKey =
    FromJSONKeyTextParser (fmap (Mary.PolicyID . ScriptHash) . parseBase16Hash)

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

instance ToJSON AccountState where
  toJSON (AccountState tr rs) =
    object
      [ "treasury" .= tr,
        "reserves" .= rs
      ]

instance FromJSON AccountState where
  parseJSON = withObject "AccountState" $ \obj -> do
    treasury <- obj .: "treasury"
    reserves <- obj .: "reserves"
    pure $ AccountState treasury reserves

--- ===================================================================================

parseUTxO :: ConduitM BS.ByteString (TxIn C, Alonzo.TxOut CurrentEra) IO ()
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

loadNewEpochState :: FilePath -> IO (NewEpochState CurrentEra)
loadNewEpochState fp =
  LBS.readFile fp <&> deserialiseFromBytes fromCBOR >>= \case
    Left exc -> throwIO exc
    Right (extra, newEpochState) -> do
      unless (LBS.null extra) $
        putStrLn $
          "Unexpected leftover: "
            <> case LBS.splitAt 50 extra of
              (_, "") -> show extra
              (extraCut, _) -> show (extraCut <> "...")
      pure newEpochState

loadLedgerState :: FilePath -> IO (LedgerState CurrentEra)
loadLedgerState fp = esLState . nesEs <$> loadNewEpochState fp


consumeUTxO :: FilePath -> ConduitT (TxIn C, Alonzo.TxOut CurrentEra) Void IO b -> IO b
consumeUTxO fp sink = do
  withSourceFile fp $ \c -> runConduit $ c .| (sinkParser prefixParser *> parseUTxO) .| sink

loadUTxO_ :: FilePath -> IO ()
loadUTxO_ fp = (consumeUTxO fp lengthC :: IO Int) >>= print

foldlUTxO :: FilePath -> (a -> (TxIn C, Alonzo.TxOut CurrentEra) -> a) -> a -> IO a
foldlUTxO fp f = consumeUTxO fp . foldlC f

loadUTxO :: FilePath -> IO (Map.Map (TxIn C) (Alonzo.TxOut CurrentEra))
loadUTxO fp = do
  utxo <- foldlUTxO fp (\ !m !(!k, !v) -> Map.insert k v m) mempty
  -- print $ heapWords utxo
  -- let ws = heapWords utxo
  -- putStrLn $ "cardano-prelude - HeapWords: " ++ show ws ++ " NumBytes: " ++ show (ws * 8)
  -- sz <- runHeapsize 100 (recursiveSize utxo)
  -- putStrLn $ "heapsize - NumBytes: " ++ show sz
  pure utxo

loadUTxOn :: FilePath -> IO (Map.Map (TxId C) (IntMap.IntMap (Alonzo.TxOut CurrentEra)))
loadUTxOn fp = foldlUTxO fp txIdNestedInsert mempty

txIdNestedInsert ::
     (Map.Map (TxId C) (IntMap.IntMap (Alonzo.TxOut CurrentEra)))
  -> (TxIn C, Alonzo.TxOut CurrentEra)
  -> (Map.Map (TxId C) (IntMap.IntMap (Alonzo.TxOut CurrentEra)))
txIdNestedInsert !m (TxIn !txId !txIx, !v) =
  let !e = IntMap.singleton (fromIntegral txIx) v
   in Map.insertWith (<>) txId e m

loadUTxOni :: FilePath -> IO (IntMap.IntMap (Map.Map (TxId C) (Alonzo.TxOut CurrentEra)))
loadUTxOni fp = foldlUTxO fp nestedInsertTxId mempty

nestedInsertTxId ::
     IntMap.IntMap (Map.Map (TxId C) a)
  -> (TxIn C, a)
  -> IntMap.IntMap (Map.Map (TxId C) a)
nestedInsertTxId !im (TxIn !txId !txIx, !v) =
      let f =
            \case
              Nothing -> Just $! Map.singleton txId v
              Just !m -> Just $! Map.insert txId v m
       in IntMap.alter f (fromIntegral txIx) im

loadUTxOhm :: FilePath -> IO (IntMap.IntMap (KeyMap.HashMap (Alonzo.TxOut CurrentEra)))
loadUTxOhm fp = foldlUTxO fp nestedInsert mempty
  where
    nestedInsert ::
         IntMap.IntMap (KeyMap.HashMap (Alonzo.TxOut CurrentEra))
      -> (TxIn C, Alonzo.TxOut CurrentEra)
      -> IntMap.IntMap (KeyMap.HashMap (Alonzo.TxOut CurrentEra))
    nestedInsert !m (TxInCompact32 x1 x2 x3 x4 txIx, !v) =
      let !key = KeyMap.Key x1 x2 x3 x4
          f =
            \case
              Nothing -> Just $! KeyMap.Leaf key v
              Just hm -> Just $! KeyMap.insert key v hm
       in IntMap.alter f (fromIntegral txIx) m

instance HM.Hashable (TxIn C) where
  hashWithSalt _salt (TxInCompact32 x1 _ _ _ _) = fromIntegral x1
  hash (TxInCompact32 x1 _ _ _ _) = fromIntegral x1

instance HM.Hashable (TxId C) where
  hashWithSalt _salt = hashTxId
  hash = hashTxId

hashTxId :: TxId C -> Int
hashTxId (TxId sh) =
  case HS.viewHash32 (SafeHash.extractHash sh) of
    HS.ViewHashNot32 -> error "irrelevant"
    HS.ViewHash32 a _ _ _ -> fromIntegral a


loadUTxOuhm' :: FilePath -> IO (HM.HashMap (TxIn C) ())
loadUTxOuhm' fp = foldlUTxO fp (\ !m !(!k, _) -> HM.insert k () m) mempty


loadUTxOuhm'' :: FilePath -> IO (IntMap.IntMap (HM.HashMap (TxId C) ()))
loadUTxOuhm'' fp = foldlUTxO fp nestedInsert mempty
  where
    nestedInsert ::
         IntMap.IntMap (HM.HashMap (TxId C) ())
      -> (TxIn C, Alonzo.TxOut CurrentEra)
      -> IntMap.IntMap (HM.HashMap (TxId C) ())
    nestedInsert !m (TxIn txId txIx, _) =
      let f =
            \case
              Nothing -> Just $! HM.singleton txId ()
              Just hm -> Just $! HM.insert txId () hm
       in IntMap.alter f (fromIntegral txIx) m


loadUTxO' :: FilePath -> IO (Map.Map (TxIn C) ())
loadUTxO' fp = foldlUTxO fp (\ !m !(!k, _) -> Map.insert k () m) mempty

loadUTxOni' :: FilePath -> IO (IntMap.IntMap (Map.Map (TxId C) ()))
loadUTxOni' fp = foldlUTxO fp (\m txin -> nestedInsertTxId m (() <$ txin)) mempty

loadUTxOhm' :: FilePath -> IO (IntMap.IntMap (KeyMap.HashMap ()))
loadUTxOhm' fp = foldlUTxO fp nestedInsertHM' mempty


nestedInsertHM' ::
     IntMap.IntMap (KeyMap.HashMap ())
  -> (TxIn C, Alonzo.TxOut CurrentEra)
  -> IntMap.IntMap (KeyMap.HashMap ())
nestedInsertHM' !m (TxInCompact32 x1 x2 x3 x4 txIx, _) =
  let !key = KeyMap.Key x1 x2 x3 x4
      f =
        \case
          Nothing -> Just $! KeyMap.Leaf key ()
          Just hm -> Just $! KeyMap.insert key () hm
   in IntMap.alter f (fromIntegral txIx) m


loadUTxOihm' :: FilePath -> IO (KeyMap.HashMap (IntMap.IntMap ()))
loadUTxOihm' fp = foldlUTxO fp nestedInsert KeyMap.Empty
  where
    nestedInsert ::
         KeyMap.HashMap (IntMap.IntMap ())
      -> (TxIn C, Alonzo.TxOut CurrentEra)
      -> KeyMap.HashMap (IntMap.IntMap ())
    nestedInsert !hm (TxInCompact32 x1 x2 x3 x4 txIx, _) =
      let !key = KeyMap.Key x1 x2 x3 x4
       in case KeyMap.lookupHM key hm of
            Nothing ->
              let !v = IntMap.singleton (fromIntegral txIx) ()
               in KeyMap.insert key v hm
            Just im ->
              let !v = IntMap.insert (fromIntegral txIx) () im
               in KeyMap.insert key v $ KeyMap.delete key hm

totalADA :: Map.Map (TxIn C) (Alonzo.TxOut CurrentEra) -> Mary.Value C
totalADA = foldMap (\(Alonzo.TxOut _ v _) -> v)

collectStatsFromJSON :: FilePath -> IO ()
collectStatsFromJSON fp = consumeUTxO fp collectStats

loadBinUTxO :: FilePath -> IO (UTxO CurrentEra)
loadBinUTxO fp = do
  ls <- loadNewEpochState fp
  pure $! _utxo $ _utxoState $ esLState $ nesEs ls
  -- runConduit $ C.sourceList (Map.toList u) .| collectStats

collectStats :: ConduitT (TxIn C, Alonzo.TxOut CurrentEra) Void IO ()
collectStats = do
  (uniques, stats) <- foldlC collect (emptyUniques, initStats)
  lift $ reportStats uniques stats
  where
    collect ::
      (UTxOUniques, UTxOStats') ->
      (TxIn C, Alonzo.TxOut CurrentEra) ->
      (UTxOUniques, UTxOStats')
    collect (u@UTxOUniques {..}, s@UTxOStats' {..}) (TxIn txId txIx, Alonzo.TxOut addr _val _datum) =
      let u' = u {txIds = Set.insert txId txIds, txIxs = Set.insert txIx txIxs}
          s' = s {statsTotalTxOuts = statsTotalTxOuts + 1}
          updateStakingStats sr (su, ss) =
            case sr of
              StakeRefNull ->
                (su, ss {stateTotalStakeNulls = stateTotalStakeNulls + 1})
              StakeRefPtr ptr ->
                ( su {stakePtrs = Set.insert ptr stakePtrs},
                  ss {statsTotalStakePtrs = statsTotalStakePtrs + 1}
                )
              StakeRefBase a
                | KeyHashObj kh <- a ->
                  ( su {stakeKeys = Set.insert kh stakeKeys},
                    ss {statsTotalStakeKeys = statsTotalStakeKeys + 1}
                  )
                | ScriptHashObj sh <- a ->
                  ( su {stakeScripts = Set.insert sh stakeScripts},
                    ss {statsTotalStakeScripts = statsTotalStakeScripts + 1}
                  )
       in case addr of
            AddrBootstrap _ ->
              (u', s' {statsByronTxOuts = statsByronTxOuts + 1})
            Addr _ni pc sr
              | KeyHashObj kh <- pc ->
                updateStakingStats
                  sr
                  ( u' {paymentKeys = Set.insert kh paymentKeys},
                    s' {statsTotalPaymentKeys = statsTotalPaymentKeys + 1}
                  )
              | ScriptHashObj kh <- pc ->
                updateStakingStats
                  sr
                  ( u' {paymentScripts = Set.insert kh paymentScripts},
                    s' {statsTotalPaymentScripts = statsTotalPaymentScripts + 1}
                  )

reportStats :: UTxOUniques -> UTxOStats' -> IO ()
reportStats UTxOUniques {..} UTxOStats' {..} = do
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

newtype Count = Count Int
  deriving (Eq, Ord, Enum, Real, Integral, Num, Pretty)

data Stat k = Stat
  { statUnique :: !(Set.Set k),
    statCount :: !Count
  }

instance Ord k => Semigroup (Stat k) where
  (<>) s1 s2 = Stat (statUnique s1 <> statUnique s2) (statCount s1 + statCount s2)

instance Ord k => Monoid (Stat k) where
  mempty = Stat mempty 0

instance Pretty (Stat k) where
  pretty Stat {..} =
    pretty n
      <+> "/"
      <+> pretty statCount
      <+> "(" <> pretty (intPercent n statCount) <> " unique)"
    where
      n = Set.size statUnique

data Percent = Percent Int Int

instance Pretty Percent where
  pretty (Percent x y) = pretty (printf "%d.%02d%%" x y :: String)

intPercent :: Integral i => Int -> i -> Percent
intPercent x y
  | y == 0 = Percent 0 0
  | otherwise = uncurry Percent (((10000 * x) `div` fromIntegral y) `quotRem` 100)

statSingleton :: a -> Stat a
statSingleton a = Stat (Set.singleton a) 1

statSet :: Set.Set a -> Stat a
statSet s = Stat s (Count (Set.size s))

statMapKeys :: Map.Map k v -> Stat k
statMapKeys = statSet . Map.keysSet

statFoldable :: (Ord a, Foldable t) => t a -> Stat a
statFoldable m = Stat (Set.fromList (F.toList m)) (Count (F.length m))

prettyRecord :: Doc ann -> [Doc ann] -> Doc ann
prettyRecord h content = h <> ":" <+> line <> indent 2 (vsep content)

(<:>) :: (Typeable a, Pretty a) => Doc ann -> a -> Doc ann
(<:>) x y =
  "[" <> x <> "]:" <+> pretty y <+> "<" <> pretty (showsTypeRep (typeOf y) ">")

infixr 6 <:>

data SnapShotStats = SnapShotStats
  { sssStake :: !(Stat (Credential 'Staking C)),
    sssDelegationCredential :: !(Stat (Credential 'Staking C)),
    sssDelegationStakePool :: !(Stat (KeyHash 'StakePool C)),
    sssPoolParams :: !(Stat (KeyHash 'StakePool C)),
    sssPoolParamsStats :: !PoolParamsStats
  }

instance Semigroup SnapShotStats where
  (<>) (SnapShotStats x1 x2 x3 x4 x5) (SnapShotStats y1 y2 y3 y4 y5) =
    SnapShotStats
      (x1 <> y1)
      (x2 <> y2)
      (x3 <> y3)
      (x4 <> y4)
      (x5 <> y5)

instance Monoid SnapShotStats where
  mempty = SnapShotStats mempty mempty mempty mempty mempty

instance Pretty SnapShotStats where
  pretty SnapShotStats {..} =
    prettyRecord
      "SnapShot"
      [ "Stake" <:> sssStake,
        "DelegationCredential" <:> sssDelegationCredential,
        "DelegationStakePool" <:> sssDelegationStakePool,
        "PoolParams" <:> sssPoolParams,
        pretty sssPoolParamsStats
      ]

instance AggregateStat SnapShotStats where
  aggregateStat SnapShotStats {..} =
    (aggregateStat sssPoolParamsStats)
      { gsCredentialStaking = sssStake <> sssDelegationCredential,
        gsKeyHashStakePool = sssDelegationStakePool <> sssPoolParams
      }

countSnapShotStat :: SnapShot C -> SnapShotStats
countSnapShotStat SnapShot {..} =
  SnapShotStats
    { sssStake = statMapKeys (unStake _stake),
      sssDelegationCredential = statMapKeys _delegations,
      sssDelegationStakePool = statFoldable _delegations,
      sssPoolParams = statMapKeys _poolParams,
      sssPoolParamsStats = foldMap countPoolParamsStats _poolParams
    }

data PoolParamsStats = PoolParamsStats
  { ppsPoolId :: !(Stat (KeyHash 'StakePool C)),
    ppsRewardAcnt :: !(Stat (Credential 'Staking C)),
    ppsOwners :: !(Stat (KeyHash 'Staking C))
  }

instance Semigroup PoolParamsStats where
  (<>) (PoolParamsStats x1 x2 x3) (PoolParamsStats y1 y2 y3) =
    PoolParamsStats
      (x1 <> y1)
      (x2 <> y2)
      (x3 <> y3)

instance Monoid PoolParamsStats where
  mempty = PoolParamsStats mempty mempty mempty

instance Pretty PoolParamsStats where
  pretty PoolParamsStats {..} =
    prettyRecord
      "PoolParamsStats"
      [ "PoolId" <:> ppsPoolId,
        "RewardAcnt" <:> ppsRewardAcnt,
        "Owners" <:> ppsOwners
      ]

instance AggregateStat PoolParamsStats where
  aggregateStat PoolParamsStats {..} =
    mempty {gsCredentialStaking = ppsRewardAcnt, gsKeyHashStakePool = ppsPoolId}

countPoolParamsStats :: PoolParams C -> PoolParamsStats
countPoolParamsStats PoolParams {..} =
  PoolParamsStats
    { ppsPoolId = statSingleton _poolId,
      ppsRewardAcnt = statSingleton (getRwdCred _poolRAcnt),
      ppsOwners = statSet _poolOwners
    }

data RewardUpdateStats = RewardUpdateStats

instance Pretty RewardUpdateStats where
  pretty RewardUpdateStats {} =
    prettyRecord "RewardUpdateStats" []

instance AggregateStat RewardUpdateStats where
  aggregateStat RewardUpdateStats = mempty

data PoolDistrStats = PoolDistrStats
  { pdsStakePoolKeyHash :: !(Stat (KeyHash 'StakePool C)),
    pdsStakePoolStakeVrf :: !(Stat (Hash C (VerKeyVRF C)))
  }

instance Pretty PoolDistrStats where
  pretty PoolDistrStats {..} =
    prettyRecord
      "PoolDistrStats"
      [ "StakePoolKeyHash" <:> pdsStakePoolKeyHash,
        "StakePoolStakeVrf" <:> pdsStakePoolStakeVrf
      ]

instance AggregateStat PoolDistrStats where
  aggregateStat PoolDistrStats {..} =
    mempty
      { gsKeyHashStakePool = pdsStakePoolKeyHash,
        gsVerKeyVRF = pdsStakePoolStakeVrf
      }

calcPoolDistrStats :: PoolDistr C -> PoolDistrStats
calcPoolDistrStats (PoolDistr pd) =
  PoolDistrStats
    { pdsStakePoolKeyHash = statMapKeys pd,
      pdsStakePoolStakeVrf = statFoldable (individualPoolStakeVrf <$> Map.elems pd)
    }

data NewEpochStateStats = NewEpochStateStats
  { nessPrevBlocksMade :: !(Stat (KeyHash 'StakePool C)),
    nessCurBlocksMade :: !(Stat (KeyHash 'StakePool C)),
    nessBlocksMade :: !(Stat (KeyHash 'StakePool C)),
    nessEpochStateStats :: !EpochStateStats,
    nessRewardUpdate :: !RewardUpdateStats,
    nessPoolDistrStats :: !PoolDistrStats,
    nessAggregateStats :: !AggregateStats
  }

instance Pretty NewEpochStateStats where
  pretty NewEpochStateStats {..} =
    prettyRecord
      "NewEpochStateStats"
      [ "PrevBlocksMade" <:> statCount nessPrevBlocksMade,
        "CurBlocksMade" <:> statCount nessCurBlocksMade,
        "BlocksMade" <:> nessBlocksMade,
        pretty nessEpochStateStats,
        pretty nessRewardUpdate <> "TODO",
        pretty nessPoolDistrStats,
        pretty nessAggregateStats
      ]

countNewEpochStateStats :: NewEpochState CurrentEra -> NewEpochStateStats
countNewEpochStateStats NewEpochState {..} =
  let ness =
        NewEpochStateStats
          { nessPrevBlocksMade = statMapKeys (unBlocksMade nesBprev),
            nessCurBlocksMade = statMapKeys (unBlocksMade nesBcur),
            nessBlocksMade = mempty,
            nessEpochStateStats = countEpochStateStats nesEs,
            nessRewardUpdate = RewardUpdateStats,
            nessPoolDistrStats = calcPoolDistrStats nesPd,
            nessAggregateStats = mempty
          }
   in ness
        { nessBlocksMade = nessPrevBlocksMade ness <> nessCurBlocksMade ness,
          nessAggregateStats =
            mconcat
              [ aggregateStat (nessPrevBlocksMade ness),
                aggregateStat (nessCurBlocksMade ness),
                aggregateStat (nessRewardUpdate ness),
                essAggregateStats (nessEpochStateStats ness),
                aggregateStat (nessPoolDistrStats ness)
              ]
        }

printNewEpochStateStats :: NewEpochStateStats -> IO ()
printNewEpochStateStats = putStrLn . show . pretty

data EpochStateStats = EpochStateStats
  { essMarkSnapShotStats :: !SnapShotStats,
    essSetSnapShotStats :: !SnapShotStats,
    essGoSnapShotStats :: !SnapShotStats,
    essSnapShotsStats :: !SnapShotStats,
    essLedgerStateStats :: !LedgerStateStats,
    essNonMyopic :: !(Stat (KeyHash 'StakePool C)),
    essAggregateStats :: !AggregateStats
  }

instance Pretty EpochStateStats where
  pretty EpochStateStats {..} =
    prettyRecord
      "EpochStateStats"
      [ "mark" <:> statCount (sssStake essMarkSnapShotStats),
        "set" <:> statCount (sssStake essSetSnapShotStats),
        "go" <:> statCount (sssStake essGoSnapShotStats),
        "mark+set+go =" <+> pretty essSnapShotsStats,
        pretty essLedgerStateStats,
        "NonMyopic" <:> essNonMyopic,
        pretty essAggregateStats
      ]

countEpochStateStats :: EpochState CurrentEra -> EpochStateStats
countEpochStateStats EpochState {..} =
  let mark = countSnapShotStat (_pstakeMark esSnapshots)
      set = countSnapShotStat (_pstakeSet esSnapshots)
      go = countSnapShotStat (_pstakeGo esSnapshots)
      stats =
        EpochStateStats
          { essMarkSnapShotStats = mark,
            essSetSnapShotStats = set,
            essGoSnapShotStats = go,
            essSnapShotsStats = mark <> set <> go,
            essLedgerStateStats = countLedgerStateStats esLState,
            essNonMyopic = statMapKeys (likelihoodsNM esNonMyopic),
            essAggregateStats = mempty
          }
   in stats
        { essAggregateStats =
            mconcat
              [ aggregateStat (essSnapShotsStats stats),
                aggregateStat (essLedgerStateStats stats),
                aggregateStat (essNonMyopic stats)
              ]
        }

data DStateStats = DStateStats
  { dssCredentialStaking :: !(Stat (Credential 'Staking C)),
    dssDelegations :: !(Stat (KeyHash 'StakePool C)),
    dssKeyHashGenesis :: !(Stat (KeyHash 'Genesis C)),
    dssKeyHashGenesisDelegate :: !(Stat (KeyHash 'GenesisDelegate C)),
    dssHashVerKeyVRF :: !(Stat (Hash C (VerKeyVRF C)))
  }

instance Pretty DStateStats where
  pretty DStateStats {..} =
    prettyRecord
      "DStateStats"
      [ "CredentialStaking" <:> dssCredentialStaking,
        "Delegations" <:> dssDelegations,
        "KeyHashGenesis" <:> dssKeyHashGenesis,
        "KeyHashGenesisDelegate" <:> dssKeyHashGenesisDelegate,
        "HashVerKeyVRF" <:> dssHashVerKeyVRF
      ]

instance AggregateStat DStateStats where
  aggregateStat DStateStats {..} =
    mempty
      { gsCredentialStaking = dssCredentialStaking,
        gsKeyHashStakePool = dssDelegations,
        gsKeyHashGenesis = dssKeyHashGenesis,
        gsKeyHashGenesisDelegate = dssKeyHashGenesisDelegate,
        gsVerKeyVRF = dssHashVerKeyVRF
      }

countDStateStats :: DState C -> DStateStats
countDStateStats DState {..} =
  DStateStats
    { dssCredentialStaking =
        statMapKeys _rewards
          <> statMapKeys _delegations
          <> statSet (range _ptrs),
      dssDelegations = statFoldable _delegations,
      dssKeyHashGenesis =
        statFoldable (fGenDelegGenKeyHash <$> Map.keys _fGenDelegs)
          <> statMapKeys (unGenDelegs _genDelegs),
      dssKeyHashGenesisDelegate =
        statFoldable (genDelegKeyHash <$> Map.elems _fGenDelegs)
          <> statFoldable
            (genDelegKeyHash <$> Map.elems (unGenDelegs _genDelegs)),
      dssHashVerKeyVRF =
        statFoldable (genDelegVrfHash <$> Map.elems _fGenDelegs)
          <> statFoldable
            (genDelegVrfHash <$> Map.elems (unGenDelegs _genDelegs))
    }

data PStateStats = PStateStats
  { pssKeyHashStakePool :: !(Stat (KeyHash 'StakePool C)),
    pssPoolParamsStats :: !PoolParamsStats
  }

instance Pretty PStateStats where
  pretty PStateStats {..} =
    prettyRecord
      "PStateStats"
      [ "KeyHashStakePool" <:> pssKeyHashStakePool,
        pretty pssPoolParamsStats
      ]

instance AggregateStat PStateStats where
  aggregateStat PStateStats {..} =
    (aggregateStat pssPoolParamsStats) {gsKeyHashStakePool = pssKeyHashStakePool}

countPStateStats :: PState C -> PStateStats
countPStateStats PState {..} =
  PStateStats
    { pssKeyHashStakePool =
        statMapKeys _pParams
          <> statMapKeys _fPParams
          <> statMapKeys _retiring,
      pssPoolParamsStats =
        foldMap countPoolParamsStats _pParams <> foldMap countPoolParamsStats _fPParams
    }

data LedgerStateStats = LedgerStateStats
  { lssUTxOStats :: !UTxOStats,
    lssDStateStats :: !DStateStats,
    lssPStateStats :: !PStateStats
  }

instance Pretty LedgerStateStats where
  pretty LedgerStateStats {..} =
    prettyRecord
      "LedgerStateStats"
      [ pretty lssUTxOStats,
        pretty lssDStateStats,
        pretty lssPStateStats
      ]

instance AggregateStat LedgerStateStats where
  aggregateStat LedgerStateStats {..} =
    mconcat
      [ aggregateStat lssUTxOStats,
        aggregateStat lssDStateStats,
        aggregateStat lssPStateStats
      ]

countLedgerStateStats :: LedgerState CurrentEra -> LedgerStateStats
countLedgerStateStats LedgerState {..} =
  LedgerStateStats
    { lssUTxOStats = countUTxOStats (_utxo _utxoState),
      lssDStateStats = countDStateStats (_dstate _delegationState),
      lssPStateStats = countPStateStats (_pstate _delegationState)
    }

data TxInStats = TxInStats
  { tisTxId :: !(Stat (TxId C)),
    tisTxIx :: !(Stat Natural)
  }

instance Pretty TxInStats where
  pretty TxInStats {..} =
    prettyRecord "TxInStats" ["TxId" <:> tisTxId, "TxIx" <:> tisTxIx]

countTxInStats :: [TxIn C] -> TxInStats
countTxInStats txIns =
  case unzip (fmap (\(TxIn txId txIx) -> (txId, txIx)) txIns) of
    (txIds, txIxs) ->
      TxInStats
        { tisTxId = statFoldable txIds,
          tisTxIx = statFoldable txIxs
        }

data TxOutStats = TxOutStats
  { tosBootstrap :: !(Stat (BootstrapAddress C)),
    tosPaymentCredential :: !(Stat (Credential 'Payment C)),
    tosStakingCredential :: !(Stat (Credential 'Staking C)),
    tosStakingPtr :: !(Stat Ptr),
    tosNetwork :: !(Stat Network),
    tosValue :: !(Stat Integer),
    tosPolicyId :: !(Stat (PolicyID C)),
    tosAssetName :: !(Stat AssetName),
    tosAssetValue :: !(Stat Integer),
    tosDataHash :: !(Stat (DataHash C))
  }

instance Semigroup TxOutStats where
  (<>) (TxOutStats x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) (TxOutStats y0 y1 y2 y3 y4 y5 y6 y7 y8 y9) =
    TxOutStats
      (x0 <> y0)
      (x1 <> y1)
      (x2 <> y2)
      (x3 <> y3)
      (x4 <> y4)
      (x5 <> y5)
      (x6 <> y6)
      (x7 <> y7)
      (x8 <> y8)
      (x9 <> y9)

instance Monoid TxOutStats where
  mempty = TxOutStats mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance Pretty TxOutStats where
  pretty TxOutStats {..} =
    prettyRecord
      "TxOutStats"
      [ "Bootstrap" <:> tosBootstrap,
        "PaymentCredential" <:> tosPaymentCredential,
        "StakingCredential" <:> tosStakingCredential,
        "StakingPtr" <:> tosStakingPtr,
        "Network" <:> tosNetwork,
        "Value" <:> tosValue,
        "PolicyId" <:> tosPolicyId,
        "AssetName" <:> tosAssetName,
        "AssetValue" <:> tosAssetValue,
        "DataHash" <:> tosDataHash
      ]

instance AggregateStat TxOutStats where
  aggregateStat TxOutStats {..} = aggregateStat tosStakingCredential

countTxOutStats :: [Alonzo.TxOut CurrentEra] -> TxOutStats
countTxOutStats = foldMap countTxOutStat
  where
    countTxOutStat :: Alonzo.TxOut CurrentEra -> TxOutStats
    countTxOutStat (Alonzo.TxOut addr (Value v vm) mData) =
      let !dataStat =
            strictMaybe
              mempty
              (\d -> mempty {tosDataHash = statSingleton d})
              mData
          !vmElems = Map.elems vm
          !valueStat =
            dataStat
              { tosValue = statSingleton v,
                tosPolicyId = statMapKeys vm,
                tosAssetName = foldMap statMapKeys vmElems,
                tosAssetValue = foldMap statFoldable vmElems
              }
          !networkStat = valueStat {tosNetwork = statSingleton (getNetwork addr)}
       in case addr of
            AddrBootstrap addrBootstrap ->
              networkStat {tosBootstrap = statSingleton addrBootstrap}
            Addr _ pc sr ->
              let stakeStat =
                    case sr of
                      StakeRefNull -> networkStat
                      StakeRefPtr ptr ->
                        networkStat {tosStakingPtr = statSingleton ptr}
                      StakeRefBase cred ->
                        networkStat {tosStakingCredential = statSingleton cred}
               in stakeStat {tosPaymentCredential = statSingleton pc}

data UTxOStats = UTxOStats
  { usTxInStats :: !TxInStats,
    usTxOutStats :: !TxOutStats
  }

instance Pretty UTxOStats where
  pretty UTxOStats {..} =
    prettyRecord
      "UTxOStats"
      [pretty usTxInStats, pretty usTxOutStats]

instance AggregateStat UTxOStats where
  aggregateStat = aggregateStat . usTxOutStats

countUTxOStats :: UTxO (AlonzoEra StandardCrypto) -> UTxOStats
countUTxOStats (UTxO m) =
  UTxOStats
    { usTxInStats = countTxInStats (Map.keys m),
      usTxOutStats = countTxOutStats (Map.elems m)
    }

data AggregateStats = AggregateStats
  { gsCredentialStaking :: !(Stat (Credential 'Staking C)),
    gsKeyHashStakePool :: !(Stat (KeyHash 'StakePool C)),
    gsKeyHashGenesis :: !(Stat (KeyHash 'Genesis C)),
    gsKeyHashGenesisDelegate :: !(Stat (KeyHash 'GenesisDelegate C)),
    gsVerKeyVRF :: !(Stat (Hash C (VerKeyVRF C))),
    gsScriptHash :: !(Stat (ScriptHash C))
  }

instance Semigroup AggregateStats where
  (<>) (AggregateStats x1 x2 x3 x4 x5 x6) (AggregateStats y1 y2 y3 y4 y5 y6) =
    AggregateStats
      (x1 <> y1)
      (x2 <> y2)
      (x3 <> y3)
      (x4 <> y4)
      (x5 <> y5)
      (x6 <> y6)

instance Monoid AggregateStats where
  mempty = AggregateStats mempty mempty mempty mempty mempty mempty

instance Pretty AggregateStats where
  pretty AggregateStats {..} =
    prettyRecord
      "AggregateStats"
      [ "StakingCredential" <:> gsCredentialStaking,
        "KeyHashStakePool" <:> gsKeyHashStakePool,
        "ScriptHash" <:> gsScriptHash
      ]

class AggregateStat s where
  aggregateStat :: s -> AggregateStats

instance AggregateStat (Stat (Credential 'Staking C)) where
  aggregateStat s = mempty {gsCredentialStaking = s}

instance AggregateStat (Stat (KeyHash 'StakePool C)) where
  aggregateStat s = mempty {gsKeyHashStakePool = s}

instance AggregateStat (Stat (ScriptHash C)) where
  aggregateStat s = mempty {gsScriptHash = s}

data UTxOUniques = UTxOUniques
  { paymentKeys :: !(Set.Set (KeyHash 'Payment C)),
    paymentScripts :: !(Set.Set (ScriptHash C)),
    stakeKeys :: !(Set.Set (KeyHash 'Staking C)),
    stakeScripts :: !(Set.Set (ScriptHash C)),
    stakePtrs :: !(Set.Set Ptr),
    scripts :: !(Set.Set (ScriptHash C)),
    txIds :: !(Set.Set (TxId C)),
    txIxs :: !(Set.Set Natural)
  }

emptyUniques :: UTxOUniques
emptyUniques = UTxOUniques mempty mempty mempty mempty mempty mempty mempty mempty

data UTxOStats' = UTxOStats'
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

initStats :: UTxOStats'
initStats = UTxOStats' 0 0 0 0 0 0 0 0

-- newtype UTxO era = UTxO (Map (TxId (Crypto era)) (IntMap (TxOut era)))
