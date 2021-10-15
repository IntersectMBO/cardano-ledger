{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.State.Orphans where

import Control.DeepSeq
import Cardano.Binary
import Cardano.Crypto.Hash.Class
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Coin
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley.CompactAddr
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.TxIn
import Data.ByteString.Short
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite

instance PersistField ShortByteString where
  toPersistValue = PersistByteString . fromShort
  fromPersistValue (PersistByteString bs) = Right $ toShort bs
  fromPersistValue _ = Left "Unexpected type"

instance PersistFieldSql ShortByteString where
  sqlType _ = SqlBlob

instance PersistField (TxId C) where
  toPersistValue = PersistByteString . hashToBytes . extractHash . _unTxId
  fromPersistValue (PersistByteString bs) =
    case hashFromBytes bs of
      Nothing -> Left "Invalid number of bytes for the hash"
      Just h -> Right $ TxId $ unsafeMakeSafeHash h
  fromPersistValue _ = Left "Unexpected type"

instance PersistFieldSql (TxId C) where
  sqlType _ = SqlBlob

instance PersistField Coin where
  toPersistValue = PersistInt64 . fromIntegral . unCoin
  fromPersistValue (PersistInt64 i64) = Right $ Coin $ fromIntegral i64
  fromPersistValue _ = Left "Unexpected type"

instance PersistFieldSql Coin where
  sqlType _ = SqlInt64

instance NFData (TxOut CurrentEra) where
  rnf = \case
    TxOutCompact _ _ -> ()
    TxOutCompactDH _ _ _ -> ()

instance PersistField (TxOut CurrentEra) where
  toPersistValue = PersistByteString . serialize'
  fromPersistValue = decodePersistValue

instance PersistFieldSql (TxOut CurrentEra) where
  sqlType _ = SqlBlob

instance PersistField (PPUPState CurrentEra) where
  toPersistValue = PersistByteString . serialize'
  fromPersistValue = decodePersistValue

instance PersistFieldSql (PPUPState CurrentEra) where
  sqlType _ = SqlBlob

instance PersistField (PState C) where
  toPersistValue = PersistByteString . serialize'
  fromPersistValue = decodePersistValue

instance PersistFieldSql (PState C) where
  sqlType _ = SqlBlob

instance PersistField (DState C) where
  toPersistValue = PersistByteString . serialize'
  fromPersistValue = decodePersistValue

instance PersistFieldSql (DState C) where
  sqlType _ = SqlBlob

decodePersistValue :: FromCBOR b => PersistValue -> Either T.Text b
decodePersistValue (PersistByteString bs) =
  case decodeFull' bs of
    Left err -> Left $ "Could not decode: " <> T.pack (show err)
    Right v -> Right v
decodePersistValue _ = Left "Unexpected type"
