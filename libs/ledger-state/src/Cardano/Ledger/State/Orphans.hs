{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.State.Orphans where

import Cardano.Crypto.Hash.Class
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PoolRank
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.TxIn
import Data.ByteString.Short
import qualified Data.Text as T
import Data.Typeable
import Database.Persist
import Database.Persist.Sqlite

data SnapShotType
  = SnapShotMark
  | SnapShotSet
  | SnapShotGo
  deriving (Show, Eq, Enum, Bounded)

instance PersistField SnapShotType where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 i64) = Right $ toEnum $ fromIntegral i64
  fromPersistValue _ = Left "Unexpected type"

instance PersistFieldSql SnapShotType where
  sqlType _ = SqlInt32

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

deriving instance PersistField (CompactForm Coin)

deriving instance PersistFieldSql (CompactForm Coin)

deriving instance PersistField TxIx

deriving instance PersistFieldSql TxIx

instance PersistField Coin where
  toPersistValue = PersistInt64 . fromIntegral . unCoin
  fromPersistValue (PersistInt64 i64) = Right $ Coin $ fromIntegral i64
  fromPersistValue _ = Left "Unexpected type"

instance PersistFieldSql Coin where
  sqlType _ = SqlInt64

instance PersistField DeltaCoin where
  toPersistValue (DeltaCoin dc) = PersistInt64 $ fromIntegral dc
  fromPersistValue (PersistInt64 i64) = Right $ DeltaCoin $ fromIntegral i64
  fromPersistValue _ = Left "Unexpected type"

instance PersistFieldSql DeltaCoin where
  sqlType _ = SqlInt64

newtype Enc a = Enc {unEnc :: a}

instance (ToCBOR a, FromCBOR a) => PersistField (Enc a) where
  toPersistValue = PersistByteString . serialize' (eraProtVerHigh @CurrentEra) . unEnc
  fromPersistValue = fmap Enc . decodePersistValue

instance (ToCBOR a, FromCBOR a) => PersistFieldSql (Enc a) where
  sqlType _ = SqlBlob

decodePersistValue :: FromCBOR b => PersistValue -> Either T.Text b
decodePersistValue (PersistByteString bs) =
  case decodeFull' (eraProtVerHigh @CurrentEra) bs of
    Left err -> Left $ "Could not decode: " <> T.pack (show err)
    Right v -> Right v
decodePersistValue _ = Left "Unexpected type"

deriving via Enc (KeyHash r C) instance Typeable r => PersistField (KeyHash r C)

deriving via Enc (KeyHash r C) instance Typeable r => PersistFieldSql (KeyHash r C)

deriving via Enc (Credential r C) instance Typeable r => PersistField (Credential r C)

deriving via Enc (Credential r C) instance Typeable r => PersistFieldSql (Credential r C)

deriving via Enc Ptr instance PersistField Ptr

deriving via Enc Ptr instance PersistFieldSql Ptr

deriving via Enc (PPUPState CurrentEra) instance PersistField (PPUPState CurrentEra)

deriving via Enc (PPUPState CurrentEra) instance PersistFieldSql (PPUPState CurrentEra)

deriving via Enc (AlonzoTxOut CurrentEra) instance PersistField (AlonzoTxOut CurrentEra)

deriving via Enc (AlonzoTxOut CurrentEra) instance PersistFieldSql (AlonzoTxOut CurrentEra)

instance FromCBOR (DState C) where
  fromCBOR = fromNotSharedCBOR

deriving via Enc (DState C) instance PersistField (DState C)

deriving via Enc (DState C) instance PersistFieldSql (DState C)

deriving via Enc (PState C) instance PersistField (PState C)

deriving via Enc (PState C) instance PersistFieldSql (PState C)

deriving via Enc (GenDelegs C) instance PersistField (GenDelegs C)

deriving via Enc (GenDelegs C) instance PersistFieldSql (GenDelegs C)

deriving via Enc (PoolParams C) instance PersistField (PoolParams C)

deriving via Enc (PoolParams C) instance PersistFieldSql (PoolParams C)

instance FromCBOR (NonMyopic C) where
  fromCBOR = fromNotSharedCBOR

deriving via Enc (NonMyopic C) instance PersistField (NonMyopic C)

deriving via Enc (NonMyopic C) instance PersistFieldSql (NonMyopic C)

deriving via Enc (AlonzoPParams CurrentEra) instance PersistField (AlonzoPParams CurrentEra)

deriving via Enc (AlonzoPParams CurrentEra) instance PersistFieldSql (AlonzoPParams CurrentEra)
