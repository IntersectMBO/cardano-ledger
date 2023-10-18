{-# LANGUAGE DerivingStrategies #-}

module Cardano.Ledger.Alonzo.RedeemerPointer
  (
  ) where

data AlonzoRedeemerPointer
  = AlonzoRedeemerPointer
      !Tag
      {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, Generic)

instance NoThunks AlonzoRedeemerPointer

instance NFData AlonzoRedeemerPointer

instance ToExpr AlonzoRedeemerPointer

-- EncCBOR and DecCBOR for AlonzoRedeemerPointer is used in UTXOW for error reporting
instance DecCBOR AlonzoRedeemerPointer where
  decCBOR = AlonzoRedeemerPointer <$> decCBOR <*> decCBOR

instance EncCBOR AlonzoRedeemerPointer where
  encCBOR (AlonzoRedeemerPointer t w) = encCBOR t <> encCBOR w

instance EncCBORGroup AlonzoRedeemerPointer where
  listLen _ = 2
  listLenBound _ = 2
  encCBORGroup (AlonzoRedeemerPointer t w) = encCBOR t <> encCBOR w
  encodedGroupSizeExpr size_ _proxy =
    encodedSizeExpr size_ (Proxy :: Proxy Tag)
      + encodedSizeExpr size_ (Proxy :: Proxy Word64)

instance DecCBORGroup AlonzoRedeemerPointer where
  decCBORGroup = AlonzoRedeemerPointer <$> decCBOR <*> decCBOR

newtype RedeemersRaw era = RedeemersRaw (Map AlonzoRedeemerPointer (Data era, ExUnits))
  deriving (Eq, Generic, Typeable, NFData)
  deriving newtype (NoThunks)
