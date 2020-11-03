{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.Scripts (Script (..)) where

import Cardano.Binary
  ( Annotator,
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Shelley as Shelley
import Cardano.Ledger.ShelleyMA (MaryOrAllegra, ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock,
    ValidityInterval,
    hashTimelockScript,
    validateTimelock,
  )
import Data.Coders (decodeRecordSum, invalidKey)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Tx
  ( ValidateScript (..),
    hashMultiSigScript,
    validateNativeMultiSigScript,
  )

data Script era
  = ScriptMSig (MultiSig era)
  | ScriptTimelock (Timelock era)
  deriving (Show, Typeable, Eq, Generic)

instance Typeable era => NoThunks (Script era)

instance Typeable era => ToCBOR (Script era) where
  toCBOR = \case
    ScriptMSig s -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR s
    ScriptTimelock s -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR s

instance Era era => FromCBOR (Annotator (Script era)) where
  fromCBOR =
    decodeRecordSum "Script" $
      \case
        0 -> do
          msig <- fromCBOR
          pure (2, ScriptMSig <$> msig)
        1 -> do
          tl <- fromCBOR
          pure (2, ScriptTimelock <$> tl)
        k -> invalidKey k

type instance
  Core.Script (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    Script (ShelleyMAEra ma c)

instance
  ( CryptoClass.Crypto c,
    Typeable ma,
    Shelley.TxBodyConstraints (ShelleyMAEra ma c),
    (HasField "vldt" (Core.TxBody (ShelleyMAEra ma c)) ValidityInterval)
  ) =>
  ValidateScript (ShelleyMAEra ma c)
  where
  validateScript (ScriptMSig s) tx = validateNativeMultiSigScript s tx
  validateScript (ScriptTimelock s) tx = validateTimelock s tx

  hashScript (ScriptMSig s) = hashMultiSigScript s
  hashScript (ScriptTimelock s) = hashTimelockScript s
