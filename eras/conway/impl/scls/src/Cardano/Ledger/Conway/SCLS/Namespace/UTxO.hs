{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Werror #-}

-- | UTxO namespace export.
module Cardano.Ledger.Conway.SCLS.Namespace.UTxO (
  UtxoKey (..),
  UtxoOut (..),
  module Cardano.Ledger.SCLS.Namespace.UTxO.V0,
) where

import Cardano.Ledger.Address (compactAddr, decompactAddr)
import Cardano.Ledger.Allegra.Scripts (Timelock (..), TimelockRaw (..))
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Scripts (AlonzoScript (..), PlutusScript (..))
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.MemoBytes (getMemoRawType, mkMemoizedEra)
import Cardano.Ledger.Plutus.Data (BinaryData, Datum (..))
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.SCLS.Common
import Cardano.Ledger.SCLS.Common ()
import Cardano.Ledger.SCLS.Namespace.UTxO.V0
import qualified Cardano.Ledger.Shelley.TxOut as Shelley
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
-- import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

-- import Unsafe.Coerce (unsafeCoerce)
-- import Debug.Trace (trace)

instance IsCanonicalScript (AlonzoScript ConwayEra) where
  mkCanonicalScript (NativeScript n) = CanonicalScriptNative (mkCanonicalNativeScript n)
  mkCanonicalScript (PlutusScript bs) = CanonicalScriptPlutus (mkCanonicalPlutusScript bs)
  fromCanonicalScript (CanonicalScriptNative n) = NativeScript (fromCanonicalNativeScript n)
  fromCanonicalScript (CanonicalScriptPlutus n) = PlutusScript (fromCanonicalPlutusScript n)

instance IsCanonicalPlutusScript (PlutusScript ConwayEra) where
  mkCanonicalPlutusScript (ConwayPlutusV1 (Plutus pv1)) = CanonicalPlutusScriptV1 pv1
  mkCanonicalPlutusScript (ConwayPlutusV2 (Plutus pv2)) = CanonicalPlutusScriptV2 pv2
  mkCanonicalPlutusScript (ConwayPlutusV3 (Plutus pv3)) = CanonicalPlutusScriptV3 pv3
  fromCanonicalPlutusScript (CanonicalPlutusScriptV1 pv1) = ConwayPlutusV1 (Plutus pv1)
  fromCanonicalPlutusScript (CanonicalPlutusScriptV2 pv2) = ConwayPlutusV2 (Plutus pv2)
  fromCanonicalPlutusScript (CanonicalPlutusScriptV3 pv3) = ConwayPlutusV3 (Plutus pv3)

instance IsCanonicalNativeScript (TimelockRaw ConwayEra) where
  mkCanonicalNativeScript (TimelockSignature v) = CanonicalNativeScriptPubKey v
  mkCanonicalNativeScript (TimelockAllOf v) = CanonicalNativeScriptAllOf (mkCanonicalNativeScript <$> v)
  mkCanonicalNativeScript (TimelockAnyOf v) = CanonicalNativeScriptAnyOf (mkCanonicalNativeScript <$> v)
  mkCanonicalNativeScript (TimelockMOf f g) = CanonicalNativeScriptMOfN f (mkCanonicalNativeScript <$> g)
  mkCanonicalNativeScript (TimelockTimeStart t) = CanonicalNativeScriptInvalidBefore t
  mkCanonicalNativeScript (TimelockTimeExpire t) = CanonicalNativeScriptInvalidAfter t
  fromCanonicalNativeScript (CanonicalNativeScriptPubKey v) = TimelockSignature v
  fromCanonicalNativeScript (CanonicalNativeScriptAllOf v) = TimelockAllOf (mkMemoizedEra @ConwayEra . fromCanonicalNativeScript <$> v)
  fromCanonicalNativeScript (CanonicalNativeScriptAnyOf v) = TimelockAnyOf (mkMemoizedEra @ConwayEra . fromCanonicalNativeScript <$> v)
  fromCanonicalNativeScript (CanonicalNativeScriptMOfN f g) = TimelockMOf f (mkMemoizedEra @ConwayEra . fromCanonicalNativeScript <$> g)
  fromCanonicalNativeScript (CanonicalNativeScriptInvalidBefore t) = TimelockTimeStart t
  fromCanonicalNativeScript (CanonicalNativeScriptInvalidAfter t) = TimelockTimeExpire t

instance IsCanonicalNativeScript (Timelock ConwayEra) where
  mkCanonicalNativeScript = mkCanonicalNativeScript . getMemoRawType
  fromCanonicalNativeScript = mkMemoizedEra @ConwayEra . fromCanonicalNativeScript


instance IsCanonicalDatum (Datum ConwayEra) where
  mkCanonicalDatum NoDatum = CanonicalNoDatum
  mkCanonicalDatum (DatumHash dh) = CanonicalDatumHash dh
  mkCanonicalDatum (Datum d) =
    CanonicalDatum
      (toStrictByteString $ toPlainEncoding (natVersion @9) (encCBOR (d :: BinaryData ConwayEra)))

  fromCanonicalDatum CanonicalNoDatum = NoDatum
  fromCanonicalDatum (CanonicalDatumHash bs) = DatumHash bs
  fromCanonicalDatum (CanonicalDatum d) =
    case deserialiseFromBytes (toPlainDecoder Nothing (natVersion @9) decCBOR) (BL.fromStrict d) of
      Right (_, val) -> Datum val
      Left e -> error $ "Unexpected failure in fromCanonicalDatum: " ++ show e

instance IsCanonicalBabbageTxOut (Babbage.BabbageTxOut ConwayEra) where
  mkCanonicalBabbageTxOut (Babbage.BabbageTxOut babbageTxOutAddr babbageTxOutValue babbageTxOutDatum babbageTxOutRefScript) =
    CanonicalBabbageTxOut
      { babbageTxOutCompactAddr = compactAddr babbageTxOutAddr
      , babbageTxOutCompactValue = mkCanonicalValue babbageTxOutValue
      , babbageTxOutDatum = mkCanonicalDatum babbageTxOutDatum
      , babbageTxOutRefScript = mkCanonicalScript <$> babbageTxOutRefScript
      }
  fromCanonicalBabbageTxOut
    CanonicalBabbageTxOut
      { babbageTxOutCompactAddr
      , babbageTxOutCompactValue
      , babbageTxOutDatum
      , babbageTxOutRefScript
      } =
      Babbage.BabbageTxOut
        (decompactAddr babbageTxOutCompactAddr)
        (fromCanonicalValue babbageTxOutCompactValue)
        (fromCanonicalDatum babbageTxOutDatum)
        (fromCanonicalScript <$> babbageTxOutRefScript)

instance IsCanonicalShelleyTxOut (Shelley.ShelleyTxOut ConwayEra) where
  mkCanonicalShelleyTxOut (Shelley.ShelleyTxOut addr value) =
    CanonicalShelleyTxOut
      { txOutCompactValue = mkCanonicalValue value
      , txOutCompactAddr = compactAddr addr
      , txOutDatumHash = SNothing
      }
  fromCanonicalShelleyTxOut CanonicalShelleyTxOut {..} =
    Shelley.ShelleyTxOut
      (decompactAddr txOutCompactAddr)
      (fromCanonicalValue txOutCompactValue)

instance IsCanonicalValue MaryValue where
  mkCanonicalValue (MaryValue coin (MultiAsset multiAsset)) =
    CanonicalValue canonicalValueCoin canonicalValueMultiAsset
    where
      canonicalValueCoin = mkCanonicalCoin coin
      canonicalValueMultiAsset =
        Map.fromList
          [ ( scriptHash
            , Map.fromList
                [ (name, mkCanonicalCoin asset)
                | (AssetName name, asset) <- Map.toList assets
                ]
            )
          | (PolicyID scriptHash, assets) <- Map.toList multiAsset
          ]
  fromCanonicalValue (CanonicalValue coin multiAsset) =
    MaryValue
      (fromCanonicalCoin coin)
      ( MultiAsset $!
          Map.fromList
            [ ( PolicyID scriptHash
              , Map.fromList
                  [ (AssetName name, fromCanonicalCoin asset)
                  | (name, asset) <- Map.toList assets
                  ]
              )
            | (scriptHash, assets) <- Map.toList multiAsset
            ]
      )
