{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conformance (
  SpecTranslate (..),
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..))
import Cardano.Crypto.Hash (Hash, hashToBytes)
import Cardano.Ledger.Address (Addr (..), serialiseAddr)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo (AlonzoScript, AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Scripts (CostModels, ExUnits (..), Prices, Tag (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Data, Datum (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoTxWits (..),
  RdmrPtr (..),
  Redeemers (..),
  TxDats (..),
 )
import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe,
  TxIx (..),
  getVersion,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  AlonzoEraTxBody (..),
  CoinPerByte (..),
  EraScript (..),
  EraTxWits (..),
  vldtTxBodyL,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  DRepVotingThresholds,
  PoolVotingThresholds,
  THKD (..),
 )
import Cardano.Ledger.Conway.TxOut (BabbageTxOut (..))
import Cardano.Ledger.Core (
  Era,
  EraIndependentData,
  EraIndependentScriptIntegrity,
  EraIndependentTxBody,
  EraTx (..),
  EraTxAuxData (..),
  EraTxBody (..),
  EraTxOut (..),
  PParams,
 )
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (KeyHash (..), VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (Identity, UtxoEnv (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Data.Bitraversable (bimapM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data (Typeable)
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import Lens.Micro ((^.))
import qualified Lib as Agda

type SpecTranslationError = Text

class SpecTranslate a where
  type SpecRep a

  toSpecRep :: a -> Either SpecTranslationError (SpecRep a)

instance SpecTranslate TxIx where
  type SpecRep TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate (TxIn era) where
  type SpecRep (TxIn era) = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRep (txId, txIx)

byteStringToInteger :: ByteString -> Integer
byteStringToInteger = BS.foldr' (\x y -> y * 256 + toInteger x) 0

instance SpecTranslate (Addr era) where
  type SpecRep (Addr era) = Agda.Addr

  toSpecRep = pure . byteStringToInteger . serialiseAddr

instance SpecTranslate (SafeHash c EraIndependentData) where
  type SpecRep (SafeHash c EraIndependentData) = Agda.DataHash

  toSpecRep _ = pure ()

instance SpecTranslate (SafeHash c EraIndependentScriptIntegrity) where
  type SpecRep (SafeHash c EraIndependentScriptIntegrity) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate (SafeHash c EraIndependentTxBody) where
  type SpecRep (SafeHash c EraIndependentTxBody) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance EraTxOut era => SpecTranslate (BabbageTxOut era) where
  type SpecRep (BabbageTxOut era) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum _) =
    let mkTxOut x = do
          addr' <- toSpecRep addr
          coin' <- toSpecRep $ coin val
          pure (addr', (coin', x))
     in case datum of
          NoDatum -> mkTxOut Nothing
          DatumHash h -> mkTxOut . Just =<< toSpecRep h
          Datum _ -> Left "Inline datums not supported by spec"

instance SpecTranslate Integer where
  type SpecRep Integer = Integer

  toSpecRep = pure

deriving instance SpecTranslate Coin

deriving instance
  ( SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate (UTxO era)

instance
  ( SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate (UTxOState era)
  where
  type SpecRep (UTxOState era) = Agda.UTxOState

  toSpecRep x =
    Agda.MkUTxOState
      <$> toSpecRep (utxosUtxo x)
      <*> toSpecRep (utxosFees x)

deriving instance SpecTranslate SlotNo

deriving instance SpecTranslate EpochNo

instance SpecTranslate ProtVer where
  type SpecRep ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

data VotingThresholds
  = VotingThresholds
      DRepVotingThresholds
      PoolVotingThresholds

instance SpecTranslate VotingThresholds where
  type SpecRep VotingThresholds = ()

  toSpecRep _ = pure ()

instance SpecTranslate CostModels where
  type SpecRep CostModels = Agda.AgdaEmpty

  toSpecRep _ =
    pure $
      error "TODO change this to unit once it is fixed in the spec"

instance SpecTranslate Prices where
  type SpecRep Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate ExUnits where
  type SpecRep ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

deriving instance SpecTranslate OrdExUnits

deriving instance SpecTranslate CoinPerByte

instance SpecTranslate (HKD f a) => SpecTranslate (THKD r f a) where
  type SpecRep (THKD r f a) = SpecRep (HKD f a)

  toSpecRep = toSpecRep . unTHKD

instance SpecTranslate (ConwayPParams Identity era) where
  type SpecRep (ConwayPParams Identity era) = Agda.PParams

  toSpecRep x =
    Agda.MkPParams
      <$> toSpecRep (cppMinFeeA x)
      <*> toSpecRep (cppMinFeeB x)
      <*> Right (toInteger . unTHKD $ cppMaxBBSize x)
      <*> Right (toInteger . unTHKD $ cppMaxTxSize x)
      <*> Right (toInteger . unTHKD $ cppMaxBHSize x)
      <*> Right (toInteger . unTHKD $ cppMaxValSize x)
      <*> Right (error "minUTxO is not in Conway, do not use this field")
      <*> toSpecRep (cppPoolDeposit x)
      <*> toSpecRep (cppEMax x)
      <*> toSpecRep (cppProtocolVersion x)
      <*> toSpecRep
        ( VotingThresholds
            (unTHKD $ cppDRepVotingThresholds x)
            (unTHKD $ cppPoolVotingThresholds x)
        )
      <*> toSpecRep (cppGovActionLifetime x)
      <*> toSpecRep (cppGovActionDeposit x)
      <*> toSpecRep (cppDRepDeposit x)
      <*> toSpecRep (cppDRepActivity x)
      <*> Right (toInteger . unTHKD $ cppCommitteeMinSize x)
      <*> Right (toInteger . unTHKD $ cppCommitteeMaxTermLength x)
      -- This is the minimumAVS field which we ended up not needing. It will be
      -- removed from the spec.
      <*> Right (error "minimumAVS is not in Conway, do not use this field")
      <*> toSpecRep (cppCostModels x)
      <*> toSpecRep (cppPrices x)
      <*> toSpecRep (cppMaxTxExUnits x)
      <*> toSpecRep (cppMaxBlockExUnits x)
      <*> toSpecRep (cppCoinsPerUTxOByte x)
      <*> Right (toInteger . unTHKD $ cppMaxCollateralInputs x)

instance
  ( SpecTranslate (PParams era)
  , SpecRep (PParams era) ~ Agda.PParams
  ) =>
  SpecTranslate (UtxoEnv era)
  where
  type SpecRep (UtxoEnv era) = Agda.UTxOEnv

  toSpecRep x =
    Agda.MkUTxOEnv
      <$> toSpecRep (ueSlot x)
      <*> toSpecRep (uePParams x)

instance SpecTranslate a => SpecTranslate (Set a) where
  type SpecRep (Set a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . Set.toList

instance SpecTranslate a => SpecTranslate (StrictSeq a) where
  type SpecRep (StrictSeq a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate a => SpecTranslate (Sized a) where
  type SpecRep (Sized a) = SpecRep a

  toSpecRep (Sized x _) = toSpecRep x

instance SpecTranslate ValidityInterval where
  type SpecRep ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRep (lo, hi)

instance SpecTranslate (Hash a b) where
  type SpecRep (Hash a b) = Agda.Hash

  toSpecRep = pure . byteStringToInteger . hashToBytes

deriving instance SpecTranslate (KeyHash r c)

instance Crypto c => SpecTranslate (VKey k c) where
  type SpecRep (VKey k c) = Integer

  toSpecRep (VKey x) = pure . byteStringToInteger $ rawSerialiseVerKeyDSIGN x

instance DSIGNAlgorithm v => SpecTranslate (SignedDSIGN v a) where
  type SpecRep (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) =
    pure . byteStringToInteger $ rawSerialiseSigDSIGN x

instance (Crypto c, Typeable k) => SpecTranslate (WitVKey k c) where
  type SpecRep (WitVKey k c) = (Integer, Integer)

  toSpecRep (WitVKey vk sk) = toSpecRep (vk, sk)

instance Era era => SpecTranslate (TxDats era) where
  type SpecRep (TxDats era) = [(Agda.DataHash, Agda.Datum)]

  toSpecRep (TxDats x) = toSpecRep x

instance (SpecTranslate k, SpecTranslate v) => SpecTranslate (Map k v) where
  type SpecRep (Map k v) = [(SpecRep k, SpecRep v)]

  toSpecRep = traverse (bimapM toSpecRep toSpecRep) . Map.toList

instance SpecTranslate Tag where
  type SpecRep Tag = Agda.Tag

  toSpecRep Spend = pure Agda.Spend
  toSpecRep Mint = pure Agda.Mint
  toSpecRep Cert = pure Agda.Cert
  toSpecRep Rewrd = pure Agda.Rewrd

instance SpecTranslate Word64 where
  type SpecRep Word64 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate RdmrPtr where
  type SpecRep RdmrPtr = Agda.RdmrPtr

  toSpecRep (RdmrPtr t x) = toSpecRep (t, x)

instance (SpecTranslate a, SpecTranslate b) => SpecTranslate (a, b) where
  type SpecRep (a, b) = (SpecRep a, SpecRep b)

  toSpecRep (x, y) = (,) <$> toSpecRep x <*> toSpecRep y

instance SpecTranslate (Data era) where
  type SpecRep (Data era) = ()

  toSpecRep _ = pure ()

instance Era era => SpecTranslate (Redeemers era) where
  type SpecRep (Redeemers era) = [(Agda.RdmrPtr, (Agda.Redeemer, Agda.ExUnits))]

  toSpecRep (Redeemers x) = toSpecRep x

instance
  ( Era era
  , Script era ~ AlonzoScript era
  ) =>
  SpecTranslate (AlonzoTxWits era)
  where
  type SpecRep (AlonzoTxWits era) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> toSpecRep (txwitsVKey x)
      <*> pure []
      <*> toSpecRep (txdats x)
      <*> toSpecRep (txrdmrs x)

instance SpecTranslate a => SpecTranslate (StrictMaybe a) where
  type SpecRep (StrictMaybe a) = Maybe (SpecRep a)

  toSpecRep = traverse toSpecRep . strictMaybeToMaybe

instance SpecTranslate (AlonzoTxAuxData era) where
  type SpecRep (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep _ = pure ()

deriving instance SpecTranslate (TxId era)

toAgdaTxBody ::
  ( SpecRep (TxOut era) ~ Agda.TxOut
  , EraTx era
  , AlonzoEraTxBody era
  , SpecTranslate (TxOut era)
  ) =>
  Tx era ->
  Either SpecTranslationError Agda.TxBody
toAgdaTxBody tx =
  Agda.MkTxBody
    <$> toSpecRep (tx ^. bodyTxL . inputsTxBodyL)
    <*> (zip [0 ..] <$> toSpecRep (tx ^. bodyTxL . outputsTxBodyL))
    <*> toSpecRep (tx ^. bodyTxL . feeTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . vldtTxBodyL)
    <*> pure (tx ^. sizeTxF)
    <*> toSpecRep (txid $ tx ^. bodyTxL)
    <*> toSpecRep (tx ^. bodyTxL . collateralInputsTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . reqSignerHashesTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . scriptIntegrityHashTxBodyL)

instance
  ( SpecRep (TxBody era) ~ Agda.TxBody
  , SpecTranslate (TxWits era)
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecTranslate (TxAuxData era)
  , SpecRep (TxAuxData era) ~ Agda.AuxiliaryData
  , SpecRep (TxOut era) ~ Agda.TxOut
  , Tx era ~ AlonzoTx era
  , EraTx era
  , AlonzoEraTxBody era
  , SpecTranslate (TxOut era)
  ) =>
  SpecTranslate (AlonzoTx era)
  where
  type SpecRep (AlonzoTx era) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> toAgdaTxBody @era tx
      <*> toSpecRep (wits tx)
      <*> toSpecRep (auxiliaryData tx)
