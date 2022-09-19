{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Babbage.Core
  ( BabbageEraTxOut (..),
    BabbageEraTxBody (..),
    BabbageEraPParams (..),
    CoinPerByte (..),
    ppCoinsPerUTxOByteL,
    ppuCoinsPerUTxOByteL,
    module Cardano.Ledger.Alonzo.Core,
  )
where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Data (Data, Datum)
import Cardano.Ledger.Binary (FromCBOR, Sized (..), ToCBOR)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.HKD (HKD, HKDFunctor)
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Lens.Micro (Lens', SimpleGetter)
import NoThunks.Class (NoThunks)

class (AlonzoEraTxOut era, EraScript era) => BabbageEraTxOut era where
  referenceScriptTxOutL :: Lens' (TxOut era) (StrictMaybe (Script era))

  dataTxOutL :: Lens' (TxOut era) (StrictMaybe (Data era))

  datumTxOutL :: Lens' (TxOut era) (Datum era)

class (AlonzoEraTxBody era, BabbageEraTxOut era) => BabbageEraTxBody era where
  sizedOutputsTxBodyL :: Lens' (TxBody era) (StrictSeq (Sized (TxOut era)))

  referenceInputsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

  totalCollateralTxBodyL :: Lens' (TxBody era) (StrictMaybe Coin)

  collateralReturnTxBodyL :: Lens' (TxBody era) (StrictMaybe (TxOut era))

  sizedCollateralReturnTxBodyL :: Lens' (TxBody era) (StrictMaybe (Sized (TxOut era)))

  allSizedOutputsTxBodyF :: SimpleGetter (TxBody era) (StrictSeq (Sized (TxOut era)))

newtype CoinPerByte = CoinPerByte Coin
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToCBOR, FromCBOR, ToJSON, FromJSON, NFData, NoThunks, ToExpr)

class AlonzoEraPParams era => BabbageEraPParams era where
  hkdCoinsPerUTxOByteL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f CoinPerByte)

ppCoinsPerUTxOByteL ::
  forall era. BabbageEraPParams era => Lens' (PParams era) CoinPerByte
ppCoinsPerUTxOByteL = ppLens . hkdCoinsPerUTxOByteL @era @Identity

ppuCoinsPerUTxOByteL ::
  forall era. BabbageEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe CoinPerByte)
ppuCoinsPerUTxOByteL = ppuLens . hkdCoinsPerUTxOByteL @era @StrictMaybe
