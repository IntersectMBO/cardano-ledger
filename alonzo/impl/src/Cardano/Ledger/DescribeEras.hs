{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.DescribeEras
  ( Witness (..),
    Checks (..),
    DescribesShelley,
    DescribesAllegra,
    DescribesMary,
    DescribesAlonzo,
    StandardCrypto,
  )
where

import qualified Cardano.Ledger.Allegra as Allegra
import qualified Cardano.Ledger.Alonzo as Alonzo
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (WellFormed)
import qualified Cardano.Ledger.Mary as Mary
import qualified Cardano.Ledger.Shelley as Shelley

type DescribesShelley era =
  ( WellFormed era,
    Core.Value era ~ Shelley.Value era,
    Core.TxBody era ~ Shelley.TxBody era,
    Core.TxOut era ~ Shelley.TxOut era,
    Core.Script era ~ Shelley.Script era,
    Core.AuxiliaryData era ~ Shelley.AuxiliaryData era,
    Core.PParams era ~ Shelley.PParams era,
    Core.PParamsDelta era ~ Shelley.PParamsDelta era
  )

type DescribesAllegra era =
  ( WellFormed era,
    Core.Value era ~ Shelley.Value era,
    Core.TxBody era ~ Allegra.TxBody era,
    Core.TxOut era ~ Shelley.TxOut era,
    Core.Script era ~ Allegra.Script era,
    Core.AuxiliaryData era ~ Allegra.AuxiliaryData era,
    Core.PParams era ~ Shelley.PParams era,
    Core.PParamsDelta era ~ Shelley.PParamsDelta era
  )

type DescribesMary era =
  ( WellFormed era,
    Core.Value era ~ Mary.Value era,
    Core.TxBody era ~ Allegra.TxBody era,
    Core.TxOut era ~ Shelley.TxOut era,
    Core.Script era ~ Allegra.Script era,
    Core.AuxiliaryData era ~ Allegra.AuxiliaryData era,
    Core.PParams era ~ Shelley.PParams era,
    Core.PParamsDelta era ~ Shelley.PParamsDelta era
  )

type DescribesAlonzo era =
  ( WellFormed era,
    Core.Value era ~ Mary.Value era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.Script era ~ Alonzo.Script era,
    Core.AuxiliaryData era ~ Alonzo.AuxiliaryData era,
    Core.PParams era ~ Alonzo.PParams era,
    Core.PParamsDelta era ~ Alonzo.PParamsDelta era
  )

-- | If an instance for this class compiles, then era meets whatever superclass its given.
class Checks era where
  checks :: Witness era -> Bool

instance DescribesShelley (Shelley.Self c) => Checks (Shelley.Self c) where
  checks Shelley = True

instance DescribesAllegra (Allegra.Self c) => Checks (Allegra.Self c) where
  checks Allegra = True

instance DescribesMary (Mary.Self c) => Checks (Mary.Self c) where
  checks Mary = True

instance DescribesAlonzo (Alonzo.Self c) => Checks (Alonzo.Self c) where
  checks Alonzo = True

-- ==========================================================

-- | Witness of a valid (predefined) era
data Witness era where
  Shelley :: Witness (Shelley.ShelleyEra StandardCrypto)
  Mary :: Witness (Mary.MaryEra StandardCrypto)
  Allegra :: Witness (Allegra.AllegraEra StandardCrypto)
  Alonzo :: Witness (Alonzo.AlonzoEra StandardCrypto)

instance Show (Witness e) where
  show = \case
    Shelley -> "Shelley"
    Allegra -> "Allegra"
    Mary -> "Mary"
    Alonzo -> "Alonzo"
