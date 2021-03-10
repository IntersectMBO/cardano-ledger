{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.DescribeEras
  ( Evidence (..),
    Witness (..),
    Checks (..),
    DescribesShelley,
    DescribesAllegra,
    DescribesMary,
    DescribesAlonzo,
    StandardCrypto,
    TestCrypto,
  )
where

import qualified Cardano.Ledger.Allegra as Allegra
import qualified Cardano.Ledger.Alonzo as Alonzo
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto, TestCrypto)
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
    Core.Tx era ~ Shelley.Tx era
  )

type DescribesAllegra era =
  ( WellFormed era,
    Core.Value era ~ Shelley.Value era,
    Core.TxBody era ~ Allegra.TxBody era,
    Core.TxOut era ~ Shelley.TxOut era,
    Core.Script era ~ Allegra.Script era,
    Core.AuxiliaryData era ~ Allegra.AuxiliaryData era,
    Core.PParams era ~ Shelley.PParams era,
    Core.Tx era ~ Shelley.Tx era
  )

type DescribesMary era =
  ( WellFormed era,
    Core.Value era ~ Mary.Value era,
    Core.TxBody era ~ Allegra.TxBody era,
    Core.TxOut era ~ Shelley.TxOut era,
    Core.Script era ~ Allegra.Script era,
    Core.AuxiliaryData era ~ Allegra.AuxiliaryData era,
    Core.PParams era ~ Shelley.PParams era,
    Core.Tx era ~ Shelley.Tx era
  )

type DescribesAlonzo era =
  ( WellFormed era,
    Core.Value era ~ Mary.Value era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.Script era ~ Alonzo.Script era,
    Core.AuxiliaryData era ~ Alonzo.AuxiliaryData era,
    Core.PParams era ~ Alonzo.PParams era,
    Core.Tx era ~ Alonzo.Tx era
  )

-- | If an instance for this class compiles, then era meets whatever superclass its given.
class Checks era where
  checks :: Witness era -> Bool

instance DescribesShelley (Shelley.Self c) => Checks (Shelley.Self c) where
  checks (Shelley _) = True

instance DescribesAllegra (Allegra.Self c) => Checks (Allegra.Self c) where
  checks (Allegra _) = True

instance DescribesMary (Mary.Self c) => Checks (Mary.Self c) where
  checks (Mary _) = True

instance DescribesAlonzo (Alonzo.Self c) => Checks (Alonzo.Self c) where
  checks (Alonzo _) = True

-- ==========================================================

-- | Evidence that a valid (predefined) crypto exists
data Evidence c where
  Standard :: Evidence StandardCrypto
  Test :: Evidence TestCrypto

instance Show (Evidence c) where
  show Standard = "Standard"
  show Test = "Test"

-- | Witness of a valid (predefined) era
data Witness era where
  Shelley :: Evidence c -> Witness (Shelley.ShelleyEra c)
  Mary :: Evidence c -> Witness (Mary.MaryEra c)
  Allegra :: Evidence c -> Witness (Allegra.AllegraEra c)
  Alonzo :: Evidence c -> Witness (Alonzo.AlonzoEra c)

instance Show (Witness e) where
  show (Shelley c) = "Shelley " ++ show c
  show (Allegra c) = "Allegra " ++ show c
  show (Mary c) = "Mary " ++ show c
  show (Alonzo c) = "Alonzo " ++ show c
