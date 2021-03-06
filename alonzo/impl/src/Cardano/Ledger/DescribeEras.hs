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
    t1,
    t2,
    t3,
    t4,
  )
where

import Cardano.Crypto.DSIGN (Ed25519DSIGN, MockDSIGN)
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256, MD5Prefix)
import Cardano.Crypto.KES (MockKES, Sum6KES)
import Cardano.Crypto.VRF.Praos
-- (ShelleyEra,Era,proxy,Value,TxBody,TxOut,Script,AuxiliaryData,Tx,PParams)

-- (MaryEra,Era,proxy,Value,TxBody,TxOut,Script,AuxiliaryData,Tx,PParams)
import qualified Cardano.Ledger.Allegra as Allegra
-- (AllegraEra,Era,proxy,Value,TxBody,TxOut,Script,AuxiliaryData,Tx,PParams)
import qualified Cardano.Ledger.Alonzo as Alonzo
-- (AlonzoEra,Era,proxy,Value,TxBody,TxOut,Script,AuxiliaryData,Tx,PParams)
import Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (WellFormed)
import qualified Cardano.Ledger.Mary as Mary
import qualified Cardano.Ledger.Shelley as Shelley
import Shelley.Spec.Ledger.API (PraosCrypto)
import Test.Cardano.Crypto.VRF.Fake (FakeVRF)

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

class Checks era where
  checks :: Witness era -> Bool

instance DescribesShelley (Shelley.Era c) => Checks (Shelley.Era c) where
  checks (Shelley _) = True

instance DescribesAllegra (Allegra.Era c) => Checks (Allegra.Era c) where
  checks (Allegra _) = True

instance DescribesMary (Mary.Era c) => Checks (Mary.Era c) where
  checks (Mary _) = True

instance DescribesAlonzo (Alonzo.Era c) => Checks (Alonzo.Era c) where
  checks (Alonzo _) = True

t1, t2, t3, t4 :: Bool
t1 = checks (Shelley Standard)
t2 = checks (Allegra Standard)
t3 = checks (Mary Standard)
t4 = checks (Alonzo Standard)

{------------------------------------------------------------------------------
 First construct concrete versions of Crypto where the Hashing
 is concrete. Without this we won't be able to Hash things
------------------------------------------------------------------------------}
data TestCrypto

instance CryptoClass.Crypto TestCrypto where
  type HASH TestCrypto = MD5Prefix 10
  type ADDRHASH TestCrypto = MD5Prefix 8
  type DSIGN TestCrypto = MockDSIGN
  type KES TestCrypto = MockKES 10
  type VRF TestCrypto = FakeVRF

instance PraosCrypto TestCrypto

data StandardCrypto

instance CryptoClass.Crypto StandardCrypto where
  type DSIGN StandardCrypto = Ed25519DSIGN
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF StandardCrypto = PraosVRF
  type HASH StandardCrypto = Blake2b_256
  type ADDRHASH StandardCrypto = Blake2b_224

instance PraosCrypto StandardCrypto

data Evidence c where
  Standard :: Evidence StandardCrypto
  Test :: Evidence TestCrypto

data Witness era where
  Shelley :: Evidence c -> Witness (Shelley.ShelleyEra c)
  Mary :: Evidence c -> Witness (Mary.Era c)
  Allegra :: Evidence c -> Witness (Allegra.Era c)
  Alonzo :: Evidence c -> Witness (Alonzo.AlonzoEra c)
