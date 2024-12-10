{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxOut () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.TxOut (
  ShelleyTxOut (..),
  addrEitherShelleyTxOutL,
  valueEitherShelleyTxOutL,
 )
import Data.Coerce (coerce)
import Lens.Micro ((^.))

instance EraTxOut AllegraEra where
  type TxOut AllegraEra = ShelleyTxOut AllegraEra

  mkBasicTxOut = ShelleyTxOut

  upgradeTxOut (TxOutCompact addr cfval) = TxOutCompact (coerce addr) cfval

  addrEitherTxOutL = addrEitherShelleyTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherShelleyTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinTxOut pp _txOut = pp ^. ppMinUTxOValueL
