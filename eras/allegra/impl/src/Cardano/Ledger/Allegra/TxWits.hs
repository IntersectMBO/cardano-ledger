{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxWits () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.TxAuxData ()
import Cardano.Ledger.Core (EraScript (upgradeScript), EraTxWits (..))
import Cardano.Ledger.Shelley.TxWits (
  ShelleyTxWits (..),
  addrShelleyTxWitsL,
  bootAddrShelleyTxWitsL,
  scriptShelleyTxWitsL,
 )

instance EraTxWits AllegraEra where
  type TxWits AllegraEra = ShelleyTxWits AllegraEra

  mkBasicTxWits = mempty

  addrTxWitsL = addrShelleyTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrShelleyTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptShelleyTxWitsL
  {-# INLINE scriptTxWitsL #-}

  upgradeTxWits stw =
    ShelleyTxWits
      (addrWits stw)
      (upgradeScript <$> scriptWits stw)
      (bootWits stw)
