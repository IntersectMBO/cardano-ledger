{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxWits () where

import Cardano.Ledger.Core (EraTxWits (..), upgradeScript)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Shelley.TxWits (
  ShelleyTxWits (..),
  addrShelleyTxWitsL,
  bootAddrShelleyTxWitsL,
  scriptShelleyTxWitsL,
 )

instance EraTxWits MaryEra where
  type TxWits MaryEra = ShelleyTxWits MaryEra

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
