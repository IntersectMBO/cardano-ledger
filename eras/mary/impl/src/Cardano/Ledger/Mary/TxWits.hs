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
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Shelley.TxWits (
  ShelleyTxWits (..),
  addrShelleyTxWitsL,
  bootAddrShelleyTxWitsL,
  scriptShelleyTxWitsL,
 )

instance Crypto c => EraTxWits (MaryEra c) where
  {-# SPECIALIZE instance EraTxWits (MaryEra StandardCrypto) #-}

  type TxWits (MaryEra c) = ShelleyTxWits (MaryEra c)

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
