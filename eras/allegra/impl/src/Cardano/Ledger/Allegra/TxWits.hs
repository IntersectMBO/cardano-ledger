{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxWits () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.TxAuxData ()
import Cardano.Ledger.Core (EraScript (upgradeScript), EraTxWits (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Shelley.TxWits (
  ShelleyTxWits (..),
  addrShelleyTxWitsL,
  bootAddrShelleyTxWitsL,
  scriptShelleyTxWitsL,
 )

instance Crypto c => EraTxWits (AllegraEra c) where
  {-# SPECIALIZE instance EraTxWits (AllegraEra StandardCrypto) #-}

  type TxWits (AllegraEra c) = ShelleyTxWits (AllegraEra c)

  mkBasicTxWits = mempty

  addrTxWitsL = addrShelleyTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrShelleyTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptShelleyTxWitsL
  {-# INLINE scriptTxWitsL #-}

  upgradeTxWits (ShelleyTxWits {addrWits, scriptWits, bootWits}) =
    ShelleyTxWits addrWits (fmap upgradeScript scriptWits) bootWits
