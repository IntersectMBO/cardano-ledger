{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.TxWits () where

import Cardano.Ledger.Core (EraTxWits (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.TxWits
  ( ShelleyTxWits,
    addrShelleyTxWitsL,
    bootAddrShelleyTxWitsL,
    scriptShelleyTxWitsL,
  )
import Cardano.Ledger.ShelleyMA.AuxiliaryData ()
import Cardano.Ledger.ShelleyMA.Era (MAClass, MaryOrAllegra (..), ShelleyMAEra)

instance MAClass ma crypto => EraTxWits (ShelleyMAEra ma crypto) where
  {-# SPECIALIZE instance EraTxWits (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance EraTxWits (ShelleyMAEra 'Allegra StandardCrypto) #-}

  type TxWits (ShelleyMAEra ma crypto) = ShelleyTxWits (ShelleyMAEra ma crypto)

  mkBasicTxWits = mempty

  addrTxWitsL = addrShelleyTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrShelleyTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptShelleyTxWitsL
  {-# INLINE scriptTxWitsL #-}
