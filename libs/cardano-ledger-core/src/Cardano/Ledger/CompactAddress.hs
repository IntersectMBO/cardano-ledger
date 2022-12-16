module Cardano.Ledger.CompactAddress
  {-# DEPRECATED "Use `Cardano.Ledger.Address` instead" #-}
  ( Addr (..),
    RewardAcnt (..),
    BootstrapAddress (..),
    fromBoostrapCompactAddress,
    compactAddr,
    decompactAddr,
    CompactAddr,
    unCompactAddr,
    isPayCredScriptCompactAddr,
    isBootstrapCompactAddr,
    decodeAddr,
    decodeAddrShort,
    decodeAddrEither,
    decodeAddrShortEither,
    fromCborAddr,
    fromCborBothAddr,
    fromCborCompactAddr,
    fromCborBackwardsBothAddr,
    decodeRewardAcnt,
    fromCborRewardAcnt,
  )
where

import Cardano.Ledger.Address
