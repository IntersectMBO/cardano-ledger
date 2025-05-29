{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- WARNING! This module contains types that are meant for internal use only.
-- Even when used internally, care must be taken that these types are NOT used
-- prior to their eras.
module Cardano.Ledger.Internal.Era where

data ShelleyEra
#if __GLASGOW_HASKELL__ >= 908
{-# WARNING in "x-unsafe-internal" ShelleyEra ["!! FOR INTERNAL (`cardano-ledger`) USE ONLY !!", "This is not meant to be used prior to Shelley era."] #-}
#endif

data AllegraEra
#if __GLASGOW_HASKELL__ >= 908
{-# WARNING in "x-unsafe-internal" AllegraEra ["!! FOR INTERNAL (`cardano-ledger`) USE ONLY !!", "This is not meant to be used prior to Allegra era."] #-}
#endif

data MaryEra
#if __GLASGOW_HASKELL__ >= 908
{-# WARNING in "x-unsafe-internal" MaryEra ["!! FOR INTERNAL (`cardano-ledger`) USE ONLY !!", "This is not meant to be used prior to Mary era."] #-}
#endif

data AlonzoEra
#if __GLASGOW_HASKELL__ >= 908
{-# WARNING in "x-unsafe-internal" AlonzoEra ["!! FOR INTERNAL (`cardano-ledger`) USE ONLY !!", "This is not meant to be used prior to Alonzo era."] #-}
#endif

data BabbageEra
#if __GLASGOW_HASKELL__ >= 908
{-# WARNING in "x-unsafe-internal" BabbageEra ["!! FOR INTERNAL (`cardano-ledger`) USE ONLY !!", "This is not meant to be used prior to Babbage era."] #-}
#endif

data ConwayEra
#if __GLASGOW_HASKELL__ >= 908
{-# WARNING in "x-unsafe-internal" ConwayEra ["!! FOR INTERNAL (`cardano-ledger`) USE ONLY !!", "This is not meant to be used prior to Conway era."] #-}
#endif

data DijkstraEra
#if __GLASGOW_HASKELL__ >= 908
{-# WARNING in "x-unsafe-internal" DijkstraEra ["!! FOR INTERNAL (`cardano-ledger`) USE ONLY !!", "This is not meant to be used prior to Dijkstra era."] #-}
#endif
