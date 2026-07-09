{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Export (
  ExportCanonicalState (..),
  ExportCanonicalNamespace (..),
  addNamespaceToPlan,
) where

import Cardano.Ledger.BaseTypes (
  EpochNo,
 )
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, SomeChunkEntry)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (
  SerializationPlan,
  addNamespacedChunks,
 )
import Cardano.SCLS.NamespaceCodec (KnownNamespace (..))
import Data.MemPack.Extra (RawBytes)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownSymbol)
import Streaming.Prelude (Of, Stream)

class ExportCanonicalState era where
  type State era
  stateSerializationPlan :: Monad m => State era -> SerializationPlan (SomeChunkEntry RawBytes) m
  getEpochNo :: State era -> EpochNo

class KnownNamespace ns => ExportCanonicalNamespace era ns where
  canonicalNamespaceEntries ::
    Monad m =>
    State era ->
    Stream (Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) m ()

addNamespaceToPlan ::
  forall era ns m.
  (Monad m, KnownSymbol ns, ExportCanonicalNamespace era ns) =>
  State era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addNamespaceToPlan s = addNamespacedChunks (Proxy :: Proxy ns) (canonicalNamespaceEntries @era @ns s)
