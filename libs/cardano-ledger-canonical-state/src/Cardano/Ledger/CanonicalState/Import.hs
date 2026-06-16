{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.CanonicalState.Import where

import Cardano.Ledger.BaseTypes (
  EpochNo,
  SlotNo,
 )
import Cardano.Ledger.CanonicalState.Export (
  ExportCanonicalState (State),
 )
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, decodeChunkEntry)
import Cardano.SCLS.Internal.Reader (withNamespacedDataHandle)
import Cardano.SCLS.NamespaceCodec (KnownNamespace (NamespaceEntry, NamespaceKey))
import Cardano.Types.Namespace (fromSymbol)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy)
import GHC.IO.Handle (Handle)
import GHC.TypeLits (KnownSymbol)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S

class KnownNamespace ns => ImportCanonicalNamespace era ns where
  importNamespaceEntries ::
    Monad m =>
    State era ->
    Stream (Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) m () ->
    m (State era)

class ImportCanonicalState era where
  importCanonicalState ::
    FilePath ->
    EpochNo ->
    IO (SlotNo, State era)

importNamespaceFromHandle ::
  forall era v m.
  (KnownSymbol v, ImportCanonicalNamespace era v, MonadIO m, MonadFail m) =>
  Handle -> Proxy v -> State era -> m (State era)
importNamespaceFromHandle h (p :: Proxy v) nes =
  withNamespacedDataHandle h (fromSymbol p) $ \s ->
    importNamespaceEntries @era @v
      nes
      ( S.mapM
          ( \e -> case decodeChunkEntry p e of
              Just chunkEntry -> pure chunkEntry
              Nothing -> fail $ "Failed to decode chunk entry for namespace: " <> show (fromSymbol p)
          )
          s
      )
