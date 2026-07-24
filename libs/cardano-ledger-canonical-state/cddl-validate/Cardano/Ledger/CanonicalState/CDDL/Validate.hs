{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

-- | Various helper functions for CBOR validation against supported CDDL specifications.
module Cardano.Ledger.CanonicalState.CDDL.Validate (
  validateBytesAgainst,
  invalidSpecs,
  validSpecs,
) where

import Cardano.Ledger.CanonicalState.Namespace.CDDL
import Cardano.SCLS.NamespaceSymbol (
  KnownSpec (namespaceSpec),
  SomeNamespaceSymbol (SomeNamespaceSymbol),
 )
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (Evidenced, ValidationTrace)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoReferenced,
  NameResolutionFailure,
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle (toCDDL)
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (mapIndex), mapCDDLDropExt)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Pre-compiled CDDL specifications for all supported namespaces.
invalidSpecs :: Map.Map SomeNamespaceSymbol NameResolutionFailure
validSpecs :: Map.Map SomeNamespaceSymbol (CTreeRoot Codec.CBOR.Cuddle.CDDL.Resolve.MonoReferenced)
(invalidSpecs, validSpecs) = Map.mapEither
  do
    \hddl -> do
      buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap $ mapCDDLDropExt $ toCDDL hddl)
  do namespacesM
  where
    namespacesM =
      Map.fromList
        [ ( nsSym
          , namespaceSpec p
          )
        | nsSym@(SomeNamespaceSymbol p) <- knownNamespaces
        ]

-- | Validate raw bytes against a rule in the namespace.
validateBytesAgainst :: ByteString -> Text -> Text -> Maybe (Evidenced ValidationTrace)
validateBytesAgainst bytes namespace name = do
  cddl <- namespaceSymbolFromText namespace >>= flip Map.lookup validSpecs
  case validateCBOR bytes (Name name) (mapIndex cddl) of
    Right res -> pure res
    Left _ -> Nothing
