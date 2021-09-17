{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Chain.Byron.API.Common
  ( -- * Extract info from genesis config
    allowedDelegators,

    -- * Extract info from chain state
    getDelegationMap,
    getProtocolParams,
    getMaxBlockSize,

    -- * Annotations
    reAnnotateBlock,
    reAnnotateBoundary,
    reAnnotateMagic,
    reAnnotateMagicId,
    reAnnotateUsing,

    -- * Headers
    abobMatchesBody,
  )
where

import Cardano.Binary
import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as D.Iface
import qualified Cardano.Chain.Genesis as Gen
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Interface as U.Iface
import Cardano.Crypto.ProtocolMagic
import Cardano.Prelude
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as T

{-------------------------------------------------------------------------------
  Extract info from genesis config
-------------------------------------------------------------------------------}

allowedDelegators :: Gen.Config -> Set CC.KeyHash
allowedDelegators =
  Gen.unGenesisKeyHashes
    . Gen.configGenesisKeyHashes

{-------------------------------------------------------------------------------
  Extract info from chain state
-------------------------------------------------------------------------------}

getDelegationMap :: CC.ChainValidationState -> Delegation.Map
getDelegationMap =
  D.Iface.delegationMap
    . CC.cvsDelegationState

getProtocolParams :: CC.ChainValidationState -> Update.ProtocolParameters
getProtocolParams =
  U.Iface.adoptedProtocolParameters
    . CC.cvsUpdateState

getMaxBlockSize :: CC.ChainValidationState -> Word32
getMaxBlockSize =
  fromIntegral
    . Update.ppMaxBlockSize
    . getProtocolParams

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

reAnnotateMagicId :: ProtocolMagicId -> Annotated ProtocolMagicId ByteString
reAnnotateMagicId pmi = reAnnotate $ Annotated pmi ()

reAnnotateMagic :: ProtocolMagic -> AProtocolMagic ByteString
reAnnotateMagic (AProtocolMagic a b) = AProtocolMagic (reAnnotate a) b

reAnnotateBlock :: CC.EpochSlots -> CC.ABlock () -> CC.ABlock ByteString
reAnnotateBlock epochSlots =
  reAnnotateUsing
    (CC.toCBORBlock epochSlots)
    (CC.fromCBORABlock epochSlots)

reAnnotateBoundary ::
  ProtocolMagicId ->
  CC.ABoundaryBlock () ->
  CC.ABoundaryBlock ByteString
reAnnotateBoundary pm =
  reAnnotateUsing
    (CC.toCBORABoundaryBlock pm)
    CC.fromCBORABoundaryBlock

-- | Generalization of 'reAnnotate'
reAnnotateUsing ::
  forall f a.
  Functor f =>
  (f a -> Encoding) ->
  (forall s. Decoder s (f ByteSpan)) ->
  f a ->
  f ByteString
reAnnotateUsing encoder decoder =
  (\bs -> splice bs $ CBOR.deserialiseFromBytes decoder bs)
    . CBOR.toLazyByteString
    . encoder
  where
    splice ::
      Show err =>
      Lazy.ByteString ->
      Either err (Lazy.ByteString, f ByteSpan) ->
      f ByteString
    splice bs (Right (left, fSpan))
      | Lazy.null left = (Lazy.toStrict . slice bs) <$> fSpan
      | otherwise = roundtripFailure "leftover bytes"
    splice _ (Left err) = roundtripFailure $ show err

    roundtripFailure :: forall x. T.Text -> x
    roundtripFailure err =
      panic $
        T.intercalate ": " $
          [ "annotateBoundary",
            "serialization roundtrip failure",
            show err
          ]

{-------------------------------------------------------------------------------
  Header of a regular block or EBB

  The ledger layer defines 'ABlockOrBoundary', but no equivalent for headers.
-------------------------------------------------------------------------------}

-- | Check if a block matches its header
--
-- For EBBs, we're currently being more permissive here and not performing any
-- header-body validation but only checking whether an EBB header and EBB block
-- were provided. This seems to be fine as it won't cause any loss of consensus
-- with the old `cardano-sl` nodes.
abobMatchesBody ::
  CC.ABlockOrBoundaryHdr ByteString ->
  CC.ABlockOrBoundary ByteString ->
  Bool
abobMatchesBody hdr blk =
  case (hdr, blk) of
    (CC.ABOBBlockHdr hdr', CC.ABOBBlock blk') -> matchesBody hdr' blk'
    (CC.ABOBBoundaryHdr _, CC.ABOBBoundary _) -> True
    (CC.ABOBBlockHdr _, CC.ABOBBoundary _) -> False
    (CC.ABOBBoundaryHdr _, CC.ABOBBlock _) -> False
  where
    matchesBody :: CC.AHeader ByteString -> CC.ABlock ByteString -> Bool
    matchesBody hdr' blk' =
      isRight $
        CC.validateHeaderMatchesBody hdr' (CC.blockBody blk')
