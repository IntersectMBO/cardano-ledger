{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Shelley.API.ByronTranslation (
  translateToShelleyLedgerState,
  translateToShelleyLedgerStateFromUtxo,

  -- * Exported for testing purposes
  translateCompactTxOutByronToShelley,
  translateTxIdByronToShelley,
)
where

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hashing as Hashing
import Cardano.Ledger.Address (fromBoostrapCompactAddress, isBootstrapRedeemer)
import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo, TxIx (..))
import Cardano.Ledger.Coin (CompactForm (CompactCoin))
import Cardano.Ledger.Crypto (Crypto (ADDRHASH))
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState.Types (curPParamsEpochStateL, prevPParamsEpochStateL)
import Cardano.Ledger.Shelley.Rules ()
import Cardano.Ledger.Shelley.Translation (FromByronTranslationContext (..))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (coinBalance)
import Cardano.Ledger.Val (zero, (<->))
import qualified Data.ByteString.Short as SBS
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Word
import GHC.Stack (HasCallStack)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Extras (view)

-- | We use the same hashing algorithm so we can unwrap and rewrap the bytes.
-- We don't care about the type that is hashed, which will differ going from
-- Byron to Shelley, we just use the hashes as IDs.
translateTxIdByronToShelley ::
  (Crypto c, ADDRHASH c ~ Crypto.Blake2b_224) =>
  Byron.TxId ->
  TxId c
translateTxIdByronToShelley =
  TxId . unsafeMakeSafeHash . hashFromShortBytesE . Hashing.abstractHashToShort

hashFromShortBytesE ::
  forall h a.
  (Crypto.HashAlgorithm h, HasCallStack) =>
  SBS.ShortByteString ->
  Crypto.Hash h a
hashFromShortBytesE sbs =
  case Crypto.hashFromBytesShort sbs of
    Just !h -> h
    Nothing ->
      error $ "hashFromBytesShort called with ShortByteString of the wrong length: " <> show sbs

translateCompactTxOutByronToShelley :: Byron.CompactTxOut -> ShelleyTxOut (ShelleyEra c)
translateCompactTxOutByronToShelley (Byron.CompactTxOut compactAddr amount) =
  TxOutCompact
    (fromBoostrapCompactAddress compactAddr)
    (CompactCoin (Byron.unsafeGetLovelace amount))

translateCompactTxInByronToShelley ::
  (Crypto c, ADDRHASH c ~ Crypto.Blake2b_224) =>
  Byron.CompactTxIn ->
  TxIn c
translateCompactTxInByronToShelley (Byron.CompactTxInUtxo compactTxId idx) =
  TxIn
    (translateTxIdByronToShelley (Byron.fromCompactTxId compactTxId))
    (TxIx ((fromIntegral :: Word16 -> Word64) idx))

translateUTxOByronToShelley ::
  forall c.
  (Crypto c, ADDRHASH c ~ Crypto.Blake2b_224) =>
  Byron.UTxO ->
  UTxO (ShelleyEra c)
translateUTxOByronToShelley (Byron.UTxO utxoByron) =
  UTxO $
    Map.fromList
      [ (txInShelley, txOutShelley)
      | (txInByron, txOutByron) <- Map.toList utxoByron
      , let txInShelley = translateCompactTxInByronToShelley txInByron
            txOutShelley = translateCompactTxOutByronToShelley txOutByron
      , -- In some testnets there are a few TxOuts with zero values injected at
      -- initialization of Byron. We do not allow zero values in TxOuts in Shelley
      -- onwards.
      txOutShelley ^. coinTxOutL /= zero
      ]

translateToShelleyLedgerState ::
  forall c.
  (Crypto c, ADDRHASH c ~ Crypto.Blake2b_224) =>
  FromByronTranslationContext c ->
  EpochNo ->
  Byron.ChainValidationState ->
  NewEpochState (ShelleyEra c)
translateToShelleyLedgerState transContext epochNo cvs =
  translateToShelleyLedgerStateFromUtxo transContext epochNo (Byron.cvsUtxo cvs)

translateToShelleyLedgerStateFromUtxo ::
  forall c.
  (Crypto c, ADDRHASH c ~ Crypto.Blake2b_224) =>
  FromByronTranslationContext c ->
  EpochNo ->
  Byron.UTxO ->
  NewEpochState (ShelleyEra c)
translateToShelleyLedgerStateFromUtxo transCtxt epochNo utxoByron =
  NewEpochState
    { nesEL = epochNo
    , nesBprev = BlocksMade Map.empty
    , nesBcur = BlocksMade Map.empty
    , nesEs = epochState
    , nesRu = SNothing
    , nesPd = PoolDistr Map.empty mempty
    , -- At this point, we compute the stashed AVVM addresses, while we are able
      -- to do a linear scan of the UTxO, and stash them away for use at the
      -- Shelley/Allegra boundary.
      stashedAVVMAddresses =
        let UTxO utxo = utxosUtxo . lsUTxOState . esLState $ epochState
            redeemers =
              Map.filter (maybe False isBootstrapRedeemer . view bootAddrTxOutF) utxo
         in UTxO redeemers
    }
  where
    pparams :: PParams (ShelleyEra c)
    pparams = fbtcProtocolParams transCtxt

    -- NOTE: we ignore the Byron delegation map because the genesis and
    -- delegation verification keys are hashed using a different hashing
    -- scheme. This means we can't simply convert them, as Byron nowhere stores
    -- the original verification keys.
    --
    -- Fortunately, no Byron genesis delegations have happened yet, and if
    -- they did, we would be aware of them before the hard fork, as we
    -- instigate the hard fork. We just have to make sure that the hard-coded
    -- Shelley genesis contains the same genesis and delegation verification
    -- keys, but hashed with the right algorithm.
    genDelegs :: GenDelegs c
    genDelegs = GenDelegs $ fbtcGenDelegs transCtxt

    reserves :: Coin
    reserves =
      word64ToCoin (fbtcMaxLovelaceSupply transCtxt) <-> coinBalance utxoShelley

    epochState :: EpochState (ShelleyEra c)
    epochState =
      EpochState
        { esAccountState = AccountState (Coin 0) reserves
        , esSnapshots = emptySnapShots
        , esLState = ledgerState
        , esNonMyopic = def
        }
        & prevPParamsEpochStateL .~ pparams
        & curPParamsEpochStateL .~ pparams

    utxoShelley :: UTxO (ShelleyEra c)
    utxoShelley = translateUTxOByronToShelley utxoByron

    ledgerState :: LedgerState (ShelleyEra c)
    ledgerState =
      LedgerState
        { lsUTxOState =
            UTxOState
              { utxosUtxo = utxoShelley
              , utxosDeposited = Coin 0
              , utxosFees = Coin 0
              , utxosGovState = emptyGovState
              , utxosStakeDistr = IStake mempty Map.empty
              , utxosDonation = mempty
              }
        , lsCertState =
            CertState
              { certDState = dState
              , certPState = def
              , certVState = def
              }
        }

    dState :: DState (ShelleyEra c)
    dState =
      DState
        { dsUnified = UM.empty
        , dsFutureGenDelegs = Map.empty
        , dsGenDelegs = genDelegs
        , dsIRewards = def
        }
