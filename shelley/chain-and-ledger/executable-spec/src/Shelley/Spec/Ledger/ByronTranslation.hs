{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Shelley.Spec.Ledger.ByronTranslation
  ( mkInitialShelleyLedgerView,
    translateToShelleyLedgerState,

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
import Cardano.Ledger.Crypto (ADDRHASH)
import Cardano.Ledger.Era
import Control.Monad.Reader (runReader)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.API
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin (word64ToCoin)
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.OverlaySchedule
import Shelley.Spec.Ledger.Rewards
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.UTxO
import qualified Cardano.Ledger.Val as Val

-- | We use the same hashing algorithm so we can unwrap and rewrap the bytes.
-- We don't care about the type that is hashed, which will differ going from
-- Byron to Shelley, we just use the hashes as IDs.
translateTxIdByronToShelley ::
  (Era era, ADDRHASH (Crypto era) ~ Crypto.Blake2b_224) =>
  Byron.TxId ->
  TxId era
translateTxIdByronToShelley =
  TxId . hashFromShortBytesE . Hashing.abstractHashToShort

hashFromShortBytesE ::
  forall h a.
  (Crypto.HashAlgorithm h, HasCallStack) =>
  SBS.ShortByteString ->
  Crypto.Hash h a
hashFromShortBytesE sbs = fromMaybe (error msg) $ Crypto.hashFromBytesShort sbs
  where
    msg =
      "hashFromBytesShort called with ShortByteString of the wrong length: "
        <> show sbs

translateCompactTxOutByronToShelley :: Byron.CompactTxOut -> TxOut era
translateCompactTxOutByronToShelley (Byron.CompactTxOut compactAddr amount) =
  TxOutCompact
    (Byron.unsafeGetCompactAddress compactAddr)
    (Byron.unsafeGetLovelace amount)

translateCompactTxInByronToShelley ::
  (Era era, ADDRHASH (Crypto era) ~ Crypto.Blake2b_224) =>
  Byron.CompactTxIn ->
  TxIn era
translateCompactTxInByronToShelley (Byron.CompactTxInUtxo compactTxId idx) =
  TxInCompact
    (translateTxIdByronToShelley (Byron.fromCompactTxId compactTxId))
    (fromIntegral idx)

translateUTxOByronToShelley ::
  forall era.
  (Era era, ADDRHASH (Crypto era) ~ Crypto.Blake2b_224) =>
  Byron.UTxO ->
  UTxO era
translateUTxOByronToShelley (Byron.UTxO utxoByron) =
  UTxO $
    Map.fromList
      [ (txInShelley, txOutShelley)
        | (txInByron, txOutByron) <- Map.toList utxoByron,
          let txInShelley = translateCompactTxInByronToShelley txInByron
              txOutShelley = translateCompactTxOutByronToShelley txOutByron
      ]

translateToShelleyLedgerState ::
  forall era.
  (Era era, ADDRHASH (Crypto era) ~ Crypto.Blake2b_224) =>
  ShelleyGenesis era ->
  Globals ->
  EpochNo ->
  Byron.ChainValidationState ->
  ShelleyState era
translateToShelleyLedgerState genesisShelley globals epochNo cvs =
  NewEpochState
    { nesEL = epochNo,
      nesBprev = BlocksMade Map.empty,
      nesBcur = BlocksMade Map.empty,
      nesEs = epochState,
      nesRu = SNothing,
      nesPd = PoolDistr Map.empty,
      nesOsched = oSchedule
    }
  where
    pparams :: PParams
    pparams = sgProtocolParams genesisShelley

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
    genDelegs :: GenDelegs era
    genDelegs = GenDelegs $ sgGenDelegs genesisShelley

    reserves :: Coin
    reserves =
      word64ToCoin (sgMaxLovelaceSupply genesisShelley) Val.~~ balance utxoShelley

    epochState :: EpochState era
    epochState =
      EpochState
        { esAccountState = AccountState (Coin 0) reserves,
          esSnapshots = emptySnapShots,
          esLState = ledgerState,
          esPrevPp = pparams,
          esPp = pparams,
          esNonMyopic = emptyNonMyopic
        }

    utxoByron :: Byron.UTxO
    utxoByron = Byron.cvsUtxo cvs

    utxoShelley :: UTxO era
    utxoShelley = translateUTxOByronToShelley utxoByron

    ledgerState :: LedgerState era
    ledgerState =
      LedgerState
        { _utxoState =
            UTxOState
              { _utxo = utxoShelley,
                _deposited = Coin 0,
                _fees = Coin 0,
                _ppups = emptyPPUPState
              },
          _delegationState =
            DPState
              { _dstate = emptyDState {_genDelegs = genDelegs},
                _pstate = emptyPState
              }
        }
    oSchedule :: OverlaySchedule era
    oSchedule =
      flip runReader globals $
        overlaySchedule
          epochNo
          (Map.keysSet (sgGenDelegs genesisShelley))
          pparams

-- | We construct a 'LedgerView' using the Shelley genesis config in the same
-- way as 'translateToShelleyLedgerState'.
mkInitialShelleyLedgerView ::
  forall era.
  ShelleyGenesis era ->
  Globals ->
  EpochNo ->
  LedgerView era
mkInitialShelleyLedgerView genesisShelley globals epochNo =
  LedgerView
    { lvProtParams = sgProtocolParams genesisShelley,
      lvOverlaySched = oSchedule,
      lvPoolDistr = PoolDistr Map.empty,
      lvGenDelegs = GenDelegs $ sgGenDelegs genesisShelley
    }
  where
    oSchedule :: OverlaySchedule era
    oSchedule =
      flip runReader globals $
        overlaySchedule
          epochNo
          (Map.keysSet (sgGenDelegs genesisShelley))
          (sgProtocolParams genesisShelley)
