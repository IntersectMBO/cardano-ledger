{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Shelley.Spec.Ledger.API.ByronTranslation
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
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Val ((<->))
import qualified Data.ByteString.Short as SBS
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.API.Protocol
import Shelley.Spec.Ledger.API.Types
import Shelley.Spec.Ledger.Coin (CompactForm (CompactCoin))
import Shelley.Spec.Ledger.CompactAddr (CompactAddr (UnsafeCompactAddr))
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.STS.Chain (pparamsToChainChecksData)
import Shelley.Spec.Ledger.Slot

-- | We use the same hashing algorithm so we can unwrap and rewrap the bytes.
-- We don't care about the type that is hashed, which will differ going from
-- Byron to Shelley, we just use the hashes as IDs.
translateTxIdByronToShelley ::
  (CC.Crypto c, CC.ADDRHASH c ~ Crypto.Blake2b_224) =>
  Byron.TxId ->
  TxId c
translateTxIdByronToShelley =
  TxId . unsafeMakeSafeHash . hashFromShortBytesE . Hashing.abstractHashToShort

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

translateCompactTxOutByronToShelley :: Byron.CompactTxOut -> TxOut (ShelleyEra c)
translateCompactTxOutByronToShelley (Byron.CompactTxOut compactAddr amount) =
  TxOutCompact
    (UnsafeCompactAddr (Byron.unsafeGetCompactAddress compactAddr))
    (CompactCoin (Byron.unsafeGetLovelace amount))

translateCompactTxInByronToShelley ::
  (CC.Crypto c, CC.ADDRHASH c ~ Crypto.Blake2b_224) =>
  Byron.CompactTxIn ->
  TxIn c
translateCompactTxInByronToShelley (Byron.CompactTxInUtxo compactTxId idx) =
  TxInCompact
    (translateTxIdByronToShelley (Byron.fromCompactTxId compactTxId))
    (fromIntegral idx)

translateUTxOByronToShelley ::
  forall c.
  (CC.Crypto c, CC.ADDRHASH c ~ Crypto.Blake2b_224) =>
  Byron.UTxO ->
  UTxO (ShelleyEra c)
translateUTxOByronToShelley (Byron.UTxO utxoByron) =
  UTxO $
    Map.fromList
      [ (txInShelley, txOutShelley)
        | (txInByron, txOutByron) <- Map.toList utxoByron,
          let txInShelley = translateCompactTxInByronToShelley txInByron
              txOutShelley = translateCompactTxOutByronToShelley txOutByron
      ]

translateToShelleyLedgerState ::
  forall c.
  (CC.Crypto c, CC.ADDRHASH c ~ Crypto.Blake2b_224) =>
  ShelleyGenesis (ShelleyEra c) ->
  EpochNo ->
  Byron.ChainValidationState ->
  NewEpochState (ShelleyEra c)
translateToShelleyLedgerState genesisShelley epochNo cvs =
  NewEpochState
    { nesEL = epochNo,
      nesBprev = BlocksMade Map.empty,
      nesBcur = BlocksMade Map.empty,
      nesEs = epochState,
      nesRu = SNothing,
      nesPd = PoolDistr Map.empty
    }
  where
    pparams :: PParams (ShelleyEra c)
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
    genDelegs :: GenDelegs c
    genDelegs = GenDelegs $ sgGenDelegs genesisShelley

    reserves :: Coin
    reserves =
      word64ToCoin (sgMaxLovelaceSupply genesisShelley) <-> balance utxoShelley

    epochState :: EpochState (ShelleyEra c)
    epochState =
      EpochState
        { esAccountState = AccountState (Coin 0) reserves,
          esSnapshots = emptySnapShots,
          esLState = ledgerState,
          esPrevPp = pparams,
          esPp = pparams,
          esNonMyopic = def
        }

    utxoByron :: Byron.UTxO
    utxoByron = Byron.cvsUtxo cvs

    utxoShelley :: UTxO (ShelleyEra c)
    utxoShelley = translateUTxOByronToShelley utxoByron

    ledgerState :: LedgerState (ShelleyEra c)
    ledgerState =
      LedgerState
        { _utxoState =
            UTxOState
              { _utxo = utxoShelley,
                _deposited = Coin 0,
                _fees = Coin 0,
                _ppups = def
              },
          _delegationState =
            DPState
              { _dstate = def {_genDelegs = genDelegs},
                _pstate = def
              }
        }

-- | We construct a 'LedgerView' using the Shelley genesis config in the same
-- way as 'translateToShelleyLedgerState'.
mkInitialShelleyLedgerView ::
  forall c.
  ShelleyGenesis (ShelleyEra c) ->
  LedgerView c
mkInitialShelleyLedgerView genesisShelley =
  LedgerView
    { lvD = _d . sgProtocolParams $ genesisShelley,
      lvExtraEntropy = _extraEntropy . sgProtocolParams $ genesisShelley,
      lvPoolDistr = PoolDistr Map.empty,
      lvGenDelegs = GenDelegs $ sgGenDelegs genesisShelley,
      lvChainChecks = pparamsToChainChecksData . sgProtocolParams $ genesisShelley
    }
