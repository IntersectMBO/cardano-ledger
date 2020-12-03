{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- We disable warnings for name shadowing because of
https://gitlab.haskell.org/ghc/ghc/-/issues/14630, which means that we get
shadowing warnings for the named field puns when used with a pattern synonym.
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Translation where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era hiding (Crypto)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (Value (..))
import Cardano.Ledger.ShelleyMA.Metadata (Metadata (..), pattern Metadata)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.ShelleyMA.TxBody
import qualified Cardano.Ledger.Val as Val
import Control.Iterate.SetAlgebra (biMapFromList, lifo)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.API hiding (Metadata, TxBody)
import qualified Shelley.Spec.Ledger.EpochBoundary as EB
import qualified Shelley.Spec.Ledger.LedgerState as LS
import Shelley.Spec.Ledger.Rewards (NonMyopic (NonMyopic), likelihoodsNM, rewardPotNM)
import Shelley.Spec.Ledger.Tx
  ( WitnessSetHKD (WitnessSet, addrWits, bootWits, scriptWits),
  )

--------------------------------------------------------------------------------
-- Translation from Allegra to Mary
--
-- The instances below are needed by the consensus layer. Do not remove any of
-- them without coordinating with consensus.
--
-- Please add auxiliary instances and other declarations at the bottom of this
-- module, not in the list below so that it remains clear which instances the
-- consensus layer needs.
--
-- WARNING: when a translation instance currently uses the default
-- 'TranslationError', i.e., 'Void', it means the consensus layer relies on it
-- being total. Do not change it!
--------------------------------------------------------------------------------

type instance PreviousEra (MaryEra c) = AllegraEra c

-- | Currently no context is needed to translate from Allegra to Mary.
--
-- Note: if context is needed, please coordinate with consensus, who will have
-- to provide the context in the right place.
type instance TranslationContext (MaryEra c) = ()

instance Crypto c => TranslateEra (MaryEra c) NewEpochState where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = translateEra' ctxt $ nesBprev nes,
          nesBcur = translateEra' ctxt $ nesBcur nes,
          nesEs = translateEra' ctxt $ nesEs nes,
          nesRu = translateEra' ctxt <$> nesRu nes,
          nesPd = nesPd nes
        }

instance Crypto c => TranslateEra (MaryEra c) Tx where
  translateEra ctx (Tx body witness md) =
    pure $
      Tx
        { _body = translateEra' ctx body,
          _witnessSet = translateEra' ctx witness,
          _metadata = translateEra' ctx <$> md
        }

-- TODO when a genesis has been introduced for Mary, this instance can be
-- removed.
instance Crypto c => TranslateEra (MaryEra c) ShelleyGenesis where
  translateEra ctxt genesis =
    return
      ShelleyGenesis
        { sgSystemStart = sgSystemStart genesis,
          sgNetworkMagic = sgNetworkMagic genesis,
          sgNetworkId = sgNetworkId genesis,
          sgActiveSlotsCoeff = sgActiveSlotsCoeff genesis,
          sgSecurityParam = sgSecurityParam genesis,
          sgEpochLength = sgEpochLength genesis,
          sgSlotsPerKESPeriod = sgSlotsPerKESPeriod genesis,
          sgMaxKESEvolutions = sgMaxKESEvolutions genesis,
          sgSlotLength = sgSlotLength genesis,
          sgUpdateQuorum = sgUpdateQuorum genesis,
          sgMaxLovelaceSupply = sgMaxLovelaceSupply genesis,
          sgProtocolParams = translateEra' ctxt (sgProtocolParams genesis),
          sgGenDelegs = sgGenDelegs genesis,
          sgInitialFunds = Map.mapKeys (translateEra' ctxt) $ sgInitialFunds genesis,
          sgStaking = translateEra' ctxt $ sgStaking genesis
        }

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance (Crypto c, Functor f) => TranslateEra (MaryEra c) (PParams' f)

instance Crypto c => TranslateEra (MaryEra c) RewardAcnt

instance Crypto c => TranslateEra (MaryEra c) PoolParams where
  translateEra ctxt poolParams =
    return $
      PoolParams
        { _poolId = _poolId poolParams,
          _poolVrf = _poolVrf poolParams,
          _poolPledge = _poolPledge poolParams,
          _poolCost = _poolCost poolParams,
          _poolMargin = _poolMargin poolParams,
          _poolRAcnt = translateEra' ctxt (_poolRAcnt poolParams),
          _poolOwners = _poolOwners poolParams,
          _poolRelays = _poolRelays poolParams,
          _poolMD = _poolMD poolParams
        }

instance Crypto c => TranslateEra (MaryEra c) ShelleyGenesisStaking where
  translateEra ctxt sgs =
    return $
      ShelleyGenesisStaking
        { sgsPools = Map.map (translateEra' ctxt) (sgsPools sgs),
          sgsStake = sgsStake sgs
        }

instance Crypto c => TranslateEra (MaryEra c) Addr

instance Crypto c => TranslateEra (MaryEra c) EB.BlocksMade

instance Crypto c => TranslateEra (MaryEra c) SnapShot where
  translateEra ctxt snap =
    return
      SnapShot
        { _stake = Stake $ Map.mapKeys (translateEra' ctxt) $ unStake . _stake $ snap,
          EB._delegations = Map.mapKeys (translateEra' ctxt) $ EB._delegations snap,
          _poolParams = Map.map (translateEra' ctxt) $ _poolParams snap
        }

instance Crypto c => TranslateEra (MaryEra c) SnapShots where
  translateEra ctxt snaps =
    return
      SnapShots
        { _pstakeMark = translateEra' ctxt $ _pstakeMark snaps,
          _pstakeSet = translateEra' ctxt $ _pstakeSet snaps,
          _pstakeGo = translateEra' ctxt $ _pstakeGo snaps,
          _feeSS = _feeSS snaps
        }

instance Crypto c => TranslateEra (MaryEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = translateEra' ctxt $ esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translateEra' ctxt $ esPrevPp es,
          esPp = translateEra' ctxt $ esPp es,
          esNonMyopic = translateEra' ctxt $ esNonMyopic es
        }

instance Crypto c => TranslateEra (MaryEra c) NonMyopic where
  translateEra _ nm =
    return
      NonMyopic
        { likelihoodsNM = likelihoodsNM nm,
          rewardPotNM = rewardPotNM nm
        }

instance Crypto c => TranslateEra (MaryEra c) (Credential kr) where
  translateEra ctxt (ScriptHashObj h) = return (ScriptHashObj (translateEra' ctxt h))
  translateEra _ (KeyHashObj h) = return (KeyHashObj h)

instance Crypto c => TranslateEra (MaryEra c) ScriptHash

instance Crypto c => TranslateEra (MaryEra c) InstantaneousRewards where
  translateEra ctxt ir =
    return
      InstantaneousRewards
        { iRReserves = Map.mapKeys (translateEra' ctxt) (iRReserves ir),
          iRTreasury = Map.mapKeys (translateEra' ctxt) (iRTreasury ir)
        }

instance Crypto c => TranslateEra (MaryEra c) DState where
  translateEra ctxt ds =
    return
      DState
        { _rewards = Map.mapKeys (translateEra' ctxt) (_rewards ds),
          LS._delegations = Map.mapKeys (translateEra' ctxt) (LS._delegations ds),
          _ptrs =
            biMapFromList const $
              toList $
                fmap
                  (\(ptr, cred) -> (ptr, translateEra' ctxt cred))
                  (lifo (_ptrs ds)),
          _fGenDelegs = _fGenDelegs ds,
          _genDelegs = _genDelegs ds,
          _irwd = translateEra' ctxt $ _irwd ds
        }

instance Crypto c => TranslateEra (MaryEra c) PState where
  translateEra ctxt ps =
    return
      PState
        { _pParams = Map.map (translateEra' ctxt) (_pParams ps),
          _fPParams = Map.map (translateEra' ctxt) (_fPParams ps),
          _retiring = _retiring ps
        }

instance Crypto c => TranslateEra (MaryEra c) DPState where
  translateEra ctxt dp =
    return
      DPState
        { _dstate = translateEra' ctxt $ _dstate dp,
          _pstate = translateEra' ctxt $ _pstate dp
        }

instance Crypto c => TranslateEra (MaryEra c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { _utxoState = translateEra' ctxt $ _utxoState ls,
          _delegationState = translateEra' ctxt $ _delegationState ls
        }

instance Crypto c => TranslateEra (MaryEra c) ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance Crypto c => TranslateEra (MaryEra c) PPUPState where
  translateEra ctxt ps =
    return
      PPUPState
        { proposals = translateEra' ctxt $ proposals ps,
          futureProposals = translateEra' ctxt $ futureProposals ps
        }

instance Crypto c => TranslateEra (MaryEra c) UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { _utxo = translateEra' ctxt $ _utxo us,
          _deposited = _deposited us,
          _fees = _fees us,
          _ppups = translateEra' ctxt $ _ppups us
        }

instance Crypto c => TranslateEra (MaryEra c) RewardUpdate where
  translateEra ctxt ru =
    return
      RewardUpdate
        { deltaT = deltaT ru,
          deltaR = deltaR ru,
          rs = Map.mapKeys (translateEra' ctxt) $ rs ru,
          deltaF = deltaF ru,
          nonMyopic = translateEra' ctxt $ nonMyopic ru
        }

instance Crypto c => TranslateEra (MaryEra c) TxId

instance Crypto c => TranslateEra (MaryEra c) TxIn where
  translateEra ctxt (TxIn txid ix) = return $ TxIn (translateEra' ctxt txid) ix

instance Crypto c => TranslateEra (MaryEra c) TxOut where
  translateEra () (TxOutCompact addr cfval) =
    pure $ TxOutCompact (coerce addr) (translateCompactValue cfval)

instance Crypto c => TranslateEra (MaryEra c) UTxO where
  translateEra ctxt utxo =
    return $
      UTxO $
        Map.mapKeys (translateEra' ctxt) $
          Map.map (translateEra' ctxt) $
            unUTxO utxo

instance Crypto c => TranslateEra (MaryEra c) WitnessSet where
  translateEra ctxt WitnessSet {addrWits, scriptWits, bootWits} =
    pure $
      WitnessSet
        { addrWits = Set.map (translateEra' @(MaryEra c) ctxt) addrWits,
          scriptWits =
            Map.map coerce
              . Map.mapKeysMonotonic coerce
              $ scriptWits,
          bootWits = Set.map (translateEra' @(MaryEra c) ctxt) bootWits
        }

instance Crypto c => TranslateEra (MaryEra c) BootstrapWitness where
  translateEra _ BootstrapWitness {bwKey, bwSig, bwChainCode, bwAttributes} =
    pure
      BootstrapWitness
        { bwKey = bwKey,
          bwSig = coerce bwSig,
          bwChainCode,
          bwAttributes
        }

instance
  (Crypto c, Typeable kr) =>
  TranslateEra (MaryEra c) (WitVKey kr)
  where
  translateEra _ (WitVKey k s) =
    pure $
      WitVKey k s

instance Crypto c => TranslateEra (MaryEra c) Wdrl where
  translateEra _ (Wdrl w) = pure . Wdrl $ Map.mapKeysMonotonic coerce w

instance Crypto c => TranslateEra (MaryEra c) DCert where
  translateEra _ (DCertDeleg c) = pure . DCertDeleg $ coerce c
  translateEra ctx (DCertPool c) = DCertPool <$> translateEra ctx c
  translateEra _ (DCertGenesis c) = pure . DCertGenesis $ coerce c
  translateEra ctx (DCertMir c) = DCertMir <$> translateEra ctx c

instance Crypto c => TranslateEra (MaryEra c) PoolCert where
  translateEra ctx (RegPool pp) = pure . RegPool $ translateEra' ctx pp
  translateEra _ (RetirePool kh e) = pure $ RetirePool (coerce kh) e

instance Crypto c => TranslateEra (MaryEra c) MIRCert where
  translateEra _ (MIRCert pot rewards) =
    pure $
      MIRCert pot (Map.mapKeysMonotonic coerce rewards)

instance Crypto c => TranslateEra (MaryEra c) Update where
  translateEra _ (Update pp en) = pure $ Update (coerce pp) en

instance Crypto c => TranslateEra (MaryEra c) TxBody where
  translateEra ctx (TxBody i o d w fee vi u m mint) =
    pure $
      TxBody
        (Set.map (translateEra' ctx) i)
        (translateEra' ctx <$> o)
        (translateEra' ctx <$> d)
        (translateEra' ctx w)
        fee
        vi
        (translateEra' ctx <$> u)
        (coerce m)
        (translateValue mint)

instance Crypto c => TranslateEra (MaryEra c) Metadata where
  translateEra ctx (Metadata blob sp) =
    pure $
      Metadata blob (translateEra' ctx <$> sp)

instance Crypto c => TranslateEra (MaryEra c) Timelock

translateValue :: Era era => Coin -> Value era
translateValue = Val.inject

translateCompactValue :: Era era => CompactForm Coin -> CompactForm (Value era)
translateCompactValue =
  fromMaybe (error msg) . toCompact . translateValue . fromCompact
  where
    msg = "impossible error: compact coin is out of range"
