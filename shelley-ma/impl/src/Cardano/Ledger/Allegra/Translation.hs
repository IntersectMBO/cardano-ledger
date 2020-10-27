{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Translation where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era hiding (Crypto)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
import Control.Iterate.SetAlgebra (biMapFromList, lifo)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API
import qualified Shelley.Spec.Ledger.EpochBoundary as EB
import qualified Shelley.Spec.Ledger.LedgerState as LS (returnRedeemAddrsToReserves, _delegations)
import Shelley.Spec.Ledger.Rewards (NonMyopic (..))

--------------------------------------------------------------------------------
-- Translation from Shelley to Allegra
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

type instance PreviousEra (AllegraEra c) = ShelleyEra c

-- | Currently no context is needed to translate from Shelley to Allegra.
--
-- Note: if context is needed, please coordinate with consensus, who will have
-- to provide the context in the right place.
type instance TranslationContext (AllegraEra c) = ()

instance (ShelleyBased (AllegraEra c), Crypto c) => TranslateEra (AllegraEra c) NewEpochState where
  -- TODO remove the ShelleyBased (AllegraEra c) constraint
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = translateEra' ctxt $ nesBprev nes,
          nesBcur = translateEra' ctxt $ nesBcur nes,
          nesEs = translateEra' ctxt $ LS.returnRedeemAddrsToReserves . nesEs $ nes,
          nesRu = translateEra' ctxt <$> nesRu nes,
          nesPd = nesPd nes
        }

instance Crypto c => TranslateEra (AllegraEra c) Tx where
  type TranslationError (AllegraEra c) Tx = ()
  translateEra _ = error "TODO Shelley to Allegra translation"

instance Crypto c => TranslateEra (AllegraEra c) ShelleyGenesis where
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

instance Crypto c => TranslateEra (AllegraEra c) (PParams' f) where
  translateEra _ pp =
    return $
      PParams
        { _minfeeA = _minfeeA pp,
          _minfeeB = _minfeeB pp,
          _maxBBSize = _maxBBSize pp,
          _maxTxSize = _maxTxSize pp,
          _maxBHSize = _maxBHSize pp,
          _keyDeposit = _keyDeposit pp,
          _poolDeposit = _poolDeposit pp,
          _eMax = _eMax pp,
          _nOpt = _nOpt pp,
          _a0 = _a0 pp,
          _rho = _rho pp,
          _tau = _tau pp,
          _d = _d pp,
          _extraEntropy = _extraEntropy pp,
          _protocolVersion = _protocolVersion pp,
          _minUTxOValue = _minUTxOValue pp,
          _minPoolCost = _minPoolCost pp
        }

instance Crypto c => TranslateEra (AllegraEra c) RewardAcnt where
  translateEra ctxt ra =
    return $
      RewardAcnt
        { getRwdNetwork = getRwdNetwork ra,
          getRwdCred = translateEra' ctxt $ getRwdCred ra
        }

instance Crypto c => TranslateEra (AllegraEra c) PoolParams where
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

instance Crypto c => TranslateEra (AllegraEra c) ShelleyGenesisStaking where
  translateEra ctxt sgs =
    return $
      ShelleyGenesisStaking
        { sgsPools = Map.map (translateEra' ctxt) (sgsPools sgs),
          sgsStake = sgsStake sgs
        }

instance Crypto c => TranslateEra (AllegraEra c) Addr where
  translateEra _ = error "TODO Shelley to Allegra translation"

instance Crypto c => TranslateEra (AllegraEra c) EB.BlocksMade where
  translateEra _ bm = return . EB.BlocksMade . EB.unBlocksMade $ bm

instance Crypto c => TranslateEra (AllegraEra c) NonMyopic where
  translateEra _ nm =
    return
      NonMyopic
        { likelihoodsNM = likelihoodsNM nm,
          rewardPotNM = rewardPotNM nm
        }

instance Crypto c => TranslateEra (AllegraEra c) ScriptHash where
  translateEra _ = error "TODO Shelley to Allegra translation"

instance Crypto c => TranslateEra (AllegraEra c) (Credential kr) where
  translateEra ctxt (ScriptHashObj h) = return (ScriptHashObj (translateEra' ctxt h))
  translateEra _ (KeyHashObj h) = return (KeyHashObj h)

instance Crypto c => TranslateEra (AllegraEra c) RewardUpdate where
  translateEra ctxt ru =
    return
      RewardUpdate
        { deltaT = deltaT ru,
          deltaR = deltaR ru,
          rs = Map.mapKeys (translateEra' ctxt) $ rs ru,
          deltaF = deltaF ru,
          nonMyopic = translateEra' ctxt $ nonMyopic ru
        }

instance Crypto c => TranslateEra (AllegraEra c) SnapShot where
  translateEra ctxt snap =
    return
      SnapShot
        { _stake = Stake $ Map.mapKeys (translateEra' ctxt) $ unStake . _stake $ snap,
          EB._delegations = Map.mapKeys (translateEra' ctxt) $ EB._delegations snap,
          _poolParams = Map.map (translateEra' ctxt) $ _poolParams snap
        }

instance Crypto c => TranslateEra (AllegraEra c) SnapShots where
  translateEra ctxt snaps =
    return
      SnapShots
        { _pstakeMark = translateEra' ctxt $ _pstakeMark snaps,
          _pstakeSet = translateEra' ctxt $ _pstakeSet snaps,
          _pstakeGo = translateEra' ctxt $ _pstakeGo snaps,
          _feeSS = _feeSS snaps
        }

instance Crypto c => TranslateEra (AllegraEra c) ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance Crypto c => TranslateEra (AllegraEra c) PPUPState where
  translateEra ctxt ps =
    return
      PPUPState
        { proposals = translateEra' ctxt $ proposals ps,
          futureProposals = translateEra' ctxt $ futureProposals ps
        }

instance Crypto c => TranslateEra (AllegraEra c) TxId where
  translateEra _ = error "TODO Shelley to Allegra translation"

instance Crypto c => TranslateEra (AllegraEra c) TxIn where
  translateEra ctxt (TxIn txid ix) = return $ TxIn (translateEra' ctxt txid) ix

instance Crypto c => TranslateEra (AllegraEra c) TxOut where
  translateEra _ = error "TODO Shelley to Allegra translation"

instance Crypto c => TranslateEra (AllegraEra c) UTxO where
  translateEra ctxt utxo =
    return $
      UTxO $
        Map.mapKeys (translateEra' ctxt) $
          Map.map (translateEra' ctxt) $
            unUTxO utxo

instance Crypto c => TranslateEra (AllegraEra c) UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { _utxo = translateEra' ctxt $ _utxo us,
          _deposited = _deposited us,
          _fees = _fees us,
          _ppups = translateEra' ctxt $ _ppups us
        }

instance Crypto c => TranslateEra (AllegraEra c) InstantaneousRewards where
  translateEra ctxt ir =
    return
      InstantaneousRewards
        { iRReserves = Map.mapKeys (translateEra' ctxt) (iRReserves ir),
          iRTreasury = Map.mapKeys (translateEra' ctxt) (iRTreasury ir)
        }

instance Crypto c => TranslateEra (AllegraEra c) DState where
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

instance Crypto c => TranslateEra (AllegraEra c) PState where
  translateEra ctxt ps =
    return
      PState
        { _pParams = Map.map (translateEra' ctxt) (_pParams ps),
          _fPParams = Map.map (translateEra' ctxt) (_fPParams ps),
          _retiring = _retiring ps
        }

instance Crypto c => TranslateEra (AllegraEra c) DPState where
  translateEra ctxt dp =
    return
      DPState
        { _dstate = translateEra' ctxt $ _dstate dp,
          _pstate = translateEra' ctxt $ _pstate dp
        }

instance Crypto c => TranslateEra (AllegraEra c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { _utxoState = translateEra' ctxt $ _utxoState ls,
          _delegationState = translateEra' ctxt $ _delegationState ls
        }

instance Crypto c => TranslateEra (AllegraEra c) EpochState where
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
