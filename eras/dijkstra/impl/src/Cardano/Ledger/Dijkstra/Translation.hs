{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Translation () where

import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  ConwayGovState (..),
  DRepPulsingState (..),
  EnactState (..),
  GovAction (..),
  GovActionState (..),
  ProposalProcedure (..),
  Proposals,
  PulsingSnapshot,
  RatifyState (..),
  finishDRepPulser,
  mkEnactState,
  rsEnactStateL,
  setCompleteDRepPulsingState,
  translateProposals,
 )
import Cardano.Ledger.Conway.Governance.DRepPulser (PulsingSnapshot (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.Governance ()
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Dijkstra.TxAuxData ()
import Cardano.Ledger.Dijkstra.TxBody (upgradeGovAction, upgradeProposals)
import Cardano.Ledger.Dijkstra.TxWits ()
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  NewEpochState (..),
  UTxOState (..),
  epochStateGovStateL,
  lsCertStateL,
  lsUTxOStateL,
 )
import Data.Coerce (coerce)
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~), (^.))

type instance TranslationContext DijkstraEra = DijkstraGenesis

instance TranslateEra DijkstraEra (Tx TopTx) where
  type TranslationError DijkstraEra (Tx TopTx) = DecoderError
  translateEra _ctxt tx = case toSTxLevel tx of
    STopTxOnly -> do
      -- Note that this does not preserve the hidden bytes field of the transaction.
      -- This is under the premise that this is irrelevant for TxInBlocks, which are
      -- not transmitted as contiguous chunks.
      txBody <- translateEraThroughCBOR "TxBody" $ tx ^. bodyTxL
      txWits <- translateEraThroughCBOR "TxWits" $ tx ^. witsTxL
      auxData <- mapM (translateEraThroughCBOR "TxAuxData") (tx ^. auxDataTxL)
      let isValidTx = tx ^. isValidTxL
      pure $
        mkBasicTx txBody
          & witsTxL .~ txWits
          & isValidTxL .~ isValidTx
          & auxDataTxL .~ auxData

instance TranslateEra DijkstraEra NewEpochState where
  translateEra ctxt nes = do
    let es = translateEra' ctxt $ nesEs nes
        -- We need to ensure that we have the same initial EnactState in the pulser as
        -- well as in the current EnactState, otherwise in the very first EPOCH rule call
        -- the pulser will reset it.
        ratifyState =
          def
            & rsEnactStateL .~ mkEnactState (es ^. epochStateGovStateL)
    pure $
      NewEpochState
        { nesEL = nesEL nes
        , nesBprev = nesBprev nes
        , nesBcur = nesBcur nes
        , nesEs = setCompleteDRepPulsingState def ratifyState es
        , nesRu = nesRu nes
        , nesPd = nesPd nes
        , stashedAVVMAddresses = ()
        }

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance TranslateEra DijkstraEra PParams where
  translateEra DijkstraGenesis {dgUpgradePParams} =
    pure . upgradePParams dgUpgradePParams

instance TranslateEra DijkstraEra FuturePParams where
  translateEra ctxt = \case
    NoPParamsUpdate -> pure NoPParamsUpdate
    DefinitePParamsUpdate pp -> DefinitePParamsUpdate <$> translateEra ctxt pp
    PotentialPParamsUpdate mpp -> PotentialPParamsUpdate <$> mapM (translateEra ctxt) mpp

instance TranslateEra DijkstraEra EpochState where
  translateEra ctxt es =
    pure $
      EpochState
        { esChainAccountState = esChainAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance TranslateEra DijkstraEra DState where
  translateEra _ DState {dsAccounts = ConwayAccounts accounts, ..} =
    pure DState {dsAccounts = ConwayAccounts (Map.map coerce accounts), ..}

instance TranslateEra DijkstraEra PState where
  translateEra _ PState {..} = pure PState {..}

instance TranslateEra DijkstraEra API.LedgerState where
  translateEra ctx ls =
    pure
      API.LedgerState
        { API.lsUTxOState = translateEra' ctx $ ls ^. lsUTxOStateL
        , API.lsCertState = translateCertState ctx $ ls ^. lsCertStateL
        }

translateCertState ::
  TranslationContext DijkstraEra ->
  API.CertState ConwayEra ->
  API.CertState DijkstraEra
translateCertState ctx scert =
  def
    & certDStateL .~ translateEra' ctx (scert ^. certDStateL)
    & certPStateL .~ translateEra' ctx (scert ^. certPStateL)

instance TranslateEra DijkstraEra GovAction where
  translateEra _ = pure . upgradeGovAction

instance TranslateEra DijkstraEra ProposalProcedure where
  translateEra _ = pure . upgradeProposals

instance TranslateEra DijkstraEra GovActionState where
  translateEra ctxt GovActionState {..} =
    pure $
      GovActionState
        { gasId = gasId
        , gasCommitteeVotes = gasCommitteeVotes
        , gasDRepVotes = gasDRepVotes
        , gasStakePoolVotes = gasStakePoolVotes
        , gasProposalProcedure = translateEra' ctxt gasProposalProcedure
        , gasProposedIn = gasProposedIn
        , gasExpiresAfter = gasExpiresAfter
        }

instance TranslateEra DijkstraEra Proposals where
  translateEra ctxt = pure . translateProposals @DijkstraEra ctxt

instance TranslateEra DijkstraEra PulsingSnapshot where
  translateEra ctxt PulsingSnapshot {..} =
    pure $
      PulsingSnapshot
        { psProposals = translateEra' ctxt <$> psProposals
        , psDRepDistr = psDRepDistr
        , psDRepState = psDRepState
        , psPoolDistr = psPoolDistr
        }

instance TranslateEra DijkstraEra EnactState where
  translateEra ctxt EnactState {..} =
    pure $
      EnactState
        { ensCommittee = coerce ensCommittee
        , ensConstitution = coerce ensConstitution
        , ensCurPParams = translateEra' ctxt ensCurPParams
        , ensPrevPParams = translateEra' ctxt ensPrevPParams
        , ensTreasury = ensTreasury
        , ensWithdrawals = ensWithdrawals
        , ensPrevGovActionIds = ensPrevGovActionIds
        }

instance TranslateEra DijkstraEra RatifyState where
  translateEra ctxt RatifyState {..} =
    pure $
      RatifyState
        { rsEnactState = translateEra' ctxt rsEnactState
        , rsEnacted = translateEra' ctxt <$> rsEnacted
        , rsExpired = rsExpired
        , rsDelayed = rsDelayed
        }

instance TranslateEra DijkstraEra DRepPulsingState where
  translateEra ctxt dps = pure $ DRComplete (translateEra' ctxt x) (translateEra' ctxt y)
    where
      (x, y) = finishDRepPulser dps

instance TranslateEra DijkstraEra ConwayGovState where
  translateEra ctxt ConwayGovState {..} =
    pure $
      ConwayGovState
        { cgsCommittee = coerce cgsCommittee
        , cgsProposals = translateEra' ctxt cgsProposals
        , cgsConstitution = coerce cgsConstitution
        , cgsCurPParams = translateEra' ctxt cgsCurPParams
        , cgsPrevPParams = translateEra' ctxt cgsPrevPParams
        , cgsFuturePParams = translateEra' ctxt cgsFuturePParams
        , cgsDRepPulsingState = translateEra' ctxt cgsDRepPulsingState
        }

instance TranslateEra DijkstraEra UTxOState where
  translateEra ctxt us =
    pure
      UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us
        , API.utxosDeposited = API.utxosDeposited us
        , API.utxosFees = API.utxosFees us
        , API.utxosGovState = translateEra' ctxt $ API.utxosGovState us
        , API.utxosInstantStake = coerce $ API.utxosInstantStake us
        , API.utxosDonation = API.utxosDonation us
        }

instance TranslateEra DijkstraEra API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ upgradeTxOut `Map.map` API.unUTxO utxo
