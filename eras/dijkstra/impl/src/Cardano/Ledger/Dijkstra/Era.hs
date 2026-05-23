{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  BBODY,
  CERT,
  GOV,
  GOVCERT,
  LEDGER,
  MEMPOOL,
  SUBCERT,
  SUBCERTS,
  SUBDELEG,
  SUBGOV,
  SUBGOVCERT,
  SUBLEDGER,
  SUBLEDGERS,
  SUBPOOL,
  SUBUTXOW,
  SUBUTXO,
  UTXO,
  UTXOW,
  DijkstraEraBlockHeader (..),
  DijkstraBbodySignal (..),
) where

import Cardano.Ledger.BaseTypes (Nonce)
import Cardano.Ledger.Block (Block, EraBlockHeader)
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Internal.Era (DijkstraEra)
import Cardano.Ledger.Mary (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Lens.Micro

instance EraTxLevel DijkstraEra where
  type STxLevel l DijkstraEra = STxBothLevels l DijkstraEra

data DijkstraBbodySignal era
  = forall h. DijkstraEraBlockHeader h era => DijkstraBbodySignal (Block h era)

class EraBlockHeader h era => DijkstraEraBlockHeader h era where
  prevNonceBlockHeaderL :: Lens' (Block h era) Nonce

-------------------------------------------------------------------------------
-- Deprecated rules
-------------------------------------------------------------------------------

type instance EraRule "UPEC" DijkstraEra = VoidEraRule "UPEC" DijkstraEra

type instance EraRuleFailure "UPEC" DijkstraEra = VoidEraRule "UPEC" DijkstraEra

type instance EraRuleEvent "UPEC" DijkstraEra = VoidEraRule "UPEC" DijkstraEra

type instance EraRule "NEWPP" DijkstraEra = VoidEraRule "NEWPP" DijkstraEra

type instance EraRuleFailure "NEWPP" DijkstraEra = VoidEraRule "NEWPP" DijkstraEra

type instance EraRuleEvent "NEWPP" DijkstraEra = VoidEraRule "NEWPP" DijkstraEra

type instance EraRule "PPUP" DijkstraEra = VoidEraRule "PPUP" DijkstraEra

type instance EraRuleFailure "PPUP" DijkstraEra = VoidEraRule "PPUP" DijkstraEra

type instance EraRuleEvent "PPUP" DijkstraEra = VoidEraRule "PPUP" DijkstraEra

type instance EraRule "MIR" DijkstraEra = VoidEraRule "MIR" DijkstraEra

type instance EraRuleFailure "MIR" DijkstraEra = VoidEraRule "MIR" DijkstraEra

type instance EraRuleEvent "MIR" DijkstraEra = VoidEraRule "MIR" DijkstraEra

type instance EraRule "DELEGS" DijkstraEra = VoidEraRule "DELEGS" DijkstraEra

type instance EraRuleFailure "DELEGS" DijkstraEra = VoidEraRule "DELEGS" DijkstraEra

type instance EraRuleEvent "DELEGS" DijkstraEra = VoidEraRule "DELEGS" DijkstraEra

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

type instance Value DijkstraEra = MaryValue

data SUBLEDGERS era

type instance EraRule "SUBLEDGERS" DijkstraEra = SUBLEDGERS DijkstraEra

data SUBLEDGER era

type instance EraRule "SUBLEDGER" DijkstraEra = SUBLEDGER DijkstraEra

data SUBCERTS era

type instance EraRule "SUBCERTS" DijkstraEra = SUBCERTS DijkstraEra

data SUBCERT era

type instance EraRule "SUBCERT" DijkstraEra = SUBCERT DijkstraEra

data SUBDELEG era

type instance EraRule "SUBDELEG" DijkstraEra = SUBDELEG DijkstraEra

data SUBGOV era

type instance EraRule "SUBGOV" DijkstraEra = SUBGOV DijkstraEra

data SUBGOVCERT era

type instance EraRule "SUBGOVCERT" DijkstraEra = SUBGOVCERT DijkstraEra

data SUBPOOL era

type instance EraRule "SUBPOOL" DijkstraEra = SUBPOOL DijkstraEra

data SUBUTXO era

type instance EraRule "SUBUTXO" DijkstraEra = SUBUTXO DijkstraEra

data SUBUTXOW era

type instance EraRule "SUBUTXOW" DijkstraEra = SUBUTXOW DijkstraEra

data GOV era

type instance EraRule "GOV" DijkstraEra = GOV DijkstraEra

type instance EraRule "NEWEPOCH" DijkstraEra = Conway.NEWEPOCH DijkstraEra

type instance EraRule "EPOCH" DijkstraEra = Conway.EPOCH DijkstraEra

type instance EraRule "ENACT" DijkstraEra = Conway.ENACT DijkstraEra

type instance EraRule "UTXOS" DijkstraEra = Conway.UTXOS DijkstraEra

data LEDGER era

type instance EraRule "LEDGER" DijkstraEra = LEDGER DijkstraEra

type instance EraRule "TICKF" DijkstraEra = Conway.TICKF DijkstraEra

type instance EraRule "RATIFY" DijkstraEra = Conway.RATIFY DijkstraEra

type instance EraRule "CERTS" DijkstraEra = Conway.CERTS DijkstraEra

data CERT era

type instance EraRule "CERT" DijkstraEra = CERT DijkstraEra

type instance EraRule "DELEG" DijkstraEra = Conway.DELEG DijkstraEra

data GOVCERT era

type instance EraRule "GOVCERT" DijkstraEra = GOVCERT DijkstraEra

data UTXOW era

type instance EraRule "UTXOW" DijkstraEra = UTXOW DijkstraEra

data UTXO era

type instance EraRule "UTXO" DijkstraEra = UTXO DijkstraEra

data BBODY era

type instance EraRule "BBODY" DijkstraEra = BBODY DijkstraEra

data MEMPOOL era

type instance EraRule "MEMPOOL" DijkstraEra = MEMPOOL DijkstraEra

type instance EraRule "HARDFORK" DijkstraEra = Conway.HARDFORK DijkstraEra

-- Rules inherited from Shelley

type instance EraRule "LEDGERS" DijkstraEra = API.LEDGERS DijkstraEra

type instance EraRule "POOLREAP" DijkstraEra = API.POOLREAP DijkstraEra

type instance EraRule "RUPD" DijkstraEra = Shelley.RUPD DijkstraEra

type instance EraRule "SNAP" DijkstraEra = Shelley.SNAP DijkstraEra

type instance EraRule "TICK" DijkstraEra = Shelley.TICK DijkstraEra

type instance EraRule "POOL" DijkstraEra = Shelley.POOL DijkstraEra
