{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.LedgerState.Types where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core (EraCrypto, EraGov, EraTxOut, GovState)
import Cardano.Ledger.FRxO (FRxO)
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  IncrementalStake,
 )
import Cardano.Ledger.UTxO (UTxO)
import Control.DeepSeq (NFData)
import Data.Default.Class (Default (def))
import GHC.Generics (Generic)

-- type instance Ledger (ConwayEra c) = LedgerStateTemp (ConwayEra c)

-- | The state associated with a 'Ledger'.
data LedgerStateTemp era = LedgerStateTemp
  { lstUTxOState :: !(UTxOStateTemp era)
  -- ^ The current unspent transaction outputs.
  , lstCertState :: !(CertState era)
  }
  deriving (Generic)

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (LedgerStateTemp era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (LedgerStateTemp era)

-- instance
--   ( EraTxOut era
--   , NoThunks (GovState era)
--   ) =>
--   NoThunks (LedgerStateTemp era)

-- instance
--   ( EraTxOut era
--   , NFData (GovState era)
--   ) =>
--   NFData (LedgerStateTemp era)

-- instance
--   ( EraTxOut era
--   , EncCBOR (GovState era)
--   ) =>
--   EncCBOR (LedgerStateTemp era)
--   where
--   encCBOR LedgerStateTemp {lstUTxOState, lstCertState} =
--     encodeListLen 2
--       <> encCBOR lstCertState -- encode delegation state first to improve sharing
--       <> encCBOR lstUTxOState

-- instance
--   ( EraTxOut era
--   , EraGov era
--   ) =>
--   DecShareCBOR (LedgerStateTemp era)
--   where
--   type
--     Share (LedgerStateTemp era) =
--       ( Interns (Credential 'Staking (EraCrypto era))
--       , Interns (KeyHash 'StakePool (EraCrypto era))
--       )
--   decSharePlusCBOR =
--     decodeRecordNamedT "LedgerStateTemp" (const 2) $ do
--       lstCertState <- decSharePlusCBOR
--       lstUTxOState <- decShareLensCBOR _1
--       pure LedgerStateTemp {lstUTxOState, lstCertState}

-- instance (EraTxOut era, EraGov era) => ToCBOR (LedgerStateTemp era) where
--   toCBOR = toEraCBOR @era

-- instance (EraTxOut era, EraGov era) => FromCBOR (LedgerStateTemp era) where
--   fromCBOR = toPlainDecoder (eraProtVerLow @era) decNoShareCBOR

-- instance (EraTxOut era, EraGov era) => ToJSON (LedgerStateTemp era) where
--   toJSON = object . toLedgerStatePairs
--   toEncoding = pairs . mconcat . toLedgerStatePairs

-- toLedgerStatePairs ::
--   (EraTxOut era, EraGov era, KeyValue e a, ToJSON (UTxOStateTemp era)) => LedgerStateTemp era -> [a]
-- toLedgerStatePairs ls@(LedgerStateTemp _ _) =
--   let LedgerStateTemp {..} = ls
--    in [ "utxoState" .= lstUTxOState
--       , "delegationState" .= lstCertState
--       ]

--------

data UTxOStateTemp era = UTxOStateTemp
  { utxostUtxo :: !(UTxO era)
  , utxostFrxo :: !(FRxO era)
  , utxostDeposited :: Coin
  -- ^ This field is left lazy, because we only use it for assertions
  , utxostFees :: !Coin
  , utxostGovState :: !(GovState era)
  , utxostStakeDistr :: !(IncrementalStake (EraCrypto era))
  , utxostDonation :: !Coin
  }
  deriving (Generic)

-- ====================================================

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

instance EraGov era => Default (UTxOStateTemp era) where
  def = UTxOStateTemp mempty mempty mempty mempty def mempty mempty

-------

instance
  ( EraTxOut era
  , NFData (GovState era)
  ) =>
  NFData (UTxOStateTemp era)

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (UTxOStateTemp era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (UTxOStateTemp era)

-- deriving via
--   AllowThunksIn
--     '["utxosDeposited"]
--     (UTxOStateTemp era)
--   instance
--     ( NoThunks (UTxO era)
--     , NoThunks (FRxO era)
--     , NoThunks (GovState era)
--     , Era era
--     ) =>
--     NoThunks (UTxOStateTemp era)

-- instance
--   ( EraTxOut era
--   , EncCBOR (GovState era)
--   ) =>
--   EncCBOR (UTxOStateTemp era)
--   where
--   encCBOR (UTxOStateTemp ut fr dp fs us sd don) =
--     encode $
--       Rec UTxOStateTemp
--         !> To ut
--         !> To fr
--         !> To dp
--         !> To fs
--         !> To us
--         !> To sd
--         !> To don

-- instance
--   ( EraTxOut era
--   , EraGov era
--   ) =>
--   DecShareCBOR (UTxOStateTemp era)
--   where
--   type
--     Share (UTxOStateTemp era) =
--       Interns (Credential 'Staking (EraCrypto era))
--   decShareCBOR credInterns =
--     decodeRecordNamed "UTxOStateTemp" (const 6) $ do
--       utxostUtxo <- decShareCBOR credInterns
--       let utxostFrxo = mempty
--       utxostDeposited <- decCBOR
--       utxostFees <- decCBOR
--       -- TODO: implement proper sharing: https://github.com/intersectmbo/cardano-ledger/issues/3486
--       utxostGovState <- decNoShareCBOR
--       utxostStakeDistr <- decShareCBOR credInterns
--       utxostDonation <- decCBOR
--       pure UTxOStateTemp {..}

-- instance (EraTxOut era, EraGov era) => ToCBOR (UTxOStateTemp era) where
--   toCBOR = toEraCBOR @era

-- instance (EraTxOut era, EraGov era) => FromCBOR (UTxOStateTemp era) where
--   fromCBOR = toPlainDecoder (eraProtVerLow @era) decNoShareCBOR

-- instance (EraTxOut era, EraGov era) => ToJSON (UTxOStateTemp era) where
--   toJSON = object . toUTxOStateTempPairs
--   toEncoding = pairs . mconcat . toUTxOStateTempPairs

-- toUTxOStateTempPairs ::
--   (EraTxOut era, EraGov era, KeyValue e a) => UTxOStateTemp era -> [a]
-- toUTxOStateTempPairs utxoState@(UTxOStateTemp {}) =
--   let UTxOStateTemp {..} = utxoState
--    in [ "utxo" .= utxostUtxo
--       , "frxo" .= utxostFrxo
--       , "deposited" .= utxostDeposited
--       , "fees" .= utxostFees
--       , "ppups" .= utxostGovState
--       , "stake" .= utxostStakeDistr
--       ]

-- utxostUtxosL :: Lens' (UTxOStateTemp era) (UTxOState era)
-- utxostUtxosL = lens getter setter
--   where
--     getter (UTxOStateTemp utxos _ deposited fees govState stakeDistr donation) =
--       UTxOState utxos deposited fees govState stakeDistr donation
--     setter (UTxOStateTemp {utxostFrxo}) (UTxOState utxos deposited fees govState stakeDistr donation) =
--       UTxOStateTemp utxos utxostFrxo deposited fees govState stakeDistr donation