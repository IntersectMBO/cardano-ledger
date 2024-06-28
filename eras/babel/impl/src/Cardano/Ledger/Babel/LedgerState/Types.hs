{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.LedgerState.Types where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (encCBOR),
  Interns,
  decNoShareCBOR,
  decShareLensCBOR,
  decodeRecordNamed,
  decodeRecordNamedT,
 )
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Binary.Decoding (DecShareCBOR (..))
import Cardano.Ledger.Binary.Encoding (encodeListLen)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core (EraCrypto, EraGov, EraTxOut, GovState)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.FRxO (FRxO)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  HasLedgerState (..),
  IncrementalStake,
  UTxOState (UTxOState),
 )
import Cardano.Ledger.UTxO (UTxO)
import Control.DeepSeq (NFData)
import Data.Default.Class (Default (def))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- type instance Ledger (ConwayEra c) = LedgerStateTemp (ConwayEra c)

-- | The state associated with a 'Ledger'.
data LedgerStateTemp era = LedgerStateTemp
  { lstUTxOState :: !(UTxOStateTemp era)
  -- ^ The current unspent transaction outputs.
  , lstCertState :: !(CertState era)
  }
  deriving (Generic, Default)

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

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  ) =>
  NoThunks (LedgerStateTemp era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  ) =>
  NFData (LedgerStateTemp era)

instance
  ( EraTxOut era
  , EncCBOR (GovState era)
  ) =>
  EncCBOR (LedgerStateTemp era)
  where
  encCBOR LedgerStateTemp {lstUTxOState, lstCertState} =
    encodeListLen 2
      <> encCBOR lstCertState -- encode delegation state first to improve sharing
      <> encCBOR lstUTxOState

instance
  ( EraTxOut era
  , EncCBOR (GovState era)
  ) =>
  EncCBOR (UTxOStateTemp era)
  where
  encCBOR (UTxOStateTemp ut fr dp fs us sd don) =
    encode $
      Rec UTxOStateTemp
        !> To ut
        !> To fr
        !> To dp
        !> To fs
        !> To us
        !> To sd
        !> To don

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  ) =>
  NoThunks (UTxOStateTemp era)

-- ====================================================
-- LedgerState

utxoStateFromTemp :: UTxOStateTemp era -> UTxOState era
utxoStateFromTemp (UTxOStateTemp a b c d e f g) = UTxOState a b c d e f g

lstUTxOStateL :: Lens' (LedgerStateTemp era) (UTxOState era)
lstUTxOStateL = lens (utxoStateFromTemp . lstUTxOState) (\x _y -> x {lstUTxOState = undefined}) -- TODO WG

lstUTxOStateTempL :: Lens' (LedgerStateTemp era) (UTxOStateTemp era)
lstUTxOStateTempL = lens lstUTxOState (\x y -> x {lstUTxOState = y})

lstCertStateL :: Lens' (LedgerStateTemp era) (CertState era)
lstCertStateL = lens lstCertState (\x y -> x {lstCertState = y})

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecShareCBOR (UTxOStateTemp era)
  where
  type
    Share (UTxOStateTemp era) =
      Interns (Credential 'Staking (EraCrypto era))
  decShareCBOR credInterns =
    decodeRecordNamed "UTxOStateTemp" (const 6) $ do
      utxostUtxo <- decShareCBOR credInterns
      utxostFrxo <- decShareCBOR credInterns
      utxostDeposited <- decCBOR
      utxostFees <- decCBOR
      -- TODO: implement proper sharing: https://github.com/intersectmbo/cardano-ledger/issues/3486
      utxostGovState <- decNoShareCBOR
      utxostStakeDistr <- decShareCBOR credInterns
      utxostDonation <- decCBOR
      pure UTxOStateTemp {..}

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecShareCBOR (LedgerStateTemp era)
  where
  type
    Share (LedgerStateTemp era) =
      ( Interns (Credential 'Staking (EraCrypto era))
      , Interns (KeyHash 'StakePool (EraCrypto era))
      )
  decSharePlusCBOR =
    decodeRecordNamedT "LedgerState" (const 2) $ do
      lstCertState <- decSharePlusCBOR
      lstUTxOState <- decShareLensCBOR _1
      pure $ LedgerStateTemp {lstUTxOState, lstCertState}

newtype BabelLedgerState era = BabelLedgerState {unBabelLedgerState :: LedgerStateTemp era}
  deriving (Generic, Default)

babelLedgerStateL :: Lens' (BabelLedgerState era) (LedgerStateTemp era)
babelLedgerStateL = lens unBabelLedgerState (\x y -> x {unBabelLedgerState = y})

instance (Crypto c, EraGov (BabelEra c), EraTxOut (BabelEra c)) => HasLedgerState (BabelEra c) where
  type EraLedgerState (BabelEra c) = BabelLedgerState (BabelEra c)
  hlsUTxOStateL = babelLedgerStateL . lstUTxOStateL
  hlsCertStateL = babelLedgerStateL . lstCertStateL

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (BabelLedgerState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (BabelLedgerState era)

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  ) =>
  NoThunks (BabelLedgerState era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  ) =>
  NFData (BabelLedgerState era)

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  EncCBOR (BabelLedgerState era)
  where
  encCBOR (BabelLedgerState ls) = encCBOR ls

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecCBOR (BabelLedgerState era)
  where
  decCBOR = decNoShareCBOR

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecShareCBOR (BabelLedgerState era)
  where
  type
    Share (BabelLedgerState era) =
      ( Interns (Credential 'Staking (EraCrypto era))
      , Interns (KeyHash 'StakePool (EraCrypto era))
      )
  decSharePlusCBOR = BabelLedgerState <$> decSharePlusCBOR

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
