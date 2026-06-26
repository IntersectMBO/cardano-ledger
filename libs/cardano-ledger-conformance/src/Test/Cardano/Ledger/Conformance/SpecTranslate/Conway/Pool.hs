{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool where

import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (SpecRep, toSpecRep),
  toSpecRepMap,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance SpecTranslate ConwayEra (Shelley.PoolEnv ConwayEra) where
  type SpecRep ConwayEra (Shelley.PoolEnv ConwayEra) = Agda.PParams

  toSpecRep (Shelley.PoolEnv _ pp) = toSpecRep pp

instance SpecTranslate ConwayEra (PState ConwayEra) where
  type SpecRep ConwayEra (PState ConwayEra) = Agda.PState

  toSpecRep PState {..} =
    Agda.MkPState
      <$> toSpecRepMap (Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) psStakePools)
      <*> toSpecRepMap psFutureStakePoolParams
      <*> toSpecRepMap psRetiring

-- | This instance is only used for conformance testing (e.g. @ExecSpecRule@).
-- This instance uses arbitrary tags (0 and 1) that don't match the CDDL spec.
--
-- The actual spec-compliant CBOR encoding that follows the CDDL is handled by 'encodePoolCert'.
instance EncCBOR PoolCert where
  encCBOR =
    encode . \case
      RegPool pp -> Sum RegPool 0 !> To pp
      RetirePool kh eNo -> Sum RetirePool 1 !> To kh !> To eNo

instance SpecTranslate ConwayEra PoolCert where
  type SpecRep ConwayEra PoolCert = Agda.DCert

  toSpecRep (RegPool p@StakePoolParams {sppId = KeyHash ppHash}) =
    Agda.Regpool
      <$> toSpecRep ppHash
      <*> toSpecRep p
  toSpecRep (RetirePool (KeyHash ppHash) e) =
    Agda.Retirepool
      <$> toSpecRep ppHash
      <*> toSpecRep e
