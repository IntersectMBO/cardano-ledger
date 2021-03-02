{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common basis for the Utxow rule
module Cardano.Ledger.CoreUtxow
  ( CoreUtxow (..),
    ValidateScript (..),
  )
where

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.SafeHash (HashAnnotated)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Keys (KeyRole (Witness))
import Shelley.Spec.Ledger.PParams (Update (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.TxBody (DCert, EraIndependentTxBody, TxIn, Wdrl, WitVKey)

-- ==============================

-- | Typeclass for Script data types. Supports script validation and hashing.
class Era era => ValidateScript era where
  validateScript :: Core.Script era -> Core.Tx era -> Bool
  hashScript :: Core.Script era -> ScriptHash (Crypto era)
  isNativeScript :: Core.Script era -> Bool
  isNativeScript _ = True

class
  ( Era era,
    HashAnnotated (body era) EraIndependentTxBody (Crypto era),
    ValidateScript era, -- So we can Validate
    Core.Tx era ~ tx era, -- So (Core.Tx era) in ValidateScript aligns with tx in this class
    Core.TxOut era ~ txout era -- So addressOut can be applied to Core.TxOut
  ) =>
  CoreUtxow era tx body wit txout
    | era -> tx body wit txout
  where
  bodyTx :: tx era -> body era
  witTx :: tx era -> wit era
  metaTx :: tx era -> StrictMaybe (Core.AuxiliaryData era)
  addrWit :: wit era -> Set (WitVKey 'Witness (Crypto era))
  bootWit :: wit era -> Set (BootstrapWitness (Crypto era))
  scriptWit :: wit era -> Map.Map (ScriptHash (Crypto era)) (Core.Script era)
  updateBody :: body era -> StrictMaybe (Update era)
  wdrlsBody :: body era -> Wdrl (Crypto era)
  certsBody :: body era -> StrictSeq (DCert (Crypto era))
  inputsBody :: body era -> Set (TxIn (Crypto era))
  mintBody :: body era -> Set (ScriptHash (Crypto era))
  adHashBody :: body era -> StrictMaybe (AuxiliaryDataHash (Crypto era))
  addressOut :: txout era -> Addr (Crypto era)
