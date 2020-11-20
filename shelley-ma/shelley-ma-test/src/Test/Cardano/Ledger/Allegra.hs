{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC  -fno-warn-orphans #-}


-- | This module is meant to supply the Era independent testing instances for the AllegraEra
--   use it something like this:   import Test.Cardano.Ledger.Allegra()

module Test.Cardano.Ledger.Allegra where

import Cardano.Ledger.ShelleyMA.Timelocks(Timelock(..))
import Cardano.Ledger.ShelleyMA.TxBody(TxBody(..))
import qualified Cardano.Ledger.Crypto as CryptoClass
import qualified Cardano.Ledger.Core as Core(Script)
import Test.Cardano.Ledger.EraBuffet( AllegraEra )
import Test.Cardano.Ledger.Mary
  ( genMATxBody,
    txBodyZero,
    someLeaf,
    quantifyTL,
    unQuantifyTL,
  )
import Test.Shelley.Spec.Ledger.Generator.Core(EraGen (..))
import Test.Shelley.Spec.Ledger.Generator.EraGen(genUtxo0)
import Test.Shelley.Spec.Ledger.Generator.TypeFamilyClasses
  ( ScriptClass(..),
    ValueClass(..),
    TxBodyClass(..),
    genCoin,
  )

-- =====================================
-- EraGen instances for the AllegraEra
-- =====================================

instance ( CryptoClass.Crypto c) => ScriptClass (AllegraEra c) where
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

type instance Core.Script (AllegraEra c) = Timelock (AllegraEra c)

instance (CryptoClass.Crypto c) => ValueClass  (AllegraEra c) where
   genValue _ = genCoin

instance (CryptoClass.Crypto c) => TxBodyClass (AllegraEra c) where
  emptyTxBody = txBodyZero
  liftTxBody gen = do { _tx <- gen; pure undefined }

instance CryptoClass.Crypto c => EraGen (AllegraEra c) where
  genEraUtxo0 = genUtxo0
  genEraTxBody = genMATxBody
  updateEraTxBody (TxBody _in _out cert wdrl _txfee vi upd meta forge) fee ins outs =
     (TxBody ins outs cert wdrl fee vi upd meta forge)
