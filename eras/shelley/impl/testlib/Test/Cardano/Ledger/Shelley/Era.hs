{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Era (
  EraTest (..),
  ShelleyEraTest,
  shelleyGenPParams,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (EpochInterval (..), ProtVer (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  EraPParams (..),
  EraTxOut (..),
  PParams,
  emptyPParams,
  eraProtVerLow,
  ppEMaxL,
  ppKeyDepositL,
  ppMaxTxSizeL,
  ppMinFeeAL,
  ppMinFeeBL,
  ppPoolDepositL,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Plutus (emptyCostModels)
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.Transition
import Cardano.Ledger.State
import Data.Default
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.TreeDiff ()

class
  ( EraTest era
  , ShelleyEraScript era
  , EraTransition era
  , Arbitrary (TransitionConfig era)
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , ToExpr (StashedAVVMAddresses era)
  , NFData (StashedAVVMAddresses era)
  , Default (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
  , ToExpr (ScriptsNeeded era)
  ) =>
  ShelleyEraTest era

shelleyGenPParams :: EraPParams era => Gen (PParams era)
shelleyGenPParams = do
  minfeeA <- Coin <$> choose (0, 1000)
  minfeeB <- Coin <$> choose (0, 10000)
  pure $
    emptyPParams
      & ppMinFeeAL .~ minfeeA
      & ppMinFeeBL .~ minfeeB
      & ppMaxTxSizeL .~ fromIntegral (maxBound :: Int)
      & ppProtocolVersionL .~ ProtVer (eraProtVerLow @ShelleyEra) 0
      & ppPoolDepositL .~ Coin 5
      & ppKeyDepositL .~ Coin 2
      & ppEMaxL .~ EpochInterval 5

instance EraTest ShelleyEra where
  validTxOut scripts txOut =
    case txOut ^. addrTxOutL of
      Addr _ (KeyHashObj _) _ -> True
      Addr _ (ScriptHashObj sh) _ -> Map.member sh scripts
      AddrBootstrap {} -> False
  zeroCostModels = emptyCostModels
  genPParams _ = shelleyGenPParams

instance ShelleyEraTest ShelleyEra
