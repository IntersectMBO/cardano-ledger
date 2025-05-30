{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the UTXO rule
module Test.Cardano.Ledger.Constrained.Conway.Utxo where

import Cardano.Ledger.Babbage.TxOut
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  Era (..),
  EraPParams (..),
  EraTx,
  EraTxAuxData (..),
  EraTxWits (..),
 )
import Cardano.Ledger.Conway.Governance (GovActionId, Proposals, gasDeposit, pPropsL)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.Tx (AlonzoTx)
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.Shelley.Rules (Identity, epochFromSlot, utxoEnvCertStateL)
import Constrained.API
import Control.DeepSeq (NFData)
import Control.Monad.Reader (runReader)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Common (Arbitrary (..), ToExpr, oneof)
import Test.Cardano.Ledger.Constrained.Conway.Gov (proposalsSpec)
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.Utils (testGlobals)

instance HasSimpleRep DepositPurpose

instance HasSpec DepositPurpose

witnessDepositPurpose ::
  forall era.
  Era era =>
  WitUniv era -> Specification DepositPurpose
witnessDepositPurpose univ = constrained $ \ [var|depPurpose|] ->
  (caseOn depPurpose)
    -- CredentialDeposit !(Credential 'Staking c)
    (branch $ \cred -> witness univ cred)
    -- PoolDeposit !(KeyHash 'StakePool c)
    (branch $ \keyhash -> witness univ keyhash)
    -- DRepDeposit !(Credential 'DRepRole c)
    (branch $ \drep -> witness univ drep)
    -- GovActionDeposit
    (branch $ \_ -> True)

data DepositPurpose
  = CredentialDeposit !(Credential 'Staking)
  | PoolDeposit !(KeyHash 'StakePool)
  | DRepDeposit !(Credential 'DRepRole)
  | GovActionDeposit !GovActionId
  deriving (Generic, Eq, Show, Ord)

instance Arbitrary DepositPurpose where
  arbitrary =
    oneof
      [ CredentialDeposit <$> arbitrary
      , PoolDeposit <$> arbitrary
      , DRepDeposit <$> arbitrary
      , GovActionDeposit <$> arbitrary
      ]

instance DecCBOR DepositPurpose where
  decCBOR =
    decode . Summands "DepositPurpose" $
      \case
        0 -> SumD CredentialDeposit <! From
        1 -> SumD PoolDeposit <! From
        2 -> SumD DRepDeposit <! From
        3 -> SumD GovActionDeposit <! From
        k -> Invalid k

instance EncCBOR DepositPurpose where
  encCBOR =
    encode . \case
      CredentialDeposit c -> Sum CredentialDeposit 0 !> To c
      PoolDeposit kh -> Sum PoolDeposit 1 !> To kh
      DRepDeposit c -> Sum DRepDeposit 2 !> To c
      GovActionDeposit gaid -> Sum GovActionDeposit 3 !> To gaid

instance NFData DepositPurpose

instance ToExpr DepositPurpose

utxoEnvSpec ::
  UtxoExecContext ConwayEra ->
  Specification (UtxoEnv ConwayEra)
utxoEnvSpec UtxoExecContext {..} =
  constrained $ \utxoEnv ->
    utxoEnv ==. lit uecUtxoEnv

utxoStateSpec ::
  UtxoExecContext ConwayEra ->
  UtxoEnv ConwayEra ->
  Specification (UTxOState ConwayEra)
utxoStateSpec UtxoExecContext {uecUTxO} UtxoEnv {ueSlot, ueCertState} =
  constrained $ \utxoState ->
    match utxoState $
      \utxosUtxo
       _utxosDeposited
       _utxosFees
       utxosGovState
       _utxosStakeDistr
       _utxosDonation ->
          [ assert $ utxosUtxo ==. lit uecUTxO
          , match utxosGovState $ \props _ constitution _ _ _ _ ->
              match constitution $ \_ policy ->
                satisfies props $ proposalsSpec (lit curEpoch) policy (lit ueCertState)
          ]
  where
    curEpoch = runReader (epochFromSlot ueSlot) testGlobals

data UtxoExecContext era = UtxoExecContext
  { uecTx :: !(AlonzoTx era)
  , uecUTxO :: !(UTxO era)
  , uecUtxoEnv :: !(UtxoEnv era)
  }
  deriving (Generic)

instance
  ( EraTx era
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  , EraCertState era
  ) =>
  NFData (UtxoExecContext era)

instance
  ( EraTx era
  , ToExpr (TxOut era)
  , ToExpr (TxBody era)
  , ToExpr (TxWits era)
  , ToExpr (TxAuxData era)
  , ToExpr (PParamsHKD Identity era)
  , EraCertState era
  , ToExpr (CertState era)
  ) =>
  ToExpr (UtxoExecContext era)

instance
  ( EraPParams era
  , EncCBOR (TxOut era)
  , EncCBOR (TxBody era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (TxWits era)
  , EraCertState era
  ) =>
  EncCBOR (UtxoExecContext era)
  where
  encCBOR x@(UtxoExecContext _ _ _) =
    let UtxoExecContext {..} = x
     in encode $
          Rec UtxoExecContext
            !> To uecTx
            !> To uecUTxO
            !> To uecUtxoEnv

instance CertState era ~ ConwayCertState era => Inject (UtxoExecContext era) (ConwayCertState era) where
  inject ctx = (uecUtxoEnv ctx) ^. utxoEnvCertStateL

utxoTxSpec ::
  HasSpec (AlonzoTx era) =>
  UtxoExecContext era ->
  Specification (AlonzoTx era)
utxoTxSpec UtxoExecContext {uecTx} =
  constrained $ \tx -> tx ==. lit uecTx

correctAddrAndWFCoin ::
  Term (TxOut ConwayEra) ->
  Pred
correctAddrAndWFCoin txOut =
  match txOut $ \addr v _ _ ->
    [ match v $ \c -> [0 <. c, c <=. fromIntegral (maxBound :: Word64)]
    , (caseOn addr)
        (branch $ \n _ _ -> n ==. lit Testnet)
        ( branch $ \bootstrapAddr ->
            match bootstrapAddr $ \_ nm _ ->
              (caseOn nm)
                (branch $ \_ -> False)
                (branch $ \_ -> True)
        )
    ]

depositsMap ::
  ConwayEraCertState era => CertState era -> Proposals era -> Map.Map DepositPurpose Coin
depositsMap certState props =
  Map.unions
    [ Map.mapKeys CredentialDeposit $
        Map.map
          (fromCompact . (^. depositAccountStateL))
          (certState ^. certDStateL . accountsL . accountsMapL)
    , Map.mapKeys PoolDeposit $ certState ^. certPStateL . psDepositsL
    , fmap drepDeposit . Map.mapKeys DRepDeposit $ certState ^. certVStateL . vsDRepsL
    , Map.fromList . fmap (bimap GovActionDeposit gasDeposit) $ OMap.assocList (props ^. pPropsL)
    ]
