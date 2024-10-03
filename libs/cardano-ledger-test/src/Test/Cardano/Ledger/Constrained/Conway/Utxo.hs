{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the UTXO rule
module Test.Cardano.Ledger.Constrained.Conway.Utxo where

import Cardano.Ledger.Babbage.TxOut
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley.API.Types
import Data.Word

import Constrained

import Cardano.Ledger.Conway (ConwayEra, Conway)
import Cardano.Ledger.Conway.Core
  ( EraTxWits (..)
  , EraTxAuxData (..)
  , EraTx
  , EraTxBody (..)
  , EraPParams (..), Era (..)
  )
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Cardano.Ledger.Crypto (StandardCrypto, Crypto)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Gov (proposalsSpec)
import Cardano.Ledger.Shelley.Rules (epochFromSlot, Identity)
import Control.Monad.Reader (runReader)
import Test.Cardano.Ledger.Core.Utils (testGlobals)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Common (ToExpr, Arbitrary (..), oneof)
import Cardano.Ledger.Binary (EncCBOR (..), DecCBOR (..))
import Cardano.Ledger.Binary.Coders (Encode(..), encode, (!>), decode, Decode (..), (<!))
import Cardano.Ledger.Conway.Tx (AlonzoTx)
import Cardano.Ledger.Conway.Governance (GovActionId, gasDeposit, pPropsL, Proposals)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Cardano.Ledger.UMap (depositMap)
import Cardano.Ledger.CertState (certDStateL, dsUnifiedL, certPStateL, psDepositsL, DRepState (..), vsDRepsL, certVStateL)
import Data.Bifunctor (Bifunctor(..))
import qualified Data.OMap.Strict as OMap

data DepositPurpose c
  = CredentialDeposit !(Credential 'Staking c)
  | PoolDeposit !(KeyHash 'StakePool c)
  | DRepDeposit !(Credential 'DRepRole c)
  | GovActionDeposit !(GovActionId c)
  deriving (Generic, Eq, Show, Ord)

instance Crypto c => Arbitrary (DepositPurpose c) where
  arbitrary =
    oneof
      [ CredentialDeposit <$> arbitrary
      , PoolDeposit <$> arbitrary
      , DRepDeposit <$> arbitrary
      , GovActionDeposit <$> arbitrary
      ]

instance Crypto c => DecCBOR (DepositPurpose c) where
  decCBOR =
    decode . Summands "DepositPurpose" $
      \case
        0 -> SumD CredentialDeposit <! From
        1 -> SumD PoolDeposit <! From
        2 -> SumD DRepDeposit <! From
        3 -> SumD GovActionDeposit <! From
        k -> Invalid k

instance Crypto c => EncCBOR (DepositPurpose c) where
  encCBOR =
    encode . \case
      CredentialDeposit c -> Sum CredentialDeposit 0 !> To c
      PoolDeposit kh -> Sum PoolDeposit 1 !> To kh
      DRepDeposit c -> Sum DRepDeposit 2 !> To c
      GovActionDeposit gaid -> Sum GovActionDeposit 3 !> To gaid

instance Crypto c => NFData (DepositPurpose c)

instance ToExpr (DepositPurpose c)

utxoEnvSpec ::
  IsConwayUniv fn =>
  UtxoExecContext Conway ->
  Specification fn (UtxoEnv (ConwayEra StandardCrypto))
utxoEnvSpec UtxoExecContext {..} =
  constrained $ \utxoEnv ->
    utxoEnv ==. lit uecUtxoEnv

utxoStateSpec ::
  IsConwayUniv fn =>
  UtxoExecContext (ConwayEra StandardCrypto) ->
  UtxoEnv (ConwayEra StandardCrypto) ->
  Specification fn (UTxOState (ConwayEra StandardCrypto))
utxoStateSpec UtxoExecContext {uecUTxO} UtxoEnv {ueSlot} =
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
                satisfies props $ proposalsSpec (lit curEpoch) policy
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
  ) =>
  NFData (UtxoExecContext era)

instance
  ( EraTx era
  , ToExpr (TxOut era)
  , ToExpr (TxBody era)
  , ToExpr (TxWits era)
  , ToExpr (TxAuxData era)
  , ToExpr (PParamsHKD Identity era)
  ) =>
  ToExpr (UtxoExecContext era)

instance
  ( EraPParams era
  , EncCBOR (TxOut era)
  , EncCBOR (TxBody era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (TxWits era)
  ) =>
  EncCBOR (UtxoExecContext era)
  where
  encCBOR x@(UtxoExecContext _ _ _) =
    let  UtxoExecContext {..} = x
     in encode $ Rec UtxoExecContext
      !> To uecTx
      !> To uecUTxO
      !> To uecUtxoEnv

utxoTxSpec ::
  ( IsConwayUniv fn
  , HasSpec fn (AlonzoTx era)
  ) =>
  UtxoExecContext era ->
  Specification fn (AlonzoTx era)
utxoTxSpec UtxoExecContext {uecTx} =
  constrained $ \tx -> tx ==. lit uecTx

correctAddrAndWFCoin ::
  IsConwayUniv fn =>
  Term fn (TxOut (ConwayEra StandardCrypto)) ->
  Pred fn
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

depositsMap :: CertState era -> Proposals era -> Map.Map (DepositPurpose (EraCrypto era)) Coin
depositsMap certState props =
  Map.unions
    [ Map.mapKeys CredentialDeposit $ depositMap (certState ^. certDStateL . dsUnifiedL)
    , Map.mapKeys PoolDeposit $ certState ^. certPStateL . psDepositsL
    , fmap drepDeposit . Map.mapKeys DRepDeposit $ certState ^. certVStateL . vsDRepsL
    , Map.fromList . fmap (bimap GovActionDeposit gasDeposit) $ OMap.assocList (props ^. pPropsL)
    ]
