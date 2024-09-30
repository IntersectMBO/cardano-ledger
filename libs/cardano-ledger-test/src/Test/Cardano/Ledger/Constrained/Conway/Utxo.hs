{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the UTXO rule
module Test.Cardano.Ledger.Constrained.Conway.Utxo where

import Cardano.Ledger.Babbage.TxOut
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  EraPParams,
  EraTx (..),
  EraTxAuxData (..),
  EraTxBody (..),
  EraTxWits (..),
  PParams,
  ppMaxCollateralInputsL,
 )
import Cardano.Ledger.Conway.Governance (GovActionId)
import Cardano.Ledger.Conway.Tx (AlonzoTx)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.UTxO
import Constrained
import Control.DeepSeq (NFData)
import Data.Foldable
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro
import Test.Cardano.Ledger.Common (Arbitrary (..), oneof)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams
import Test.Cardano.Ledger.Constrained.Conway.SimplePParams (maxTxSize_, protocolVersion_)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)

utxoEnvSpec :: IsConwayUniv fn => Specification fn (UtxoEnv (ConwayEra StandardCrypto))
utxoEnvSpec =
  constrained $ \utxoEnv ->
    match utxoEnv $
      \_ueSlot
       uePParams
       _ueCertState ->
          [ satisfies uePParams pparamsSpec
          , match uePParams $ \spp ->
              -- NOTE cpp has type (Term fn (SimplePParams era))
              -- NOTE: this is for testing only! We should figure out a nicer way
              -- of splitting generation and checking constraints here!
              [ assert $ protocolVersion_ spp ==. lit (ProtVer (natVersion @10) 0)
              , assert $ lit 3000 ==. maxTxSize_ spp
              ]
          ]

utxoStateSpec ::
  IsConwayUniv fn =>
  UtxoEnv (ConwayEra StandardCrypto) ->
  Specification fn (UTxOState (ConwayEra StandardCrypto))
utxoStateSpec _env =
  constrained $ \utxoState ->
    match utxoState $
      \utxosUtxo
       _utxosDeposited
       _utxosFees
       _utxosGovState
       _utxosStakeDistr
       _utxosDonation ->
          [ assert $ utxosUtxo /=. lit mempty
          , match utxosUtxo $ \utxoMap ->
              forAll (rng_ utxoMap) correctAddrAndWFCoin
          ]

utxoTxSpec ::
  IsConwayUniv fn =>
  UtxoEnv (ConwayEra StandardCrypto) ->
  UTxOState (ConwayEra StandardCrypto) ->
  Specification fn (Tx (ConwayEra StandardCrypto))
utxoTxSpec env st =
  constrained $ \tx ->
    match tx $ \bdy _wits isValid _auxData ->
      [ match isValid assert
      , match bdy $
          \ctbSpendInputs
           ctbCollateralInputs
           _ctbReferenceInputs
           ctbOutputs
           ctbCollateralReturn
           _ctbTotalCollateral
           _ctbCerts
           ctbWithdrawals
           ctbTxfee
           ctbVldt
           _ctbReqSignerHashes
           _ctbMint
           _ctbScriptIntegrityHash
           _ctbAdHash
           ctbTxNetworkId
           _ctbVotingProcedures
           ctbProposalProcedures
           _ctbCurrentTreasuryValue
           ctbTreasuryDonation ->
              [ assert $ ctbSpendInputs /=. lit mempty
              , assert $ ctbSpendInputs `subset_` lit (Map.keysSet $ unUTxO $ utxosUtxo st)
              , match ctbWithdrawals $ \withdrawalMap ->
                  forAll' (dom_ withdrawalMap) $ \net _ ->
                    net ==. lit Testnet
              , -- TODO: we need to do this for collateral as well?
                match ctbProposalProcedures $ \proposalsList ->
                  match ctbOutputs $ \outputList ->
                    [ (reify ctbSpendInputs)
                        ( \actualInputs ->
                            fold
                              [ c
                              | i <- Set.toList actualInputs
                              , BabbageTxOut _ (MaryValue c _) _ _ <- maybeToList . txinLookup i . utxosUtxo $ st
                              ]
                        )
                        $ \totalValueConsumed ->
                          [ let outputSum =
                                  foldMap_
                                    (maryValueCoin_ . txOutVal_ . sizedValue_)
                                    outputList
                                depositSum =
                                  foldMap_
                                    pProcDeposit_
                                    proposalsList
                             in outputSum + depositSum + ctbTxfee + ctbTreasuryDonation ==. totalValueConsumed
                          ]
                    , forAll outputList (flip onSized correctAddrAndWFCoin)
                    ]
              , match ctbVldt $ \before after ->
                  [ onJust' before (<=. lit (ueSlot env))
                  , onJust' after (lit (ueSlot env) <.)
                  ]
              , onJust' ctbTxNetworkId (==. lit Testnet)
              , onJust' ctbCollateralReturn $ flip onSized correctAddrAndWFCoin
              , assert $ size_ ctbCollateralInputs <=. lit (fromIntegral $ uePParams env ^. ppMaxCollateralInputsL)
              ]
      ]

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

data UtxoExecContext era = UtxoExecContext
  { uecTx :: !(AlonzoTx era)
  , uecUTxO :: !(UTxO era)
  , uecSlotNo :: !SlotNo
  , uecPParams :: !(PParams era)
  , uecDeposits :: !(Map.Map (DepositPurpose StandardCrypto) Coin)
  }
  deriving (Generic)

instance Inject (UtxoExecContext era) (Map.Map (DepositPurpose StandardCrypto) Coin) where
  inject = uecDeposits

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
  , ToExpr (PParams era)
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
  encCBOR x@(UtxoExecContext _ _ _ _ _) =
    let UtxoExecContext {..} = x
     in encode $
          Rec UtxoExecContext
            !> To uecTx
            !> To uecUTxO
            !> To uecSlotNo
            !> To uecPParams
