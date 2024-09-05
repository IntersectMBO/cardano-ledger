{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , PParams
  , EraPParams
  )
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Gov (proposalsSpec)
import Cardano.Ledger.Shelley.Rules (epochFromSlot)
import Control.Monad.Reader (runReader)
import Test.Cardano.Ledger.Core.Utils (testGlobals)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Common (ToExpr)
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Encode(..), encode, (!>))
import Cardano.Ledger.Conway.Tx (AlonzoTx)

utxoEnvSpec ::
  IsConwayUniv fn =>
  UtxoExecContext Conway ->
  Specification fn (UtxoEnv (ConwayEra StandardCrypto))
utxoEnvSpec UtxoExecContext {..} =
  constrained $ \utxoEnv ->
    match utxoEnv $
      \ueSlot
       uePParams
       _ueCertState ->
          [ assert $ uePParams ==. lit uecPParams
          , assert $ ueSlot ==. lit uecSlotNo
          ]

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
  , uecSlotNo :: !SlotNo
  , uecPParams :: !(PParams era)
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
  encCBOR x@(UtxoExecContext _ _ _ _) =
    let  UtxoExecContext {..} = x
     in encode $ Rec UtxoExecContext
      !> To uecTx
      !> To uecUTxO
      !> To uecSlotNo
      !> To uecPParams

utxoTxSpec ::
  ( IsConwayUniv fn
  , HasSpec fn (AlonzoTx era)
  ) =>
  UtxoExecContext era ->
  Specification fn (AlonzoTx era)
utxoTxSpec UtxoExecContext {uecTx} =
  constrained $ \tx -> tx ==. lit uecTx

-- utxoTxSpec ::
--   IsConwayUniv fn =>
--   UtxoExecContext Conway ->
--   UtxoEnv (ConwayEra StandardCrypto) ->
--   UTxOState (ConwayEra StandardCrypto) ->
--   Specification fn (Tx (ConwayEra StandardCrypto))
-- utxoTxSpec UtxoExecContext {..} env UTxOState {utxosUtxo} =
--   constrained $ \[var|tx|] ->
--     match tx $ \[var|bdy|] _wits [var|isValid|] [var|auxData|] ->
--       let
--         outputSum = sum $ view coinTxOutL . sizedValue <$> uecOutputs
--         inputSum = sum $ Set.map (view coinTxOutL . fromJust . (`txinLookup` utxosUtxo)) uecInputs
--       in
--       [ match isValid assert
--       , match bdy $
--           \[var|ctbSpendInputs|]
--            [var|ctbCollateralInputs|]
--            [var|_ctbReferenceInputs|]
--            [var|ctbOutputs|]
--            [var|ctbCollateralReturn|]
--            _ctbTotalCollateral
--            [var|_ctbCerts|]
--            [var|ctbWithdrawals|]
--            [var|ctbTxfee|]
--            [var|ctbVldt|]
--            [var|_ctbReqSignerHashes|]
--            _ctbMint
--            _ctbScriptIntegrityHash
--            _ctbAdHash
--            [var|ctbTxNetworkId|]
--            [var|_ctbVotingProcedures|]
--            [var|ctbProposalProcedures|]
--            _ctbCurrentTreasuryValue
--            [var|ctbTreasuryDonation|] ->
--               [ match ctbWithdrawals $ \[var|withdrawalMap|] ->
--                   forAll' (dom_ withdrawalMap) $ \[var|net|] _ ->
--                     net ==. lit Testnet
--               , -- TODO: we need to do this for collateral as well?
--                 assert $ lit (uecDepositsSum + outputSum) + ctbTxfee + ctbTreasuryDonation ==. lit inputSum
--               -- , forAll outputList (`onSized` correctAddrAndWFCoin)
--               , assert $ ctbSpendInputs ==. lit uecInputs
--               , match ctbProposalProcedures $ \x ->
--                   lit uecDepositsSum ==. foldMap_ pProcDeposit_ x
--               , match ctbOutputs $ \x ->
--                   lit outputSum ==. foldMap_ (maryValueCoin_ . txOutVal_ . sizedValue_) x
--               , match ctbVldt $ \[var|before|] [var|after|] ->
--                   [ onJust' before (<=. lit (ueSlot env))
--                   , onJust' after (lit (ueSlot env) <.)
--                   ]
--               , onJust' ctbTxNetworkId (==. lit Testnet)
--               , onJust' ctbCollateralReturn $ flip onSized correctAddrAndWFCoin
--               , assert $ ctbOutputs ==. lit uecOutputs
--               , assert $ ctbSpendInputs ==. lit uecInputs
--               , assert $ size_ ctbCollateralInputs <=. lit (fromIntegral $ uePParams env ^. ppMaxCollateralInputsL)
--                 -- TODO why does auxData take forever to generate?
--               , assert $ auxData ==. lit SNothing
--               ]
--       ]

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
