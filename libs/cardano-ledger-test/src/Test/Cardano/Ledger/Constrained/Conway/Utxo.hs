{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the UTXO rule
module Test.Cardano.Ledger.Constrained.Conway.Utxo where

import Cardano.Ledger.Babbage.TxOut
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (EraTx (..), ppMaxCollateralInputsL)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.UTxO
import Constrained
import Data.Foldable
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Word
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)

utxoEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (UtxoEnv (ConwayEra StandardCrypto))
utxoEnvSpec =
  constrained $ \utxoEnv ->
    match utxoEnv $
      \_ueSlot
       uePParams
       _ueCertState ->
          [ satisfies uePParams pparamsSpec
          , -- NOTE: this is for testing only! We should figure out a nicer way
            -- of splitting generation and checking constraints here!
            match uePParams $ \cpp -> [assert $ (lit 3000) <=. maxTxSize_ cpp]
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
                              [ c | i <- Set.toList actualInputs, BabbageTxOut _ (MaryValue c _) _ _ <- maybeToList . txinLookup i . utxosUtxo $ st
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
