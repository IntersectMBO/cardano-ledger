{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.Tx (spec) where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.PParams
import Cardano.Ledger.Api.Tx
import Cardano.Ledger.Binary
import Cardano.Ledger.Core (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable, hashKey, makeBootstrapWitness)
import Cardano.Ledger.SafeHash (extractHash, hashAnnotated)
import Cardano.Ledger.Val (Val ((<×>)))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Numeric.Natural
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (ByronKeyPair (..), KeyPair (..), mkWitnessVKey)

txSpec ::
  forall era.
  ( EraTx era
  , Arbitrary (Tx era)
  , Arbitrary (PParams era)
  , DSIGN (EraCrypto era) ~ Ed25519DSIGN
  , DSignable (EraCrypto era) (Hash.Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  ) =>
  Spec
txSpec = describe (eraName @era) $ do
  describe "estimateMinFeeTx" $ do
    prop "no Bootstrap" $ \(pp :: PParams era) (tx :: Tx era) keyPairsList ->
      let
        txBody = tx ^. bodyTxL
        txBodyHash = hashAnnotated txBody

        keyPairs = Map.fromList [(hashKey $ vKey kp, kp) | kp <- keyPairsList]
        wits = Set.fromList $ map (mkWitnessVKey txBodyHash) $ Map.elems keyPairs

        txSigned = tx & (witsTxL . addrTxWitsL <>~ wits)
       in
        estimateMinFeeTx pp tx (Map.size keyPairs) 0 0
          === (setMinFeeTx pp txSigned 0 ^. bodyTxL . feeTxBodyL)
    prop "with Bootstrap" $ \(pp :: PParams era) (tx :: Tx era) keyPairsList byronKeyPairsList ->
      let
        txBody = tx ^. bodyTxL
        txBodyHash = hashAnnotated txBody

        keyPairs = Map.fromList [(hashKey $ vKey kp, kp) | kp <- keyPairsList]
        wits = Set.fromList $ map (mkWitnessVKey txBodyHash) $ Map.elems keyPairs

        byronKeyPairs =
          Map.fromList
            [ ( bkpVerificationKey kp
              , (bkpSigningKey kp, Byron.mkAttributes $ attrs {Byron.aaVKDerivationPath = Nothing})
              )
            | (kp, attrs) <-
                byronKeyPairsList
            ]
        byronWits =
          Set.fromList $
            map (uncurry (makeBootstrapWitness (extractHash txBodyHash))) (Map.elems byronKeyPairs)

        txSigned =
          tx
            & (witsTxL . addrTxWitsL <>~ wits)
            & (witsTxL . bootAddrTxWitsL <>~ byronWits)

        -- Conversion to Natural is necessary to guard against negative numbers thus
        -- checking overestimation:
        serializeByronAttrs :: Byron.Attributes Byron.AddrAttributes -> Natural
        serializeByronAttrs = fromIntegral . BS.length . serialize' byronProtVer
        assumedAttrs =
          Byron.AddrAttributes
            { Byron.aaVKDerivationPath = Nothing
            , Byron.aaNetworkMagic = Byron.NetworkTestnet maxBound
            }
        -- This is the minimum amount by which over estimation can happen.
        overestimations =
          [ serializeByronAttrs (Byron.mkAttributes assumedAttrs) - serializeByronAttrs attrs
          | (_, attrs) <- Map.elems byronKeyPairs
          ]
       in
        -- Overestimating transaction size can lead to the overestimated fee affecting the
        -- size of the transaction, which in turn affects the overestimation. For this
        -- reason we can only check `>=`
        let
          overestimatedMinFeeA = toInteger (sum overestimations) <×> pp ^. ppMinFeeAL
          estimation = estimateMinFeeTx pp tx (Map.size keyPairs) (Map.size byronKeyPairs) 0
          actual = setMinFeeTx pp txSigned 0 ^. bodyTxL . feeTxBodyL
         in
          tabulate "Attrs overestimation in bytes" (map show overestimations) $
            estimation >= actual <> overestimatedMinFeeA

spec :: Spec
spec = do
  txSpec @Shelley
  txSpec @Allegra
  txSpec @Mary
  txSpec @Alonzo
  txSpec @Babbage
  txSpec @Conway
