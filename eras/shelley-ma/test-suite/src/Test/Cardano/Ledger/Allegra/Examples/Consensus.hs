{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Allegra.Examples.Consensus where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.AuxiliaryData
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.PParams (Update (..))
import Cardano.Protocol.HeaderCrypto
import Cardano.Slotting.Slot
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Shelley.Examples.Consensus

-- | ShelleyLedgerExamples for Allegra era
ledgerExamplesAllegra :: ShelleyLedgerExamples Allegra StandardCrypto
ledgerExamplesAllegra =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @Allegra))
    id
    exampleCoin
    (exampleAllegraTxBody (Proxy @StandardCrypto) exampleCoin)
    exampleAllegraTxAuxData
    ()

exampleAllegraTxBody ::
  forall era hc.
  ( AllegraEraTxBody era
  , ProtVerAtMost era 8
  , HeaderCrypto hc
  ) =>
  Proxy hc ->
  Value era ->
  TxBody era
exampleAllegraTxBody p value =
  mkBasicTxBody
    & inputsTxBodyL .~ exampleTxIns
    & outputsTxBodyL
      .~ StrictSeq.singleton (mkBasicTxOut (mkAddr (examplePayKey, exampleStakeKey)) value)
    & certsTxBodyL .~ (exampleCerts p)
    & withdrawalsTxBodyL .~ (exampleWithdrawals p)
    & feeTxBodyL .~ Coin 3
    & vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))
    & updateTxBodyL .~ SJust (Update exampleProposedPPUpdates (EpochNo 0))
    & auxDataHashTxBodyL .~ SJust auxiliaryDataHash
  where
    -- Dummy hash to decouple from the auxiliary data in 'exampleTx'.
    auxiliaryDataHash :: AuxiliaryDataHash (EraCrypto era)
    auxiliaryDataHash =
      AuxiliaryDataHash $ mkDummySafeHash (Proxy @(EraCrypto era)) 30

exampleTimelock :: Era era => Timelock era
exampleTimelock =
  RequireMOf 2 $
    StrictSeq.fromList
      [ RequireAllOf $
          StrictSeq.fromList
            [ RequireTimeStart (SlotNo 0)
            , RequireTimeExpire (SlotNo 9)
            ]
      , RequireAnyOf $
          StrictSeq.fromList
            [ RequireSignature (mkKeyHash 0)
            , RequireSignature (mkKeyHash 1)
            ]
      , RequireSignature (mkKeyHash 100)
      ]

exampleAllegraTxAuxData :: Era era => AllegraTxAuxData era
exampleAllegraTxAuxData = AllegraTxAuxData exampleAuxDataMap (StrictSeq.fromList [exampleTimelock])
