{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Allegra.Examples.Consensus where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.AuxiliaryData
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Era
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.AuxiliaryData
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Ledger.ShelleyMA.TxBody
import Cardano.Slotting.Slot
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import Test.Cardano.Ledger.Shelley.Examples.Consensus
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils hiding (mkVRFKeyPair)

type StandardAllegra = AllegraEra StandardCrypto

-- | ShelleyLedgerExamples for Allegra era
ledgerExamplesAllegra :: ShelleyLedgerExamples StandardAllegra
ledgerExamplesAllegra =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @StandardAllegra))
    id
    exampleCoin
    exampleTxBodyAllegra
    exampleAuxiliaryDataMA
    ()

exampleTxBodyAllegra :: Cardano.Ledger.ShelleyMA.TxBody.TxBody StandardAllegra
exampleTxBodyAllegra = exampleTxBodyMA exampleCoin

exampleTxBodyMA ::
  forall era.
  ( ShelleyBasedEra' era,
    PParamsDelta era ~ PParams' StrictMaybe era
  ) =>
  Cardano.Ledger.Core.Value era ->
  Cardano.Ledger.ShelleyMA.TxBody.TxBody era
exampleTxBodyMA value =
  Cardano.Ledger.ShelleyMA.TxBody.TxBody
    exampleTxIns
    ( StrictSeq.fromList
        [ TxOut (mkAddr (examplePayKey, exampleStakeKey)) value
        ]
    )
    exampleCerts
    exampleWithdrawals
    (Coin 3)
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4)))
    (SJust (Update exampleProposedPPUpdates (EpochNo 0)))
    (SJust auxiliaryDataHash)
    value
  where
    -- Dummy hash to decouple from the auxiliary data in 'exampleTx'.
    auxiliaryDataHash :: AuxiliaryDataHash (Cardano.Ledger.Era.Crypto era)
    auxiliaryDataHash =
      AuxiliaryDataHash $ mkDummySafeHash (Proxy @(Cardano.Ledger.Era.Crypto era)) 30

exampleAuxiliaryDataMA :: Cardano.Ledger.Crypto.Crypto c => Cardano.Ledger.ShelleyMA.AuxiliaryData.AuxiliaryData (ShelleyMAEra ma c)
exampleAuxiliaryDataMA =
  AuxiliaryData
    exampleMetadataMap
    (StrictSeq.fromList [exampleScriptMA])

exampleScriptMA :: Cardano.Ledger.Crypto.Crypto c => Script (ShelleyMAEra ma c)
exampleScriptMA =
  Cardano.Ledger.ShelleyMA.Timelocks.RequireMOf 2 $
    StrictSeq.fromList
      [ Cardano.Ledger.ShelleyMA.Timelocks.RequireAllOf $
          StrictSeq.fromList
            [ RequireTimeStart (SlotNo 0),
              RequireTimeExpire (SlotNo 9)
            ],
        Cardano.Ledger.ShelleyMA.Timelocks.RequireAnyOf $
          StrictSeq.fromList
            [ Cardano.Ledger.ShelleyMA.Timelocks.RequireSignature (mkKeyHash 0),
              Cardano.Ledger.ShelleyMA.Timelocks.RequireSignature (mkKeyHash 1)
            ],
        Cardano.Ledger.ShelleyMA.Timelocks.RequireSignature (mkKeyHash 100)
      ]
