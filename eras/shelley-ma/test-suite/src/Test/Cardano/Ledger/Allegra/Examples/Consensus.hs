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
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate, Update (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxOut (..))
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

type StandardAllegra = AllegraEra CC.StandardCrypto

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

exampleTxBodyAllegra :: MATxBody StandardAllegra
exampleTxBodyAllegra = exampleTxBodyMA exampleCoin

exampleTxBodyMA ::
  forall era.
  ( ShelleyMAEraTxBody era,
    ShelleyBasedEra' era,
    PParamsUpdate era ~ ShelleyPParamsUpdate era
  ) =>
  Value era ->
  MATxBody era
exampleTxBodyMA value =
  MATxBody
    exampleTxIns
    ( StrictSeq.fromList
        [ ShelleyTxOut (mkAddr (examplePayKey, exampleStakeKey)) value
        ]
    )
    exampleCerts
    exampleWithdrawals
    (Coin 3)
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4)))
    (SJust (Update exampleProposedPPUpdates (EpochNo 0)))
    (SJust auxiliaryDataHash)
    mempty
  where
    -- Dummy hash to decouple from the auxiliary data in 'exampleTx'.
    auxiliaryDataHash :: AuxiliaryDataHash (Crypto era)
    auxiliaryDataHash =
      AuxiliaryDataHash $ mkDummySafeHash (Proxy @(Crypto era)) 30

exampleAuxiliaryDataMA :: CC.Crypto c => MAAuxiliaryData (ShelleyMAEra ma c)
exampleAuxiliaryDataMA =
  MAAuxiliaryData
    exampleMetadataMap
    (StrictSeq.fromList [exampleScriptMA])

exampleScriptMA :: CC.Crypto c => Script (ShelleyMAEra ma c)
exampleScriptMA =
  RequireMOf 2 $
    StrictSeq.fromList
      [ RequireAllOf $
          StrictSeq.fromList
            [ RequireTimeStart (SlotNo 0),
              RequireTimeExpire (SlotNo 9)
            ],
        RequireAnyOf $
          StrictSeq.fromList
            [ RequireSignature (mkKeyHash 0),
              RequireSignature (mkKeyHash 1)
            ],
        RequireSignature (mkKeyHash 100)
      ]
