{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Shelley.Spec.Ledger.Genesis.Example
  ( exampleShelleyGenesis,
  )
where

import Cardano.Crypto.ProtocolMagic
import Cardano.Slotting.Slot (EpochSize (..))
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Shelley.Spec.Ledger.BaseTypes (Network (..), truncateUnitInterval)
import Shelley.Spec.Ledger.Genesis (ShelleyGenesis (..), emptyGenesisStaking)
import Shelley.Spec.Ledger.PParams (PParams' (..), emptyPParams)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes hiding (Addr, KeyHash, VerKeyVRF)

exampleShelleyGenesis :: ShelleyGenesis (ConcreteCrypto h)
exampleShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer),
      sgNetworkMagic = 4036000900,
      sgNetworkId = Testnet,
      sgProtocolMagicId = ProtocolMagicId 838299499,
      sgActiveSlotsCoeff = 6.259,
      sgSecurityParam = 120842,
      sgEpochLength = EpochSize 1215,
      sgSlotsPerKESPeriod = 8541,
      sgMaxKESEvolutions = 28899,
      sgSlotLength = 8,
      sgUpdateQuorum = 16991,
      sgMaxLovelaceSupply = 71,
      sgProtocolParams =
        emptyPParams
          { _d = truncateUnitInterval . realToFrac $ (1.9e-2 :: Double),
            _maxBBSize = 239857,
            _maxBHSize = 217569
          },
      sgGenDelegs = Map.empty,
      sgInitialFunds = Map.empty,
      sgStaking = emptyGenesisStaking
    }
