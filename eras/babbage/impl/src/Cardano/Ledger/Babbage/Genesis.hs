{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Ledger.Babbage.Genesis
  ( BabbageGenesis (..),
    extendPPWithGenesis,
  )
where

import Cardano.Binary
import Cardano.Ledger.Babbage.Language (Language)
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Scripts
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Data.Coders
import Data.Functor.Identity
import Data.Map.Strict
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural

data BabbageGenesis = BabbageGenesis
  { coinsPerUTxOWord :: !Coin,
    costmdls :: !(Map Language CostModel),
    prices :: !Prices,
    maxTxExUnits :: !ExUnits,
    maxBlockExUnits :: !ExUnits,
    maxValSize :: !Natural,
    collateralPercentage :: !Natural,
    maxCollateralInputs :: !Natural
  }
  deriving (Eq, Generic, NoThunks)

-- | Given the missing pieces turn a Shelley.PParams' into an Params'
extendPPWithGenesis ::
  Shelley.PParams' Identity era1 ->
  BabbageGenesis ->
  PParams' Identity era2
extendPPWithGenesis
  pp
  BabbageGenesis
    { coinsPerUTxOWord,
      costmdls,
      prices,
      maxTxExUnits,
      maxBlockExUnits,
      maxValSize,
      collateralPercentage,
      maxCollateralInputs
    } =
    extendPP
      pp
      coinsPerUTxOWord
      costmdls
      prices
      maxTxExUnits
      maxBlockExUnits
      maxValSize
      collateralPercentage
      maxCollateralInputs

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance FromCBOR BabbageGenesis where
  fromCBOR =
    decode $
      RecD BabbageGenesis
        <! From
        <! D decodeCostModelMap
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance ToCBOR BabbageGenesis where
  toCBOR
    BabbageGenesis
      { coinsPerUTxOWord,
        costmdls,
        prices,
        maxTxExUnits,
        maxBlockExUnits,
        maxValSize,
        collateralPercentage,
        maxCollateralInputs
      } =
      encode $
        Rec BabbageGenesis
          !> To coinsPerUTxOWord
          !> To costmdls
          !> To prices
          !> To maxTxExUnits
          !> To maxBlockExUnits
          !> To maxValSize
          !> To collateralPercentage
          !> To maxCollateralInputs
