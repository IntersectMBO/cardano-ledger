{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Chain.Genesis.Spec
  ( GenesisSpec(..)
  , mkGenesisSpec
  )
where

import Cardano.Prelude
import Prelude (String)

import Control.Monad.Except (MonadError(throwError))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.List (nub)
import qualified Data.Map.Strict as M

import Cardano.Chain.Common.SharedSeed (SharedSeed)
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import Cardano.Chain.Genesis.Delegation (GenesisDelegation(..))
import Cardano.Chain.Genesis.Initializer (GenesisInitializer(..))
import Cardano.Chain.Genesis.ProtocolConstants (GenesisProtocolConstants(..))
import Cardano.Chain.Update.BlockVersionData (BlockVersionData)


-- | Specification how to generate full genesis data.
data GenesisSpec = UnsafeGenesisSpec
  { gsAvvmDistr         :: !GenesisAvvmBalances
  -- ^ Genesis data describes avvm utxo.
  , gsFtsSeed           :: !SharedSeed
  -- ^ Seed for FTS for 0-th epoch.
  , gsHeavyDelegation   :: !GenesisDelegation
  -- ^ Genesis state of heavyweight delegation. Will be concatenated
  -- with genesis delegation for bootstrap stakeholders if
  -- 'tiUseHeavyDlg' is 'True'.
  , gsBlockVersionData  :: !BlockVersionData
  -- ^ Genesis 'BlockVersionData'.
  , gsProtocolConstants :: !GenesisProtocolConstants
  -- ^ Other constants which affect consensus.
  , gsInitializer       :: !GenesisInitializer
  -- ^ Other data which depend on genesis type.
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''GenesisSpec

-- | Safe constructor for 'GenesisSpec'. Throws error if something
-- goes wrong.
mkGenesisSpec
  :: GenesisAvvmBalances
  -> SharedSeed
  -> GenesisDelegation
  -> BlockVersionData
  -> GenesisProtocolConstants
  -> GenesisInitializer
  -> Either String GenesisSpec
mkGenesisSpec avvmDistr seed delega bvd pc specType = do
  let avvmKeys = M.keys $ getGenesisAvvmBalances avvmDistr
  unless (length (nub avvmKeys) == length avvmKeys)
    $ throwError "mkGenesisSpec: there are duplicates in avvm balances"
  -- All checks passed
  pure $ UnsafeGenesisSpec avvmDistr seed delega bvd pc specType
