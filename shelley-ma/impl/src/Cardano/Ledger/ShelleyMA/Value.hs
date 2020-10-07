-- | The Value module only exports the Value type and its operations 'insert', 'lookup', 'showValue'.
--   Of course Value is an instance of both Val(zero,isZero,(<+>),(<->),(<×>),coin,inject,...) and
--   Albelian(memtpy,(<>)), so one can use those operations as well.
module Cardano.Ledger.ShelleyMA.Value
  ( PolicyID (..),
    AssetID (..),
    Value,
    insert,
    lookup,
    showValue,
  )
where

import Cardano.Ledger.ShelleyMA.ValueInternal
  ( AssetID (..),
    PolicyID (..),
    Value,
    insert,
    lookup,
    showValue,
  )
import Prelude hiding (lookup)

-- ============================================================================
-- Multi Assests
--
-- A Value is a map from 'PolicyID's to a quantity of assets with this policy.
-- This map implements a finitely supported functions ovr PolicyId. A PolicyID
-- is not stored in the Map, then its quantity is assumed to be 0.
--
-- Operations on assets are usually implemented 'pointwise'. That is, we apply
-- the operation to the quantities for each asset in turn. So when we add two
-- 'Value's the resulting 'Value' has, for each asset, the sum of the quantities
-- of /that particular/ asset in the argument 'Value'. The effect of this is
-- that the assets in the 'Value' are "independent", and are operated on
-- separately.
--
-- We can think of 'Value' as a vector space whose dimensions are assets. At the
-- moment there is only a single asset type (Ada), so 'Value' contains
-- one-dimensional vectors. When asset-creating transactions are implemented,
-- this will change and the definition of 'Value' will change to a 'Map Asset
-- Int', effectively a vector with infinitely many dimensions whose non-zero
-- values are recorded in the map.
--
-- To create a value of 'Value', we need to specifiy an asset policy. This can
-- be done using 'Ledger.Ada.adaValueOf'. To get the ada dimension of 'Value' we
-- use 'Ledger.Ada.fromValue'. Plutus contract authors will be able to define
-- modules similar to 'Ledger.Ada' for their own assets.
-- ============================================================================
