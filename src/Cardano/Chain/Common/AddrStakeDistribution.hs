{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Chain.Common.AddrStakeDistribution
  ( AddrStakeDistribution(..)
  , mkMultiKeyDistr
  , MultiKeyDistrError(..)
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..))
import Formatting (bprint, build, sformat)
import qualified Formatting.Buildable as B (Buildable(..))

import Cardano.Binary.Class (Bi(..), szCases)
import qualified Cardano.Binary.Class as Bi
import Cardano.Chain.Common.LovelacePortion
  (LovelacePortion(..), lovelacePortionDenominator)
import Cardano.Chain.Common.StakeholderId (StakeholderId, shortStakeholderF)


-- | Stake distribution associated with an address.
data AddrStakeDistribution
    = BootstrapEraDistr
    -- ^ Stake distribution for bootstrap era.
    | SingleKeyDistr !StakeholderId
    -- ^ Stake distribution stating that all stake should go to the given stakeholder.
    | UnsafeMultiKeyDistr !(Map StakeholderId LovelacePortion)
    -- ^ Stake distribution which gives stake to multiple
    -- stakeholders. 'LovelacePortion' is a portion of an output (output
    -- has a value, portion of this value is stake). The constructor
    -- is unsafe because there are some predicates which must hold:
    --
    -- • the sum of portions must be @maxBound@ (basically 1);
    -- • all portions must be positive;
    -- • there must be at least 2 items, because if there is only one item,
    -- 'SingleKeyDistr' can be used instead (which is smaller).
    deriving (Eq, Ord, Show, Generic)

instance B.Buildable AddrStakeDistribution where
    build = \case
        BootstrapEraDistr -> "Bootstrap era distribution"
        SingleKeyDistr id ->
            bprint ("Single key distribution (" . shortStakeholderF . ")") id
        UnsafeMultiKeyDistr distr ->
            bprint ("Multi key distribution: " . mapJson) distr

instance NFData AddrStakeDistribution

instance Bi AddrStakeDistribution where
  encode = \case
    BootstrapEraDistr         -> Bi.encodeListLen 0
    SingleKeyDistr      id    -> encode (0 :: Word8, id)
    UnsafeMultiKeyDistr distr -> encode (1 :: Word8, distr)

  decode = Bi.decodeListLen >>= \case
    0 -> pure BootstrapEraDistr
    2 -> decode @Word8 >>= \case
      0 -> SingleKeyDistr <$> decode
      1 -> toCborError . first (sformat build) . mkMultiKeyDistr =<< decode
      tag ->
        cborError
          $  "decode @AddrStakeDistribution: unexpected tag "
          <> sformat build tag
    len ->
      cborError
        $  "decode @AddrStakeDistribution: unexpected length "
        <> sformat build len

  encodedSizeExpr size _ = szCases
    [ Bi.Case "BoostrapEraDistr" 1
    , Bi.Case "SingleKeyDistr" $ size $ Proxy @(Word8, StakeholderId)
    , Bi.Case "UnsafeMultiKeyDistr"
    $ size
    $ Proxy @(Word8, Map StakeholderId LovelacePortion)
    ]

data MultiKeyDistrError
    = MkdMapIsEmpty
    | MkdMapIsSingleton
    | MkdNegativePortion
    | MkdSumNot1
    deriving (Show)

instance B.Buildable MultiKeyDistrError where
    build = mappend "mkMultiKeyDistr: " . \case
        MkdMapIsEmpty      -> "map is empty"
        MkdMapIsSingleton  -> "map's size is 1, use SingleKeyDistr"
        MkdNegativePortion -> "all portions must be positive"
        MkdSumNot1         -> "distributions' sum must be equal to 1"

-- | Safe constructor of multi-key distribution. It checks invariants
-- of this distribution and returns an error if something is violated.
mkMultiKeyDistr
  :: forall m
   . MonadError MultiKeyDistrError m
  => Map StakeholderId LovelacePortion
  -> m AddrStakeDistribution
mkMultiKeyDistr distrMap = UnsafeMultiKeyDistr distrMap <$ checkDistrMap
 where
  checkDistrMap :: m ()
  checkDistrMap = do
    not (null distrMap) `orThrowError` MkdMapIsEmpty
    (length distrMap /= 1) `orThrowError` MkdMapIsSingleton
    all ((> 0) . getLovelacePortion) distrMap
      `orThrowError` MkdNegativePortion
    let distrSum = sum $ map getLovelacePortion distrMap
    (distrSum == lovelacePortionDenominator) `orThrowError` MkdSumNot1
