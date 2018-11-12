{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Common.Gen
  ( genAddrAttributes
  , genAddress
  , genAddrType
  , genAddrSpendingData
  , genAddrStakeDistribution
  , genBlockCount
  , genChainDifficulty
  , genCoeff
  , genLovelace
  , genLovelacePortion
  , genMerkleRoot
  , genMerkleTree
  , genScript
  , genScriptVersion
  , genStakeholderId
  , genTxFeePolicy
  , genTxSizeLinear
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Fixed (Fixed(..))
import qualified Data.Map.Strict as Map
import Formatting (build, sformat)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Binary.Class (Bi)
import Cardano.Chain.Common
  ( AddrAttributes(..)
  , AddrSpendingData(..)
  , AddrStakeDistribution(..)
  , AddrType(..)
  , Address(..)
  , BlockCount(..)
  , ChainDifficulty(..)
  , Coeff(..)
  , Lovelace
  , LovelacePortion(..)
  , MerkleRoot(..)
  , MerkleTree
  , Script(..)
  , ScriptVersion
  , StakeholderId
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  , lovelacePortionDenominator
  , makeAddress
  , maxLovelaceVal
  , mkLovelace
  , mkMerkleTree
  , mkStakeholderId
  , mtRoot
  )

import Test.Cardano.Crypto.Gen
  (genHDAddressPayload, genPublicKey, genRedeemPublicKey)


genAddrAttributes :: Gen AddrAttributes
genAddrAttributes = AddrAttributes <$> hap <*> genAddrStakeDistribution
  where hap = Gen.maybe genHDAddressPayload

genAddress :: Gen Address
genAddress = makeAddress <$> genAddrSpendingData <*> genAddrAttributes

genAddrType :: Gen AddrType
genAddrType = Gen.choice
  [ pure ATPubKey
  , pure ATScript
  , pure ATRedeem
      -- Values 0,1,2 are reserved, as they are used to tag
      -- the above 3 constructors ------------+
      --                                      |
  , ATUnknown <$> Gen.word8 (Range.constant 3 maxBound)
  ]

genAddrSpendingData :: Gen AddrSpendingData
genAddrSpendingData = Gen.choice gens
 where
  gens =
    [ PubKeyASD <$> genPublicKey
    , ScriptASD <$> genScript
    , RedeemASD <$> genRedeemPublicKey
         -- Values 0,1,2 are reserved, as they are used to tag
         -- the above 3 constructors ---------------+
         --                                         |
    , UnknownASD <$> Gen.word8 (Range.constant 3 maxBound) <*> gen32Bytes
    ]

genAddrStakeDistribution :: Gen AddrStakeDistribution
genAddrStakeDistribution = Gen.choice
  [ pure BootstrapEraDistr
  , SingleKeyDistr <$> genStakeholderId
  , UnsafeMultiKeyDistr <$> genMultiKeyDistr
  ]
 where
    -- Lifted from `Pos.Arbitrary.Core`. There are very particular constraints
    -- on the AddrStakeDistribution, which are mixed into encoding/decoding.
  genMultiKeyDistr :: Gen (Map StakeholderId LovelacePortion)
  -- We don't want to generate too much, hence 'scale'.
  genMultiKeyDistr = Gen.scale (`mod` 16) $ do
    holder0     <- genStakeholderId
    holder1     <- Gen.filter (/= holder0) genStakeholderId
    moreHolders <- Gen.list (Range.linear 0 100) genStakeholderId
    -- Must be at least 2 non-repeating stakeholders.
    let holders = ordNub (holder0 : holder1 : moreHolders)
    portions <- genPortions (length holders) []
    return $ Map.fromList $ holders `zip` portions
  genPortions :: Int -> [LovelacePortion] -> Gen [LovelacePortion]
  genPortions 0 res = pure res
  genPortions n res = do
    let
      limit =
        foldl' (-) lovelacePortionDenominator $ map getLovelacePortion res
    case (n, limit) of
        -- Limit is exhausted, can't create more.
      (_, 0) -> return res
      -- The last portion, we must ensure the sum is correct.
      (1, _) -> return (LovelacePortion limit : res)
      -- We intentionally don't generate 'limit', because we
      -- want to generate at least 2 portions.  However, if
      -- 'limit' is 1, we will generate 1, because we must
      -- have already generated one portion.
      _      -> do
        portion <- LovelacePortion
          <$> Gen.word64 (Range.linear 1 (max 1 (limit - 1)))
        genPortions (n - 1) (portion : res)

genBlockCount :: Gen BlockCount
genBlockCount = BlockCount <$> Gen.word64 Range.constantBounded

genChainDifficulty :: Gen ChainDifficulty
genChainDifficulty = ChainDifficulty <$> genBlockCount

genCoeff :: Gen Coeff
genCoeff = do
    -- A `Coeff` wraps a Nano-precision integral value, which corresponds to a
    -- number of "Lovelace" (10^6 Lovelace == 1 ADA). The `Coeff` values used
    -- in Cardano correspond to less than 1 ADA.
  let e = 9 + 6 :: Integer
  integer <- Gen.integral (Range.constant 0 (10 ^ e))
  pure $ Coeff (MkFixed integer)

genLovelace :: Gen Lovelace
genLovelace =
  mkLovelace <$> Gen.word64 (Range.constant 0 maxLovelaceVal) >>= \case
    Right lovelace -> pure lovelace
    Left err ->
      panic $ sformat ("The impossible happened in genLovelace: " . build) err

genLovelacePortion :: Gen LovelacePortion
genLovelacePortion =
  LovelacePortion <$> Gen.word64 (Range.constant 0 lovelacePortionDenominator)

-- slow
genMerkleTree :: Bi a => Gen a -> Gen (MerkleTree a)
genMerkleTree genA = mkMerkleTree <$> Gen.list (Range.linear 0 10) genA

-- slow
genMerkleRoot :: Bi a => Gen a -> Gen (MerkleRoot a)
genMerkleRoot genA = mtRoot <$> genMerkleTree genA

genScript :: Gen Script
genScript = Script <$> genScriptVersion <*> gen32Bytes

genScriptVersion :: Gen ScriptVersion
genScriptVersion = Gen.word16 Range.constantBounded

genStakeholderId :: Gen StakeholderId
genStakeholderId = mkStakeholderId <$> genPublicKey

genTxFeePolicy :: Gen TxFeePolicy
genTxFeePolicy = Gen.choice
  [ TxFeePolicyTxSizeLinear <$> genTxSizeLinear
  , TxFeePolicyUnknown <$> genUnknownPolicy <*> genUTF8Byte
  ]
 where
    -- 0 is a reserved policy, so we go from 1 to max.
    -- The Bi instance decoder for TxFeePolicy consolidates the
    -- tag and the policy number, so a 0 policy in TxFeePolicyUnknown
    -- causes a decoder error.
  genUnknownPolicy :: Gen Word8
  genUnknownPolicy = Gen.word8 (Range.constant 1 maxBound)

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genCoeff <*> genCoeff
