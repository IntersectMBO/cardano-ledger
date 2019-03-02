{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Common.Gen
  ( genAddrAttributes
  , genAddress
  , genAddrType
  , genAddrSpendingData
  , genAddrStakeDistribution
  , genBlockCount
  , genCanonicalTxFeePolicy
  , genChainDifficulty
  , genCustomLovelace
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

genCanonicalTxFeePolicy :: Gen TxFeePolicy
genCanonicalTxFeePolicy = TxFeePolicyTxSizeLinear <$> genCanonicalTxSizeLinear

genCanonicalTxSizeLinear :: Gen TxSizeLinear
genCanonicalTxSizeLinear = TxSizeLinear <$> genLovelace' <*> genLovelace'
 where
  genLovelace' :: Gen Lovelace
  genLovelace' =
    mkLovelace
      <$> Gen.word64 (Range.constant 0 maxCanonicalLovelaceVal)
      >>= \case
            Right lovelace -> pure lovelace
            Left  err      -> panic $ sformat
              ("The impossible happened in genLovelace: " . build)
              err
  -- | Maximal possible value of `Lovelace` in Canonical JSON (JSNum !Int54)
  -- This should be (2^53 - 1) ~ 9e15, however in the Canonical ToJSON instance of
  -- `TxFeePolicy` this number is multiplied by 1e9 to keep compatibility with 'Nano'
  --  coefficients
  maxCanonicalLovelaceVal :: Word64
  maxCanonicalLovelaceVal = 9e6

genChainDifficulty :: Gen ChainDifficulty
genChainDifficulty = ChainDifficulty <$> genBlockCount

genCustomLovelace :: Word64 -> Gen Lovelace
genCustomLovelace size =
  mkLovelace <$> Gen.word64 (Range.linear 0 size) >>= \case
    Right lovelace -> pure lovelace
    Left  err      -> panic $ sformat build err

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
genTxFeePolicy = TxFeePolicyTxSizeLinear <$> genTxSizeLinear

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genLovelace <*> genLovelace
