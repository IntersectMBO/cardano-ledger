{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Common.Gen
  ( genAddrAttributes
  , genAddress
  , genAddrType
  , genAddrSpendingData
  , genBlockCount
  , genCanonicalTxFeePolicy
  , genChainDifficulty
  , genCustomLovelace
  , genLovelace
  , genLovelacePortion
  , genMerkleRoot
  , genMerkleTree
  , genScriptVersion
  , genStakeholderId
  , genTxFeePolicy
  , genTxSizeLinear
  )
where

import Cardano.Prelude

import Formatting (build, sformat)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Binary.Class (Bi)
import Cardano.Chain.Common
  ( AddrAttributes(..)
  , AddrSpendingData(..)
  , AddrType(..)
  , Address(..)
  , BlockCount(..)
  , ChainDifficulty(..)
  , Lovelace
  , LovelacePortion(..)
  , MerkleRoot(..)
  , MerkleTree
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
genAddrAttributes = AddrAttributes <$> hap
  where hap = Gen.maybe genHDAddressPayload

genAddress :: Gen Address
genAddress = makeAddress <$> genAddrSpendingData <*> genAddrAttributes

genAddrType :: Gen AddrType
genAddrType = Gen.choice [pure ATPubKey, pure ATRedeem]

genAddrSpendingData :: Gen AddrSpendingData
genAddrSpendingData =
  Gen.choice [PubKeyASD <$> genPublicKey, RedeemASD <$> genRedeemPublicKey]

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
genChainDifficulty = ChainDifficulty <$> Gen.word64 Range.constantBounded

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

genScriptVersion :: Gen Word16
genScriptVersion = Gen.word16 Range.constantBounded

genStakeholderId :: Gen StakeholderId
genStakeholderId = mkStakeholderId <$> genPublicKey

genTxFeePolicy :: Gen TxFeePolicy
genTxFeePolicy = TxFeePolicyTxSizeLinear <$> genTxSizeLinear

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genLovelace <*> genLovelace
