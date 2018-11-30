module Test.Cardano.Chain.Genesis.Gen
  ( genGenesisHash
  , genFakeAvvmOptions
  , genGenesisAvvmBalances
  , genGenesisDelegation
  , genGenesisInitializer
  , genGenesisSpec
  , genTestnetBalanceOptions
  , genStaticConfig
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Formatting (build, sformat)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Genesis
  ( FakeAvvmOptions(..)
  , GenesisAvvmBalances(..)
  , GenesisDelegation(..)
  , GenesisHash(..)
  , GenesisInitializer(..)
  , GenesisSpec(..)
  , StaticConfig(..)
  , TestnetBalanceOptions(..)
  , mkGenesisDelegation
  , mkGenesisSpec
  )
import Cardano.Crypto (ProtocolMagic)

import Test.Cardano.Chain.Common.Gen
  (genBlockCount, genLovelace, genLovelacePortion)
import Test.Cardano.Chain.Delegation.Gen (genCertificateDistinctList)
import Test.Cardano.Chain.Update.Gen (genBlockVersionData)
import Test.Cardano.Crypto.Gen
  (genHashRaw, genProtocolMagic, genRedeemPublicKey, genTextHash)

genGenesisHash :: Gen GenesisHash
genGenesisHash = do
  th <- genTextHash
  pure (GenesisHash (coerce th))

genStaticConfig :: ProtocolMagic -> Gen StaticConfig
genStaticConfig pm = Gen.choice
  [ GCSrc <$> Gen.string (Range.constant 10 25) Gen.alphaNum <*> genHashRaw
  , GCSpec <$> genGenesisSpec pm
  ]

genFakeAvvmOptions :: Gen FakeAvvmOptions
genFakeAvvmOptions =
  FakeAvvmOptions <$> Gen.word Range.constantBounded <*> genLovelace

genGenesisDelegation :: ProtocolMagic -> Gen GenesisDelegation
genGenesisDelegation pm = do
  certificates <- genCertificateDistinctList pm
  case mkGenesisDelegation certificates of
    Left  err    -> panic $ sformat build err
    Right genDel -> pure genDel

genGenesisInitializer :: Gen GenesisInitializer
genGenesisInitializer =
  GenesisInitializer
    <$> genTestnetBalanceOptions
    <*> genFakeAvvmOptions
    <*> genLovelacePortion
    <*> Gen.bool
    <*> Gen.integral (Range.constant 0 10)

genGenesisSpec :: ProtocolMagic -> Gen GenesisSpec
genGenesisSpec pm = mkGenSpec >>= either (panic . toS) pure
 where
  mkGenSpec =
    mkGenesisSpec
      <$> genGenesisAvvmBalances
      <*> genGenesisDelegation pm
      <*> genBlockVersionData
      <*> genBlockCount
      <*> genProtocolMagic
      <*> genGenesisInitializer

genTestnetBalanceOptions :: Gen TestnetBalanceOptions
genTestnetBalanceOptions =
  TestnetBalanceOptions
    <$> Gen.word Range.constantBounded
    <*> Gen.word Range.constantBounded
    <*> genLovelace
    <*> genLovelacePortion
    <*> Gen.bool

genGenesisAvvmBalances :: Gen GenesisAvvmBalances
genGenesisAvvmBalances =
  GenesisAvvmBalances <$> customMapGen genRedeemPublicKey genLovelace

--------------------------------------------------------------------------------
-- Helper Generators
--------------------------------------------------------------------------------

customMapGen :: Ord k => Gen k -> Gen v -> Gen (Map k v)
customMapGen keyGen valGen =
  M.fromList <$> (Gen.list (Range.linear 1 10) $ (,) <$> keyGen <*> valGen)
