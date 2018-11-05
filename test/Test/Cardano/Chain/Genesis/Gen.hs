module Test.Cardano.Chain.Genesis.Gen
       ( genGenesisHash
       , genFakeAvvmOptions
       , genGenesisAvvmBalances
       , genGenesisDelegation
       , genGenesisInitializer
       , genGenesisProtocolConstants
       , genGenesisSpec
       , genSharedSeed
       , genTestnetBalanceOptions
       , genStaticConfig
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Data.Coerce
    (coerce)
import qualified Data.Map.Strict as M
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Chain.Common
    (SharedSeed (..))
import           Cardano.Chain.Genesis
    ( FakeAvvmOptions (..)
    , GenesisAvvmBalances (..)
    , GenesisDelegation (..)
    , GenesisHash (..)
    , GenesisInitializer (..)
    , GenesisProtocolConstants (..)
    , GenesisSpec (..)
    , StaticConfig (..)
    , TestnetBalanceOptions (..)
    , mkGenesisDelegation
    , mkGenesisSpec
    )
import           Cardano.Crypto
    (ProtocolMagic)

import           Test.Cardano.Chain.Common.Gen
    (genCoin, genCoinPortion)
import           Test.Cardano.Chain.Delegation.Gen
    (genProxySKHeavyDistinctList)
import           Test.Cardano.Chain.ProtocolConstants.Gen
    (genVssMaxTTL, genVssMinTTL)
import           Test.Cardano.Chain.Update.Gen
    (genBlockVersionData)
import           Test.Cardano.Crypto.Gen
    (genHashRaw, genProtocolMagic, genRedeemPublicKey, genTextHash)

genGenesisHash :: Gen GenesisHash
genGenesisHash = do
  th <- genTextHash
  pure (GenesisHash (coerce th))

genSharedSeed :: Gen SharedSeed
genSharedSeed = SharedSeed <$> gen32Bytes

genStaticConfig :: ProtocolMagic -> Gen StaticConfig
genStaticConfig pm =
  Gen.choice [ GCSrc
                 <$> Gen.string (Range.constant 10 25) Gen.alphaNum
                 <*> genHashRaw
             , GCSpec <$> genGenesisSpec pm
             ]

genFakeAvvmOptions :: Gen FakeAvvmOptions
genFakeAvvmOptions =
  FakeAvvmOptions
    <$> Gen.word Range.constantBounded
    <*> Gen.word64 Range.constantBounded

genGenesisDelegation :: ProtocolMagic -> Gen GenesisDelegation
genGenesisDelegation pm = do
  proxySKHeavyList <- genProxySKHeavyDistinctList pm
  case mkGenesisDelegation proxySKHeavyList of
    Left err     -> panic err
    Right genDel -> pure genDel

genGenesisInitializer :: Gen GenesisInitializer
genGenesisInitializer =
  GenesisInitializer
    <$> genTestnetBalanceOptions
    <*> genFakeAvvmOptions
    <*> genCoinPortion
    <*> Gen.bool
    <*> Gen.integral (Range.constant 0 10)

genGenesisProtocolConstants :: Gen GenesisProtocolConstants
genGenesisProtocolConstants =
  GenesisProtocolConstants
    <$> Gen.int (Range.constant 0 100)
    <*> genProtocolMagic
    <*> genVssMaxTTL
    <*> genVssMinTTL

genGenesisSpec :: ProtocolMagic -> Gen GenesisSpec
genGenesisSpec pm = mkGenSpec >>=  either (panic . toS) pure
  where
    mkGenSpec =
      mkGenesisSpec
        <$> genGenesisAvvmBalances
        <*> genSharedSeed
        <*> genGenesisDelegation pm
        <*> genBlockVersionData
        <*> genGenesisProtocolConstants
        <*> genGenesisInitializer

genTestnetBalanceOptions :: Gen TestnetBalanceOptions
genTestnetBalanceOptions =
  TestnetBalanceOptions
    <$> Gen.word Range.constantBounded
    <*> Gen.word Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.double (Range.constant 0 10)
    <*> Gen.bool

genGenesisAvvmBalances :: Gen GenesisAvvmBalances
genGenesisAvvmBalances =
  GenesisAvvmBalances <$> customMapGen genRedeemPublicKey genCoin

--------------------------------------------------------------------------------
-- Helper Generators
--------------------------------------------------------------------------------

customMapGen :: Ord k => Gen k -> Gen v -> Gen (Map k v)
customMapGen keyGen valGen =
  M.fromList
    <$> (Gen.list (Range.linear 1 10) $ (,) <$> keyGen <*> valGen)
