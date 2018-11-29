{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

-- | Generation of genesis data for testnet

module Cardano.Chain.Genesis.Generate
  ( GeneratedSecrets(..)
  , gsSecretKeys
  , gsSecretKeysPoor
  , PoorSecret(..)
  , generateGenesisData
  , GenesisDataGenerationError(..)

  -- * Helpers which are also used by keygen.
  , poorSecretToEncKey
  , poorSecretToKey
  )
where

import Cardano.Prelude

import Control.Lens (coerced, (%~))
import Control.Monad.Except (liftEither)
import Crypto.Random (MonadRandom, getRandomBytes)
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)

import Cardano.Binary.Class (serialize')
import Cardano.Chain.Common
  ( Address
  , IsBootstrapEraAddr(..)
  , Lovelace
  , LovelaceError
  , addLovelace
  , applyLovelacePortionDown
  , deriveFirstHDAddress
  , divLovelace
  , makePubKeyAddressBoot
  , mkKnownLovelace
  , mkStakeholderId
  , modLovelace
  , scaleLovelace
  , subLovelace
  , sumLovelace
  )
import qualified Cardano.Chain.Delegation.Certificate as Delegation
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import Cardano.Chain.Genesis.Data (GenesisData(..))
import Cardano.Chain.Genesis.Delegation
  (GenesisDelegation(..), GenesisDelegationError, mkGenesisDelegation)
import Cardano.Chain.Genesis.Initializer
  (FakeAvvmOptions(..), GenesisInitializer(..), TestnetBalanceOptions(..))
import Cardano.Chain.Genesis.NonAvvmBalances (GenesisNonAvvmBalances(..))
import Cardano.Chain.Genesis.Spec (GenesisSpec(..))
import Cardano.Chain.Genesis.WStakeholders (GenesisWStakeholders(..))
import Cardano.Crypto
  ( EncryptedSecretKey
  , SecretKey
  , createPsk
  , deterministic
  , emptyPassphrase
  , encToSecret
  , keyGen
  , noPassEncrypt
  , noPassSafeSigner
  , redeemDeterministicKeyGen
  , safeKeyGen
  , toPublic
  )


-- | Poor node secret
data PoorSecret = PoorSecret SecretKey | PoorEncryptedSecret EncryptedSecretKey

-- | Valuable secrets which can unlock genesis data.
data GeneratedSecrets = GeneratedSecrets
    { gsDlgIssuersSecrets :: ![SecretKey]
    -- ^ Secret keys which issued heavyweight delegation certificates
    -- in genesis data. If genesis heavyweight delegation isn't used,
    -- this list is empty.
    , gsRichSecrets       :: ![SecretKey]
    -- ^ All secrets of rich nodes.
    , gsPoorSecrets       :: ![PoorSecret]
    -- ^ Keys for HD addresses of poor nodes.
    , gsFakeAvvmSeeds     :: ![ByteString]
    -- ^ Fake avvm seeds.
    }

gsSecretKeys :: GeneratedSecrets -> [SecretKey]
gsSecretKeys gs = gsRichSecrets gs <> gsSecretKeysPoor gs

gsSecretKeysPoor :: GeneratedSecrets -> [SecretKey]
gsSecretKeysPoor = map poorSecretToKey . gsPoorSecrets

data GenesisDataGenerationError
  = GenesisDataAddressBalanceMismatch Text Int Int
  | GenesisDataGenerationDelegationError GenesisDelegationError
  | GenesisDataGenerationDistributionMismatch Lovelace Lovelace
  | GenesisDataGenerationLovelaceError LovelaceError
  | GenesisDataGenerationPassPhraseMismatch
  | GenesisDataGenerationRedeemKeyGen
  deriving (Eq, Show)

generateGenesisData
  :: MonadError GenesisDataGenerationError m
  => UTCTime
  -> GenesisSpec
  -> m (GenesisData, GeneratedSecrets)
generateGenesisData startTime genesisSpec = do

  -- Bootstrap stakeholders
  let
    bootSecrets = if giUseHeavyDlg gi then dlgIssuersSecrets else richSecrets

    bootStakeholders :: GenesisWStakeholders
    bootStakeholders = GenesisWStakeholders $ M.fromList $ map
      ((, 1) . mkStakeholderId . toPublic)
      bootSecrets

  -- Heavyweight delegation.
  -- genesisDlgList is empty if giUseHeavyDlg = False
  let
    genesisDlgList :: [Delegation.Certificate]
    genesisDlgList =
      (\(issuerSK, delegateSK) ->
          createPsk pm (noPassSafeSigner issuerSK) (toPublic delegateSK) 0
        )
        <$> zip dlgIssuersSecrets richSecrets

  genesisDlg <-
    liftEither
    .  first GenesisDataGenerationDelegationError
    $  mkGenesisDelegation
    $  M.elems (unGenesisDelegation $ gsHeavyDelegation genesisSpec)
    <> genesisDlgList

  -- Real AVVM Balances
  let
    applyAvvmBalanceFactor :: Map k Lovelace -> Map k Lovelace
    applyAvvmBalanceFactor =
      map (applyLovelacePortionDown $ giAvvmBalanceFactor gi)

    realAvvmMultiplied :: GenesisAvvmBalances
    realAvvmMultiplied = realAvvmBalances & coerced %~ applyAvvmBalanceFactor

  -- Fake AVVM Balances
  fakeAvvmPublicKeys <-
    mapM (maybe (throwError GenesisDataGenerationRedeemKeyGen) (pure . fst))
      $ fmap redeemDeterministicKeyGen (gsFakeAvvmSeeds generatedSecrets)
  let
    fakeAvvmDistr = GenesisAvvmBalances . M.fromList $ map
      (, faoOneBalance fao)
      fakeAvvmPublicKeys

  -- Non AVVM balances
  ---- Addresses
  let
    createAddressPoor
      :: MonadError GenesisDataGenerationError m => PoorSecret -> m Address
    createAddressPoor (PoorEncryptedSecret hdwSk) =
      maybe (throwError GenesisDataGenerationPassPhraseMismatch) (pure . fst)
        $ deriveFirstHDAddress (IsBootstrapEraAddr True) emptyPassphrase hdwSk
    createAddressPoor (PoorSecret secret) =
      pure $ makePubKeyAddressBoot (toPublic secret)
  let richAddresses = map (makePubKeyAddressBoot . toPublic) richSecrets

  poorAddresses        <- mapM createAddressPoor poorSecrets

  ---- Balances
  totalFakeAvvmBalance <-
    liftEither . first GenesisDataGenerationLovelaceError $ scaleLovelace
      (faoOneBalance fao)
      (faoCount fao)

  -- Compute total balance to generate
  avvmSum <-
    liftEither
    . first GenesisDataGenerationLovelaceError
    $ sumLovelace
    $ getGenesisAvvmBalances realAvvmMultiplied
  maxTnBalance <-
    liftEither . first GenesisDataGenerationLovelaceError $ subLovelace
      maxBound
      avvmSum
  let tnBalance = min maxTnBalance (tboTotalBalance tbo)

  let
    safeZip
      :: MonadError GenesisDataGenerationError m
      => Text
      -> [a]
      -> [b]
      -> m [(a, b)]
    safeZip s a b = if length a /= length b
      then throwError
        $ GenesisDataAddressBalanceMismatch s (length a) (length b)
      else pure $ zip a b

  nonAvvmBalance <-
    liftEither . first GenesisDataGenerationLovelaceError $ subLovelace
      tnBalance
      totalFakeAvvmBalance

  (richBals, poorBals) <- genTestnetDistribution tbo nonAvvmBalance

  richDistr            <- safeZip "richDistr" richAddresses richBals
  poorDistr            <- safeZip "poorDistr" poorAddresses poorBals

  let
    nonAvvmDistr = GenesisNonAvvmBalances . M.fromList $ richDistr ++ poorDistr

  let
    genesisData = GenesisData
      { gdBootStakeholders = bootStakeholders
      , gdHeavyDelegation  = genesisDlg
      , gdStartTime        = startTime
      , gdNonAvvmBalances  = nonAvvmDistr
      , gdBlockVersionData = gsBlockVersionData genesisSpec
      , gdK                = gsK genesisSpec
      , gdProtocolMagic    = pm
      , gdAvvmDistr        = fakeAvvmDistr <> realAvvmMultiplied
      }

  pure (genesisData, generatedSecrets)
 where
  pm                = gsProtocolMagic genesisSpec
  realAvvmBalances  = gsAvvmDistr genesisSpec

  gi                = gsInitializer genesisSpec

  generatedSecrets  = generateSecrets gi
  dlgIssuersSecrets = gsDlgIssuersSecrets generatedSecrets
  richSecrets       = gsRichSecrets generatedSecrets
  poorSecrets       = gsPoorSecrets generatedSecrets

  fao               = giFakeAvvmBalance gi
  tbo               = giTestBalance gi


generateSecrets :: GenesisInitializer -> GeneratedSecrets
generateSecrets gi = deterministic (serialize' $ giSeed gi) $ do

  -- Generate fake AVVM seeds
  fakeAvvmSeeds <- replicateM (fromIntegral $ faoCount fao) (getRandomBytes 32)

  -- Generate secret keys
  dlgIssuersSecrets <- if giUseHeavyDlg gi
    then replicateRich (snd <$> keyGen)
    else pure []

  richSecrets <- replicateRich (snd <$> keyGen)

  poorSecrets <- replicateM (fromIntegral $ tboPoors tbo) genPoorSecret

  pure $ GeneratedSecrets
    { gsDlgIssuersSecrets = dlgIssuersSecrets
    , gsRichSecrets       = richSecrets
    , gsPoorSecrets       = poorSecrets
    , gsFakeAvvmSeeds     = fakeAvvmSeeds
    }
 where
  fao = giFakeAvvmBalance gi
  tbo = giTestBalance gi

  replicateRich :: Applicative m => m a -> m [a]
  replicateRich = replicateM (fromIntegral $ tboRichmen tbo)

  genPoorSecret :: MonadRandom m => m PoorSecret
  genPoorSecret = if tboUseHDAddresses tbo
    then PoorEncryptedSecret . snd <$> safeKeyGen emptyPassphrase
    else PoorSecret . snd <$> keyGen


----------------------------------------------------------------------------
-- Exported helpers
----------------------------------------------------------------------------

poorSecretToKey :: PoorSecret -> SecretKey
poorSecretToKey (PoorSecret          key   ) = key
poorSecretToKey (PoorEncryptedSecret encKey) = encToSecret encKey

poorSecretToEncKey :: PoorSecret -> EncryptedSecretKey
poorSecretToEncKey (PoorSecret          key ) = noPassEncrypt key
poorSecretToEncKey (PoorEncryptedSecret encr) = encr


----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------

-- | Generates balance distribution for testnet
genTestnetDistribution
  :: MonadError GenesisDataGenerationError m
  => TestnetBalanceOptions
  -> Lovelace
  -> m ([Lovelace], [Lovelace])
genTestnetDistribution tbo testBalance = do

  (richBalances, poorBalances, totalBalance) <-
    liftEither . first GenesisDataGenerationLovelaceError $ do
      oneRichmanBalance <- if numRichmen == 0
        then pure $ mkKnownLovelace @0
        else addLovelace
          (divLovelace desiredRichBalance numRichmen)
          (if modLovelace desiredRichBalance numRichmen > mkKnownLovelace @0
            then mkKnownLovelace @1
            else mkKnownLovelace @0
          )

      realRichBalance     <- scaleLovelace oneRichmanBalance numRichmen

      desiredPoorsBalance <- subLovelace testBalance realRichBalance

      let
        onePoorBalance = if numPoors == 0
          then mkKnownLovelace @0
          else divLovelace desiredPoorsBalance numPoors

      realPoorBalance <- scaleLovelace onePoorBalance numPoors

      totalBalance    <- addLovelace realRichBalance realPoorBalance

      pure
        ( replicate numRichmen oneRichmanBalance
        , replicate numPoors   onePoorBalance
        , totalBalance
        )

  if totalBalance <= testBalance
    then pure (richBalances, poorBalances)
    else throwError
      $ GenesisDataGenerationDistributionMismatch testBalance totalBalance
 where
  numRichmen :: Int
  numRichmen = fromIntegral $ tboRichmen tbo

  numPoors :: Int
  numPoors = fromIntegral $ tboPoors tbo

  desiredRichBalance =
    applyLovelacePortionDown (tboRichmenShare tbo) testBalance
