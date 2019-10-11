{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Delegation
  ( genDCerts )
  where

import qualified Data.Map as Map
import           Data.Ratio ((%))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Hedgehog (Gen)
import           Lens.Micro (to, (^.))

import qualified Hedgehog.Gen as Gen

import           Coin (Coin (..))
import           Delegation.Certificates (pattern DeRegKey, pattern RegKey, pattern RegPool,
                     decayKey, isDeRegKey)
import           Examples (unsafeMkUnitInterval)
import           Generator.Core (genInteger, genNatural, toCred)
import           Keys (hashKey, hashKeyVRF, vKey)
import           Ledger.Core (dom, (∈), (∉))
import           LedgerState (dstate, keyRefund, pParams, pstate, stkCreds, _pstate, _stPools,
                     _stkCreds)
import           MockTypes (DCert, DPState, DState, KeyPair, KeyPairs, PoolParams, VrfKeyPairs)
import           Mutator (getAnyStakeKey)
import           PParams (PParams (..))
import           Slot (Slot)
import           TxData (Credential (KeyHashObj), pattern PoolParams, RewardAcnt (..), _poolPubKey,
                     _poolVrf)
import           UTxO (deposits)

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genDCerts
  :: KeyPairs
  -> VrfKeyPairs
  -> PParams
  -> DPState
  -> Slot
  -> Gen (Seq DCert, [KeyPair], Coin, Coin)
genDCerts keys vrfKeys pparams dpState slotWithTTL = do
  -- TODO @uroboros Generate _multiple_ certs per Tx
  -- TODO ensure that the `Seq` is constructed with the list reversed, or that
  -- later traversals are done backwards, to be consistent with the executable
  -- spec (see `delegsTransition` in `STS.Delegs`) which consumes the list
  -- starting at the tail end.
  cert <- genDCert keys vrfKeys dpState
  case cert of
    Nothing ->
      return (Seq.empty, [], Coin 0, Coin 0)
    Just (cert_, witKey) -> do
      let certs = [cert_]
          witKeys = [witKey]

          deposits_ = deposits pparams (_stPools (_pstate dpState)) certs

          deRegStakeCreds = filter isDeRegKey certs
          rewardForCred crt =
            let (dval, dmin, lambda) = decayKey pparams
             in keyRefund dval
                          dmin
                          lambda
                          (dpState ^. dstate . stkCreds)
                          slotWithTTL
                          crt
          refunds_ = sum (rewardForCred <$> deRegStakeCreds)

      return (Seq.fromList certs, witKeys, deposits_, refunds_)


-- | Occasionally generate a valid certificate
genDCert
  :: KeyPairs
  -> VrfKeyPairs
  -> DPState
  -> Gen (Maybe (DCert, KeyPair))
  -- -> Gen (Maybe (DCert, DCertReturnStuff))
genDCert keys vrfKeys dpState =
  -- TODO @uroboros Generate _RetirePool_ Certificates
  -- TODO @uroboros Generate _Delegate_ Certificates
  Gen.frequency [ (2, genRegKeyCert keys dState)
                , (3, genDeRegKeyCert keys dState)
                , (3, genRegPool keys vrfKeys dpState)
                , (1, pure Nothing)
                ]
 where
  dState = dpState ^. dstate

-- | Generate a RegKey certificate along and also returns the stake key
-- (needed to witness the certificate)
genRegKeyCert
  :: KeyPairs
  -> DState
  -> Gen (Maybe (DCert, KeyPair))
genRegKeyCert keys delegSt =
  case availableKeys of
    [] -> pure Nothing
    _ -> do
           (_payKey, stakeKey) <- Gen.element availableKeys
           pure $ Just $ (RegKey (toCred stakeKey), stakeKey)
  where
    notRegistered k = k ∉ dom (_stkCreds delegSt)
    availableKeys = filter (notRegistered . toCred . snd) keys

-- | Generate a DeRegKey certificate along with the stake key, which is needed
-- to witness the certificate.
genDeRegKeyCert
  :: KeyPairs
  -> DState
  -> Gen (Maybe (DCert, KeyPair))
genDeRegKeyCert keys delegSt =
  case availableKeys of
    [] -> pure Nothing
    _ -> do
           (_payKey, stakeKey) <- Gen.element availableKeys
           pure $ Just (DeRegKey (toCred stakeKey), stakeKey)
  where
    registered k = k ∈ dom (_stkCreds delegSt)
    availableKeys = filter (registered . toCred . snd) keys

-- | Generate and return a RegPool certificate along with its witnessing key.
genRegPool
  :: KeyPairs
  -> VrfKeyPairs
  -> DPState
  -> Gen (Maybe (DCert, KeyPair))
genRegPool keys vrfKeys dpState =
  if null availableKeys || null availableVrfKeys
     then pure Nothing
     else do
       Just <$> genDCertRegPool availableKeys availableVrfKeys
 where
  notRegistered k = k `notElem` (dpState ^. pstate . pParams . to Map.elems . to (_poolPubKey <$>))
  availableKeys = filter (notRegistered . hashKey . vKey . snd) keys

  notRegisteredVrf k = k `notElem` (dpState ^. pstate . pParams . to Map.elems . to (_poolVrf <$>))
  availableVrfKeys = filter (notRegisteredVrf . hashKeyVRF . snd) vrfKeys

-- | Generate PoolParams and the key witness.
genStakePool :: KeyPairs -> VrfKeyPairs -> Gen (PoolParams, KeyPair)
genStakePool skeys vrfKeys = do
  poolKeyPair   <- Gen.element skeys
  vrfKey        <- snd <$> Gen.element vrfKeys
  cost          <- Coin <$> genInteger 1 100
  pledge        <- Coin <$> genInteger 1 100
  marginPercent <- genNatural 0 100
  acntKey       <- getAnyStakeKey skeys
  let interval = unsafeMkUnitInterval $ fromIntegral marginPercent % 100
  let pps = PoolParams
              (hashKey . vKey . snd $ poolKeyPair)
              (hashKeyVRF vrfKey)
              pledge
              cost
              interval
              (RewardAcnt $ KeyHashObj $ hashKey acntKey)
              Set.empty
  pure (pps, snd poolKeyPair)

-- | Generate `RegPool` and the key witness.
genDCertRegPool :: KeyPairs -> VrfKeyPairs -> Gen (DCert, KeyPair)
genDCertRegPool skeys vrfKeys = do
  (pps, poolKey) <- genStakePool skeys vrfKeys
  pure (RegPool pps, poolKey)
