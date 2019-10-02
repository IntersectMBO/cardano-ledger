{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Delegation
  ( genDCerts
  , genPParams
  )
  where

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Hedgehog (Gen)
import           Lens.Micro ((^.))

import qualified Hedgehog.Gen as Gen

import           Coin (Coin (..))
import           Delegation.Certificates (pattern DeRegKey, pattern RegKey, decayKey)
import           Examples (unsafeMkUnitInterval)
import           Generator.Core (toCred)
import           Ledger.Core (dom, (∈), (∉))
import           LedgerState (dstate, keyRefund, stKeys, _dstate, _pstate, _stKeys, _stPools)
import           MockTypes (DCert, DPState, DState, KeyPair, KeyPairs)
import           PParams (PParams (..), emptyPParams)
import           Slot (Epoch (..), Slot)
import           TxData (isDeRegKey)
import           UTxO (deposits)

-- TODO @uroboros Generate a range of protocol params
-- TODO @uroboros for now, keeping minA/B at zero until we generate fees in genTx
genPParams :: Gen PParams
genPParams = pure $ emptyPParams {
                                   _minfeeA = 0
                                 , _minfeeB = 0
                                 , _maxBBSize = 50000
                                 , _maxBHSize = 10000
                                 , _maxTxSize = 10000
                                 , _eMax = Epoch 10000
                                 , _keyDeposit = Coin 7
                                 , _poolDeposit = Coin 250
                                 , _d = unsafeMkUnitInterval 0.5
                                 , _activeSlotCoeff = unsafeMkUnitInterval 0.1
                                 , _tau = unsafeMkUnitInterval 0.2
                                 , _rho = unsafeMkUnitInterval 0.0021
                                 , _keyDecayRate = 0.002
                                 , _keyMinRefund = unsafeMkUnitInterval 0.5
                                 , _poolDecayRate = 0.001
                                 , _poolMinRefund = unsafeMkUnitInterval 0.5
                                 }

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genDCerts
  :: KeyPairs
  -> PParams
  -> DPState
  -> Slot
  -> Gen (Seq DCert, [KeyPair], Coin, Coin)
genDCerts keys pparams dpState slotWithTTL = do
  -- TODO @uroboros Generate _multiple_ certs per Tx
  cert <- genDCert keys (_dstate dpState)
  case cert of
    Nothing ->
      return (Seq.empty, [], Coin 0, Coin 0)
    Just (cert_, witKey) -> do
      let certs = [cert_]
          witKeys = [witKey]
          --
          deposits_ = deposits pparams (_stPools (_pstate dpState)) certs
          --
          deRegStakeCreds = filter isDeRegKey certs
          rewardForCred crt =
            let (dval, dmin, lambda) = decayKey pparams
             in keyRefund dval
                          dmin
                          lambda
                          (dpState ^. dstate . stKeys)
                          slotWithTTL
                          crt
          refunds_ = sum (rewardForCred <$> deRegStakeCreds)

      return (Seq.fromList certs, witKeys, deposits_, refunds_)


-- | Occasionally generate a valid certificate
genDCert
  :: KeyPairs
  -> DState
  -> Gen (Maybe (DCert, KeyPair))
genDCert keys dState =
  -- TODO @uroboros Generate _RegPool_ Certificates
  -- TODO @uroboros Generate _RetirePool_ Certificates
  -- TODO @uroboros Generate _Delegate_ Certificates
  Gen.frequency [ (1, genRegKeyCert keys dState)
                , (1, genDeRegKeyCert keys dState)
                , (1, pure Nothing)
                ]

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
    notRegistered k = k ∉ dom (_stKeys delegSt)
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
           pure $ Just $ (DeRegKey (toCred stakeKey), stakeKey)
  where
    registered k = k ∈ dom (_stKeys delegSt)
    availableKeys = filter (registered . toCred . snd) keys
