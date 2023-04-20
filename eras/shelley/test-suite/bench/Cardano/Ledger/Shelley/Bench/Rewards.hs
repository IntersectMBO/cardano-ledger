{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.Bench.Rewards (
  createRUpd,
  createRUpdWithProv,
  genChainInEpoch,
)
where

import Cardano.Crypto.VRF (hashVerKeyVRF)
import Cardano.Ledger.Address (
  Addr (..),
  mkRwdAcnt,
 )
import Cardano.Ledger.BaseTypes (
  Globals (activeSlotCoeff, securityParameter),
  Network (Testnet),
  StrictMaybe (..),
  epochInfoPure,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (Staking))
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesisStaking (..))
import qualified Cardano.Ledger.Shelley.LedgerState as LS
import Cardano.Ledger.Shelley.TxBody (PoolParams (..), ShelleyTxOut (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Slotting.EpochInfo
import Cardano.Slotting.Slot (EpochNo)
import Control.Monad.Reader (runReader, runReaderT)
import Control.State.Transition.Extended (IRC (..), TRC (..), applySTS)
import Data.Either (fromRight)
import Data.Functor.Identity (runIdentity)
import qualified Data.ListMap as LM
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Shelley.BenchmarkFunctions (B, B_Crypto)
import Test.Cardano.Ledger.Shelley.Constants (
  defaultConstants,
  maxGenesisUTxOouts,
  minGenesisUTxOouts,
 )
import Test.Cardano.Ledger.Shelley.Generator.Block (genBlockWithTxGen)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  GenEnv (..),
  ScriptSpace (..),
  VRFKeyPair (..),
  geConstants,
  geKeySpace,
  ksStakePools,
 )
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (
  mkGenesisChainState,
  registerGenesisStaking,
 )
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState, chainNes, totalAda)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Test.QuickCheck (Gen)

-- | Generate a chain state at a given epoch. Since we are only concerned about
-- rewards, this will populate the chain with empty blocks (only issued by the
-- original genesis delegates).
genChainInEpoch :: EpochNo -> Gen (ChainState B)
genChainInEpoch epoch = do
  genesisChainState <-
    fromRight (error "genChainState failed")
      <$> mkGenesisChainState @B (GenEnv ks (ScriptSpace [] [] Map.empty Map.empty) cs) (IRC ())
  -- Our genesis chain state contains no registered staking. Since we want to
  -- calculate a reward update, we will set some up.
  -- What do we want to do here?
  -- - We find all the addresses which are present in the initial UTxO, and
  --   register them all for staking
  -- - We group them into 50 groups (number of stake pools). The first one in
  --   each group becomes the pool owner, with pledge set to the value in that
  --   address.
  let initUtxo =
        LS.utxosUtxo
          . LS.lsUTxOState
          . LS.esLState
          . LS.nesEs
          $ chainNes genesisChainState
      initUtxoAddrs =
        Maybe.mapMaybe (\(ShelleyTxOut addr _) -> addrToKeyHash addr)
          . Map.elems
          . unUTxO
          $ initUtxo
      stakeMap =
        zip stakePools $
          chunk (length initUtxoAddrs `div` length stakePools) initUtxoAddrs

  let chainState =
        registerGenesisStaking
          (mkGenesisStaking stakeMap)
          genesisChainState
  -- Now run the empty block generator over the chain state until we hit the
  -- desired epoch.
  applyUntil
    chainState
    ( \x ->
        if (LS.nesEL $ chainNes x) >= epoch
          then Nothing
          else Just $ applyBlk x <$> genEmptyBlock x
    )
  where
    applyUntil :: Monad m => a -> (a -> Maybe (m a)) -> m a
    applyUntil x f = case f x of
      Nothing -> pure x
      Just x' -> x' >>= flip applyUntil f
    applyBlk cs' blk =
      (either err id)
        . flip runReader testGlobals
        . applySTS @(CHAIN B B_Crypto)
        $ TRC ((), cs', blk)
      where
        err :: Show a => a -> b
        err msg = error $ "Panic! applyBlk failed: " <> (show msg)
    ge = genEnv (Proxy @B) (Proxy @B_Crypto) defaultConstants
    -- Small UTxO set; we just want enough to stake to pools
    cs =
      (geConstants ge)
        { minGenesisUTxOouts = 5000
        , maxGenesisUTxOouts = 5000
        }
    ks = geKeySpace ge
    genEmptyBlock = genBlockWithTxGen @B (\_ _ _ _ -> pure mempty) ge
    mkGenesisStaking stakeMap =
      ShelleyGenesisStaking
        { sgsPools =
            LM.ListMap
              [ (aikColdKeyHash, pp)
              | (AllIssuerKeys {aikVrf, aikColdKeyHash}, (owner : _)) <- stakeMap
              , let pp =
                      PoolParams
                        { ppId = aikColdKeyHash
                        , ppVrf = toPoolStakeVRF $ hashVerKeyVRF $ vrfVerKey aikVrf
                        , ppPledge = Coin 1
                        , ppCost = Coin 1
                        , ppMargin = minBound
                        , ppRewardAcnt = mkRwdAcnt Testnet $ KeyHashObj owner
                        , ppOwners = Set.singleton owner
                        , ppRelays = StrictSeq.empty
                        , ppMetadata = SNothing
                        }
              ]
        , sgsStake =
            LM.ListMap
              [ (dlg, aikColdKeyHash)
              | (AllIssuerKeys {aikColdKeyHash}, dlgs) <- stakeMap
              , dlg <- dlgs
              ]
        }
    stakePools = ksStakePools ks
    chunk :: Int -> [a] -> [[a]]
    chunk n _ | n <= 0 = []
    chunk n xs = go [] xs
      where
        go !acc [] = acc
        go !acc xs' = let (a, b) = splitAt n xs' in go (a : acc) b

    addrToKeyHash :: Addr c -> Maybe (KeyHash 'Staking c)
    addrToKeyHash (Addr _ _ (StakeRefBase (KeyHashObj kh))) = Just kh
    addrToKeyHash _ = Nothing

-- | Benchmark creating a reward update.
createRUpd ::
  Globals ->
  ChainState B ->
  LS.RewardUpdate B_Crypto
createRUpd globals cs =
  runIdentity $
    runReaderT
      (LS.createRUpd epochSize bm es total asc k)
      globals
  where
    nes = chainNes cs
    bm = LS.nesBprev nes
    es = LS.nesEs nes
    total = totalAda cs
    epochSize =
      runIdentity $
        epochInfoSize (epochInfoPure globals) (LS.nesEL nes)
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

-- | Benchmark creating a reward update.
createRUpdWithProv ::
  Globals ->
  ChainState B ->
  (LS.RewardUpdate B_Crypto)
createRUpdWithProv globals cs =
  runIdentity $
    runReaderT
      (LS.createRUpd epochSize bm es total asc k)
      globals
  where
    nes = chainNes cs
    bm = LS.nesBprev nes
    es = LS.nesEs nes
    total = totalAda cs
    epochSize =
      runIdentity $
        epochInfoSize (epochInfoPure globals) (LS.nesEL nes)
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals
