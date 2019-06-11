{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Test.Cardano.Chain.Block.ValidationMode
  ( tests
  )
where

import Cardano.Prelude hiding (State)
import Test.Cardano.Prelude

import Control.Lens ((^.))
import qualified Data.Bimap as BM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

import Cardano.Binary (Annotated (..))
import Cardano.Chain.Block
  ( ABlock (..)
  , AHeader (..)
  , BlockValidationMode (..)
  , Proof (..)
  , blockProof
  , initialChainValidationState
  , updateBlock
  )
import Cardano.Chain.Delegation as Delegation
import Cardano.Chain.UTxO (TxProof)
import Cardano.Crypto (Hash)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Cardano.Spec.Chain.STS.Block as Abstract
import qualified Cardano.Spec.Chain.STS.Rule.Chain as Abstract
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN)
import Cardano.Ledger.Spec.STS.UTXOWS (UTXOWS)
import Cardano.Ledger.Spec.STS.UTXO (UTxOEnv (..), UTxOState (..))
import Control.State.Transition
import Control.State.Transition.Generator
import qualified Ledger.Core as Abstract
import Ledger.Delegation
  ( ADELEGS
  , DELEG
  , DIState (..)
  , DSEnv (..)
  , DState (..)
  )
import Ledger.GlobalParams (lovelaceCap)
import qualified Ledger.Update as Abstract
import qualified Ledger.UTxO as Abstract

import Test.Cardano.Chain.Block.Model (elaborateAndUpdate)
import qualified Test.Cardano.Chain.Delegation.Gen as Delegation
import Test.Cardano.Chain.Elaboration.Block (abEnvToCfg, elaborateBS, rcDCert)
import qualified Test.Cardano.Chain.Update.Gen as Update
import Test.Cardano.Chain.UTxO.Gen (genTxProof)
import Test.Cardano.Chain.UTxO.Model (elaborateInitialUTxO)
import Test.Cardano.Crypto.Gen (feedPM, genAbstractHash)
import Test.Options (TSGroup, TSProperty, withTestsTS)

--------------------------------------------------------------------------------
-- BlockValidationMode Properties
--------------------------------------------------------------------------------

-- | Property: When calling 'updateBlock' given a valid 'Block', validation
-- should pass in all 'BlockValidationMode's.
ts_prop_updateBlock_Valid :: TSProperty
ts_prop_updateBlock_Valid =
  withTestsTS 100
    . property
    $ do
      chainEnv@(_, abstractInitialUTxO, _, _) <- forAll $ initEnvGen @CHAIN
      chainState <- forAll $ genInitialChainState chainEnv
      abstractBlock <- forAll $
        Abstract.sigGenChain
          Abstract.NoGenDelegation
          Abstract.NoGenUTxO
          chainEnv
          chainState
      let config = abEnvToCfg chainEnv
          cvs = either (panic . show) (\a -> a) (initialChainValidationState config)
          (_, txIdMap) = elaborateInitialUTxO abstractInitialUTxO
      bvmode <- forAll $ genBlockValidationMode
      case elaborateAndUpdate bvmode config (cvs, txIdMap) (chainState, abstractBlock) of
        Left _  -> failure
        Right _ -> success

-- | Property: When calling 'updateBlock' given a 'Block' with an invalid
-- 'Proof', 'Block' validation should only pass in the 'NoBlockValidation' mode.
-- This is because this mode does not perform any validation on the 'Block'.
ts_prop_updateBlock_InvalidProof :: TSProperty
ts_prop_updateBlock_InvalidProof =
  withTestsTS 100
    . property
    $ do
      chainEnv@(_, abstractInitialUTxO, _, _) <- forAll $ initEnvGen @CHAIN
      chainState <- forAll $ genInitialChainState chainEnv
      abstractBlock <- forAll $
        Abstract.sigGenChain
          Abstract.NoGenDelegation
          Abstract.NoGenUTxO
          chainEnv
          chainState
      let config = abEnvToCfg chainEnv
          cvs = either (panic . show) (\a -> a) (initialChainValidationState config)
          (_, txIdMap) = elaborateInitialUTxO abstractInitialUTxO
          dCert = rcDCert (abstractBlock ^. Abstract.bHeader . Abstract.bhIssuer) chainState
      bvmode <- forAll $ genBlockValidationMode
      let (concreteBlock, _txIdMap') = elaborateBS txIdMap config dCert cvs abstractBlock
      annotateShow concreteBlock
      invalidBlock <- forAll $ invalidateABlockProof concreteBlock
      case updateBlock bvmode config cvs invalidBlock of
        Left _ ->
          if bvmode == BlockValidation
          then success
          else failure
        Right _ ->
          if bvmode == NoBlockValidation
          then success
          else failure

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genInitialChainState
  :: Environment CHAIN
  -> Gen (State CHAIN)
genInitialChainState env = do
  let (_slot, utxo0', _dsenv, pps') = env
      utxoEnv = UTxOEnv { utxo0 = utxo0', pps = pps' }
      s0 = Abstract.Slot 0
      utxoSt0 = createInitialUTxOState utxoEnv
  initialDelegEnv <- initEnvGen @DELEG
  let initialADelegsEnv = _dSEnvAllowedDelegators initialDelegEnv
  let ds = createInitialDIState (createInitialDState initialADelegsEnv)
  pure $! ( s0
          , (Seq.fromList . BM.keys . _dIStateDelegationMap) ds
          , Abstract.genesisHash
          , utxoSt0
          , ds
          , Abstract.emptyUPIState
          )

genHash :: Gen Abstract.Hash
genHash = Abstract.Hash <$> Gen.int Range.constantBounded

genBlockValidationMode :: Gen BlockValidationMode
genBlockValidationMode = Gen.element [BlockValidation, NoBlockValidation]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

createInitialUTxOState
  :: Environment UTXOWS
  -> State UTXOWS
createInitialUTxOState utxoEnv =
  UTxOState{ utxo = utxo0, reserves = lovelaceCap - Abstract.balance utxo0 }
 where
  UTxOEnv
    { utxo0
    } = utxoEnv

createInitialDState
  :: Environment ADELEGS
  -> State ADELEGS
createInitialDState env =
  DState
    { _dStateDelegationMap = BM.fromList $
        map (\vkg@(Abstract.VKeyGenesis key) -> (vkg, key))
            (S.toList env)
    , _dStateLastDelegation = M.fromSet (const (Abstract.Slot 0)) env
    }

createInitialDIState
  :: State ADELEGS
  -> State DELEG
createInitialDIState dState =
  DIState
    { _dIStateDelegationMap = _dStateDelegationMap dState
    , _dIStateLastDelegation = _dStateLastDelegation dState
    , _dIStateScheduledDelegations = []
    , _dIStateKeyEpochDelegations = S.empty
    }

modifyAHeader
  :: (AHeader ByteString -> AHeader ByteString)
  -> ABlock ByteString
  -> ABlock ByteString
modifyAHeader ahModifier ab =
  ab { blockHeader = ahModifier (blockHeader ab) }

modifyAProof
  :: (Annotated Proof ByteString -> Annotated Proof ByteString)
  -> ABlock ByteString
  -> ABlock ByteString
modifyAProof apModifier ab =
  modifyAHeader ahModifier ab
 where
  ahModifier :: AHeader ByteString -> AHeader ByteString
  ahModifier ah = ah { aHeaderProof = apModifier (aHeaderProof ah) }

modifyDelegationProof
  :: (Hash Delegation.Payload -> Hash Delegation.Payload)
  -> ABlock ByteString
  -> ABlock ByteString
modifyDelegationProof dpModifier ab =
  modifyAProof apModifier ab
 where
  apModifier :: Annotated Proof ByteString -> Annotated Proof ByteString
  apModifier (Annotated p bs) = Annotated
    p { proofDelegation = dpModifier (proofDelegation p) }
    bs

modifyTxProof
  :: (TxProof -> TxProof)
  -> ABlock ByteString
  -> ABlock ByteString
modifyTxProof tpModifier ab =
  modifyAProof apModifier ab
 where
  apModifier :: Annotated Proof ByteString -> Annotated Proof ByteString
  apModifier (Annotated p bs) = Annotated
    p { proofUTxO = tpModifier (proofUTxO p) }
    bs

invalidateABlockProof
  :: ABlock ByteString
  -> Gen (ABlock ByteString)
invalidateABlockProof ab =
  -- 'Gen.filter' to ensure we don't generate a valid proof
  Gen.filter (\x -> blockProof x /= blockProof ab) $ do
    txProof  <- Gen.choice
      [ pure $ (proofUTxO . blockProof) ab
      , feedPM genTxProof
      ]
    dlgProof <- Gen.choice
      [ pure $ (proofDelegation . blockProof) ab
      , genAbstractHash (feedPM Delegation.genPayload)
      ]
    updProof <- Gen.choice
      [ pure $ proofUpdate (blockProof ab)
      , feedPM Update.genProof
      ]
    pure $ modifyAProof
      (\(Annotated p bs) -> Annotated
        (p
          { proofUTxO = txProof
          , proofDelegation = dlgProof
          , proofUpdate = updProof
          }
        )
        bs
      )
      ab

--------------------------------------------------------------------------------
-- Main Test Export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = $$discoverPropArg
