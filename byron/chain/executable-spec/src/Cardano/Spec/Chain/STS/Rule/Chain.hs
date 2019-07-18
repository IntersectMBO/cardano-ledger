{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.Chain where

import           Control.Lens (Lens', (&), (.~), (^.), _1, _5)
import           Data.Bimap (Bimap)
import           Data.Bits (shift)
import           Data.ByteString (ByteString)
import qualified Data.Hashable as H
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word8)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Numeric.Natural (Natural)

import           Cardano.Ledger.Spec.STS.UTXO (UTxOEnv (UTxOEnv, pps, utxo0), UTxOState)
import           Cardano.Ledger.Spec.STS.UTXOWS (UTXOWS)
import           Control.State.Transition
import           Control.State.Transition.Generator
import           Ledger.Core
import qualified Ledger.Core.Generators as CoreGen
import           Ledger.Delegation
import           Ledger.Update hiding (delegationMap)
import qualified Ledger.Update as Update
import           Ledger.UTxO (UTxO)

import           Cardano.Spec.Chain.STS.Block
import           Cardano.Spec.Chain.STS.Rule.BBody
import           Cardano.Spec.Chain.STS.Rule.BHead
import           Cardano.Spec.Chain.STS.Rule.Epoch (EPOCH, sEpoch)
import           Cardano.Spec.Chain.STS.Rule.Pbft
import qualified Cardano.Spec.Chain.STS.Rule.SigCnt as SigCntGen

data CHAIN

instance STS CHAIN where
  type Environment CHAIN =
    ( Slot            -- Current slot
    , UTxO
    -- Components needed to be able to bootstrap the traces generator. They are
    -- not part of the formal spec. These might be removed once we have decided
    -- on how do we want to model initial states.
    , Set VKeyGenesis -- Allowed delegators. Needed by the initial delegation rules. The number of
                      -- genesis keys is the size of this set.
    , PParams         -- Needed to bootstrap this part of the chain state,
                      -- which is used in the delegation rules
    , BlockCount      -- Chain stability parameter
    )

  type State CHAIN =
    ( Slot
    , Seq VKeyGenesis
    , Hash
    , UTxOState
    , DIState
    , UPIState
    )

  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = BHeadFailure (PredicateFailure BHEAD)
    | BBodyFailure (PredicateFailure BBODY)
    | PBFTFailure (PredicateFailure PBFT)
    | MaximumBlockSize Natural Natural
    | LedgerDelegationFailure (PredicateFailure DELEG)
    | LedgerUTxOFailure (PredicateFailure UTXOWS)
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC (_sNow, utxo0', ads, pps', k) <- judgmentContext
        let s0 = Slot 0
            -- Since we only test the delegation state we initialize the
            -- remaining fields of the update interface state to (m)empty. Once
            -- the formal spec includes initial rules for update we can use
            -- them here.
            upiState0 = ( (ProtVer 0 0 0, pps')
                        , []
                        , Map.empty
                        , Map.empty
                        , Map.empty
                        , Map.empty
                        , Set.empty
                        , Set.empty
                        , Map.empty
                        )
        utxoSt0 <- trans @UTXOWS $ IRC UTxOEnv {utxo0 = utxo0', pps = pps' }
        let dsEnv = DSEnv
                    { _dSEnvAllowedDelegators = ads
                    , _dSEnvEpoch = sEpoch s0 k
                    , _dSEnvSlot = s0
                    , _dSEnvK = k
                    }
        ds      <- trans @DELEG $ IRC dsEnv
        pure $! ( s0
                , []
                , genesisHash
                , utxoSt0
                , ds
                , upiState0
                )
    ]

  transitionRules =
    [ do
      TRC (_, _, b) <- judgmentContext
      case bIsEBB b of
        True  -> isEBBRule
        False -> notEBBRule
    ]
   where
    isEBBRule :: TransitionRule CHAIN
    isEBBRule = do
      TRC ((_sNow, _, _, _, _), (sLast, sgs, _, utxo, ds, us), b) <- judgmentContext
      bSize b <= (2 `shift` 21) ?! MaximumBlockSize (bSize b) (2 `shift` 21)
      let h' = bhHash (b ^. bHeader)
      pure $! (sLast, sgs, h', utxo, ds, us)

    notEBBRule :: TransitionRule CHAIN
    notEBBRule = do
      TRC ((sNow, utxoGenesis, ads, _pps, k), (sLast, sgs, h, utxoSt, ds, us), b) <- judgmentContext
      let dm = _dIStateDelegationMap ds :: Bimap VKeyGenesis VKey
      us' <-
        trans @BHEAD $ TRC ((dm, sLast, k), us, b ^. bHeader)
      let ppsUs' = snd (us' ^. _1)
      (h', sgs') <-
        trans @PBFT  $ TRC ((ppsUs', dm, sLast, sNow, k), (h, sgs), b ^. bHeader)
      (utxoSt', ds', us'') <- trans @BBODY $ TRC
        (
          ( ppsUs'
          , sEpoch (b ^. bHeader ^. bhSlot) k
          , utxoGenesis
          , fromIntegral (Set.size ads)
          , k
          )
        , (utxoSt, ds, us')
        , b
        )
      pure $! (b ^. bHeader ^. bhSlot, sgs', h', utxoSt', ds', us'')

instance Embed BHEAD CHAIN where
  wrapFailed = BHeadFailure

instance Embed BBODY CHAIN where
  wrapFailed = BBodyFailure

instance Embed PBFT CHAIN where
  wrapFailed = PBFTFailure

instance Embed DELEG CHAIN where
  wrapFailed = LedgerDelegationFailure

instance Embed UTXOWS CHAIN where
  wrapFailed = LedgerUTxOFailure

genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = Hash $ H.hash ("" :: ByteString)

-- | Lens for the delegation interface state contained in the chain state.
disL :: Lens' (State CHAIN) DIState
disL = _5

instance HasTrace CHAIN where

  envGen chainLength = do
    ngk <- Gen.integral (Range.linear 1 14)
    k <- CoreGen.k chainLength (chainLength `div` 10)
    sigCntT <- SigCntGen.sigCntT k ngk
    (,,,,)
      <$> gCurrentSlot
      <*> (utxo0 <$> envGen @UTXOWS chainLength)
      <*> pure (mkVkGenesisSet ngk)
      -- TODO: for now we're returning a constant set of parameters, where only '_bkSgnCntT' varies.
      <*> pure initialPParams { _bkSgnCntT = sigCntT }
      <*> pure k
    where
      -- If we want to generate large traces, we need to set up the value of the
      -- current slot to a sufficiently large value.
      gCurrentSlot = Slot <$> Gen.integral (Range.constant 32768 2147483648)

  sigGen _ = sigGenChain GenDelegation GenUTxO Nothing

data ShouldGenDelegation = GenDelegation | NoGenDelegation

data ShouldGenUTxO = GenUTxO | NoGenUTxO

sigGenChain
  :: ShouldGenDelegation
  -> ShouldGenUTxO
  -> Maybe (PredicateFailure CHAIN)
  -> Environment CHAIN
  -> State CHAIN
  -> Gen (Signal CHAIN)
sigGenChain
  shouldGenDelegation
  shouldGenUTxO
  _
  (_sNow, utxo0, ads, _pps, k)
  (Slot s, sgs, h, utxo, ds, us)
  = do
    -- We'd expect the slot increment to be close to 1, even for large Gen's
    -- size numbers.
    nextSlot <- Slot . (s +) <$> Gen.integral (Range.exponential 1 10)

    -- We need to generate delegation, update proposals, votes, and transactions
    -- after a potential update in the protocol parameters (which is triggered
    -- only at epoch boundaries). Otherwise the generators will use a state that
    -- won't hold when the rules that correspond to these generators are
    -- applied. For instance, the fees might change, which will render the
    -- transaction as invalid.
    --
    let (us', _) = applySTSIndifferently @EPOCH $ TRC ( (sEpoch (Slot s) k, k)
                                                      , us
                                                      , nextSlot
                                                      )

        pps' = protocolParameters us'

        upienv =
          ( Slot s
          , _dIStateDelegationMap ds
          , k
          , toNumberOfGenesisKeys $ Set.size ads
          )

        -- TODO: we might need to make the number of genesis keys a newtype, and
        -- provide this function in the same module where this newtype is
        -- defined.
        toNumberOfGenesisKeys n
          | fromIntegral (maxBound :: Word8) < n =
              error $ "sigGenChain: too many genesis keys: " ++ show  n
          | otherwise = fromIntegral n

    aBlockVersion <-
      Update.protocolVersionEndorsementGen upienv us'

    -- Here we do not want to shrink the issuer, since @Gen.element@ shrinks
    -- towards the first element of the list, which in this case won't provide
    -- us with better shrinks.
    vkI <- SigCntGen.issuer (pps', ds ^. dmsL, k) sgs

    delegationPayload <- case shouldGenDelegation of
      GenDelegation   ->
        -- In practice there won't be a delegation payload in every block, so we
        -- make this payload sparse.
        --
        -- NOTE: We arbitrarily chose to generate delegation payload in 30% of
        -- the cases. We could make this configurable.
        Gen.frequency
          [ (7, pure [])
          , (3,
              let dsEnv =
                    DSEnv
                      { _dSEnvAllowedDelegators = ads
                      , _dSEnvEpoch = sEpoch nextSlot k
                      , _dSEnvSlot = nextSlot
                      , _dSEnvK = k
                      }
              in dcertsGen dsEnv ds
            )
          ]
      NoGenDelegation -> pure []

    utxoPayload <- case shouldGenUTxO of
      GenUTxO   ->
        let utxoEnv = UTxOEnv utxo0 pps' in
          sigGen @UTXOWS Nothing utxoEnv utxo
      NoGenUTxO -> pure []

    (anOptionalUpdateProposal, aListOfVotes) <-
      Update.updateProposalAndVotesGen upienv us'

    let
      dummySig = Sig genesisHash (owner vkI)
      unsignedHeader = MkBlockHeader
        h
        nextSlot
        vkI
        dummySig -- Fill with a dummy signature first and then sign afterwards
        (hash utxoPayload)
        (hash delegationPayload)
        (hash (bb ^. bUpdProp, bb ^. bUpdVotes))

      signedHeader =
        unsignedHeader & bhSig .~ Sig (hashHeader unsignedHeader) (owner vkI)

      bb =
        BlockBody
          delegationPayload
          utxoPayload
          anOptionalUpdateProposal
          aListOfVotes
          aBlockVersion

    pure $ Block signedHeader bb
