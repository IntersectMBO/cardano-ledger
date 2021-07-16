{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- For the Field5 instance
{-# OPTIONS_GHC -Wno-orphans #-}

module Byron.Spec.Chain.STS.Rule.Chain where

import Byron.Spec.Chain.STS.Block
import Byron.Spec.Chain.STS.Rule.BBody
import Byron.Spec.Chain.STS.Rule.Epoch (EPOCH, sEpoch)
import Byron.Spec.Chain.STS.Rule.Pbft
import qualified Byron.Spec.Chain.STS.Rule.SigCnt as SigCntGen
import Byron.Spec.Ledger.Core
import qualified Byron.Spec.Ledger.Core.Generators as CoreGen
import Byron.Spec.Ledger.Delegation
import qualified Byron.Spec.Ledger.GlobalParams as GP
import Byron.Spec.Ledger.STS.UTXO (UTxOEnv (UTxOEnv, pps, utxo0), UTxOState)
import Byron.Spec.Ledger.STS.UTXOWS (UTXOWS)
import Byron.Spec.Ledger.UTxO (UTxO, mapUTxOValues)
import Byron.Spec.Ledger.Update hiding (delegationMap)
import qualified Byron.Spec.Ledger.Update as Update
import Control.State.Transition
import Control.State.Transition.Generator
import Data.Bimap (Bimap)
import Data.Bits (shift)
import Data.Data (Data, Typeable)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import GHC.Stack (HasCallStack)
import Hedgehog (Gen, MonadTest)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (CoverPercentage)
import qualified Hedgehog.Range as Range
import Lens.Micro (Lens', (&), (.~), (^.))
import Lens.Micro.Internal (Field1 (..), Field5 (..))
import Numeric.Natural (Natural)

data CHAIN deriving (Data, Typeable)

data ChainPredicateFailure
  = EpochFailure (PredicateFailure EPOCH)
  | HeaderSizeTooBig BlockHeader Natural (Threshold Natural)
  | BBodyFailure (PredicateFailure BBODY)
  | PBFTFailure (PredicateFailure PBFT)
  | MaximumBlockSize Natural Natural
  | LedgerDelegationFailure (PredicateFailure DELEG)
  | LedgerUTxOFailure (PredicateFailure UTXOWS)
  deriving (Eq, Show, Data, Typeable)

instance STS CHAIN where
  type
    Environment CHAIN =
      ( Slot, -- Current slot
        UTxO,
        -- Components needed to be able to bootstrap the traces generator. They are
        -- not part of the formal spec. These might be removed once we have decided
        -- on how do we want to model initial states.
        Set VKeyGenesis, -- Allowed delegators. Needed by the initial delegation rules. The number of
        -- genesis keys is the size of this set.
        PParams, -- Needed to bootstrap this part of the chain state,
        -- which is used in the delegation rules
        BlockCount -- Chain stability parameter
      )

  type
    State CHAIN =
      ( Slot,
        Seq VKeyGenesis,
        Hash,
        UTxOState,
        DIState,
        UPIState
      )

  type Signal CHAIN = Block

  type PredicateFailure CHAIN = ChainPredicateFailure

  initialRules =
    [ do
        IRC (_sNow, utxo0', ads, pps', k) <- judgmentContext
        let s0 = Slot 0
            -- Since we only test the delegation state we initialize the
            -- remaining fields of the update interface state to (m)empty. Once
            -- the formal spec includes initial rules for update we can use
            -- them here.
            upiState0 =
              ( (ProtVer 0 0 0, pps'),
                [],
                Map.empty,
                Map.empty,
                Map.empty,
                Map.empty,
                Set.empty,
                Set.empty,
                Map.empty
              )
        utxoSt0 <- trans @UTXOWS $ IRC UTxOEnv {utxo0 = utxo0', pps = pps'}
        let dsEnv =
              DSEnv
                { _dSEnvAllowedDelegators = ads,
                  _dSEnvEpoch = sEpoch s0 k,
                  _dSEnvSlot = s0,
                  _dSEnvK = k
                }
        ds <- trans @DELEG $ IRC dsEnv
        pure
          $! ( s0,
               [],
               genesisHash,
               utxoSt0,
               ds,
               upiState0
             )
    ]

  transitionRules =
    [ do
        TRC (_, _, b) <- judgmentContext
        case bIsEBB b of
          True -> isEBBRule
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
          trans @EPOCH $ TRC ((sEpoch sLast k, k), us, bSlot b)
        headerIsValid us' (b ^. bHeader)
        let ppsUs' = snd (us' ^. _1)
        (utxoSt', ds', us'') <-
          trans @BBODY $
            TRC
              ( ( ppsUs',
                  sEpoch (bSlot b) k,
                  utxoGenesis,
                  fromIntegral (Set.size ads),
                  k
                ),
                (utxoSt, ds, us'),
                b
              )
        (h', sgs') <-
          trans @PBFT $ TRC ((ppsUs', dm, sLast, sNow, k), (h, sgs), b ^. bHeader)
        pure $! (bSlot b, sgs', h', utxoSt', ds', us'')

instance Embed EPOCH CHAIN where
  wrapFailed = EpochFailure

instance Embed BBODY CHAIN where
  wrapFailed = BBodyFailure

instance Embed PBFT CHAIN where
  wrapFailed = PBFTFailure

instance Embed DELEG CHAIN where
  wrapFailed = LedgerDelegationFailure

instance Embed UTXOWS CHAIN where
  wrapFailed = LedgerUTxOFailure

isHeaderSizeTooBigFailure :: PredicateFailure CHAIN -> Bool
isHeaderSizeTooBigFailure (HeaderSizeTooBig _ _ _) = True
isHeaderSizeTooBigFailure _ = False

headerIsValid :: UPIState -> BlockHeader -> Rule CHAIN 'Transition ()
headerIsValid us bh = do
  let sMax = snd (us ^. _1) ^. maxHdrSz
  bHeaderSize bh <= sMax
    ?! HeaderSizeTooBig bh (bHeaderSize bh) (Threshold sMax)

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
      <*> (adjustUTxO . utxo0 <$> envGen @UTXOWS chainLength)
      <*> pure (mkVkGenesisSet ngk)
      -- TODO: for now we're returning a constant set of parameters, where only '_bkSgnCntT' varies.
      <*> pure initialPParams {_bkSgnCntT = Update.BkSgnCntT sigCntT}
      <*> pure k
    where
      -- If we want to generate large traces, we need to set up the value of the
      -- current slot to a sufficiently large value.
      gCurrentSlot = Slot <$> Gen.integral (Range.constant 32768 2147483648)

      -- The UTXOW generators assume the @b@ fee parameter to be @0 <= b <= 10@,
      -- and pick UTxO values that are correspondingly small. In order to allow
      -- for a difference in real transaction size between the spec and impl,
      -- however, the `initialParams` pick a value of @10 * GP.c@ for @b@
      -- instead. This @c@ correction factor allows real transactions to be
      -- @c@ times larger than the spec ones and still guarantee that the real
      -- transaction will have sufficient fees when the abstract tx does.
      -- If we don't apply this same correction factor to the generated UTxO,
      -- however, we will be unable to generate any /abstract/ transactions,
      -- because the UTxO values will not be large enough to cover tx fees.
      adjustUTxO :: UTxO -> UTxO
      adjustUTxO = mapUTxOValues $ \(Lovelace v) ->
        Lovelace $ v * fromIntegral GP.c

  sigGen = sigGenChain GenDelegation GenUTxO GenUpdate

data ShouldGenDelegation = GenDelegation | NoGenDelegation

data ShouldGenUTxO = GenUTxO | NoGenUTxO

data ShouldGenUpdate = GenUpdate | NoGenUpdate

sigGenChain ::
  ShouldGenDelegation ->
  ShouldGenUTxO ->
  ShouldGenUpdate ->
  Environment CHAIN ->
  State CHAIN ->
  Gen (Signal CHAIN)
sigGenChain
  shouldGenDelegation
  shouldGenUTxO
  shouldGenUpdate
  (_sNow, utxo0, ads, _pps, k)
  (Slot s, sgs, h, utxo, ds, us) =
    do
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
      let (us', _) =
            applySTSIndifferently @EPOCH $
              TRC
                ( (sEpoch (Slot s) k, k),
                  us,
                  nextSlot
                )

          pps' = protocolParameters us'

          upienv =
            ( Slot s,
              _dIStateDelegationMap ds,
              k,
              toNumberOfGenesisKeys $ Set.size ads
            )

          -- TODO: we might need to make the number of genesis keys a newtype, and
          -- provide this function in the same module where this newtype is
          -- defined.
          toNumberOfGenesisKeys n
            | fromIntegral (maxBound :: Word8) < n =
              error $ "sigGenChain: too many genesis keys: " ++ show n
            | otherwise = fromIntegral n

      aBlockVersion <-
        Update.protocolVersionEndorsementGen upienv us'

      -- Here we do not want to shrink the issuer, since @Gen.element@ shrinks
      -- towards the first element of the list, which in this case won't provide
      -- us with better shrinks.
      vkI <- SigCntGen.issuer (pps', ds ^. dmsL, k) sgs

      delegationPayload <-
        case shouldGenDelegation of
          GenDelegation ->
            -- In practice there won't be a delegation payload in every block, so we
            -- make this payload sparse.
            --
            -- NOTE: We arbitrarily chose to generate delegation payload in 30% of
            -- the cases. We could make this configurable.
            Gen.frequency
              [ (7, pure []),
                ( 3,
                  let dsEnv =
                        DSEnv
                          { _dSEnvAllowedDelegators = ads,
                            _dSEnvEpoch = sEpoch nextSlot k,
                            _dSEnvSlot = nextSlot,
                            _dSEnvK = k
                          }
                   in dcertsGen dsEnv ds
                )
              ]
          NoGenDelegation -> pure []

      utxoPayload <-
        case shouldGenUTxO of
          GenUTxO ->
            let utxoEnv = UTxOEnv utxo0 pps'
             in sigGen @UTXOWS utxoEnv utxo
          NoGenUTxO -> pure []

      (anOptionalUpdateProposal, aListOfVotes) <-
        case shouldGenUpdate of
          GenUpdate ->
            Update.updateProposalAndVotesGen upienv us'
          NoGenUpdate ->
            pure (Nothing, [])

      pure
        $! mkBlock
          h
          nextSlot
          vkI
          aBlockVersion
          delegationPayload
          anOptionalUpdateProposal
          aListOfVotes
          utxoPayload

-- | Produce an invalid hash for one of the three types of block payloads:
--
-- - Delegation
-- - Update
-- - UTxO
tamperWithPayloadHash ::
  Block -> Gen Block
tamperWithPayloadHash block = do
  hashLens <-
    Gen.element
      [ bhDlgHash,
        bhUpdHash,
        bhUtxoHash
      ]
  pure $! block & bHeader . hashLens .~ Hash Nothing

-- | Generate a block in which one of the three types of payload hashes:
--
-- - Delegation
-- - Update
-- - UTxO
--
-- is invalid.
invalidProofsBlockGen :: SignalGenerator CHAIN
invalidProofsBlockGen env st = sigGen @CHAIN env st >>= tamperWithPayloadHash

coverInvalidBlockProofs ::
  forall m a.
  ( MonadTest m,
    HasCallStack,
    Data a
  ) =>
  -- | Minimum percentage that each failure must occur.
  CoverPercentage ->
  -- | Structure containing the failures
  a ->
  m ()
coverInvalidBlockProofs coverPercentage =
  coverFailures @_ @BBODY
    coverPercentage
    [ InvalidDelegationHash,
      InvalidUpdateProposalHash,
      InvalidUtxoHash
    ]

--------------------------------------------------------------------------------
-- FieldX instances for a 6-tuple
--------------------------------------------------------------------------------

instance Field1 (a, b, c, d, e, f) (a', b, c, d, e, f) a a' where
  _1 k ~(a, b, c, d, e, f) = (\a' -> (a', b, c, d, e, f)) <$> k a
  {-# INLINE _1 #-}

instance Field5 (a, b, c, d, e, f) (a, b, c, d, e', f) e e' where
  _5 k ~(a, b, c, d, e, f) = (\e' -> (a, b, c, d, e', f)) <$> k e
  {-# INLINE _5 #-}
