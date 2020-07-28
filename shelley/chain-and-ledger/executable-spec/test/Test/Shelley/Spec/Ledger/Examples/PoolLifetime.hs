{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.PoolLifetime
  ( -- | = Pool Lifetime
    --
    -- Example demonstrating the creation of a new stake pool,
    -- block production under Praos, rewards, and pool retirement.
    --
    -- === Example 2A
    -- In the first block of this example, Alice, Bob, and Carl
    -- all register stake credentials, and Alice registers a stake pool.
    -- Additionall, a MIR certificate is issued to draw from the reserves
    -- and give Carl and Daria (who is unregistered) rewards.
    ex2A,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce,
    StrictMaybe (..),
  )
import Shelley.Spec.Ledger.BlockChain (Block)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Ptr (..))
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys (asWitness)
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.Slot
  ( BlockNo (..),
    SlotNo (..),
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Shelley.Spec.Ledger.TxData
  ( DCert (..),
    DelegCert (..),
    MIRCert (..),
    MIRPot (..),
    PoolCert (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Examples (CHAINExample (..))
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import Test.Shelley.Spec.Ledger.Examples.Combinators
  ( evolveNonceUnfrozen,
    feesAndDeposits,
    mir,
    newLab,
    newPool,
    newStakeCred,
    newUTxO,
  )
import Test.Shelley.Spec.Ledger.Examples.Federation
  ( coreNodeIssuerKeys,
    coreNodeKeysBySchedule,
  )
import Test.Shelley.Spec.Ledger.Examples.Init
  ( initSt,
    lastByronHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    NatNonce (..),
    genesisCoins,
    genesisId,
    mkBlock,
    mkOCert,
    zero,
  )
import Test.Shelley.Spec.Ledger.Utils (getBlockNonce)

aliceInitCoin :: Coin
aliceInitCoin = 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = 1 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: Crypto c => UTxO c
initUTxO =
  genesisCoins
    [ TxOut Cast.aliceAddr aliceInitCoin,
      TxOut Cast.bobAddr bobInitCoin
    ]

initStEx2 :: forall c. Crypto c => ChainState c
initStEx2 = initSt initUTxO

-- SET UP BLOCK

aliceCoinEx2A :: Coin
aliceCoinEx2A = aliceInitCoin - (_poolDeposit ppEx) - 3 * (_keyDeposit ppEx) - 3

carlMIR :: Coin
carlMIR = Coin 110

dariaMIR :: Coin
dariaMIR = Coin 99

feeTx2A :: Coin
feeTx2A = Coin 3

txbodyEx2A :: Crypto c => TxBody c
txbodyEx2A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr aliceCoinEx2A])
    ( StrictSeq.fromList
        ( [ DCertDeleg (RegKey Cast.aliceSHK),
            DCertDeleg (RegKey Cast.bobSHK),
            DCertDeleg (RegKey Cast.carlSHK),
            DCertPool (RegPool Cast.alicePoolParams)
          ]
            ++ [ DCertMir
                   ( MIRCert
                       ReservesMIR
                       ( Map.fromList
                           [ (Cast.carlSHK, carlMIR),
                             (Cast.dariaSHK, dariaMIR)
                           ]
                       )
                   )
               ]
        )
    )
    (Wdrl Map.empty)
    feeTx2A
    (SlotNo 10)
    SNothing
    SNothing

txEx2A :: forall c. Mock c => Tx c
txEx2A =
  Tx
    txbodyEx2A
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx2A)
            ( (asWitness <$> [Cast.alicePay, Cast.carlPay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ cold Cast.alicePoolKeys]
                <> ( asWitness
                       <$> [ cold (coreNodeIssuerKeys 0),
                             cold (coreNodeIssuerKeys 1),
                             cold (coreNodeIssuerKeys 2),
                             cold (coreNodeIssuerKeys 3),
                             cold (coreNodeIssuerKeys 4)
                           ]
                   )
            )
      }
    SNothing

blockEx2A :: forall c. (HasCallStack, Mock c) => Block c
blockEx2A =
  mkBlock
    lastByronHeaderHash
    (coreNodeKeysBySchedule ppEx 10)
    [txEx2A]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @c)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (coreNodeKeysBySchedule ppEx 10) 0 (KESPeriod 0))

blockNonce2A :: forall c. (HasCallStack, Mock c) => Nonce
blockNonce2A = getBlockNonce (blockEx2A @c)

expectedStEx2A :: forall c. Mock c => ChainState c
expectedStEx2A =
  (evolveNonceUnfrozen (blockNonce2A @c))
    . (newLab blockEx2A)
    . (feesAndDeposits feeTx2A (_keyDeposit ppEx * 3 + _poolDeposit ppEx))
    . (newUTxO txbodyEx2A)
    . (newStakeCred Cast.aliceSHK (Ptr (SlotNo 10) 0 0))
    . (newStakeCred Cast.bobSHK (Ptr (SlotNo 10) 0 1))
    . (newStakeCred Cast.carlSHK (Ptr (SlotNo 10) 0 2))
    . (newPool Cast.alicePoolParams)
    . (mir Cast.carlSHK ReservesMIR carlMIR)
    . (mir Cast.dariaSHK ReservesMIR dariaMIR)
    $ initStEx2

ex2A :: Mock c => CHAINExample c
ex2A = CHAINExample initStEx2 blockEx2A (Right expectedStEx2A)
