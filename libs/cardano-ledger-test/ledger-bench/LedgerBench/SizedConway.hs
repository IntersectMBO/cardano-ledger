{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Conway-era sized benchmark environments.
--
-- == Purpose
--
-- This module produces hand-crafted Conway transactions whose size (and
-- therefore validation cost) is precisely controlled.  Unlike the trace
-- benchmark in "LedgerBench.Conway", these transactions have no Plutus
-- scripts — every byte of transaction data corresponds directly to an
-- Ed25519 key witness that the ledger must verify.
--
-- == Why scale by inputs and witnesses, not outputs?
--
-- Outputs are cheap to validate: the ledger checks that each address is
-- well-formed and that the output values sum correctly, but there is no
-- cryptographic work per output.
--
-- Inputs require __Ed25519 signature verification__ — one
-- @verifySignature@ call per key-hash input.  This is the dominant CPU
-- cost in non-script transaction validation.  Scaling the number of inputs
-- (and their corresponding witnesses) therefore gives a direct, realistic
-- measure of the ledger's cryptographic throughput.
--
-- Each input also forces a UTxO map lookup (O(log M) in the total map size).
--
-- == Transaction structure
--
--   * N inputs:  each spends a distinct UTxO entry owned by a distinct key pair.
--   * N witnesses: one Ed25519 (vkey, signature) pair per input.
--   * 1 output:  a single output collecting all value minus the fee.
--   * Real fee:  computed from the mainnet-like fee formula applied to the
--                actual serialised transaction size (see note below).
--
-- == Fee calculation
--
-- The fee is computed in two passes to break the chicken-and-egg dependency
-- between fee, transaction size, and witness signing:
--
--   1. Build the transaction with @fee = feeFixed@ (155 381) as a placeholder
--      and sign it.  Using @feeFixed@ instead of @0@ is important: the real
--      fee is always ≥ @feeFixed@, so both values encode to the same 5-byte
--      CBOR uint (major type 0, 4-byte uint), giving the same serialised
--      size in both passes.
--   2. Compute @fee = ppTxFeeFixed + ppTxFeePerByte × size@.
--   3. Rebuild the transaction body with the real fee and re-sign.
--
-- Protocol parameters used (mainnet-like values):
--
--   * @ppTxFeeFixed    = 155 381 lovelace@
--   * @ppTxFeePerByte  =      44 lovelace\/byte@
--   * @ppCoinsPerUTxOByte = 4 310 lovelace\/byte@  (min-UTxO check)
--   * @ppMaxValSize    = 5 000 bytes@
--
-- == Size targets
--
-- Each additional input + witness adds approximately 138 bytes:
-- ~38 B for the TxIn (32 B TxId + CBOR framing) and ~100 B for the witness
-- (32 B vkey + 64 B signature + CBOR framing).  The base transaction
-- (1 input, 1 output, 1 witness) serialises to ~200 B.
--
-- +----------+---------+------------------+
-- | Size tag | Inputs  | Approx bytes     |
-- +----------+---------+------------------+
-- | Small    |       1 |  ~200 B          |
-- | Medium   |      10 | ~1 430 B         |
-- | Large    |     116 | ~16 000 B        |
-- +----------+---------+------------------+
module LedgerBench.SizedConway (
  SizedTx (..),
  numInputsForSize,
  generateSizedConwayApplyTxEnv,
) where

import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.PParams (ppMaxValSizeL)
import Cardano.Ledger.Babbage.Core (ppCoinsPerUTxOByteL)
import Cardano.Ledger.BaseTypes (Inject (inject), Network (Testnet), ProtVer (..), natVersion)
import Cardano.Ledger.Coin (Coin (..), CoinPerByte (..), compactCoinOrError)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (ppProtocolVersionL)
import Cardano.Ledger.Core (
  EraTx (witsTxL),
  EraTxBody (feeTxBodyL, inputsTxBodyL, outputsTxBodyL),
  EraTxWits (addrTxWitsL),
  TopTx,
  Tx,
  mkBasicTx,
  mkBasicTxBody,
  mkBasicTxOut,
  ppMaxTxSizeL,
  ppTxFeeFixedL,
  ppTxFeePerByteL,
  txIdTxBody,
 )
import Cardano.Ledger.Credential (Credential (KeyHashObj), StakeReference (StakeRefNull))
import Cardano.Ledger.Keys (hashKey)
import Cardano.Ledger.Shelley.API (LedgerEnv (..))
import Cardano.Ledger.Shelley.Core (EraGov (emptyGovState))
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxO (..), smartUTxOState)
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.State (ChainAccountState (..))
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Data.Default (def)
import LedgerBench.Conway (ApplyTxEnv (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkKeyPair, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, testGlobals)

-- | The three transaction size categories.
data SizedTx = Small | Medium | Large
  deriving (Show, Eq, Ord)

-- | Number of inputs (and therefore witnesses) for each size category.
numInputsForSize :: SizedTx -> Int
numInputsForSize Small = 1
numInputsForSize Medium = 10
numInputsForSize Large = 116

-- | Mainnet-like fixed fee component (lovelace).
feeFixed :: Integer
feeFixed = 155_381

-- | Mainnet-like per-byte fee component (lovelace/byte).
feePerByte :: Integer
feePerByte = 44

-- | Build a Conway 'ApplyTxEnv' containing a hand-crafted transaction of
-- the requested size, with a realistic fee computed from the mainnet fee
-- formula.
--
-- The transaction is constructed deterministically from @seed@:
--
-- 1. __Key pairs__: N key pairs are derived from the seed using 'mkKeyPair'.
--    Each pair is used both to define a UTxO entry (the public key hash
--    becomes the spending credential) and to produce a witness (the private
--    key signs the transaction body hash).
--
-- 2. __UTxO__: N spendable UTxO entries are inserted, one per key pair, each
--    holding 2 ADA.  An additional @backgroundUtxoSize@ dummy entries are
--    added to inflate the map to a realistic size without affecting
--    validation.  All entries use synthetic 'TxId' values ('mkDummySafeHash')
--    with non-overlapping index ranges.
--
-- 3. __Fee (two passes)__: the transaction is first built with @fee = feeFixed@
--    as a placeholder and signed to obtain the correct serialised byte count.
--    Using a non-zero placeholder that has the same CBOR encoding width as
--    the real fee ensures the measured size is exact.  The real fee is then
--    computed and the transaction is rebuilt with the fee deducted from the
--    output and re-signed.
--
-- 4. __Witnesses__: 'mkWitnessesVKey' signs the transaction body hash with
--    each of the N private keys, producing N Ed25519 (vkey, signature) pairs.
--    The ledger verifies all N signatures during 'applyTx'.
--
-- 5. __Protocol parameters__: mainnet-like values are used so the fee,
--    min-UTxO, and value-size checks reflect real-world conditions.
--    'ppMaxTxSizeL' is left at 'maxBound' so the large transaction is never
--    rejected on size alone (mainnet is 16 384 B; our large tx is ~16 000 B,
--    which would just fit, but we avoid the fragility).
generateSizedConwayApplyTxEnv :: SizedTx -> Int -> Int -> ApplyTxEnv ConwayEra
generateSizedConwayApplyTxEnv size seed backgroundUtxoSize =
  let n = numInputsForSize size

      -- N key pairs, each derived deterministically from the seed.
      -- Multiplying by 10_000 ensures key pair i does not collide with
      -- the "collect" key derived directly from the seed below.
      keyPairs = [mkKeyPair (seed * 10_000 + i) | i <- [0 .. n - 1]]

      -- Each key pair owns one UTxO entry via a key-hash enterprise address.
      creds = [KeyHashObj (hashKey (vKey kp)) | kp <- keyPairs]
      addrs = [Addr Testnet c StakeRefNull | c <- creds]

      -- Synthetic TxIn values: each uses a distinct dummy TxId so all N
      -- inputs are unique in the UTxO map.
      txIns = [mkTxInPartial (TxId (mkDummySafeHash i)) 0 | i <- [0 .. n - 1]]
      -- Each input holds 2 ADA.  Conway uses MaryValue (multi-asset), so
      -- 'inject' lifts a plain Coin into the multi-asset value type.
      -- 2 ADA gives enough headroom over the min-UTxO requirement (~849k
      -- lovelace for a typical output) after the fee is deducted.
      inputTxOuts = [mkBasicTxOut a (inject (Coin 2_000_000)) | a <- addrs]

      -- Background UTxO entries: never referenced by the transaction, purely
      -- to inflate the map size and make O(log M) lookups realistic.
      --
      -- TxIx is Word16 (max 65 535), so a single TxId can cover at most 65 536
      -- entries.  We pre-compute a small pool of TxIds — one per group of
      -- 65 536 entries — so that ceil(M / 65 536) hashes are needed instead
      -- of M.  For M = 1 000 000 this is 16 hashes instead of 1 000 000.
      --
      -- Pool indices start at n to guarantee no TxId collision with the
      -- spendable inputs (which use indices 0 .. n-1).  All background entries
      -- share one pre-built TxOut — their spending credential is irrelevant.
      bgGroupSize = 65_536
      numBgGroups = max 1 ((backgroundUtxoSize + bgGroupSize - 1) `div` bgGroupSize)
      bgTxIdPool  = [TxId (mkDummySafeHash (n + j)) | j <- [0 .. numBgGroups - 1]]
      bgAddr      = Addr Testnet (KeyHashObj (hashKey (vKey (mkKeyPair 0)))) StakeRefNull
      bgTxOut     = mkBasicTxOut bgAddr (inject (Coin 2_000_000))
      -- Each TxId covers up to bgGroupSize entries (TxIx is Word16, max 65535).
      -- Iterate over (tid, k) pairs directly to avoid O(numBgGroups) list
      -- indexing per entry.
      bgTxIns     = take backgroundUtxoSize
        [ mkTxInPartial tid (fromIntegral k)
        | tid <- bgTxIdPool
        , k   <- [0 .. bgGroupSize - 1]
        ]
      bgTxOuts = replicate backgroundUtxoSize bgTxOut

      utxoMap = Map.fromList (zip txIns inputTxOuts <> zip bgTxIns bgTxOuts)

      -- Total value available to the transaction (N × 2 ADA).
      totalInput = Coin (fromIntegral n * 2_000_000)
      -- Unwrap for arithmetic — Coin has no Num instance.
      Coin totalLovelace = totalInput

      -- The single output collects all value into a fresh address owned by a
      -- key derived directly from the seed.
      collectAddr = Addr Testnet (KeyHashObj (hashKey (vKey (mkKeyPair seed)))) StakeRefNull

      -- Protocol parameters: mainnet-like values so fee and min-UTxO checks
      -- reflect real-world conditions.
      pp =
        def
          -- Fee formula: fee = 155 381 + 44 × txSizeBytes
          & ppTxFeePerByteL .~ CoinPerByte (compactCoinOrError (Coin feePerByte))
          & ppTxFeeFixedL .~ Coin feeFixed
          -- Minimum UTxO value in Conway is expressed as lovelace per byte
          -- of the serialised UTxO entry (~4 310 lovelace/byte on mainnet).
          -- All our outputs hold ≥ 2 ADA which comfortably exceeds this.
          & ppCoinsPerUTxOByteL .~ CoinPerByte (compactCoinOrError (Coin 4_310))
          -- Maximum serialised size of a multi-asset Value field (5 000 B on
          -- mainnet).  Our outputs contain plain Coin values (~9 B), well
          -- within this limit.
          & ppMaxValSizeL .~ 5_000
          -- Leave max-tx-size uncapped so the large transaction (~16 000 B)
          -- is never rejected on size alone.
          & ppMaxTxSizeL .~ maxBound
          -- Pin to Conway protocol version so Conway-specific STS rules are
          -- active during validation.
          & ppProtocolVersionL .~ ProtVer (natVersion @10) 0

      -- ── Pass 1: build the transaction with a placeholder fee ────────────
      -- We need the serialised size to compute the real fee, but the size
      -- depends on the fee field value — a chicken-and-egg problem.
      -- Solution: use 'feeFixed' (155 381) as a placeholder.  This value
      -- encodes to 5 CBOR bytes (major type 0, 4-byte uint), the same
      -- encoding length as the real fee (which is always ≥ feeFixed).
      -- Because the fee and output fields are the same CBOR width in both
      -- passes, the serialised size of pass 1 equals that of pass 2
      -- exactly, and the formula produces the correct fee in a single shot.
      txBody0 =
        mkBasicTxBody
          & inputsTxBodyL .~ Set.fromList txIns
          & outputsTxBodyL
            .~ StrictSeq.singleton
              (mkBasicTxOut collectAddr (inject (Coin (totalLovelace - feeFixed))))
          & feeTxBodyL .~ Coin feeFixed
      witnessSet0 = mkWitnessesVKey (unTxId (txIdTxBody txBody0)) keyPairs
      tx0 :: Tx TopTx ConwayEra
      tx0 = mkBasicTx txBody0 & witsTxL . addrTxWitsL .~ witnessSet0

      -- ── Fee computation ──────────────────────────────────────────────────
      -- Apply the standard Cardano fee formula to the size of the pass-1 tx.
      fee =
        let sz = fromIntegral $ BL.length (Plain.serialize tx0)
         in Coin (feeFixed + feePerByte * sz)

      -- ── Pass 2: rebuild with the real fee ────────────────────────────────
      -- Deduct the fee from the collect output (value conservation:
      -- Σ inputs = Σ outputs + fee).  Re-sign because the body hash changed.
      -- Pattern-match to extract the Integer inside Coin (Coin has no Num
      -- instance, so arithmetic must be done on the wrapped Integer).
      Coin feeLovelace = fee
      changeLovelace =
        if totalLovelace - feeLovelace <= 0
          then
            error $
              "generateSizedConwayApplyTxEnv: fee ("
                <> show fee
                <> ") exceeds total input ("
                <> show totalInput
                <> "); increase input value or reduce n"
          else totalLovelace - feeLovelace
      txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ Set.fromList txIns
          & outputsTxBodyL
            .~ StrictSeq.singleton
              (mkBasicTxOut collectAddr (inject (Coin changeLovelace)))
          & feeTxBodyL .~ fee
      witnessSet = mkWitnessesVKey (unTxId (txIdTxBody txBody)) keyPairs
      tx = mkBasicTx txBody & witsTxL . addrTxWitsL .~ witnessSet

      -- Ledger state: N spendable UTxO entries plus any background entries.
      utxo = UTxO utxoMap
      utxoState = smartUTxOState pp utxo (Coin 0) (Coin 0) emptyGovState (Coin 0)
      lState = LedgerState utxoState def

      mempoolEnv =
        LedgerEnv
          { ledgerSlotNo = SlotNo 1_000
          , ledgerEpochNo = Nothing
          , ledgerIx = minBound
          , ledgerPp = pp
          , ledgerAccount = ChainAccountState (Coin 45_000_000_000) (Coin 45_000_000_000)
          }
   in ApplyTxEnv
        { ateGlobals = testGlobals
        , ateMempoolEnv = mempoolEnv
        , ateState = lState
        , ateTx = tx
        }
