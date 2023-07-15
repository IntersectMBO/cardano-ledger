{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.Ledger.Foreign.LedgerTypes where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Foreign.Haskell.Pair

data UTxOState = MkUTxOState { utxo :: UTxO, fees :: Coin } deriving Show
data UTxOEnv = MkUTxOEnv { slot :: Integer, pparams :: PParams } deriving Show
data PParams = MkPParams
  { a                :: Integer
  , b                :: Integer
  , maxBlockSize     :: Integer
  , maxTxSize        :: Integer
  , maxHeaderSize    :: Integer
  , maxValSize       :: Integer
  , minUTxOValue     :: Integer
  , poolDeposit      :: Integer
  , emax             :: Integer
  , pv               :: (Integer, Integer)
  , votingThresholds :: ()
  , minCCSize        :: Integer
  , ccTermLimit      :: Integer
  , govExpiration    :: Integer
  , govDeposit       :: Integer
  , drepDeposit      :: Integer
  , drepActivity     :: Integer
  } deriving Show
data Tx = MkTx { body :: TxBody, wits :: TxWitnesses, txAD :: Maybe () }
data TxWitnesses = MkTxWitnesses { vkSigs :: [(Integer, Integer)], scripts :: [AgdaEmpty] }
data TxBody = MkTxBody
  { txins  :: [TxIn]
  , txouts :: [(Ix, TxOut)]
  , txfee  :: Coin
  , txvldt :: (Maybe Integer, Maybe Integer)
  , txsize :: Integer
  , txid   :: TxId } deriving Show
type Coin  = Integer
type Addr  = Integer

type TxId  = Integer
type Ix    = Integer
type TxIn  = (TxId, Ix)
type TxOut = (Addr, Coin)
type UTxO  = [(TxIn, TxOut)]
data AgdaEmpty
-- Ledger.Foreign.LedgerTypes.Empty
d_Empty_6 = ()
type T_Empty_6 = AgdaEmpty
cover_Empty_6 :: AgdaEmpty -> ()
cover_Empty_6 x = case x of
-- Ledger.Foreign.LedgerTypes.HSMap
d_HSMap_8 :: () -> () -> ()
d_HSMap_8 = erased
-- Ledger.Foreign.LedgerTypes.Coin
d_Coin_14 :: ()
d_Coin_14 = erased
-- Ledger.Foreign.LedgerTypes.Addr
d_Addr_16 :: ()
d_Addr_16 = erased
-- Ledger.Foreign.LedgerTypes.TxId
d_TxId_18 :: ()
d_TxId_18 = erased
-- Ledger.Foreign.LedgerTypes.Ix
d_Ix_20 :: ()
d_Ix_20 = erased
-- Ledger.Foreign.LedgerTypes.Epoch
d_Epoch_22 :: ()
d_Epoch_22 = erased
-- Ledger.Foreign.LedgerTypes.AuxiliaryData
d_AuxiliaryData_24 :: ()
d_AuxiliaryData_24 = erased
-- Ledger.Foreign.LedgerTypes.Network
d_Network_26 :: ()
d_Network_26 = erased
-- Ledger.Foreign.LedgerTypes.TxIn
d_TxIn_28 :: ()
d_TxIn_28 = erased
-- Ledger.Foreign.LedgerTypes.TxOut
d_TxOut_30 :: ()
d_TxOut_30 = erased
-- Ledger.Foreign.LedgerTypes.UTxO
d_UTxO_32 :: ()
d_UTxO_32 = erased
-- Ledger.Foreign.LedgerTypes.TxBody
d_TxBody_34 = ()
type T_TxBody_34 = TxBody
pattern C_TxBody'46'constructor_203 a0 a1 a2 a3 a4 a5 = MkTxBody a0 a1 a2 a3 a4 a5
check_TxBody'46'constructor_203 ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    ()
    (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
       () () Integer Integer) ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    ()
    (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
       () () Integer
       (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
          () () Integer Integer)) ->
  Integer ->
  MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
    () () (MAlonzo.Code.Agda.Builtin.Maybe.T_Maybe_10 () Integer)
    (MAlonzo.Code.Agda.Builtin.Maybe.T_Maybe_10 () Integer) ->
  Integer -> Integer -> T_TxBody_34
check_TxBody'46'constructor_203 = MkTxBody
cover_TxBody_34 :: TxBody -> ()
cover_TxBody_34 x
  = case x of
      MkTxBody _ _ _ _ _ _ -> ()
-- Ledger.Foreign.LedgerTypes.TxBody.txins
d_txins_48 ::
  T_TxBody_34 ->
  [MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
     AgdaAny AgdaAny Integer Integer]
d_txins_48 v0
  = case coe v0 of
      C_TxBody'46'constructor_203 v1 v2 v3 v4 v5 v6 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.TxBody.txouts
d_txouts_50 ::
  T_TxBody_34 ->
  [MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
     AgdaAny AgdaAny Integer
     (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
        AgdaAny AgdaAny Integer Integer)]
d_txouts_50 v0
  = case coe v0 of
      C_TxBody'46'constructor_203 v1 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.TxBody.txfee
d_txfee_52 :: T_TxBody_34 -> Integer
d_txfee_52 v0
  = case coe v0 of
      C_TxBody'46'constructor_203 v1 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.TxBody.txvldt
d_txvldt_54 ::
  T_TxBody_34 ->
  MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
    AgdaAny AgdaAny (Maybe Integer) (Maybe Integer)
d_txvldt_54 v0
  = case coe v0 of
      C_TxBody'46'constructor_203 v1 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.TxBody.txsize
d_txsize_56 :: T_TxBody_34 -> Integer
d_txsize_56 v0
  = case coe v0 of
      C_TxBody'46'constructor_203 v1 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.TxBody.txid
d_txid_58 :: T_TxBody_34 -> Integer
d_txid_58 v0
  = case coe v0 of
      C_TxBody'46'constructor_203 v1 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.TxWitnesses
d_TxWitnesses_60 = ()
type T_TxWitnesses_60 = TxWitnesses
pattern C_TxWitnesses'46'constructor_331 a0 a1 = MkTxWitnesses a0 a1
check_TxWitnesses'46'constructor_331 ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    ()
    (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
       () () Integer Integer) ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () T_Empty_6 ->
  T_TxWitnesses_60
check_TxWitnesses'46'constructor_331 = MkTxWitnesses
cover_TxWitnesses_60 :: TxWitnesses -> ()
cover_TxWitnesses_60 x
  = case x of
      MkTxWitnesses _ _ -> ()
-- Ledger.Foreign.LedgerTypes.TxWitnesses.vkSigs
d_vkSigs_66 ::
  T_TxWitnesses_60 ->
  [MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
     AgdaAny AgdaAny Integer Integer]
d_vkSigs_66 v0
  = case coe v0 of
      C_TxWitnesses'46'constructor_331 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.TxWitnesses.scripts
d_scripts_68 :: T_TxWitnesses_60 -> [T_Empty_6]
d_scripts_68 v0
  = case coe v0 of
      C_TxWitnesses'46'constructor_331 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.Tx
d_Tx_70 = ()
type T_Tx_70 = Tx
pattern C_Tx'46'constructor_391 a0 a1 a2 = MkTx a0 a1 a2
check_Tx'46'constructor_391 ::
  T_TxBody_34 ->
  T_TxWitnesses_60 ->
  MAlonzo.Code.Agda.Builtin.Maybe.T_Maybe_10
    () MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6 ->
  T_Tx_70
check_Tx'46'constructor_391 = MkTx
cover_Tx_70 :: Tx -> ()
cover_Tx_70 x
  = case x of
      MkTx _ _ _ -> ()
-- Ledger.Foreign.LedgerTypes.Tx.body
d_body_78 :: T_Tx_70 -> T_TxBody_34
d_body_78 v0
  = case coe v0 of
      C_Tx'46'constructor_391 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.Tx.wits
d_wits_80 :: T_Tx_70 -> T_TxWitnesses_60
d_wits_80 v0
  = case coe v0 of
      C_Tx'46'constructor_391 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.Tx.txAD
d_txAD_82 ::
  T_Tx_70 -> Maybe MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_txAD_82 v0
  = case coe v0 of
      C_Tx'46'constructor_391 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams
d_PParams_84 = ()
type T_PParams_84 = PParams
pattern C_PParams'46'constructor_827 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 = MkPParams a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16
check_PParams'46'constructor_827 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
    () () Integer Integer ->
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6 ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> Integer -> T_PParams_84
check_PParams'46'constructor_827 = MkPParams
cover_PParams_84 :: PParams -> ()
cover_PParams_84 x
  = case x of
      MkPParams _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> ()
-- Ledger.Foreign.LedgerTypes.PParams.a
d_a_120 :: T_PParams_84 -> Integer
d_a_120 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.b
d_b_122 :: T_PParams_84 -> Integer
d_b_122 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.maxBlockSize
d_maxBlockSize_124 :: T_PParams_84 -> Integer
d_maxBlockSize_124 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.maxTxSize
d_maxTxSize_126 :: T_PParams_84 -> Integer
d_maxTxSize_126 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.maxHeaderSize
d_maxHeaderSize_128 :: T_PParams_84 -> Integer
d_maxHeaderSize_128 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.maxValSize
d_maxValSize_130 :: T_PParams_84 -> Integer
d_maxValSize_130 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.minUTxOValue
d_minUTxOValue_132 :: T_PParams_84 -> Integer
d_minUTxOValue_132 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.poolDeposit
d_poolDeposit_134 :: T_PParams_84 -> Integer
d_poolDeposit_134 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.Emax
d_Emax_136 :: T_PParams_84 -> Integer
d_Emax_136 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.pv
d_pv_138 ::
  T_PParams_84 ->
  MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
    AgdaAny AgdaAny Integer Integer
d_pv_138 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.votingThresholds
d_votingThresholds_140 ::
  T_PParams_84 -> MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_votingThresholds_140 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v11
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.minCCSize
d_minCCSize_142 :: T_PParams_84 -> Integer
d_minCCSize_142 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v12
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.ccTermLimit
d_ccTermLimit_144 :: T_PParams_84 -> Integer
d_ccTermLimit_144 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v13
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.govExpiration
d_govExpiration_146 :: T_PParams_84 -> Integer
d_govExpiration_146 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v14
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.govDeposit
d_govDeposit_148 :: T_PParams_84 -> Integer
d_govDeposit_148 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v15
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.drepDeposit
d_drepDeposit_150 :: T_PParams_84 -> Integer
d_drepDeposit_150 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v16
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.PParams.drepActivity
d_drepActivity_152 :: T_PParams_84 -> Integer
d_drepActivity_152 v0
  = case coe v0 of
      C_PParams'46'constructor_827 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v17
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.UTxOEnv
d_UTxOEnv_154 = ()
type T_UTxOEnv_154 = UTxOEnv
pattern C_UTxOEnv'46'constructor_1469 a0 a1 = MkUTxOEnv a0 a1
check_UTxOEnv'46'constructor_1469 ::
  Integer -> T_PParams_84 -> T_UTxOEnv_154
check_UTxOEnv'46'constructor_1469 = MkUTxOEnv
cover_UTxOEnv_154 :: UTxOEnv -> ()
cover_UTxOEnv_154 x
  = case x of
      MkUTxOEnv _ _ -> ()
-- Ledger.Foreign.LedgerTypes.UTxOEnv.slot
d_slot_160 :: T_UTxOEnv_154 -> Integer
d_slot_160 v0
  = case coe v0 of
      C_UTxOEnv'46'constructor_1469 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.UTxOEnv.pparams
d_pparams_162 :: T_UTxOEnv_154 -> T_PParams_84
d_pparams_162 v0
  = case coe v0 of
      C_UTxOEnv'46'constructor_1469 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.UTxOState
d_UTxOState_164 = ()
type T_UTxOState_164 = UTxOState
pattern C_UTxOState'46'constructor_1511 a0 a1 = MkUTxOState a0 a1
check_UTxOState'46'constructor_1511 ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    ()
    (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
       () ()
       (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22 () () Integer Integer)
       (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
          () () Integer Integer)) ->
  Integer -> T_UTxOState_164
check_UTxOState'46'constructor_1511 = MkUTxOState
cover_UTxOState_164 :: UTxOState -> ()
cover_UTxOState_164 x
  = case x of
      MkUTxOState _ _ -> ()
-- Ledger.Foreign.LedgerTypes.UTxOState.utxo
d_utxo_170 ::
  T_UTxOState_164 ->
  [MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
     AgdaAny AgdaAny
     (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
        AgdaAny AgdaAny Integer Integer)
     (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
        AgdaAny AgdaAny Integer Integer)]
d_utxo_170 v0
  = case coe v0 of
      C_UTxOState'46'constructor_1511 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.LedgerTypes.UTxOState.fees
d_fees_172 :: T_UTxOState_164 -> Integer
d_fees_172 v0
  = case coe v0 of
      C_UTxOState'46'constructor_1511 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
