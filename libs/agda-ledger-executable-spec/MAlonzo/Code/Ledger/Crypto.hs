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

module MAlonzo.Code.Ledger.Crypto where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Interface.Hashable
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Ledger.Crypto.isHashableSet
d_isHashableSet_6 a0 = ()
data T_isHashableSet_6
  = C_isHashableSet'46'constructor_35 MAlonzo.Code.Interface.Hashable.T_Hashable_8
                                      MAlonzo.Code.Interface.DecEq.T_DecEq_14
-- Ledger.Crypto.isHashableSet.THash
d_THash_16 :: T_isHashableSet_6 -> ()
d_THash_16 = erased
-- Ledger.Crypto.isHashableSet.T-Hashable
d_T'45'Hashable_18 ::
  T_isHashableSet_6 -> MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_T'45'Hashable_18 v0
  = case coe v0 of
      C_isHashableSet'46'constructor_35 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.isHashableSet.DecEq-THash
d_DecEq'45'THash_20 ::
  T_isHashableSet_6 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_20 v0
  = case coe v0 of
      C_isHashableSet'46'constructor_35 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.mkIsHashableSet
d_mkIsHashableSet_26 ::
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> T_isHashableSet_6
d_mkIsHashableSet_26 ~v0 ~v1 v2 v3 = du_mkIsHashableSet_26 v2 v3
du_mkIsHashableSet_26 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> T_isHashableSet_6
du_mkIsHashableSet_26 v0 v1
  = coe
      C_isHashableSet'46'constructor_35
      (coe
         MAlonzo.Code.Interface.Hashable.C_Hashable'46'constructor_9
         (coe v0))
      v1
-- Ledger.Crypto.HashableSet
d_HashableSet_52 = ()
newtype T_HashableSet_52
  = C_HashableSet'46'constructor_535 T_isHashableSet_6
-- Ledger.Crypto.HashableSet.T
d_T_58 :: T_HashableSet_52 -> ()
d_T_58 = erased
-- Ledger.Crypto.HashableSet.T-isHashable
d_T'45'isHashable_60 :: T_HashableSet_52 -> T_isHashableSet_6
d_T'45'isHashable_60 v0
  = case coe v0 of
      C_HashableSet'46'constructor_535 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.HashableSet._.DecEq-THash
d_DecEq'45'THash_64 ::
  T_HashableSet_52 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_64 v0
  = coe d_DecEq'45'THash_20 (coe d_T'45'isHashable_60 (coe v0))
-- Ledger.Crypto.HashableSet._.T-Hashable
d_T'45'Hashable_66 ::
  T_HashableSet_52 -> MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_T'45'Hashable_66 v0
  = coe d_T'45'Hashable_18 (coe d_T'45'isHashable_60 (coe v0))
-- Ledger.Crypto.HashableSet._.THash
d_THash_68 :: T_HashableSet_52 -> ()
d_THash_68 = erased
-- Ledger.Crypto.PKKScheme
d_PKKScheme_70 = ()
data T_PKKScheme_70
  = C_PKKScheme'46'constructor_2217 (AgdaAny -> AgdaAny -> AgdaAny)
                                    (AgdaAny ->
                                     AgdaAny ->
                                     AgdaAny ->
                                     MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
                                    (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
                                     AgdaAny ->
                                     AgdaAny ->
                                     MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny)
                                    MAlonzo.Code.Interface.DecEq.T_DecEq_14
                                    MAlonzo.Code.Interface.DecEq.T_DecEq_14
                                    MAlonzo.Code.Interface.DecEq.T_DecEq_14
-- Ledger.Crypto.PKKScheme.SKey
d_SKey_118 :: T_PKKScheme_70 -> ()
d_SKey_118 = erased
-- Ledger.Crypto.PKKScheme.VKey
d_VKey_120 :: T_PKKScheme_70 -> ()
d_VKey_120 = erased
-- Ledger.Crypto.PKKScheme.Sig
d_Sig_122 :: T_PKKScheme_70 -> ()
d_Sig_122 = erased
-- Ledger.Crypto.PKKScheme.Ser
d_Ser_124 :: T_PKKScheme_70 -> ()
d_Ser_124 = erased
-- Ledger.Crypto.PKKScheme.isKeyPair
d_isKeyPair_126 :: T_PKKScheme_70 -> AgdaAny -> AgdaAny -> ()
d_isKeyPair_126 = erased
-- Ledger.Crypto.PKKScheme.isSigned
d_isSigned_128 ::
  T_PKKScheme_70 -> AgdaAny -> AgdaAny -> AgdaAny -> ()
d_isSigned_128 = erased
-- Ledger.Crypto.PKKScheme.sign
d_sign_130 :: T_PKKScheme_70 -> AgdaAny -> AgdaAny -> AgdaAny
d_sign_130 v0
  = case coe v0 of
      C_PKKScheme'46'constructor_2217 v7 v8 v9 v10 v11 v12 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.PKKScheme.KeyPair
d_KeyPair_132 :: T_PKKScheme_70 -> ()
d_KeyPair_132 = erased
-- Ledger.Crypto.PKKScheme.isSigned?
d_isSigned'63'_144 ::
  T_PKKScheme_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isSigned'63'_144 v0
  = case coe v0 of
      C_PKKScheme'46'constructor_2217 v7 v8 v9 v10 v11 v12 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.PKKScheme.isSigned-correct
d_isSigned'45'correct_156 ::
  T_PKKScheme_70 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_isSigned'45'correct_156 v0
  = case coe v0 of
      C_PKKScheme'46'constructor_2217 v7 v8 v9 v10 v11 v12 -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.PKKScheme.decEq-VKey
d_decEq'45'VKey_158 ::
  T_PKKScheme_70 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'VKey_158 v0
  = case coe v0 of
      C_PKKScheme'46'constructor_2217 v7 v8 v9 v10 v11 v12 -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.PKKScheme.decEq-Sig
d_decEq'45'Sig_160 ::
  T_PKKScheme_70 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Sig_160 v0
  = case coe v0 of
      C_PKKScheme'46'constructor_2217 v7 v8 v9 v10 v11 v12 -> coe v11
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.PKKScheme.decEq-Ser
d_decEq'45'Ser_162 ::
  T_PKKScheme_70 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Ser_162 v0
  = case coe v0 of
      C_PKKScheme'46'constructor_2217 v7 v8 v9 v10 v11 v12 -> coe v12
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.Crypto
d_Crypto_164 = ()
data T_Crypto_164
  = C_Crypto'46'constructor_2767 T_PKKScheme_70 T_isHashableSet_6
                                 MAlonzo.Code.Interface.DecEq.T_DecEq_14
-- Ledger.Crypto._.KeyPair
d_KeyPair_170 :: T_PKKScheme_70 -> ()
d_KeyPair_170 = erased
-- Ledger.Crypto._.SKey
d_SKey_172 :: T_PKKScheme_70 -> ()
d_SKey_172 = erased
-- Ledger.Crypto._.Ser
d_Ser_174 :: T_PKKScheme_70 -> ()
d_Ser_174 = erased
-- Ledger.Crypto._.Sig
d_Sig_176 :: T_PKKScheme_70 -> ()
d_Sig_176 = erased
-- Ledger.Crypto._.VKey
d_VKey_178 :: T_PKKScheme_70 -> ()
d_VKey_178 = erased
-- Ledger.Crypto._.decEq-Ser
d_decEq'45'Ser_180 ::
  T_PKKScheme_70 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Ser_180 v0 = coe d_decEq'45'Ser_162 (coe v0)
-- Ledger.Crypto._.decEq-Sig
d_decEq'45'Sig_182 ::
  T_PKKScheme_70 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Sig_182 v0 = coe d_decEq'45'Sig_160 (coe v0)
-- Ledger.Crypto._.decEq-VKey
d_decEq'45'VKey_184 ::
  T_PKKScheme_70 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'VKey_184 v0 = coe d_decEq'45'VKey_158 (coe v0)
-- Ledger.Crypto._.isKeyPair
d_isKeyPair_186 :: T_PKKScheme_70 -> AgdaAny -> AgdaAny -> ()
d_isKeyPair_186 = erased
-- Ledger.Crypto._.isSigned
d_isSigned_188 ::
  T_PKKScheme_70 -> AgdaAny -> AgdaAny -> AgdaAny -> ()
d_isSigned_188 = erased
-- Ledger.Crypto._.isSigned-correct
d_isSigned'45'correct_190 ::
  T_PKKScheme_70 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_isSigned'45'correct_190 v0
  = coe d_isSigned'45'correct_156 (coe v0)
-- Ledger.Crypto._.isSigned?
d_isSigned'63'_192 ::
  T_PKKScheme_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isSigned'63'_192 v0 = coe d_isSigned'63'_144 (coe v0)
-- Ledger.Crypto._.sign
d_sign_194 :: T_PKKScheme_70 -> AgdaAny -> AgdaAny -> AgdaAny
d_sign_194 v0 = coe d_sign_130 (coe v0)
-- Ledger.Crypto._.DecEq-THash
d_DecEq'45'THash_202 ::
  T_PKKScheme_70 ->
  T_isHashableSet_6 -> () -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_202 ~v0 v1 ~v2 = du_DecEq'45'THash_202 v1
du_DecEq'45'THash_202 ::
  T_isHashableSet_6 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'THash_202 v0 = coe d_DecEq'45'THash_20 (coe v0)
-- Ledger.Crypto._.THash
d_THash_204 :: T_PKKScheme_70 -> T_isHashableSet_6 -> () -> ()
d_THash_204 = erased
-- Ledger.Crypto._.T-Hashable
d_T'45'Hashable_206 ::
  T_PKKScheme_70 ->
  T_isHashableSet_6 ->
  () -> MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_T'45'Hashable_206 ~v0 v1 ~v2 = du_T'45'Hashable_206 v1
du_T'45'Hashable_206 ::
  T_isHashableSet_6 -> MAlonzo.Code.Interface.Hashable.T_Hashable_8
du_T'45'Hashable_206 v0 = coe d_T'45'Hashable_18 (coe v0)
-- Ledger.Crypto.Crypto.pkk
d_pkk_210 :: T_Crypto_164 -> T_PKKScheme_70
d_pkk_210 v0
  = case coe v0 of
      C_Crypto'46'constructor_2767 v1 v2 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.Crypto._.KeyPair
d_KeyPair_214 :: T_Crypto_164 -> ()
d_KeyPair_214 = erased
-- Ledger.Crypto.Crypto._.SKey
d_SKey_216 :: T_Crypto_164 -> ()
d_SKey_216 = erased
-- Ledger.Crypto.Crypto._.Ser
d_Ser_218 :: T_Crypto_164 -> ()
d_Ser_218 = erased
-- Ledger.Crypto.Crypto._.Sig
d_Sig_220 :: T_Crypto_164 -> ()
d_Sig_220 = erased
-- Ledger.Crypto.Crypto._.VKey
d_VKey_222 :: T_Crypto_164 -> ()
d_VKey_222 = erased
-- Ledger.Crypto.Crypto._.decEq-Ser
d_decEq'45'Ser_224 ::
  T_Crypto_164 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Ser_224 v0
  = coe d_decEq'45'Ser_162 (coe d_pkk_210 (coe v0))
-- Ledger.Crypto.Crypto._.decEq-Sig
d_decEq'45'Sig_226 ::
  T_Crypto_164 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Sig_226 v0
  = coe d_decEq'45'Sig_160 (coe d_pkk_210 (coe v0))
-- Ledger.Crypto.Crypto._.decEq-VKey
d_decEq'45'VKey_228 ::
  T_Crypto_164 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'VKey_228 v0
  = coe d_decEq'45'VKey_158 (coe d_pkk_210 (coe v0))
-- Ledger.Crypto.Crypto._.isKeyPair
d_isKeyPair_230 :: T_Crypto_164 -> AgdaAny -> AgdaAny -> ()
d_isKeyPair_230 = erased
-- Ledger.Crypto.Crypto._.isSigned
d_isSigned_232 ::
  T_Crypto_164 -> AgdaAny -> AgdaAny -> AgdaAny -> ()
d_isSigned_232 = erased
-- Ledger.Crypto.Crypto._.isSigned-correct
d_isSigned'45'correct_234 ::
  T_Crypto_164 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_isSigned'45'correct_234 v0
  = coe d_isSigned'45'correct_156 (coe d_pkk_210 (coe v0))
-- Ledger.Crypto.Crypto._.isSigned?
d_isSigned'63'_236 ::
  T_Crypto_164 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isSigned'63'_236 v0
  = coe d_isSigned'63'_144 (coe d_pkk_210 (coe v0))
-- Ledger.Crypto.Crypto._.sign
d_sign_238 :: T_Crypto_164 -> AgdaAny -> AgdaAny -> AgdaAny
d_sign_238 v0 = coe d_sign_130 (coe d_pkk_210 (coe v0))
-- Ledger.Crypto.Crypto.khs
d_khs_240 :: T_Crypto_164 -> T_isHashableSet_6
d_khs_240 v0
  = case coe v0 of
      C_Crypto'46'constructor_2767 v1 v2 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Crypto.Crypto.ScriptHash
d_ScriptHash_242 :: T_Crypto_164 -> ()
d_ScriptHash_242 = erased
-- Ledger.Crypto.Crypto._.DecEq-THash
d_DecEq'45'THash_246 ::
  T_Crypto_164 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_246 v0
  = coe d_DecEq'45'THash_20 (coe d_khs_240 (coe v0))
-- Ledger.Crypto.Crypto._.THash
d_THash_248 :: T_Crypto_164 -> ()
d_THash_248 = erased
-- Ledger.Crypto.Crypto._.T-Hashable
d_T'45'Hashable_250 ::
  T_Crypto_164 -> MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_T'45'Hashable_250 v0
  = coe d_T'45'Hashable_18 (coe d_khs_240 (coe v0))
-- Ledger.Crypto.Crypto.decEq-ScriptHash
d_decEq'45'ScriptHash_252 ::
  T_Crypto_164 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'ScriptHash_252 v0
  = case coe v0 of
      C_Crypto'46'constructor_2767 v1 v2 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
