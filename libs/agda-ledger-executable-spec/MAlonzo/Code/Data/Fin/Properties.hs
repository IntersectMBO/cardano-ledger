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

module MAlonzo.Code.Data.Fin.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Empty
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Effect.Applicative
import qualified MAlonzo.Code.Effect.Functor
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Core
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects
import qualified MAlonzo.Code.Relation.Unary.Properties

-- Data.Fin.Properties.¬Fin0
d_'172'Fin0_20 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'172'Fin0_20 = erased
-- Data.Fin.Properties.0↔⊥
d_0'8596''8869'_22 :: MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_0'8596''8869'_22
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386 erased erased
      erased erased
-- Data.Fin.Properties.1↔⊤
d_1'8596''8868'_24 :: MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_1'8596''8868'_24
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe (\ v0 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      (coe
         (\ v0 -> seq (coe v0) (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)))
      erased erased
-- Data.Fin.Properties.2↔Bool
d_2'8596'Bool_34 :: MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_2'8596'Bool_34
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe
         (\ v0 ->
            case coe v0 of
              MAlonzo.Code.Data.Fin.Base.C_zero_12
                -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
              MAlonzo.Code.Data.Fin.Base.C_suc_16 v2
                -> coe seq (coe v2) (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
              _ -> MAlonzo.RTE.mazUnreachableError))
      (coe
         (\ v0 ->
            if coe v0
              then coe
                     MAlonzo.Code.Data.Fin.Base.C_suc_16
                     (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)
              else coe MAlonzo.Code.Data.Fin.Base.C_zero_12))
      erased erased
-- Data.Fin.Properties.0≢1+n
d_0'8802'1'43'n_44 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_0'8802'1'43'n_44 = erased
-- Data.Fin.Properties.suc-injective
d_suc'45'injective_46 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_suc'45'injective_46 = erased
-- Data.Fin.Properties._≟_
d__'8799'__48 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__48 ~v0 v1 v2 = du__'8799'__48 v1 v2
du__'8799'__48 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__48 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    erased (coe du__'8799'__48 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.≡-isDecEquivalence
d_'8801''45'isDecEquivalence_58 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_'8801''45'isDecEquivalence_58 ~v0
  = du_'8801''45'isDecEquivalence_58
du_'8801''45'isDecEquivalence_58 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
du_'8801''45'isDecEquivalence_58
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecEquivalence'46'constructor_2293
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (coe du__'8799'__48)
-- Data.Fin.Properties.≡-preorder
d_'8801''45'preorder_60 ::
  Integer -> MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8801''45'preorder_60 ~v0 = du_'8801''45'preorder_60
du_'8801''45'preorder_60 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_'8801''45'preorder_60
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_preorder_414
-- Data.Fin.Properties.≡-setoid
d_'8801''45'setoid_64 ::
  Integer -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8801''45'setoid_64 ~v0 = du_'8801''45'setoid_64
du_'8801''45'setoid_64 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_'8801''45'setoid_64
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402
-- Data.Fin.Properties.≡-decSetoid
d_'8801''45'decSetoid_68 ::
  Integer -> MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8801''45'decSetoid_68 ~v0 = du_'8801''45'decSetoid_68
du_'8801''45'decSetoid_68 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
du_'8801''45'decSetoid_68
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecSetoid'46'constructor_1373
      (coe du_'8801''45'isDecEquivalence_58)
-- Data.Fin.Properties.toℕ-injective
d_toℕ'45'injective_72 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'injective_72 = erased
-- Data.Fin.Properties.toℕ-strengthen
d_toℕ'45'strengthen_88 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'strengthen_88 = erased
-- Data.Fin.Properties.toℕ-↑ˡ
d_toℕ'45''8593''737'_96 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45''8593''737'_96 = erased
-- Data.Fin.Properties.↑ˡ-injective
d_'8593''737''45'injective_110 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8593''737''45'injective_110 = erased
-- Data.Fin.Properties.toℕ-↑ʳ
d_toℕ'45''8593''691'_126 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45''8593''691'_126 = erased
-- Data.Fin.Properties.↑ʳ-injective
d_'8593''691''45'injective_140 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8593''691''45'injective_140 = erased
-- Data.Fin.Properties.toℕ≤pred[n]
d_toℕ'8804'pred'91'n'93'_154 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_toℕ'8804'pred'91'n'93'_154 ~v0 v1
  = du_toℕ'8804'pred'91'n'93'_154 v1
du_toℕ'8804'pred'91'n'93'_154 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_toℕ'8804'pred'91'n'93'_154 v0
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v2
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_toℕ'8804'pred'91'n'93'_154 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.toℕ≤n
d_toℕ'8804'n_162 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_toℕ'8804'n_162 ~v0 v1 = du_toℕ'8804'n_162 v1
du_toℕ'8804'n_162 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_toℕ'8804'n_162 v0 = coe du_toℕ'8804'pred'91'n'93'_154 (coe v0)
-- Data.Fin.Properties.toℕ<n
d_toℕ'60'n_170 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_toℕ'60'n_170 ~v0 v1 = du_toℕ'60'n_170 v1
du_toℕ'60'n_170 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_toℕ'60'n_170 v0
  = coe
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
      (coe du_toℕ'8804'pred'91'n'93'_154 (coe v0))
-- Data.Fin.Properties.toℕ≤pred[n]′
d_toℕ'8804'pred'91'n'93''8242'_178 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_toℕ'8804'pred'91'n'93''8242'_178 ~v0 v1
  = du_toℕ'8804'pred'91'n'93''8242'_178 v1
du_toℕ'8804'pred'91'n'93''8242'_178 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_toℕ'8804'pred'91'n'93''8242'_178 v0
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'pred_5460
      (coe du_toℕ'60'n_170 (coe v0))
-- Data.Fin.Properties.toℕ-mono-<
d_toℕ'45'mono'45''60'_182 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_toℕ'45'mono'45''60'_182 ~v0 ~v1 ~v2 ~v3 v4
  = du_toℕ'45'mono'45''60'_182 v4
du_toℕ'45'mono'45''60'_182 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_toℕ'45'mono'45''60'_182 v0 = coe v0
-- Data.Fin.Properties.toℕ-mono-≤
d_toℕ'45'mono'45''8804'_186 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_toℕ'45'mono'45''8804'_186 ~v0 ~v1 ~v2 ~v3 v4
  = du_toℕ'45'mono'45''8804'_186 v4
du_toℕ'45'mono'45''8804'_186 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_toℕ'45'mono'45''8804'_186 v0 = coe v0
-- Data.Fin.Properties.toℕ-cancel-≤
d_toℕ'45'cancel'45''8804'_190 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_toℕ'45'cancel'45''8804'_190 ~v0 ~v1 ~v2 ~v3 v4
  = du_toℕ'45'cancel'45''8804'_190 v4
du_toℕ'45'cancel'45''8804'_190 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_toℕ'45'cancel'45''8804'_190 v0 = coe v0
-- Data.Fin.Properties.toℕ-cancel-<
d_toℕ'45'cancel'45''60'_194 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_toℕ'45'cancel'45''60'_194 ~v0 ~v1 ~v2 ~v3 v4
  = du_toℕ'45'cancel'45''60'_194 v4
du_toℕ'45'cancel'45''60'_194 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_toℕ'45'cancel'45''60'_194 v0 = coe v0
-- Data.Fin.Properties.toℕ-fromℕ
d_toℕ'45'fromℕ_200 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'fromℕ_200 = erased
-- Data.Fin.Properties.fromℕ-toℕ
d_fromℕ'45'toℕ_206 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromℕ'45'toℕ_206 = erased
-- Data.Fin.Properties.≤fromℕ
d_'8804'fromℕ_212 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804'fromℕ_212 ~v0 v1 = du_'8804'fromℕ_212 v1
du_'8804'fromℕ_212 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804'fromℕ_212 v0 = coe du_toℕ'8804'pred'91'n'93'_154 (coe v0)
-- Data.Fin.Properties.fromℕ<-toℕ
d_fromℕ'60''45'toℕ_222 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromℕ'60''45'toℕ_222 = erased
-- Data.Fin.Properties.toℕ-fromℕ<
d_toℕ'45'fromℕ'60'_230 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'fromℕ'60'_230 = erased
-- Data.Fin.Properties.fromℕ-def
d_fromℕ'45'def_236 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromℕ'45'def_236 = erased
-- Data.Fin.Properties.fromℕ<-cong
d_fromℕ'60''45'cong_250 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromℕ'60''45'cong_250 = erased
-- Data.Fin.Properties.fromℕ<-injective
d_fromℕ'60''45'injective_270 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromℕ'60''45'injective_270 = erased
-- Data.Fin.Properties.fromℕ<≡fromℕ<″
d_fromℕ'60''8801'fromℕ'60''8243'_284 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromℕ'60''8801'fromℕ'60''8243'_284 = erased
-- Data.Fin.Properties.toℕ-fromℕ<″
d_toℕ'45'fromℕ'60''8243'_290 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'fromℕ'60''8243'_290 = erased
-- Data.Fin.Properties.toℕ-cast
d_toℕ'45'cast_306 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'cast_306 = erased
-- Data.Fin.Properties.cast-is-id
d_cast'45'is'45'id_322 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cast'45'is'45'id_322 = erased
-- Data.Fin.Properties.subst-is-cast
d_subst'45'is'45'cast_334 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_subst'45'is'45'cast_334 = erased
-- Data.Fin.Properties.cast-trans
d_cast'45'trans_344 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cast'45'trans_344 = erased
-- Data.Fin.Properties.≤-reflexive
d_'8804''45'reflexive_356 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'reflexive_356 ~v0 v1 ~v2 ~v3
  = du_'8804''45'reflexive_356 v1
du_'8804''45'reflexive_356 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'reflexive_356 v0
  = coe
      MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
      (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v0))
-- Data.Fin.Properties.≤-refl
d_'8804''45'refl_358 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'refl_358 ~v0 v1 = du_'8804''45'refl_358 v1
du_'8804''45'refl_358 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'refl_358 v0 = coe du_'8804''45'reflexive_356 (coe v0)
-- Data.Fin.Properties.≤-trans
d_'8804''45'trans_360 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'trans_360 ~v0 ~v1 ~v2 ~v3 = du_'8804''45'trans_360
du_'8804''45'trans_360 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'trans_360
  = coe MAlonzo.Code.Data.Nat.Properties.du_'8804''45'trans_2578
-- Data.Fin.Properties.≤-antisym
d_'8804''45'antisym_362 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'antisym_362 = erased
-- Data.Fin.Properties.≤-total
d_'8804''45'total_368 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8804''45'total_368 ~v0 v1 v2 = du_'8804''45'total_368 v1 v2
du_'8804''45'total_368 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_'8804''45'total_368 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.d_'8804''45'total_2584
      (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v0))
      (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v1))
-- Data.Fin.Properties.≤-irrelevant
d_'8804''45'irrelevant_374 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'irrelevant_374 = erased
-- Data.Fin.Properties._≤?_
d__'8804''63'__376 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__376 ~v0 ~v1 v2 v3 = du__'8804''63'__376 v2 v3
du__'8804''63'__376 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8804''63'__376 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.d__'8804''63'__2612
      (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v0))
      (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v1))
-- Data.Fin.Properties._<?_
d__'60''63'__382 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__382 ~v0 ~v1 v2 v3 = du__'60''63'__382 v2 v3
du__'60''63'__382 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'60''63'__382 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.d__'8804''63'__2612
      (coe
         addInt (coe (1 :: Integer))
         (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v0)))
      (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v1))
-- Data.Fin.Properties.≤-isPreorder
d_'8804''45'isPreorder_388 ::
  Integer -> MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8804''45'isPreorder_388 ~v0 = du_'8804''45'isPreorder_388
du_'8804''45'isPreorder_388 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_'8804''45'isPreorder_388
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'8804''45'reflexive_356 v0)
      (\ v0 v1 v2 -> coe du_'8804''45'trans_360)
-- Data.Fin.Properties.≤-isPartialOrder
d_'8804''45'isPartialOrder_390 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8804''45'isPartialOrder_390 ~v0
  = du_'8804''45'isPartialOrder_390
du_'8804''45'isPartialOrder_390 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_'8804''45'isPartialOrder_390
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe du_'8804''45'isPreorder_388) erased
-- Data.Fin.Properties.≤-isTotalOrder
d_'8804''45'isTotalOrder_392 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_'8804''45'isTotalOrder_392 ~v0 = du_'8804''45'isTotalOrder_392
du_'8804''45'isTotalOrder_392 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
du_'8804''45'isTotalOrder_392
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe du_'8804''45'isPartialOrder_390) (coe du_'8804''45'total_368)
-- Data.Fin.Properties.≤-isDecTotalOrder
d_'8804''45'isDecTotalOrder_394 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_'8804''45'isDecTotalOrder_394 ~v0
  = du_'8804''45'isDecTotalOrder_394
du_'8804''45'isDecTotalOrder_394 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
du_'8804''45'isDecTotalOrder_394
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe du_'8804''45'isTotalOrder_392) (coe du__'8799'__48)
      (coe du__'8804''63'__376)
-- Data.Fin.Properties.≤-preorder
d_'8804''45'preorder_396 ::
  Integer -> MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8804''45'preorder_396 ~v0 = du_'8804''45'preorder_396
du_'8804''45'preorder_396 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_'8804''45'preorder_396
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe du_'8804''45'isPreorder_388)
-- Data.Fin.Properties.≤-poset
d_'8804''45'poset_400 ::
  Integer -> MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8804''45'poset_400 ~v0 = du_'8804''45'poset_400
du_'8804''45'poset_400 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_'8804''45'poset_400
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (coe du_'8804''45'isPartialOrder_390)
-- Data.Fin.Properties.≤-totalOrder
d_'8804''45'totalOrder_404 ::
  Integer -> MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_'8804''45'totalOrder_404 ~v0 = du_'8804''45'totalOrder_404
du_'8804''45'totalOrder_404 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
du_'8804''45'totalOrder_404
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      (coe du_'8804''45'isTotalOrder_392)
-- Data.Fin.Properties.≤-decTotalOrder
d_'8804''45'decTotalOrder_408 ::
  Integer -> MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_'8804''45'decTotalOrder_408 ~v0 = du_'8804''45'decTotalOrder_408
du_'8804''45'decTotalOrder_408 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
du_'8804''45'decTotalOrder_408
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      (coe du_'8804''45'isDecTotalOrder_394)
-- Data.Fin.Properties.<-irrefl
d_'60''45'irrefl_412 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'irrefl_412 = erased
-- Data.Fin.Properties.<-asym
d_'60''45'asym_414 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'asym_414 = erased
-- Data.Fin.Properties.<-trans
d_'60''45'trans_416 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'trans_416 ~v0 ~v1 v2 ~v3 = du_'60''45'trans_416 v2
du_'60''45'trans_416 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''45'trans_416 v0
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans_2810
      (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v0))
-- Data.Fin.Properties.<-cmp
d_'60''45'cmp_418 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_'60''45'cmp_418 ~v0 v1 v2 = du_'60''45'cmp_418 v1 v2
du_'60''45'cmp_418 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
du_'60''45'cmp_418 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 erased
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v4
               -> coe
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
                    (coe
                       MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                       (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
                    (coe
                       MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                       (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
               -> let v6 = coe du_'60''45'cmp_418 (coe v3) (coe v5) in
                  case coe v6 of
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v7
                      -> coe
                           MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7)
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v8
                      -> coe
                           MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 erased
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v9
                      -> coe
                           MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.<-respˡ-≡
d_'60''45'resp'737''45''8801'_462 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'resp'737''45''8801'_462 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'60''45'resp'737''45''8801'_462 v6
du_'60''45'resp'737''45''8801'_462 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''45'resp'737''45''8801'_462 v0 = coe v0
-- Data.Fin.Properties.<-respʳ-≡
d_'60''45'resp'691''45''8801'_466 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'resp'691''45''8801'_466 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'60''45'resp'691''45''8801'_466 v6
du_'60''45'resp'691''45''8801'_466 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''45'resp'691''45''8801'_466 v0 = coe v0
-- Data.Fin.Properties.<-resp₂-≡
d_'60''45'resp'8322''45''8801'_470 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'8322''45''8801'_470 ~v0
  = du_'60''45'resp'8322''45''8801'_470
du_'60''45'resp'8322''45''8801'_470 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'8322''45''8801'_470
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe (\ v0 v1 v2 v3 v4 -> v4)) (coe (\ v0 v1 v2 v3 v4 -> v4))
-- Data.Fin.Properties.<-irrelevant
d_'60''45'irrelevant_472 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''45'irrelevant_472 = erased
-- Data.Fin.Properties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_474 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_474 ~v0
  = du_'60''45'isStrictPartialOrder_474
du_'60''45'isStrictPartialOrder_474 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
du_'60''45'isStrictPartialOrder_474
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'60''45'trans_416 v1)
      (coe du_'60''45'resp'8322''45''8801'_470)
-- Data.Fin.Properties.<-isStrictTotalOrder
d_'60''45'isStrictTotalOrder_476 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder_476 ~v0
  = du_'60''45'isStrictTotalOrder_476
du_'60''45'isStrictTotalOrder_476 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
du_'60''45'isStrictTotalOrder_476
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'60''45'trans_416 v1)
      (coe du_'60''45'cmp_418)
-- Data.Fin.Properties.<-strictPartialOrder
d_'60''45'strictPartialOrder_478 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_478 ~v0
  = du_'60''45'strictPartialOrder_478
du_'60''45'strictPartialOrder_478 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
du_'60''45'strictPartialOrder_478
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      (coe du_'60''45'isStrictPartialOrder_474)
-- Data.Fin.Properties.<-strictTotalOrder
d_'60''45'strictTotalOrder_482 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder_482 ~v0
  = du_'60''45'strictTotalOrder_482
du_'60''45'strictTotalOrder_482 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
du_'60''45'strictTotalOrder_482
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      (coe du_'60''45'isStrictTotalOrder_476)
-- Data.Fin.Properties.i<1+i
d_i'60'1'43'i_488 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_i'60'1'43'i_488 ~v0 v1 = du_i'60'1'43'i_488 v1
du_i'60'1'43'i_488 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_i'60'1'43'i_488 v0
  = coe
      MAlonzo.Code.Data.Nat.Properties.d_n'60'1'43'n_2926
      (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v0))
-- Data.Fin.Properties.<⇒≢
d_'60''8658''8802'_490 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8802'_490 = erased
-- Data.Fin.Properties.≤∧≢⇒<
d_'8804''8743''8802''8658''60'_494 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''8743''8802''8658''60'_494 ~v0 v1 v2 v3 ~v4
  = du_'8804''8743''8802''8658''60'_494 v1 v2 v3
du_'8804''8743''8802''8658''60'_494 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''8743''8802''8658''60'_494 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe
                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v6
               -> case coe v2 of
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
                      -> coe
                           MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                           (coe
                              du_'8804''8743''8802''8658''60'_494 (coe v4) (coe v6) (coe v9))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.toℕ-inject
d_toℕ'45'inject_512 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'inject_512 = erased
-- Data.Fin.Properties.inject₁-injective
d_inject'8321''45'injective_520 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inject'8321''45'injective_520 = erased
-- Data.Fin.Properties.toℕ-inject₁
d_toℕ'45'inject'8321'_532 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'inject'8321'_532 = erased
-- Data.Fin.Properties.toℕ-inject₁-≢
d_toℕ'45'inject'8321''45''8802'_538 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_toℕ'45'inject'8321''45''8802'_538 = erased
-- Data.Fin.Properties.inject₁ℕ<
d_inject'8321'ℕ'60'_544 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_inject'8321'ℕ'60'_544 ~v0 v1 = du_inject'8321'ℕ'60'_544 v1
du_inject'8321'ℕ'60'_544 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_inject'8321'ℕ'60'_544 v0 = coe du_toℕ'60'n_170 (coe v0)
-- Data.Fin.Properties.inject₁ℕ≤
d_inject'8321'ℕ'8804'_554 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_inject'8321'ℕ'8804'_554 ~v0 v1 = du_inject'8321'ℕ'8804'_554 v1
du_inject'8321'ℕ'8804'_554 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_inject'8321'ℕ'8804'_554 v0
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684
      (coe du_inject'8321'ℕ'60'_544 (coe v0))
-- Data.Fin.Properties.≤̄⇒inject₁<
d_'8804''772''8658'inject'8321''60'_556 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''772''8658'inject'8321''60'_556 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8804''772''8658'inject'8321''60'_556 v4
du_'8804''772''8658'inject'8321''60'_556 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''772''8658'inject'8321''60'_556 v0
  = coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v0
-- Data.Fin.Properties.ℕ<⇒inject₁<
d_ℕ'60''8658'inject'8321''60'_570 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_ℕ'60''8658'inject'8321''60'_570 ~v0 v1 ~v2 v3
  = du_ℕ'60''8658'inject'8321''60'_570 v1 v3
du_ℕ'60''8658'inject'8321''60'_570 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_ℕ'60''8658'inject'8321''60'_570 v0 v1
  = coe
      seq (coe v0)
      (case coe v1 of
         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
           -> coe du_'8804''772''8658'inject'8321''60'_556 (coe v4)
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Data.Fin.Properties.i≤inject₁[j]⇒i≤1+j
d_i'8804'inject'8321''91'j'93''8658'i'8804'1'43'j_576 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_i'8804'inject'8321''91'j'93''8658'i'8804'1'43'j_576 ~v0 v1 ~v2 v3
                                                      v4
  = du_i'8804'inject'8321''91'j'93''8658'i'8804'1'43'j_576 v1 v3 v4
du_i'8804'inject'8321''91'j'93''8658'i'8804'1'43'j_576 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_i'8804'inject'8321''91'j'93''8658'i'8804'1'43'j_576 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v4
        -> coe
             seq (coe v1)
             (case coe v2 of
                MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7
                  -> coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7
                _ -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.toℕ-lower₁
d_toℕ'45'lower'8321'_592 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'lower'8321'_592 = erased
-- Data.Fin.Properties.lower₁-injective
d_lower'8321''45'injective_610 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lower'8321''45'injective_610 = erased
-- Data.Fin.Properties.inject₁-lower₁
d_inject'8321''45'lower'8321'_634 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inject'8321''45'lower'8321'_634 = erased
-- Data.Fin.Properties.lower₁-inject₁′
d_lower'8321''45'inject'8321''8242'_650 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lower'8321''45'inject'8321''8242'_650 = erased
-- Data.Fin.Properties.lower₁-inject₁
d_lower'8321''45'inject'8321'_658 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lower'8321''45'inject'8321'_658 = erased
-- Data.Fin.Properties.lower₁-irrelevant
d_lower'8321''45'irrelevant_668 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lower'8321''45'irrelevant_668 = erased
-- Data.Fin.Properties.inject₁≡⇒lower₁≡
d_inject'8321''8801''8658'lower'8321''8801'_684 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inject'8321''8801''8658'lower'8321''8801'_684 = erased
-- Data.Fin.Properties.toℕ-inject≤
d_toℕ'45'inject'8804'_694 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'inject'8804'_694 = erased
-- Data.Fin.Properties.inject≤-refl
d_inject'8804''45'refl_708 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inject'8804''45'refl_708 = erased
-- Data.Fin.Properties.inject≤-idempotent
d_inject'8804''45'idempotent_726 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inject'8804''45'idempotent_726 = erased
-- Data.Fin.Properties.inject≤-injective
d_inject'8804''45'injective_752 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inject'8804''45'injective_752 = erased
-- Data.Fin.Properties.pred<
d_pred'60'_772 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_pred'60'_772 ~v0 v1 ~v2 = du_pred'60'_772 v1
du_pred'60'_772 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_pred'60'_772 v0
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> coe
             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v2
        -> coe
             du_'8804''772''8658'inject'8321''60'_556
             (coe
                MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
                (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.splitAt-↑ˡ
d_splitAt'45''8593''737'_784 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_splitAt'45''8593''737'_784 = erased
-- Data.Fin.Properties.splitAt-↑ʳ
d_splitAt'45''8593''691'_806 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_splitAt'45''8593''691'_806 = erased
-- Data.Fin.Properties.splitAt-join
d_splitAt'45'join_828 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_splitAt'45'join_828 = erased
-- Data.Fin.Properties.join-splitAt
d_join'45'splitAt_848 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_join'45'splitAt_848 = erased
-- Data.Fin.Properties.splitAt-<
d_splitAt'45''60'_888 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_splitAt'45''60'_888 = erased
-- Data.Fin.Properties.splitAt-≥
d_splitAt'45''8805'_906 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_splitAt'45''8805'_906 = erased
-- Data.Fin.Properties.+↔⊎
d_'43''8596''8846'_916 ::
  Integer -> Integer -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'43''8596''8846'_916 v0 ~v1 = du_'43''8596''8846'_916 v0
du_'43''8596''8846'_916 ::
  Integer -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'43''8596''8846'_916 v0
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Fin.Base.du_splitAt_164 (coe v0))
      (coe MAlonzo.Code.Data.Fin.Base.du_join_178 (coe v0)) erased erased
-- Data.Fin.Properties.remQuot-combine
d_remQuot'45'combine_930 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_remQuot'45'combine_930 = erased
-- Data.Fin.Properties.combine-remQuot
d_combine'45'remQuot_960 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_combine'45'remQuot_960 = erased
-- Data.Fin.Properties.toℕ-combine
d_toℕ'45'combine_1006 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'combine_1006 = erased
-- Data.Fin.Properties.combine-monoˡ-<
d_combine'45'mono'737''45''60'_1050 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_combine'45'mono'737''45''60'_1050 ~v0 v1 v2 v3 v4 v5 v6
  = du_combine'45'mono'737''45''60'_1050 v1 v2 v3 v4 v5 v6
du_combine'45'mono'737''45''60'_1050 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_combine'45'mono'737''45''60'_1050 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
         (\ v6 v7 v8 v9 v10 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans_2810 v7 v9 v10)
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
         (\ v6 v7 v8 v9 v10 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822 v9 v10)
         (coe
            addInt (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v3))
            (coe
               mulInt (coe v0)
               (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v1))))
         (coe
            addInt
            (coe
               mulInt (coe v0)
               (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v1)))
            (coe v0))
         (coe
            MAlonzo.Code.Data.Fin.Base.du_toℕ_18
            (coe
               MAlonzo.Code.Data.Fin.Base.du_combine_238 (coe v0) (coe v2)
               (coe v4)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (\ v6 v7 v8 v9 v10 ->
               coe
                 MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v9 v10)
            (coe
               mulInt (coe v0)
               (coe
                  addInt (coe (1 :: Integer))
                  (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v1))))
            (coe
               mulInt (coe v0)
               (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v2)))
            (coe
               MAlonzo.Code.Data.Fin.Base.du_toℕ_18
               (coe
                  MAlonzo.Code.Data.Fin.Base.du_combine_238 (coe v0) (coe v2)
                  (coe v4)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
               (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
               (\ v6 v7 v8 v9 v10 ->
                  coe
                    MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v9 v10)
               (coe
                  mulInt (coe v0)
                  (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v2)))
               (coe
                  addInt (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v4))
                  (coe
                     mulInt (coe v0)
                     (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v2))))
               (coe
                  MAlonzo.Code.Data.Fin.Base.du_toℕ_18
                  (coe
                     MAlonzo.Code.Data.Fin.Base.du_combine_238 (coe v0) (coe v2)
                     (coe v4)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                  (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                  (coe
                     MAlonzo.Code.Data.Fin.Base.du_toℕ_18
                     (coe
                        MAlonzo.Code.Data.Fin.Base.du_combine_238 (coe v0) (coe v2)
                        (coe v4))))
               (coe
                  MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362
                  (coe
                     mulInt (coe v0)
                     (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v2)))))
            (coe
               MAlonzo.Code.Data.Nat.Properties.du_'42''45'mono'691''45''8804'_3914
               (coe v0) (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v2))
               (coe v5)))
         (coe
            MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'691''45''60'_3476
            (coe
               mulInt (coe v0)
               (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v1)))
            (coe du_toℕ'60'n_170 (coe v3))))
-- Data.Fin.Properties.combine-injectiveˡ
d_combine'45'injective'737'_1080 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_combine'45'injective'737'_1080 = erased
-- Data.Fin.Properties.combine-injectiveʳ
d_combine'45'injective'691'_1140 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_combine'45'injective'691'_1140 = erased
-- Data.Fin.Properties.combine-injective
d_combine'45'injective_1186 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_combine'45'injective_1186 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_combine'45'injective_1186
du_combine'45'injective_1186 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_combine'45'injective_1186
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Fin.Properties.combine-surjective
d_combine'45'surjective_1204 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_combine'45'surjective_1204 ~v0 v1 v2
  = du_combine'45'surjective_1204 v1 v2
du_combine'45'surjective_1204 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_combine'45'surjective_1204 v0 v1
  = let v2
          = coe
              MAlonzo.Code.Data.Fin.Base.du_remQuot_220 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
             (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) erased)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.*↔×
d_'42''8596''215'_1232 ::
  Integer -> Integer -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'42''8596''215'_1232 ~v0 v1 = du_'42''8596''215'_1232 v1
du_'42''8596''215'_1232 ::
  Integer -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'42''8596''215'_1232 v0
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Fin.Base.du_remQuot_220 (coe v0))
      (coe
         MAlonzo.Code.Data.Product.Base.du_uncurry_220
         (coe MAlonzo.Code.Data.Fin.Base.du_combine_238 (coe v0)))
      erased erased
-- Data.Fin.Properties.funToFin-finToFin
d_funToFin'45'finToFin_1238 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_funToFin'45'finToFin_1238 = erased
-- Data.Fin.Properties.finToFun-funToFin
d_finToFun'45'funToFin_1254 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_finToFun'45'funToFin_1254 = erased
-- Data.Fin.Properties.^↔→
d_'94''8596''8594'_1280 ::
  Integer ->
  Integer ->
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'94''8596''8594'_1280 v0 v1 ~v2 = du_'94''8596''8594'_1280 v0 v1
du_'94''8596''8594'_1280 ::
  Integer -> Integer -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'94''8596''8594'_1280 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Fin.Base.d_finToFun_254 (coe v0) (coe v1))
      (coe MAlonzo.Code.Data.Fin.Base.d_funToFin_270 (coe v1) (coe v0))
      erased erased
-- Data.Fin.Properties.lift-injective
d_lift'45'injective_1292 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lift'45'injective_1292 = erased
-- Data.Fin.Properties.<⇒≤pred
d_'60''8658''8804'pred_1316 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''8658''8804'pred_1316 ~v0 v1 ~v2 v3 v4
  = du_'60''8658''8804'pred_1316 v1 v3 v4
du_'60''8658''8804'pred_1316 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''8658''8804'pred_1316 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> coe
             seq (coe v1)
             (case coe v2 of
                MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
                  -> coe seq (coe v6) (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
                _ -> MAlonzo.RTE.mazUnreachableError)
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v4
        -> coe
             seq (coe v1)
             (case coe v2 of
                MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7 -> coe v7
                _ -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.toℕ‿ℕ-
d_toℕ'8255'ℕ'45'_1334 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'8255'ℕ'45'_1334 = erased
-- Data.Fin.Properties.ℕ-ℕ≡toℕ‿ℕ-
d_ℕ'45'ℕ'8801'toℕ'8255'ℕ'45'_1346 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_ℕ'45'ℕ'8801'toℕ'8255'ℕ'45'_1346 = erased
-- Data.Fin.Properties.nℕ-ℕi≤n
d_nℕ'45'ℕi'8804'n_1358 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_nℕ'45'ℕi'8804'n_1358 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> coe
             MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
             (coe
                MAlonzo.Code.Data.Fin.Base.d__ℕ'45'ℕ__388 (coe v0)
                (coe MAlonzo.Code.Data.Fin.Base.C_zero_12))
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v3
        -> let v4 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
             (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
             (\ v5 v6 v7 ->
                coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v7)
             (coe
                MAlonzo.Code.Data.Fin.Base.d__ℕ'45'ℕ__388 (coe v0)
                (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v3))
             (coe v0)
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                (\ v5 v6 v7 v8 v9 ->
                   coe
                     MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v8 v9)
                (coe MAlonzo.Code.Data.Fin.Base.d__ℕ'45'ℕ__388 (coe v4) (coe v3))
                (coe v4) (coe v0)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                   (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                   (\ v5 v6 v7 v8 v9 ->
                      coe
                        MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v8 v9)
                   (coe v4) (coe v0) (coe v0)
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                      (coe v0))
                   (coe
                      MAlonzo.Code.Data.Nat.Properties.d_n'8804'1'43'n_2668 (coe v4)))
                (coe d_nℕ'45'ℕi'8804'n_1358 (coe v4) (coe v3)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.punchIn-injective
d_punchIn'45'injective_1376 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_punchIn'45'injective_1376 = erased
-- Data.Fin.Properties.punchInᵢ≢i
d_punchIn'7522''8802'i_1392 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_punchIn'7522''8802'i_1392 = erased
-- Data.Fin.Properties.punchOut-cong
d_punchOut'45'cong_1408 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_punchOut'45'cong_1408 = erased
-- Data.Fin.Properties.punchOut-cong′
d_punchOut'45'cong'8242'_1442 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_punchOut'45'cong'8242'_1442 = erased
-- Data.Fin.Properties.punchOut-injective
d_punchOut'45'injective_1458 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_punchOut'45'injective_1458 = erased
-- Data.Fin.Properties.punchIn-punchOut
d_punchIn'45'punchOut_1494 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_punchIn'45'punchOut_1494 = erased
-- Data.Fin.Properties.punchOut-punchIn
d_punchOut'45'punchIn_1518 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_punchOut'45'punchIn_1518 = erased
-- Data.Fin.Properties.pinch-surjective
d_pinch'45'surjective_1534 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pinch'45'surjective_1534 ~v0 v1 v2
  = du_pinch'45'surjective_1534 v1 v2
du_pinch'45'surjective_1534 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_pinch'45'surjective_1534 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe MAlonzo.Code.Data.Fin.Base.C_zero_12) erased
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v3
        -> case coe v0 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                    (coe
                       MAlonzo.Code.Data.Fin.Base.C_suc_16
                       (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v3))
                    erased
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
               -> coe
                    MAlonzo.Code.Data.Product.Base.du_map_104
                    (coe MAlonzo.Code.Data.Fin.Base.C_suc_16) erased
                    (coe du_pinch'45'surjective_1534 (coe v5) (coe v3))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.pinch-mono-≤
d_pinch'45'mono'45''8804'_1544 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_pinch'45'mono'45''8804'_1544 ~v0 v1 v2 v3 v4
  = du_pinch'45'mono'45''8804'_1544 v1 v2 v3 v4
du_pinch'45'mono'45''8804'_1544 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_pinch'45'mono'45''8804'_1544 v0 v1 v2 v3
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v6
               -> coe
                    seq (coe v2)
                    (case coe v3 of
                       MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9 -> coe v9
                       _ -> MAlonzo.RTE.mazUnreachableError)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v7
               -> case coe v2 of
                    MAlonzo.Code.Data.Fin.Base.C_suc_16 v9
                      -> case coe v3 of
                           MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v12
                             -> coe
                                  MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                                  (coe
                                     du_pinch'45'mono'45''8804'_1544 (coe v5) (coe v7) (coe v9)
                                     (coe v12))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.pinch-injective
d_pinch'45'injective_1576 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pinch'45'injective_1576 = erased
-- Data.Fin.Properties._.∀-cons
d_'8704''45'cons_1620 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  AgdaAny ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_'8704''45'cons_1620 ~v0 ~v1 ~v2 v3 v4 v5
  = du_'8704''45'cons_1620 v3 v4 v5
du_'8704''45'cons_1620 ::
  AgdaAny ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_'8704''45'cons_1620 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12 -> coe v0
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v4 -> coe v1 v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties._.∀-cons-⇔
d_'8704''45'cons'45''8660'_1632 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8704''45'cons'45''8660'_1632 ~v0 ~v1 ~v2
  = du_'8704''45'cons'45''8660'_1632
du_'8704''45'cons'45''8660'_1632 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8704''45'cons'45''8660'_1632
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
      (coe
         MAlonzo.Code.Data.Product.Base.du_uncurry_220
         (coe du_'8704''45'cons_1620))
      (coe
         MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88
         (coe (\ v0 -> coe v0 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)))
         (coe
            (\ v0 v1 -> coe v0 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v1))))
-- Data.Fin.Properties._.∃-here
d_'8707''45'here_1638 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8707''45'here_1638 ~v0 ~v1 ~v2 v3 = du_'8707''45'here_1638 v3
du_'8707''45'here_1638 ::
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8707''45'here_1638 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe MAlonzo.Code.Data.Fin.Base.C_zero_12) (coe v0)
-- Data.Fin.Properties._.∃-there
d_'8707''45'there_1642 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8707''45'there_1642 ~v0 ~v1 ~v2 = du_'8707''45'there_1642
du_'8707''45'there_1642 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8707''45'there_1642
  = coe
      MAlonzo.Code.Data.Product.Base.du_map_104
      (coe MAlonzo.Code.Data.Fin.Base.C_suc_16) (coe (\ v0 v1 -> v1))
-- Data.Fin.Properties._.∃-toSum
d_'8707''45'toSum_1644 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8707''45'toSum_1644 ~v0 ~v1 ~v2 v3 = du_'8707''45'toSum_1644 v3
du_'8707''45'toSum_1644 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_'8707''45'toSum_1644 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v2)
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v4
               -> coe
                    MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                    (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v2))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties._.⊎⇔∃
d_'8846''8660''8707'_1652 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8846''8660''8707'_1652 ~v0 ~v1 ~v2 = du_'8846''8660''8707'_1652
du_'8846''8660''8707'_1652 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8846''8660''8707'_1652
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
         (coe du_'8707''45'here_1638) (coe du_'8707''45'there_1642))
      (coe du_'8707''45'toSum_1644)
-- Data.Fin.Properties.decFinSubset
d_decFinSubset_1664 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_decFinSubset_1664 v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_decFinSubset_1664 v0 v5 v6
du_decFinSubset_1664 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_decFinSubset_1664 v0 v1 v2
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           let v4 = coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12) in
           let v5 = coe du_'8704''45'cons_1620 in
           case coe v4 of
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v6 v7
               -> if coe v6
                    then coe
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                           (coe
                              MAlonzo.Code.Data.Product.Base.du_uncurry_220
                              (coe
                                 (\ v8 v9 v10 -> coe v5 (\ v11 -> v8) (\ v11 -> coe v9 v11) v10)))
                           (coe
                              MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                              (coe
                                 v2 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v7)))
                              (coe
                                 du_decFinSubset_1664 (coe v3)
                                 (coe (\ v8 -> coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v8)))
                                 (coe
                                    (\ v8 -> coe v2 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v8)))))
                    else coe
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                           (coe
                              (\ v8 v9 ->
                                 coe
                                   v5 (\ v10 -> coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14)
                                   (\ v10 -> coe v8 v10) v9))
                           (coe
                              du_decFinSubset_1664 (coe v3)
                              (coe (\ v8 -> coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v8)))
                              (coe
                                 (\ v8 -> coe v2 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v8))))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.any?
d_any'63'_1744 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_any'63'_1744 v0 ~v1 ~v2 v3 = du_any'63'_1744 v0 v3
du_any'63'_1744 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_any'63'_1744 v0 v1
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Nullary.Decidable.du_map_18
             (coe du_'8846''8660''8707'_1652)
             (coe
                MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'8846''45'dec__72
                (coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12))
                (coe
                   du_any'63'_1744 (coe v2)
                   (coe
                      (\ v3 -> coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v3)))))
-- Data.Fin.Properties.all?
d_all'63'_1762 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_all'63'_1762 v0 ~v1 ~v2 v3 = du_all'63'_1762 v0 v3
du_all'63'_1762 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_all'63'_1762 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (coe
         (\ v2 v3 -> coe v2 v3 (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
      (coe
         du_decFinSubset_1664 (coe v0)
         (\ v2 -> coe MAlonzo.Code.Relation.Unary.Properties.du_U'63'_34)
         (coe (\ v2 v3 -> coe v1 v2)))
-- Data.Fin.Properties.¬∀⟶∃¬-smallest
d_'172''8704''10230''8707''172''45'smallest_1804 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  ((MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'172''8704''10230''8707''172''45'smallest_1804 v0 ~v1 ~v2 v3 ~v4
  = du_'172''8704''10230''8707''172''45'smallest_1804 v0 v3
du_'172''8704''10230''8707''172''45'smallest_1804 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'172''8704''10230''8707''172''45'smallest_1804 v0 v1
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           let v3 = coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12) in
           case coe v3 of
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v4 v5
               -> if coe v4
                    then coe
                           MAlonzo.Code.Data.Product.Base.du_map_104
                           (coe MAlonzo.Code.Data.Fin.Base.C_suc_16)
                           (coe
                              (\ v6 ->
                                 coe
                                   MAlonzo.Code.Data.Product.Base.du_map_104 (coe (\ v7 -> v7))
                                   (coe
                                      (\ v7 ->
                                         coe
                                           du_'8704''45'cons_1620
                                           (coe
                                              MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42
                                              (coe v5))))))
                           (coe
                              du_'172''8704''10230''8707''172''45'smallest_1804 (coe v2)
                              (coe
                                 (\ v6 -> coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v6))))
                    else coe
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                           (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                              (coe MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v5))
                              erased)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.¬∀⟶∃¬
d_'172''8704''10230''8707''172'_1854 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  ((MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'172''8704''10230''8707''172'_1854 v0 ~v1 ~v2 v3 ~v4
  = du_'172''8704''10230''8707''172'_1854 v0 v3
du_'172''8704''10230''8707''172'_1854 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'172''8704''10230''8707''172'_1854 v0 v1
  = coe
      MAlonzo.Code.Data.Product.Base.du_map_104 (coe (\ v2 -> v2))
      (coe
         (\ v2 v3 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
      (coe
         du_'172''8704''10230''8707''172''45'smallest_1804 (coe v0)
         (coe v1))
-- Data.Fin.Properties.pigeonhole
d_pigeonhole_1870 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pigeonhole_1870 ~v0 v1 v2 v3 = du_pigeonhole_1870 v1 v2 v3
du_pigeonhole_1870 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_pigeonhole_1870 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
        -> case coe v5 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe
                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
               -> let v9
                        = coe
                            du_any'63'_1744 (coe subInt (coe v0) (coe (1 :: Integer)))
                            (coe
                               (\ v9 ->
                                  coe
                                    du__'8799'__48
                                    (coe v2 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12))
                                    (coe v2 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v9)))) in
                  case coe v9 of
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v10 v11
                      -> if coe v10
                           then case coe v11 of
                                  MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v12
                                    -> case coe v12 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v13 v14
                                           -> coe
                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                   (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v13)
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                      (coe
                                                         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                                                         (coe
                                                            MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
                                                      (coe v14)))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           else coe
                                  seq (coe v11)
                                  (let v12
                                         = coe
                                             du_pigeonhole_1870
                                             (coe subInt (coe v0) (coe (1 :: Integer)))
                                             (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8)
                                             (coe
                                                (\ v12 ->
                                                   coe
                                                     MAlonzo.Code.Data.Fin.Base.du_punchOut_412
                                                     (coe
                                                        v2
                                                        (coe MAlonzo.Code.Data.Fin.Base.C_zero_12))
                                                     (coe
                                                        v2
                                                        (coe
                                                           MAlonzo.Code.Data.Fin.Base.C_suc_16
                                                           v12)))) in
                                   case coe v12 of
                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v13 v14
                                       -> case coe v14 of
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v15 v16
                                              -> case coe v16 of
                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v17 v18
                                                     -> coe
                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                          (coe
                                                             MAlonzo.Code.Data.Fin.Base.C_suc_16
                                                             v13)
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                             (coe
                                                                MAlonzo.Code.Data.Fin.Base.C_suc_16
                                                                v15)
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                (coe
                                                                   MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                                                                   v17)
                                                                erased))
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.injective⇒≤
d_injective'8658''8804'_1924 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_injective'8658''8804'_1924 v0 v1 ~v2 ~v3
  = du_injective'8658''8804'_1924 v0 v1
du_injective'8658''8804'_1924 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_injective'8658''8804'_1924 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_injective'8658''8804'_1924 (coe v2) (coe v3))
-- Data.Fin.Properties.<⇒notInjective
d_'60''8658'notInjective_1938 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658'notInjective_1938 = erased
-- Data.Fin.Properties.ℕ→Fin-notInjective
d_ℕ'8594'Fin'45'notInjective_1946 ::
  Integer ->
  (Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_ℕ'8594'Fin'45'notInjective_1946 = erased
-- Data.Fin.Properties.cantor-schröder-bernstein
d_cantor'45'schröder'45'bernstein_1956 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cantor'45'schröder'45'bernstein_1956 = erased
-- Data.Fin.Properties._.sequence
d_sequence_2012 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> ()) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
d_sequence_2012 ~v0 ~v1 v2 v3 ~v4 v5 = du_sequence_2012 v2 v3 v5
du_sequence_2012 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
du_sequence_2012 v0 v1 v2
  = case coe v1 of
      0 -> coe MAlonzo.Code.Effect.Applicative.d_pure_32 v0 erased erased
      _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Effect.Applicative.d__'60''42''62'__34 v0 erased
             erased
             (coe
                MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
                (MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v0)) erased
                erased (coe du_'8704''45'cons_1620)
                (coe v2 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)))
             (coe
                du_sequence_2012 (coe v0) (coe v3)
                (coe
                   (\ v4 -> coe v2 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v4))))
-- Data.Fin.Properties._.sequence⁻¹
d_sequence'8315''185'_2048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Functor.T_RawFunctor_24 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_sequence'8315''185'_2048 ~v0 ~v1 v2 ~v3 ~v4 v5 v6
  = du_sequence'8315''185'_2048 v2 v5 v6
du_sequence'8315''185'_2048 ::
  MAlonzo.Code.Effect.Functor.T_RawFunctor_24 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_sequence'8315''185'_2048 v0 v1 v2
  = coe
      MAlonzo.Code.Effect.Functor.d__'60''36''62'__30 v0 erased erased
      (\ v3 -> coe v3 v2) v1
-- Data.Fin.Properties._._._≈_
d__'8776'__2072 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__2072 = erased
-- Data.Fin.Properties._.inj⇒≟
d_inj'8658''8799'_2092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_inj'8658''8799'_2092 ~v0 ~v1 ~v2 ~v3 v4
  = du_inj'8658''8799'_2092 v4
du_inj'8658''8799'_2092 ::
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_inj'8658''8799'_2092 v0
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.du_via'45'injection_124
      (coe v0) (coe du__'8799'__48)
-- Data.Fin.Properties._.inj⇒decSetoid
d_inj'8658'decSetoid_2094 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_inj'8658'decSetoid_2094 ~v0 ~v1 ~v2 v3 v4
  = du_inj'8658'decSetoid_2094 v3 v4
du_inj'8658'decSetoid_2094 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
du_inj'8658'decSetoid_2094 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecSetoid'46'constructor_1373
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsDecEquivalence'46'constructor_2293
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
         (coe du_inj'8658''8799'_2092 (coe v1)))
-- Data.Fin.Properties.opposite-prop
d_opposite'45'prop_2098 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_opposite'45'prop_2098 = erased
-- Data.Fin.Properties.opposite-involutive
d_opposite'45'involutive_2110 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_opposite'45'involutive_2110 = erased
-- Data.Fin.Properties.opposite-suc
d_opposite'45'suc_2124 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_opposite'45'suc_2124 = erased
-- Data.Fin.Properties.inject+-raise-splitAt
d_inject'43''45'raise'45'splitAt_2134 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inject'43''45'raise'45'splitAt_2134 = erased
-- Data.Fin.Properties.toℕ-raise
d_toℕ'45'raise_2136 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'raise_2136 = erased
-- Data.Fin.Properties.toℕ-inject+
d_toℕ'45'inject'43'_2144 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℕ'45'inject'43'_2144 = erased
-- Data.Fin.Properties.splitAt-inject+
d_splitAt'45'inject'43'_2156 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_splitAt'45'inject'43'_2156 = erased
-- Data.Fin.Properties.splitAt-raise
d_splitAt'45'raise_2170 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_splitAt'45'raise_2170 = erased
-- Data.Fin.Properties.Fin0↔⊥
d_Fin0'8596''8869'_2172 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_Fin0'8596''8869'_2172 = coe d_0'8596''8869'_22
-- Data.Fin.Properties.eq?
d_eq'63'_2174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_eq'63'_2174 ~v0 ~v1 ~v2 = du_eq'63'_2174
du_eq'63'_2174 ::
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_eq'63'_2174 = coe du_inj'8658''8799'_2092
-- Data.Fin.Properties.z≺s
d_z'8826's_2178 ::
  Integer -> MAlonzo.Code.Data.Fin.Base.T__'8826'__548
d_z'8826's_2178 ~v0 = du_z'8826's_2178
du_z'8826's_2178 :: MAlonzo.Code.Data.Fin.Base.T__'8826'__548
du_z'8826's_2178
  = coe
      MAlonzo.Code.Data.Fin.Base.C__'8827'toℕ__554
      (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)
-- Data.Fin.Properties.s≺s
d_s'8826's_2184 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548 ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548
d_s'8826's_2184 ~v0 ~v1 v2 = du_s'8826's_2184 v2
du_s'8826's_2184 ::
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548 ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548
du_s'8826's_2184 v0
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C__'8827'toℕ__554 v2
        -> coe
             MAlonzo.Code.Data.Fin.Base.C__'8827'toℕ__554
             (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.<⇒≺
d_'60''8658''8826'_2190 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548
d_'60''8658''8826'_2190 v0 ~v1 v2 = du_'60''8658''8826'_2190 v0 v2
du_'60''8658''8826'_2190 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548
du_'60''8658''8826'_2190 v0 v1
  = case coe v0 of
      0 -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
               -> coe seq (coe v4) (coe du_z'8826's_2178)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
               -> coe
                    du_s'8826's_2184 (coe du_'60''8658''8826'_2190 (coe v2) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.≺⇒<
d_'8826''8658''60'_2196 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8826''8658''60'_2196 ~v0 ~v1 v2 = du_'8826''8658''60'_2196 v2
du_'8826''8658''60'_2196 ::
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8826''8658''60'_2196 v0
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C__'8827'toℕ__554 v2
        -> coe du_toℕ'60'n_170 (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Properties.≺⇒<′
d_'8826''8658''60''8242'_2202 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_'8826''8658''60''8242'_2202 ~v0 v1 v2
  = du_'8826''8658''60''8242'_2202 v1 v2
du_'8826''8658''60''8242'_2202 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
du_'8826''8658''60''8242'_2202 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'60''8658''60''8242'_5962
      (coe v0) (coe du_'8826''8658''60'_2196 (coe v1))
-- Data.Fin.Properties.<′⇒≺
d_'60''8242''8658''8826'_2206 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548
d_'60''8242''8658''8826'_2206 v0 ~v1 v2
  = du_'60''8242''8658''8826'_2206 v0 v2
du_'60''8242''8658''8826'_2206 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Fin.Base.T__'8826'__548
du_'60''8242''8658''8826'_2206 v0 v1
  = coe
      du_'60''8658''8826'_2190 (coe v0)
      (coe
         MAlonzo.Code.Data.Nat.Properties.du_'60''8242''8658''60'_5970
         (coe v0) (coe v1))
