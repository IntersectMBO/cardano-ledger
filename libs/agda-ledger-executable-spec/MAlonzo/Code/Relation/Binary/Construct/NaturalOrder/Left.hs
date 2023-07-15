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

module MAlonzo.Code.Relation.Binary.Construct.NaturalOrder.Left where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.Construct.NaturalOrder.Left._.Commutative
d_Commutative_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Commutative_46 = erased
-- Relation.Binary.Construct.NaturalOrder.Left._.Idempotent
d_Idempotent_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Idempotent_56 = erased
-- Relation.Binary.Construct.NaturalOrder.Left._.Selective
d_Selective_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Selective_128 = erased
-- Relation.Binary.Construct.NaturalOrder.Left._.IsBand
d_IsBand_152 a0 a1 a2 a3 a4 a5 = ()
-- Relation.Binary.Construct.NaturalOrder.Left._.IsMagma
d_IsMagma_188 a0 a1 a2 a3 a4 a5 = ()
-- Relation.Binary.Construct.NaturalOrder.Left._.IsSemigroup
d_IsSemigroup_216 a0 a1 a2 a3 a4 a5 = ()
-- Relation.Binary.Construct.NaturalOrder.Left._.IsSemilattice
d_IsSemilattice_2466 a0 a1 a2 a3 a4 a5 = ()
-- Relation.Binary.Construct.NaturalOrder.Left._≤_
d__'8804'__2870 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d__'8804'__2870 = erased
-- Relation.Binary.Construct.NaturalOrder.Left.reflexive
d_reflexive_2876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_2876 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
  = du_reflexive_2876 v4 v5 v6 v7 v8 v9
du_reflexive_2876 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_reflexive_2876 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v1)) v3
         (coe v0 v3 v3) (coe v0 v3 v4)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v1))
            (coe v0 v3 v3) (coe v0 v3 v4) (coe v0 v3 v4)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1)))
               (coe v0 v3 v4))
            (coe
               MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v1 v3 v3 v3 v4
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1)) v3)
               v5))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1))
            (coe v0 v3 v3) v3 (coe v2 v3)))
-- Relation.Binary.Construct.NaturalOrder.Left.refl
d_refl_2938 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_refl_2938 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_refl_2938 v4 v5 v6 v7
du_refl_2938 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_refl_2938 v0 v1 v2 v3 = coe v1 (coe v0 v3 v3) v3 (coe v2 v3)
-- Relation.Binary.Construct.NaturalOrder.Left.antisym
d_antisym_2946 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_2946 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_antisym_2946 v4 v5 v6 v7 v8 v9 v10
du_antisym_2946 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisym_2946 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
            v1)
         v3 (coe v0 v3 v4) v4
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
               v1)
            (coe v0 v3 v4) (coe v0 v4 v3) v4
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
                  v1)
               (coe v0 v4 v3) v4 v4
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v1))
                  (coe v4))
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36 v1 v4
                  (coe v0 v4 v3) v6))
            (coe v2 v3 v4))
         v5)
-- Relation.Binary.Construct.NaturalOrder.Left.total
d_total_3000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_3000 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_total_3000 v4 v5 v6 v7 v8 v9 v10
du_total_3000 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_total_3000 v0 v1 v2 v3 v4 v5 v6
  = let v7 = coe v3 v5 v6 in
    case coe v7 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
        -> coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
             (coe v1 (coe v0 v5 v6) v5 v8)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v8
        -> coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
             (coe
                v1 (coe v0 v6 v5) v6
                (coe v2 (coe v0 v6 v5) (coe v0 v5 v6) v6 (coe v4 v6 v5) v8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Construct.NaturalOrder.Left.trans
d_trans_3046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3046 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_trans_3046 v4 v5 v6 v7 v8 v9 v10
du_trans_3046 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_3046 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))
         v2 (coe v0 v2 v3) (coe v0 v2 v4)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))
            (coe v0 v2 v3) (coe v0 v2 (coe v0 v3 v4)) (coe v0 v2 v4)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))
               (coe v0 v2 (coe v0 v3 v4)) (coe v0 (coe v0 v2 v3) v4)
               (coe v0 v2 v4)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))
                  (coe v0 (coe v0 v2 v3) v4) (coe v0 v2 v4) (coe v0 v2 v4)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (coe
                              MAlonzo.Code.Algebra.Structures.du_setoid_164
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))))
                     (coe v0 v2 v4))
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                     (MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
                     (coe v0 v2 v3) v2 v4 v4
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                        (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))
                        v2 (coe v0 v2 v3) v5)
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))
                        v4)))
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                  (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))
                  (coe v0 (coe v0 v2 v3) v4) (coe v0 v2 (coe v0 v3 v4))
                  (coe MAlonzo.Code.Algebra.Structures.d_assoc_446 v1 v2 v3 v4)))
            (coe
               MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
               (MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)) v2 v2 v3
               (coe v0 v3 v4)
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1)))
                  v2)
               v6))
         v5)
-- Relation.Binary.Construct.NaturalOrder.Left.respʳ
d_resp'691'_3114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_resp'691'_3114 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_resp'691'_3114 v4 v5 v6 v7 v8 v9 v10
du_resp'691'_3114 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_resp'691'_3114 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v1)) v2
         (coe v0 v2 v3) (coe v0 v2 v4)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v1))
            (coe v0 v2 v3) (coe v0 v2 v4) (coe v0 v2 v4)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1)))
               (coe v0 v2 v4))
            (coe
               MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v1 v2 v2 v3 v4
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1)) v2)
               v5))
         v6)
-- Relation.Binary.Construct.NaturalOrder.Left.respˡ
d_resp'737'_3178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_resp'737'_3178 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_resp'737'_3178 v4 v5 v6 v7 v8 v9 v10
du_resp'737'_3178 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_resp'737'_3178 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v1)) v4 v3
         (coe v0 v4 v2)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v1)) v3
            (coe v0 v3 v2) (coe v0 v4 v2)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v1))
               (coe v0 v3 v2) (coe v0 v4 v2) (coe v0 v4 v2)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1)))
                  (coe v0 v4 v2))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v1 v3 v4 v2 v2
                  v5
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1))
                     v2)))
            v6)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1)) v3
            v4 v5))
-- Relation.Binary.Construct.NaturalOrder.Left.resp₂
d_resp'8322'_3242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_resp'8322'_3242 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_resp'8322'_3242 v4 v5
du_resp'8322'_3242 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_resp'8322'_3242 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_resp'691'_3114 (coe v0) (coe v1))
      (coe du_resp'737'_3178 (coe v0) (coe v1))
-- Relation.Binary.Construct.NaturalOrder.Left.dec
d_dec_3246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_dec_3246 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_dec_3246 v4 v5 v6 v7
du_dec_3246 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_dec_3246 v0 v1 v2 v3 = coe v1 v2 (coe v0 v2 v3)
-- Relation.Binary.Construct.NaturalOrder.Left._.x∙y≤x
d_x'8729'y'8804'x_3322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'y'8804'x_3322 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_x'8729'y'8804'x_3322 v4 v5 v6 v7
du_x'8729'y'8804'x_3322 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'y'8804'x_3322 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe v0 v2 v3) (coe v0 (coe v0 v2 v2) v3)
         (coe v0 (coe v0 v2 v3) v2)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe v0 (coe v0 v2 v2) v3) (coe v0 v2 (coe v0 v2 v3))
            (coe v0 (coe v0 v2 v3) v2)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
               (coe v0 v2 (coe v0 v2 v3)) (coe v0 (coe v0 v2 v3) v2)
               (coe v0 (coe v0 v2 v3) v2)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
                         let v5
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                  (coe v0 (coe v0 v2 v3) v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454 v1 v2
                  (coe v0 v2 v3)))
            (coe
               MAlonzo.Code.Algebra.Structures.d_assoc_446
               (MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))
               v2 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
            (MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1))))
            v2 (coe v0 v2 v2) v3 v3
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
               (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))))
               (coe v0 v2 v2) v2
               (coe
                  MAlonzo.Code.Algebra.Structures.d_idem_482
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1))
                  v2))
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
               (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))))
               v3)))
-- Relation.Binary.Construct.NaturalOrder.Left._.x∙y≤y
d_x'8729'y'8804'y_3332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'y'8804'y_3332 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_x'8729'y'8804'y_3332 v4 v5 v6 v7
du_x'8729'y'8804'y_3332 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'y'8804'y_3332 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe v0 v2 v3) (coe v0 v2 (coe v0 v3 v3))
         (coe v0 (coe v0 v2 v3) v3)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe v0 v2 (coe v0 v3 v3)) (coe v0 (coe v0 v2 v3) v3)
            (coe v0 (coe v0 v2 v3) v3)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v4
                            = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
                      let v5
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
               (coe v0 (coe v0 v2 v3) v3))
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
               (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))))
               (coe v0 (coe v0 v2 v3) v3) (coe v0 v2 (coe v0 v3 v3))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_assoc_446
                  (MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))
                  v2 v3 v3)))
         (coe
            MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
            (MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1))))
            v2 v2 v3 (coe v0 v3 v3)
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
               (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))))
               v2)
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
               (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))))
               (coe v0 v3 v3) v3
               (coe
                  MAlonzo.Code.Algebra.Structures.d_idem_482
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1))
                  v3))))
-- Relation.Binary.Construct.NaturalOrder.Left._.∙-presʳ-≤
d_'8729''45'pres'691''45''8804'_3344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'pres'691''45''8804'_3344 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
                                     v9 v10
  = du_'8729''45'pres'691''45''8804'_3344 v4 v5 v6 v7 v8 v9 v10
du_'8729''45'pres'691''45''8804'_3344 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'pres'691''45''8804'_3344 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v7
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
          let v8
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v7) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
         v4 (coe v0 v4 v3) (coe v0 v4 (coe v0 v2 v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v7
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
             let v8
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v7) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
            (coe v0 v4 v3) (coe v0 (coe v0 v4 v2) v3)
            (coe v0 v4 (coe v0 v2 v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v7
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
                let v8
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v7) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
               (coe v0 (coe v0 v4 v2) v3) (coe v0 v4 (coe v0 v2 v3))
               (coe v0 v4 (coe v0 v2 v3))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v7
                               = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
                         let v8
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v7) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))))
                  (coe v0 v4 (coe v0 v2 v3)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_assoc_446
                  (MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))
                  v4 v2 v3))
            (coe
               MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
               (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1))))
               v4 (coe v0 v4 v2) v3 v3 v5
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isMagma_444
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))))
                  v3)))
         v6)
-- Relation.Binary.Construct.NaturalOrder.Left._.infimum
d_infimum_3356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_3356 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_infimum_3356 v4 v5 v6 v7
du_infimum_3356 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_infimum_3356 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_x'8729'y'8804'x_3322 (coe v0) (coe v1) (coe v2) (coe v3))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe du_x'8729'y'8804'y_3332 (coe v0) (coe v1) (coe v2) (coe v3))
         (coe
            du_'8729''45'pres'691''45''8804'_3344 (coe v0) (coe v1) (coe v2)
            (coe v3)))
-- Relation.Binary.Construct.NaturalOrder.Left.isPreorder
d_isPreorder_3362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_3362 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_isPreorder_3362 v4 v5
du_isPreorder_3362 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_isPreorder_3362 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1))))
      (coe
         du_reflexive_2876 (coe v0)
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1)))
         (coe MAlonzo.Code.Algebra.Structures.d_idem_482 (coe v1)))
      (coe
         du_trans_3046 (coe v0)
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1)))
-- Relation.Binary.Construct.NaturalOrder.Left.isPartialOrder
d_isPartialOrder_3396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_3396 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_isPartialOrder_3396 v4 v5
du_isPartialOrder_3396 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_isPartialOrder_3396 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe
         du_isPreorder_3362 (coe v0)
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))
      (coe
         du_antisym_2946 (coe v0)
         (coe
            MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1)))))
         (coe MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454 (coe v1)))
-- Relation.Binary.Construct.NaturalOrder.Left.isDecPartialOrder
d_isDecPartialOrder_3438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
d_isDecPartialOrder_3438 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isDecPartialOrder_3438 v4 v5 v6
du_isDecPartialOrder_3438 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
du_isDecPartialOrder_3438 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecPartialOrder'46'constructor_10175
      (coe du_isPartialOrder_3396 (coe v0) (coe v1)) (coe v2)
      (coe du_dec_3246 (coe v0) (coe v2))
-- Relation.Binary.Construct.NaturalOrder.Left.isTotalOrder
d_isTotalOrder_3444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_isTotalOrder_3444 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isTotalOrder_3444 v4 v5 v6
du_isTotalOrder_3444 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
du_isTotalOrder_3444 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe du_isPartialOrder_3396 (coe v0) (coe v1))
      (coe
         du_total_3000 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (coe
               MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1))))))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1))))))
         (coe v2)
         (coe MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454 (coe v1)))
-- Relation.Binary.Construct.NaturalOrder.Left.isDecTotalOrder
d_isDecTotalOrder_3488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_isDecTotalOrder_3488 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isDecTotalOrder_3488 v4 v5 v6 v7
du_isDecTotalOrder_3488 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
du_isDecTotalOrder_3488 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe du_isTotalOrder_3444 (coe v0) (coe v1) (coe v2)) (coe v3)
      (coe du_dec_3246 (coe v0) (coe v3))
-- Relation.Binary.Construct.NaturalOrder.Left.preorder
d_preorder_3496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_3496 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_preorder_3496 v4 v5
du_preorder_3496 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_3496 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe du_isPreorder_3362 (coe v0) (coe v1))
-- Relation.Binary.Construct.NaturalOrder.Left.poset
d_poset_3500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_3500 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_poset_3500 v4 v5
du_poset_3500 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_3500 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (coe du_isPartialOrder_3396 (coe v0) (coe v1))
-- Relation.Binary.Construct.NaturalOrder.Left.decPoset
d_decPoset_3504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecPoset_360
d_decPoset_3504 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_decPoset_3504 v4 v5 v6
du_decPoset_3504 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecPoset_360
du_decPoset_3504 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecPoset'46'constructor_6741
      (coe du_isDecPartialOrder_3438 (coe v0) (coe v1) (coe v2))
-- Relation.Binary.Construct.NaturalOrder.Left.totalOrder
d_totalOrder_3510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_totalOrder_3510 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_totalOrder_3510 v4 v5 v6
du_totalOrder_3510 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
du_totalOrder_3510 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      (coe du_isTotalOrder_3444 (coe v0) (coe v1) (coe v2))
-- Relation.Binary.Construct.NaturalOrder.Left.decTotalOrder
d_decTotalOrder_3516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_decTotalOrder_3516 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_decTotalOrder_3516 v4 v5 v6 v7
du_decTotalOrder_3516 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
du_decTotalOrder_3516 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      (coe du_isDecTotalOrder_3488 (coe v0) (coe v1) (coe v2) (coe v3))
