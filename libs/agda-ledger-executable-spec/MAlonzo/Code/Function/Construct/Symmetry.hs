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

module MAlonzo.Code.Function.Construct.Symmetry where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Construct.Symmetry._.f⁻¹
d_f'8315''185'_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_f'8315''185'_48 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_f'8315''185'_48 v9 v10
du_f'8315''185'_48 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_f'8315''185'_48 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v0 v1)
-- Function.Construct.Symmetry._.f∘f⁻¹≡id
d_f'8728'f'8315''185''8801'id_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_f'8728'f'8315''185''8801'id_50 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                 ~v8 v9 v10
  = du_f'8728'f'8315''185''8801'id_50 v9 v10
du_f'8728'f'8315''185''8801'id_50 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_f'8728'f'8315''185''8801'id_50 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v0 v1)
-- Function.Construct.Symmetry._.injective
d_injective_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_52 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10 v11 v12
               v13 v14 v15
  = du_injective_52 v8 v9 v10 v11 v12 v13 v14 v15
du_injective_52 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_injective_52 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      v3 v5 (coe v0 (coe du_f'8315''185'_48 (coe v1) (coe v6))) v6
      (coe
         v3 v5
         (coe
            v0
            (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
               (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1 v5)))
         (coe v0 (coe du_f'8315''185'_48 (coe v1) (coe v6)))
         (coe
            v2
            (coe
               v0
               (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                  (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1 v5)))
            v5 (coe du_f'8728'f'8315''185''8801'id_50 (coe v1) (coe v5)))
         (coe
            v4 (coe du_f'8315''185'_48 (coe v1) (coe v5))
            (coe du_f'8315''185'_48 (coe v1) (coe v6)) v7))
      (coe du_f'8728'f'8315''185''8801'id_50 (coe v1) (coe v6))
-- Function.Construct.Symmetry._.surjective
d_surjective_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_62 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du_surjective_62 v8 v9 v10
du_surjective_62 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_surjective_62 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0 v2)
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 v1
         (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
            (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1 (coe v0 v2)))
         v2
         (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
            (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1 (coe v0 v2))))
-- Function.Construct.Symmetry._.bijective
d_bijective_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_bijective_66 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10 v11 v12
  = du_bijective_66 v8 v9 v10 v11 v12
du_bijective_66 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_bijective_66 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_injective_52 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
      (coe du_surjective_62 (coe v0) (coe v1))
-- Function.Construct.Symmetry._.inverseʳ
d_inverse'691'_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'691'_94 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_inverse'691'_94 v10
du_inverse'691'_94 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'691'_94 v0 = coe v0
-- Function.Construct.Symmetry._.inverseˡ
d_inverse'737'_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'737'_98 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_inverse'737'_98 v10
du_inverse'737'_98 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'737'_98 v0 = coe v0
-- Function.Construct.Symmetry._.inverseᵇ
d_inverse'7495'_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse'7495'_102 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_inverse'7495'_102 v10
du_inverse'7495'_102 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_inverse'7495'_102 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Construct.Symmetry._.f⁻¹
d_f'8315''185'_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny
d_f'8315''185'_196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_f'8315''185'_196 v9 v10
du_f'8315''185'_196 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny
du_f'8315''185'_196 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe MAlonzo.Code.Function.Structures.d_surjective_242 v0 v1)
-- Function.Construct.Symmetry._.isBijection
d_isBijection_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
d_isBijection_198 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du_isBijection_198 v8 v9 v10
du_isBijection_198 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
du_isBijection_198 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsBijection'46'constructor_8915
      (coe
         MAlonzo.Code.Function.Structures.C_IsInjection'46'constructor_3991
         (coe
            MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
            (coe v2)
            (let v3
                   = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v1) in
             let v4
                   = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v3) in
             coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v4))
            (let v3
                   = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v1) in
             let v4
                   = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v3) in
             coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34
               (coe v4)))
         (coe
            du_injective_52 (coe v0)
            (coe MAlonzo.Code.Function.Structures.du_bijective_304 (coe v1))
            (let v3
                   = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v1) in
             let v4
                   = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v3) in
             coe
               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
               (coe
                  MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36
                  (coe v4)))
            (let v3
                   = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v1) in
             let v4
                   = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v3) in
             coe
               MAlonzo.Code.Relation.Binary.Structures.d_trans_38
               (coe
                  MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36
                  (coe v4)))
            (coe
               MAlonzo.Code.Function.Structures.d_cong_32
               (coe
                  MAlonzo.Code.Function.Structures.d_isCongruent_100
                  (coe
                     MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v1))))))
      (coe
         du_surjective_62 (coe v0)
         (coe MAlonzo.Code.Function.Structures.du_bijective_304 (coe v1)))
-- Function.Construct.Symmetry._.isBijection-≡
d_isBijection'45''8801'_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
d_isBijection'45''8801'_218 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_isBijection'45''8801'_218 v6 v7
du_isBijection'45''8801'_218 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
du_isBijection'45''8801'_218 v0 v1
  = coe
      du_isBijection_198 (coe v0) (coe v1)
      (coe
         (\ v2 v3 v4 ->
            let v5
                  = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v1) in
            let v6
                  = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v5) in
            let v7
                  = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v6) in
            coe
              MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
              (coe
                 MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v7))
              (coe du_f'8315''185'_196 (coe v1) (coe v2))))
-- Function.Construct.Symmetry._.isCongruent
d_isCongruent_312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_isCongruent_312 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10 v11
  = du_isCongruent_312 v10 v11
du_isCongruent_312 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_isCongruent_312 v0 v1
  = coe
      MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
      (coe v1)
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v0))
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v0))
-- Function.Construct.Symmetry._.isLeftInverse
d_isLeftInverse_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
d_isLeftInverse_378 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isLeftInverse_378 v10
du_isLeftInverse_378 ::
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
du_isLeftInverse_378 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsLeftInverse'46'constructor_13035
      (coe
         du_isCongruent_312
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_402 (coe v0))
         (coe MAlonzo.Code.Function.Structures.d_from'45'cong_404 (coe v0)))
      (coe
         MAlonzo.Code.Function.Structures.d_cong_32
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_402 (coe v0)))
      (coe MAlonzo.Code.Function.Structures.d_inverse'691'_406 (coe v0))
-- Function.Construct.Symmetry._.isRightInverse
d_isRightInverse_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
d_isRightInverse_448 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isRightInverse_448 v10
du_isRightInverse_448 ::
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
du_isRightInverse_448 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsRightInverse'46'constructor_16307
      (coe
         du_isCongruent_312
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v0))
         (coe MAlonzo.Code.Function.Structures.d_from'45'cong_326 (coe v0)))
      (coe
         MAlonzo.Code.Function.Structures.d_cong_32
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v0)))
      (coe MAlonzo.Code.Function.Structures.d_inverse'737'_328 (coe v0))
-- Function.Construct.Symmetry._.isInverse
d_isInverse_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468
d_isInverse_518 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isInverse_518 v10
du_isInverse_518 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468
du_isInverse_518 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsInverse'46'constructor_19111
      (coe
         du_isLeftInverse_378
         (coe
            MAlonzo.Code.Function.Structures.du_isRightInverse_544 (coe v0)))
      (coe
         MAlonzo.Code.Function.Structures.d_inverse'737'_328
         (coe
            MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0)))
-- Function.Construct.Symmetry._.IB.Eq₁._≈_
d__'8776'__640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__640 = erased
-- Function.Construct.Symmetry._.IB.Eq₂._≈_
d__'8776'__664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__664 = erased
-- Function.Construct.Symmetry._.from
d_from_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 -> AgdaAny -> AgdaAny
d_from_686 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 = du_from_686 v6 v7
du_from_686 ::
  MAlonzo.Code.Function.Bundles.T_Bijection_844 -> AgdaAny -> AgdaAny
du_from_686 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe MAlonzo.Code.Function.Bundles.du_surjective_860 v0 v1)
-- Function.Construct.Symmetry._.bijection
d_bijection_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_bijection_688 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_bijection_688 v4 v5 v6 v7
du_bijection_688 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_bijection_688 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.C_Bijection'46'constructor_13257
      (coe du_from_686 (coe v2)) (coe v3)
      (coe
         du_bijective_66
         (coe MAlonzo.Code.Function.Bundles.d_to_852 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_bijective_856 (coe v2))
         (let v4
                = coe
                    MAlonzo.Code.Function.Bundles.du_isBijection_876 (coe v0) (coe v1)
                    (coe v2) in
          let v5
                = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v4) in
          let v6
                = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v5) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36
               (coe v6)))
         (let v4
                = coe
                    MAlonzo.Code.Function.Bundles.du_isBijection_876 (coe v0) (coe v1)
                    (coe v2) in
          let v5
                = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v4) in
          let v6
                = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v5) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36
               (coe v6)))
         (coe MAlonzo.Code.Function.Bundles.d_cong_854 (coe v2)))
-- Function.Construct.Symmetry.bijection-≡
d_bijection'45''8801'_696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_bijection'45''8801'_696 ~v0 ~v1 ~v2 v3 ~v4 v5
  = du_bijection'45''8801'_696 v3 v5
du_bijection'45''8801'_696 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_bijection'45''8801'_696 v0 v1
  = coe
      du_bijection_688 (coe v0)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe v1)
      (coe
         (\ v2 v3 v4 ->
            let v5
                  = coe
                      MAlonzo.Code.Function.Bundles.du_isBijection_876 (coe v0)
                      (coe
                         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                      (coe v1) in
            let v6
                  = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v5) in
            let v7
                  = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v6) in
            let v8
                  = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v7) in
            coe
              MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
              (coe
                 MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v8))
              (coe du_from_686 (coe v1) (coe v2))))
-- Function.Construct.Symmetry._.equivalence
d_equivalence_792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_equivalence_792 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_equivalence_792 v6
du_equivalence_792 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_equivalence_792 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_Equivalence'46'constructor_17233
      (coe MAlonzo.Code.Function.Bundles.d_from_940 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to_938 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_944 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_942 (coe v0))
-- Function.Construct.Symmetry._.rightInverse
d_rightInverse_810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d_rightInverse_810 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_rightInverse_810 v6
du_rightInverse_810 ::
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
du_rightInverse_810 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_RightInverse'46'constructor_21741
      (coe MAlonzo.Code.Function.Bundles.d_from_960 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to_958 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_964 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_962 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_inverse'737'_966 (coe v0))
-- Function.Construct.Symmetry._.leftInverse
d_leftInverse_884 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d_leftInverse_884 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_leftInverse_884 v6
du_leftInverse_884 ::
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
du_leftInverse_884 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_LeftInverse'46'constructor_18307
      (coe MAlonzo.Code.Function.Bundles.d_from_1038 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to_1036 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1042 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1040 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_inverse'691'_1044 (coe v0))
-- Function.Construct.Symmetry._.inverse
d_inverse_910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_inverse_910 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_inverse_910 v6
du_inverse_910 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_inverse_910 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_Inverse'46'constructor_24077
      (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1070 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1068 (coe v0))
      (coe
         MAlonzo.Code.Data.Product.Base.du_swap_346
         (coe MAlonzo.Code.Function.Bundles.d_inverse_1072 (coe v0)))
-- Function.Construct.Symmetry.⤖-sym
d_'10518''45'sym_992 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_'10518''45'sym_992 ~v0 ~v1 ~v2 ~v3 v4 = du_'10518''45'sym_992 v4
du_'10518''45'sym_992 ::
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_'10518''45'sym_992 v0
  = coe
      du_bijection_688
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe v0) erased
-- Function.Construct.Symmetry.⇔-sym
d_'8660''45'sym_996 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8660''45'sym_996 ~v0 ~v1 ~v2 ~v3 = du_'8660''45'sym_996
du_'8660''45'sym_996 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8660''45'sym_996 = coe du_equivalence_792
-- Function.Construct.Symmetry.↩-sym
d_'8617''45'sym_998 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d_'8617''45'sym_998 ~v0 ~v1 ~v2 ~v3 = du_'8617''45'sym_998
du_'8617''45'sym_998 ::
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
du_'8617''45'sym_998 = coe du_rightInverse_810
-- Function.Construct.Symmetry.↪-sym
d_'8618''45'sym_1000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d_'8618''45'sym_1000 ~v0 ~v1 ~v2 ~v3 = du_'8618''45'sym_1000
du_'8618''45'sym_1000 ::
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
du_'8618''45'sym_1000 = coe du_leftInverse_884
-- Function.Construct.Symmetry.↔-sym
d_'8596''45'sym_1002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'8596''45'sym_1002 ~v0 ~v1 ~v2 ~v3 = du_'8596''45'sym_1002
du_'8596''45'sym_1002 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'8596''45'sym_1002 = coe du_inverse_910
-- Function.Construct.Symmetry.sym-⤖
d_sym'45''10518'_1004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_sym'45''10518'_1004 v0 v1 v2 v3 v4 = coe du_'10518''45'sym_992 v4
-- Function.Construct.Symmetry.sym-⇔
d_sym'45''8660'_1006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_sym'45''8660'_1006 v0 v1 v2 v3 = coe du_'8660''45'sym_996
-- Function.Construct.Symmetry.sym-↩
d_sym'45''8617'_1008 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d_sym'45''8617'_1008 v0 v1 v2 v3 = coe du_'8617''45'sym_998
-- Function.Construct.Symmetry.sym-↪
d_sym'45''8618'_1010 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d_sym'45''8618'_1010 v0 v1 v2 v3 = coe du_'8618''45'sym_1000
-- Function.Construct.Symmetry.sym-↔
d_sym'45''8596'_1012 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_sym'45''8596'_1012 v0 v1 v2 v3 = coe du_'8596''45'sym_1002
