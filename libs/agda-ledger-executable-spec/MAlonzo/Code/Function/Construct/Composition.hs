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

module MAlonzo.Code.Function.Construct.Composition where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Construct.Composition._.congruent
d_congruent_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_congruent_50 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
               v12 ~v13 v14 v15 v16 v17 v18
  = du_congruent_50 v12 v14 v15 v16 v17 v18
du_congruent_50 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_congruent_50 v0 v1 v2 v3 v4 v5
  = coe v2 (coe v0 v3) (coe v0 v4) (coe v1 v3 v4 v5)
-- Function.Construct.Composition._.injective
d_injective_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_56 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
               v12 ~v13 v14 v15 v16 v17 v18
  = du_injective_56 v12 v14 v15 v16 v17 v18
du_injective_56 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_injective_56 v0 v1 v2 v3 v4 v5
  = coe v1 v3 v4 (coe v2 (coe v0 v3) (coe v0 v4) v5)
-- Function.Construct.Composition._.surjective
d_surjective_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_62 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12 v13 v14 v15 v16 v17 v18
  = du_surjective_62 v12 v13 v14 v15 v16 v17 v18
du_surjective_62 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_surjective_62 v0 v1 v2 v3 v4 v5 v6
  = let v7 = coe v5 v6 in
    case coe v7 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
        -> let v10 = coe v4 v8 in
           case coe v10 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v11)
                    (coe
                       v2 (coe v1 (coe v0 v11)) (coe v1 v8) v6
                       (coe v3 (coe v0 v11) v8 v12) v9)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Construct.Composition._.bijective
d_bijective_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_bijective_114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12 v13 v14 v15 v16 v17
  = du_bijective_114 v12 v13 v14 v15 v16 v17
du_bijective_114 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_bijective_114 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> case coe v5 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                    (coe du_injective_56 (coe v0) (coe v6) (coe v8))
                    (coe
                       du_surjective_62 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7)
                       (coe v9))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Construct.Composition._.inverseˡ
d_inverse'737'_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'737'_158 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                   ~v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
  = du_inverse'737'_158 v12 v13 v14 v15 v16 v17 v18 v19 v20
du_inverse'737'_158 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'737'_158 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      v4 (coe v2 (coe v0 (coe v1 (coe v3 v8)))) (coe v2 (coe v3 v8)) v8
      (coe
         v5 (coe v0 (coe v1 (coe v3 v8))) (coe v3 v8) (coe v6 (coe v3 v8)))
      (coe v7 v8)
-- Function.Construct.Composition._.inverseʳ
d_inverse'691'_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'691'_170 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                   ~v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
  = du_inverse'691'_170 v12 v13 v14 v15 v16 v17 v18 v19 v20
du_inverse'691'_170 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'691'_170 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      v4 (coe v1 (coe v3 (coe v2 (coe v0 v8)))) (coe v1 (coe v0 v8)) v8
      (coe
         v5 (coe v3 (coe v2 (coe v0 v8))) (coe v0 v8) (coe v7 (coe v0 v8)))
      (coe v6 v8)
-- Function.Construct.Composition._.inverseᵇ
d_inverse'7495'_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse'7495'_182 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    ~v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21
  = du_inverse'7495'_182 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21
du_inverse'7495'_182 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_inverse'7495'_182 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = case coe v8 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
        -> case coe v9 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v12 v13
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                    (coe
                       du_inverse'737'_158 (coe v0) (coe v1) (coe v2) (coe v3) (coe v5)
                       (coe v6) (coe v10) (coe v12))
                    (coe
                       du_inverse'691'_170 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                       (coe v7) (coe v11) (coe v13))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Construct.Composition._.isCongruent
d_isCongruent_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_isCongruent_226 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                  v12 ~v13 v14 v15
  = du_isCongruent_226 v12 v14 v15
du_isCongruent_226 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_isCongruent_226 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
      (coe
         (\ v3 v4 v5 ->
            coe
              MAlonzo.Code.Function.Structures.d_cong_32 v2 (coe v0 v3)
              (coe v0 v4)
              (coe MAlonzo.Code.Function.Structures.d_cong_32 v1 v3 v4 v5)))
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v1))
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2))
-- Function.Construct.Composition._.isInjection
d_isInjection_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92
d_isInjection_348 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                  v12 ~v13 v14 v15
  = du_isInjection_348 v12 v14 v15
du_isInjection_348 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92
du_isInjection_348 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsInjection'46'constructor_3991
      (coe
         du_isCongruent_226 (coe v0)
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1))
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v2)))
      (coe
         du_injective_56 (coe v0)
         (coe MAlonzo.Code.Function.Structures.d_injective_102 (coe v1))
         (coe MAlonzo.Code.Function.Structures.d_injective_102 (coe v2)))
-- Function.Construct.Composition._.isSurjection
d_isSurjection_478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162
d_isSurjection_478 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                   ~v11 v12 v13 v14 v15
  = du_isSurjection_478 v12 v13 v14 v15
du_isSurjection_478 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162
du_isSurjection_478 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Structures.C_IsSurjection'46'constructor_6455
      (coe
         du_isCongruent_226 (coe v0)
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_170 (coe v2))
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_170 (coe v3)))
      (coe
         du_surjective_62 (coe v0) (coe v1)
         (let v4
                = MAlonzo.Code.Function.Structures.d_isCongruent_170 (coe v3) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36
               (coe v4)))
         (coe
            MAlonzo.Code.Function.Structures.d_cong_32
            (coe MAlonzo.Code.Function.Structures.d_isCongruent_170 (coe v3)))
         (coe MAlonzo.Code.Function.Structures.d_surjective_172 (coe v2))
         (coe MAlonzo.Code.Function.Structures.d_surjective_172 (coe v3)))
-- Function.Construct.Composition._.isBijection
d_isBijection_608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
d_isBijection_608 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                  v12 v13 v14 v15
  = du_isBijection_608 v12 v13 v14 v15
du_isBijection_608 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
du_isBijection_608 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Structures.C_IsBijection'46'constructor_8915
      (coe
         du_isInjection_348 (coe v0)
         (coe MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v2))
         (coe MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3)))
      (coe
         du_surjective_62 (coe v0) (coe v1)
         (let v4
                = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
          let v5
                = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36
               (coe v5)))
         (coe
            MAlonzo.Code.Function.Structures.d_cong_32
            (coe
               MAlonzo.Code.Function.Structures.d_isCongruent_100
               (coe MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3))))
         (coe MAlonzo.Code.Function.Structures.d_surjective_242 (coe v2))
         (coe MAlonzo.Code.Function.Structures.d_surjective_242 (coe v3)))
-- Function.Construct.Composition._.isLeftInverse
d_isLeftInverse_784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
d_isLeftInverse_784 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    ~v11 v12 v13 v14 v15 v16 v17
  = du_isLeftInverse_784 v12 v13 v14 v15 v16 v17
du_isLeftInverse_784 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
du_isLeftInverse_784 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Function.Structures.C_IsLeftInverse'46'constructor_13035
      (coe
         du_isCongruent_226 (coe v0)
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4))
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v5)))
      (coe
         du_congruent_50 (coe v3)
         (coe MAlonzo.Code.Function.Structures.d_from'45'cong_326 (coe v5))
         (coe MAlonzo.Code.Function.Structures.d_from'45'cong_326 (coe v4)))
      (coe
         du_inverse'737'_158 (coe v0) (coe v2) (coe v1) (coe v3)
         (let v6
                = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v5) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36
               (coe v6)))
         (coe
            MAlonzo.Code.Function.Structures.d_cong_32
            (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v5)))
         (coe MAlonzo.Code.Function.Structures.d_inverse'737'_328 (coe v4))
         (coe MAlonzo.Code.Function.Structures.d_inverse'737'_328 (coe v5)))
-- Function.Construct.Composition._.isRightInverse
d_isRightInverse_918 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
d_isRightInverse_918 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                     ~v11 v12 v13 v14 v15 v16 v17
  = du_isRightInverse_918 v12 v13 v14 v15 v16 v17
du_isRightInverse_918 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
du_isRightInverse_918 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Function.Structures.C_IsRightInverse'46'constructor_16307
      (coe
         du_isCongruent_226 (coe v0)
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_402 (coe v4))
         (coe MAlonzo.Code.Function.Structures.d_isCongruent_402 (coe v5)))
      (coe
         du_congruent_50 (coe v3)
         (coe MAlonzo.Code.Function.Structures.d_from'45'cong_404 (coe v5))
         (coe MAlonzo.Code.Function.Structures.d_from'45'cong_404 (coe v4)))
      (coe
         du_inverse'691'_170 (coe v0) (coe v2) (coe v1) (coe v3)
         (let v6
                = MAlonzo.Code.Function.Structures.d_isCongruent_402 (coe v4) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34
               (coe v6)))
         (coe MAlonzo.Code.Function.Structures.d_from'45'cong_404 (coe v4))
         (coe MAlonzo.Code.Function.Structures.d_inverse'691'_406 (coe v4))
         (coe MAlonzo.Code.Function.Structures.d_inverse'691'_406 (coe v5)))
-- Function.Construct.Composition._.isInverse
d_isInverse_1052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468
d_isInverse_1052 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                 v12 v13 v14 v15 v16 v17
  = du_isInverse_1052 v12 v13 v14 v15 v16 v17
du_isInverse_1052 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468
du_isInverse_1052 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Function.Structures.C_IsInverse'46'constructor_19111
      (coe
         du_isLeftInverse_784 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v4))
         (coe
            MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v5)))
      (coe
         du_inverse'691'_170 (coe v0) (coe v2) (coe v1) (coe v3)
         (let v6
                = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v4) in
          let v7
                = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v6) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34
               (coe v7)))
         (coe
            MAlonzo.Code.Function.Structures.d_from'45'cong_326
            (coe
               MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v4)))
         (coe MAlonzo.Code.Function.Structures.d_inverse'691'_480 (coe v4))
         (coe MAlonzo.Code.Function.Structures.d_inverse'691'_480 (coe v5)))
-- Function.Construct.Composition._.function
d_function_1224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642
d_function_1224 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_function_1224 v9 v10
du_function_1224 ::
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642
du_function_1224 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.C_Func'46'constructor_5935
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_648 v1
              (coe MAlonzo.Code.Function.Bundles.d_to_648 v0 v2)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_to_648 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_cong_650 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_cong_650 (coe v1)))
-- Function.Construct.Composition._.injection
d_injection_1346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
d_injection_1346 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_injection_1346 v9 v10
du_injection_1346 ::
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
du_injection_1346 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.C_Injection'46'constructor_7999
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_712 v1
              (coe MAlonzo.Code.Function.Bundles.d_to_712 v0 v2)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_to_712 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_cong_714 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_cong_714 (coe v1)))
      (coe
         du_injective_56
         (coe MAlonzo.Code.Function.Bundles.d_to_712 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_injective_716 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_injective_716 (coe v1)))
-- Function.Construct.Composition._.surjection
d_surjection_1480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
d_surjection_1480 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9 v10
  = du_surjection_1480 v7 v8 v9 v10
du_surjection_1480 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
du_surjection_1480 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.C_Surjection'46'constructor_10213
      (coe
         (\ v4 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_782 v3
              (coe MAlonzo.Code.Function.Bundles.d_to_782 v2 v4)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_to_782 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_cong_784 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_cong_784 (coe v3)))
      (coe
         du_surjective_62
         (coe MAlonzo.Code.Function.Bundles.d_to_782 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_to_782 (coe v3))
         (let v4
                = coe
                    MAlonzo.Code.Function.Bundles.du_isCongruent_790 (coe v0) (coe v1)
                    (coe v3) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36
               (coe v4)))
         (coe MAlonzo.Code.Function.Bundles.d_cong_784 (coe v3))
         (coe MAlonzo.Code.Function.Bundles.d_surjective_786 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_surjective_786 (coe v3)))
-- Function.Construct.Composition._.bijection
d_bijection_1614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_bijection_1614 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du_bijection_1614 v8 v9 v10
du_bijection_1614 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_bijection_1614 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Bundles.C_Bijection'46'constructor_13257
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_852 v2
              (coe MAlonzo.Code.Function.Bundles.d_to_852 v1 v3)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_to_852 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_cong_854 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_cong_854 (coe v2)))
      (coe
         du_bijective_114
         (coe MAlonzo.Code.Function.Bundles.d_to_852 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_to_852 (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
         (coe MAlonzo.Code.Function.Bundles.d_cong_854 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_bijective_856 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_bijective_856 (coe v2)))
-- Function.Construct.Composition._.equivalence
d_equivalence_1768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_equivalence_1768 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_equivalence_1768 v9 v10
du_equivalence_1768 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_equivalence_1768 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.C_Equivalence'46'constructor_17233
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_938 v1
              (coe MAlonzo.Code.Function.Bundles.d_to_938 v0 v2)))
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Function.Bundles.d_from_940 v0
              (coe MAlonzo.Code.Function.Bundles.d_from_940 v1 v2)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_to_938 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_942 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_942 (coe v1)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_from_940 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_944 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_944 (coe v0)))
-- Function.Construct.Composition._.leftInverse
d_leftInverse_1798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d_leftInverse_1798 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du_leftInverse_1798 v8 v9 v10
du_leftInverse_1798 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
du_leftInverse_1798 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Bundles.C_LeftInverse'46'constructor_18307
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_958 v2
              (coe MAlonzo.Code.Function.Bundles.d_to_958 v1 v3)))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Function.Bundles.d_from_960 v1
              (coe MAlonzo.Code.Function.Bundles.d_from_960 v2 v3)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_to_958 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_962 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_962 (coe v2)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_from_960 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_964 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_964 (coe v1)))
      (coe
         du_inverse'737'_158
         (coe MAlonzo.Code.Function.Bundles.d_to_958 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_from_960 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_to_958 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_from_960 (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_962 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_inverse'737'_966 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_inverse'737'_966 (coe v2)))
-- Function.Construct.Composition._.rightInverse
d_rightInverse_1940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d_rightInverse_1940 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9 v10
  = du_rightInverse_1940 v6 v9 v10
du_rightInverse_1940 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
du_rightInverse_1940 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Bundles.C_RightInverse'46'constructor_21741
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_1036 v2
              (coe MAlonzo.Code.Function.Bundles.d_to_1036 v1 v3)))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Function.Bundles.d_from_1038 v1
              (coe MAlonzo.Code.Function.Bundles.d_from_1038 v2 v3)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_to_1036 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1040 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1040 (coe v2)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_from_1038 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1042 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1042 (coe v1)))
      (coe
         du_inverse'691'_170
         (coe MAlonzo.Code.Function.Bundles.d_to_1036 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_from_1038 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_to_1036 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_from_1038 (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1042 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_inverse'691'_1044 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_inverse'691'_1044 (coe v2)))
-- Function.Construct.Composition._.inverse
d_inverse_1986 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_inverse_1986 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 v8 v9 v10
  = du_inverse_1986 v6 v8 v9 v10
du_inverse_1986 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_inverse_1986 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.C_Inverse'46'constructor_24077
      (coe
         (\ v4 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_1064 v3
              (coe MAlonzo.Code.Function.Bundles.d_to_1064 v2 v4)))
      (coe
         (\ v4 ->
            coe
              MAlonzo.Code.Function.Bundles.d_from_1066 v2
              (coe MAlonzo.Code.Function.Bundles.d_from_1066 v3 v4)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1068 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1068 (coe v3)))
      (coe
         du_congruent_50
         (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v3))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1070 (coe v3))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1070 (coe v2)))
      (coe
         du_inverse'7495'_182
         (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v3))
         (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v3))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1)))
         (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1068 (coe v3))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1070 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_inverse_1072 (coe v2))
         (coe MAlonzo.Code.Function.Bundles.d_inverse_1072 (coe v3)))
-- Function.Construct.Composition._⟶-∘_
d__'10230''45''8728'__2144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642
d__'10230''45''8728'__2144 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du__'10230''45''8728'__2144
du__'10230''45''8728'__2144 ::
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642
du__'10230''45''8728'__2144 = coe du_function_1224
-- Function.Construct.Composition._↣-∘_
d__'8611''45''8728'__2146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
d__'8611''45''8728'__2146 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du__'8611''45''8728'__2146
du__'8611''45''8728'__2146 ::
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
du__'8611''45''8728'__2146 = coe du_injection_1346
-- Function.Construct.Composition._↠-∘_
d__'8608''45''8728'__2148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
d__'8608''45''8728'__2148 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du__'8608''45''8728'__2148
du__'8608''45''8728'__2148 ::
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
du__'8608''45''8728'__2148
  = coe
      du_surjection_1480
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Composition._⤖-∘_
d__'10518''45''8728'__2150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d__'10518''45''8728'__2150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du__'10518''45''8728'__2150
du__'10518''45''8728'__2150 ::
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du__'10518''45''8728'__2150
  = coe
      du_bijection_1614
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Composition._⇔-∘_
d__'8660''45''8728'__2152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d__'8660''45''8728'__2152 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du__'8660''45''8728'__2152
du__'8660''45''8728'__2152 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du__'8660''45''8728'__2152 = coe du_equivalence_1768
-- Function.Construct.Composition._↩-∘_
d__'8617''45''8728'__2154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d__'8617''45''8728'__2154 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du__'8617''45''8728'__2154
du__'8617''45''8728'__2154 ::
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
du__'8617''45''8728'__2154
  = coe
      du_leftInverse_1798
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Composition._↪-∘_
d__'8618''45''8728'__2156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d__'8618''45''8728'__2156 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du__'8618''45''8728'__2156
du__'8618''45''8728'__2156 ::
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
du__'8618''45''8728'__2156
  = coe
      du_rightInverse_1940
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Composition._↔-∘_
d__'8596''45''8728'__2158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d__'8596''45''8728'__2158 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du__'8596''45''8728'__2158
du__'8596''45''8728'__2158 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du__'8596''45''8728'__2158
  = coe
      du_inverse_1986
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Composition._∘-⟶_
d__'8728''45''10230'__2160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642 ->
  MAlonzo.Code.Function.Bundles.T_Func_642
d__'8728''45''10230'__2160 v0 v1 v2 v3 v4 v5
  = coe du__'10230''45''8728'__2144
-- Function.Construct.Composition._∘-↣_
d__'8728''45''8611'__2162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
d__'8728''45''8611'__2162 v0 v1 v2 v3 v4 v5
  = coe du__'8611''45''8728'__2146
-- Function.Construct.Composition._∘-↠_
d__'8728''45''8608'__2164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
d__'8728''45''8608'__2164 v0 v1 v2 v3 v4 v5
  = coe du__'8608''45''8728'__2148
-- Function.Construct.Composition._∘-⤖_
d__'8728''45''10518'__2166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d__'8728''45''10518'__2166 v0 v1 v2 v3 v4 v5
  = coe du__'10518''45''8728'__2150
-- Function.Construct.Composition._∘-⇔_
d__'8728''45''8660'__2168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d__'8728''45''8660'__2168 v0 v1 v2 v3 v4 v5
  = coe du__'8660''45''8728'__2152
-- Function.Construct.Composition._∘-↩_
d__'8728''45''8617'__2170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d__'8728''45''8617'__2170 v0 v1 v2 v3 v4 v5
  = coe du__'8617''45''8728'__2154
-- Function.Construct.Composition._∘-↪_
d__'8728''45''8618'__2172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d__'8728''45''8618'__2172 v0 v1 v2 v3 v4 v5
  = coe du__'8618''45''8728'__2156
-- Function.Construct.Composition._∘-↔_
d__'8728''45''8596'__2174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d__'8728''45''8596'__2174 v0 v1 v2 v3 v4 v5
  = coe du__'8596''45''8728'__2158
