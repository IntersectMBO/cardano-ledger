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

module MAlonzo.Code.Relation.Binary.Construct.On where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Induction.WellFounded
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.Construct.On._.implies
d_implies_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_implies_38 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 ~v8 v9 v10 v11
  = du_implies_38 v4 v9 v10 v11
du_implies_38 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_implies_38 v0 v1 v2 v3
  = coe
      v1
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
         (\ v4 v5 -> v4) v2 v3)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5) v0 v2 v3)
-- Relation.Binary.Construct.On._.reflexive
d_reflexive_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_reflexive_44 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 v8
  = du_reflexive_44 v4 v7 v8
du_reflexive_44 ::
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_reflexive_44 v0 v1 v2
  = coe
      v1
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
         (\ v3 v4 -> v3) v2 v2)
-- Relation.Binary.Construct.On._.irreflexive
d_irreflexive_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irreflexive_52 = erased
-- Relation.Binary.Construct.On._.symmetric
d_symmetric_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_symmetric_58 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 v8 v9
  = du_symmetric_58 v4 v7 v8 v9
du_symmetric_58 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_symmetric_58 v0 v1 v2 v3
  = coe
      v1
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
         (\ v4 v5 -> v4) v2 v3)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5) v0 v2 v3)
-- Relation.Binary.Construct.On._.transitive
d_transitive_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transitive_64 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 v8 v9 v10
  = du_transitive_64 v4 v7 v8 v9 v10
du_transitive_64 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transitive_64 v0 v1 v2 v3 v4
  = coe
      v1
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
         (\ v5 v6 -> v5) v2 v3)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v5 v6 -> v6) v0 v2 v3)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v5 v6 -> v6) v0 v3 v4)
-- Relation.Binary.Construct.On._.antisymmetric
d_antisymmetric_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisymmetric_72 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 ~v8 v9 v10 v11
  = du_antisymmetric_72 v4 v9 v10 v11
du_antisymmetric_72 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisymmetric_72 v0 v1 v2 v3
  = coe
      v1
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
         (\ v4 v5 -> v4) v2 v3)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5) v0 v2 v3)
-- Relation.Binary.Construct.On._.asymmetric
d_asymmetric_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asymmetric_78 = erased
-- Relation.Binary.Construct.On._.respects
d_respects_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_respects_86 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 ~v8 v9 v10 v11
  = du_respects_86 v4 v9 v10 v11
du_respects_86 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_respects_86 v0 v1 v2 v3
  = coe
      v1
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
         (\ v4 v5 -> v4) v2 v3)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5) v0 v2 v3)
-- Relation.Binary.Construct.On._.respects₂
d_respects'8322'_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_respects'8322'_94 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_respects'8322'_94 v4 v9
du_respects'8322'_94 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_respects'8322'_94 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                (\ v4 v5 v6 ->
                   coe
                     v2
                     (coe
                        MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
                        (\ v7 v8 -> v7) v4 v5)
                     (coe
                        MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
                        (\ v7 v8 -> v7) v5 v6)
                     (coe
                        MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                        (\ v7 v8 -> v8) v0 v5 v6)))
             (coe
                (\ v4 v5 v6 ->
                   coe
                     v3
                     (coe
                        MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                        (\ v7 v8 -> v8) v0 v5 v4)
                     (coe
                        MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292 v0
                        (\ v7 v8 -> v7) v5 v6)
                     (coe
                        MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                        (\ v7 v8 -> v8) v0 v5 v6)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Construct.On._.decidable
d_decidable_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_decidable_102 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 v8 v9
  = du_decidable_102 v4 v7 v8 v9
du_decidable_102 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_decidable_102 v0 v1 v2 v3 = coe v1 (coe v0 v2) (coe v0 v3)
-- Relation.Binary.Construct.On._.total
d_total_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_112 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 v8 v9
  = du_total_112 v4 v7 v8 v9
du_total_112 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_total_112 v0 v1 v2 v3 = coe v1 (coe v0 v2) (coe v0 v3)
-- Relation.Binary.Construct.On._.trichotomous
d_trichotomous_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_trichotomous_124 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 ~v8 v9 v10 v11
  = du_trichotomous_124 v4 v9 v10 v11
du_trichotomous_124 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
du_trichotomous_124 v0 v1 v2 v3 = coe v1 (coe v0 v2) (coe v0 v3)
-- Relation.Binary.Construct.On._.accessible
d_accessible_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42
d_accessible_136 = erased
-- Relation.Binary.Construct.On._.wellFounded
d_wellFounded_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> MAlonzo.Code.Induction.WellFounded.T_Acc_42) ->
  AgdaAny -> MAlonzo.Code.Induction.WellFounded.T_Acc_42
d_wellFounded_146 = erased
-- Relation.Binary.Construct.On._.isEquivalence
d_isEquivalence_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_166 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 v7
  = du_isEquivalence_166 v5 v7
du_isEquivalence_166 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_166 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (coe
         du_reflexive_44 (coe v0)
         (coe MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v1)))
      (coe
         du_symmetric_58 (coe v0)
         (coe MAlonzo.Code.Relation.Binary.Structures.d_sym_36 (coe v1)))
      (coe
         du_transitive_64 (coe v0)
         (coe MAlonzo.Code.Relation.Binary.Structures.d_trans_38 (coe v1)))
-- Relation.Binary.Construct.On._.isDecEquivalence
d_isDecEquivalence_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_isDecEquivalence_186 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 v7
  = du_isDecEquivalence_186 v5 v7
du_isDecEquivalence_186 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
du_isDecEquivalence_186 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecEquivalence'46'constructor_2293
      (coe
         du_isEquivalence_166 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
            (coe v1)))
      (coe
         du_decidable_102 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d__'8799'__52 (coe v1)))
-- Relation.Binary.Construct.On._.isPreorder
d_isPreorder_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_228 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9
  = du_isPreorder_228 v6 v9
du_isPreorder_228 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_isPreorder_228 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         du_isEquivalence_166 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe v1)))
      (coe
         du_implies_38 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82 (coe v1)))
      (coe
         du_transitive_64 (coe v0)
         (coe MAlonzo.Code.Relation.Binary.Structures.d_trans_84 (coe v1)))
-- Relation.Binary.Construct.On._.isPartialOrder
d_isPartialOrder_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_264 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9
  = du_isPartialOrder_264 v6 v9
du_isPartialOrder_264 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_isPartialOrder_264 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe
         du_isPreorder_228 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1)))
      (coe
         du_antisymmetric_72 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_antisym_172 (coe v1)))
-- Relation.Binary.Construct.On._.isDecPartialOrder
d_isDecPartialOrder_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
d_isDecPartialOrder_304 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9
  = du_isDecPartialOrder_304 v6 v9
du_isDecPartialOrder_304 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
du_isDecPartialOrder_304 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecPartialOrder'46'constructor_10175
      (coe
         du_isPartialOrder_264 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
            (coe v1)))
      (coe
         du_decidable_102 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d__'8799'__218 (coe v1)))
      (coe
         du_decidable_102 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__220
            (coe v1)))
-- Relation.Binary.Construct.On._.isStrictPartialOrder
d_isStrictPartialOrder_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_isStrictPartialOrder_356 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9
  = du_isStrictPartialOrder_356 v6 v9
du_isStrictPartialOrder_356 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
du_isStrictPartialOrder_356 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (coe
         du_isEquivalence_166 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
            (coe v1)))
      (coe
         du_transitive_64 (coe v0)
         (coe MAlonzo.Code.Relation.Binary.Structures.d_trans_282 (coe v1)))
      (coe
         du_respects'8322'_94 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_'60''45'resp'45''8776'_284
            (coe v1)))
-- Relation.Binary.Construct.On._.isTotalOrder
d_isTotalOrder_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_isTotalOrder_392 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9
  = du_isTotalOrder_392 v6 v9
du_isTotalOrder_392 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
du_isTotalOrder_392 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe
         du_isPartialOrder_264 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
            (coe v1)))
      (coe
         du_total_112 (coe v0)
         (coe MAlonzo.Code.Relation.Binary.Structures.d_total_390 (coe v1)))
-- Relation.Binary.Construct.On._.isDecTotalOrder
d_isDecTotalOrder_438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_isDecTotalOrder_438 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9
  = du_isDecTotalOrder_438 v6 v9
du_isDecTotalOrder_438 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
du_isDecTotalOrder_438 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe
         du_isTotalOrder_392 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
            (coe v1)))
      (coe
         du_decidable_102 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d__'8799'__442 (coe v1)))
      (coe
         du_decidable_102 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
            (coe v1)))
-- Relation.Binary.Construct.On._.isStrictTotalOrder
d_isStrictTotalOrder_498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_isStrictTotalOrder_498 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9
  = du_isStrictTotalOrder_498 v6 v9
du_isStrictTotalOrder_498 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
du_isStrictTotalOrder_498 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe
         du_isEquivalence_166 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_508
            (coe v1)))
      (coe
         du_transitive_64 (coe v0)
         (coe MAlonzo.Code.Relation.Binary.Structures.d_trans_510 (coe v1)))
      (coe
         du_trichotomous_124 (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_compare_512 (coe v1)))
-- Relation.Binary.Construct.On.preorder
d_preorder_552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_552 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 = du_preorder_552 v5 v6
du_preorder_552 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_552 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe
         du_isPreorder_228 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0)))
-- Relation.Binary.Construct.On.setoid
d_setoid_560 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_560 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_setoid_560 v4 v5
du_setoid_560 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_560 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         du_isEquivalence_166 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
-- Relation.Binary.Construct.On.decSetoid
d_decSetoid_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_decSetoid_568 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_decSetoid_568 v4 v5
du_decSetoid_568 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
du_decSetoid_568 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecSetoid'46'constructor_1373
      (coe
         du_isDecEquivalence_186 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isDecEquivalence_100
            (coe v0)))
-- Relation.Binary.Construct.On.poset
d_poset_576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_576 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 = du_poset_576 v5 v6
du_poset_576 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_576 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (coe
         du_isPartialOrder_264 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
            (coe v0)))
-- Relation.Binary.Construct.On.decPoset
d_decPoset_584 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecPoset_360 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecPoset_360
d_decPoset_584 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 = du_decPoset_584 v5 v6
du_decPoset_584 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecPoset_360 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecPoset_360
du_decPoset_584 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecPoset'46'constructor_6741
      (coe
         du_isDecPartialOrder_304 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isDecPartialOrder_382
            (coe v0)))
-- Relation.Binary.Construct.On.strictPartialOrder
d_strictPartialOrder_592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_strictPartialOrder_592 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_strictPartialOrder_592 v5 v6
du_strictPartialOrder_592 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
du_strictPartialOrder_592 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      (coe
         du_isStrictPartialOrder_356 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isStrictPartialOrder_494
            (coe v0)))
-- Relation.Binary.Construct.On.totalOrder
d_totalOrder_600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_totalOrder_600 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_totalOrder_600 v5 v6
du_totalOrder_600 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
du_totalOrder_600 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      (coe
         du_isTotalOrder_392 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0)))
-- Relation.Binary.Construct.On.decTotalOrder
d_decTotalOrder_608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_decTotalOrder_608 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_decTotalOrder_608 v5 v6
du_decTotalOrder_608 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
du_decTotalOrder_608 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      (coe
         du_isDecTotalOrder_438 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
            (coe v0)))
-- Relation.Binary.Construct.On.strictTotalOrder
d_strictTotalOrder_616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_strictTotalOrder_616 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_strictTotalOrder_616 v5 v6
du_strictTotalOrder_616 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
du_strictTotalOrder_616 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      (coe
         du_isStrictTotalOrder_498 (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isStrictTotalOrder_882
            (coe v0)))
