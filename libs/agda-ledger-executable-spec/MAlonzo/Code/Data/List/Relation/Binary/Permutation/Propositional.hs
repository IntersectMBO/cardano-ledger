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

module MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Data.List.Relation.Binary.Permutation.Propositional._↭_
d__'8621'__16 a0 a1 a2 a3 = ()
data T__'8621'__16
  = C_refl_20 | C_prep_28 T__'8621'__16 | C_swap_38 T__'8621'__16 |
    C_trans_46 [AgdaAny] T__'8621'__16 T__'8621'__16
-- Data.List.Relation.Binary.Permutation.Propositional.↭-reflexive
d_'8621''45'reflexive_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> T__'8621'__16
d_'8621''45'reflexive_48 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_'8621''45'reflexive_48
du_'8621''45'reflexive_48 :: T__'8621'__16
du_'8621''45'reflexive_48 = coe C_refl_20
-- Data.List.Relation.Binary.Permutation.Propositional.↭-refl
d_'8621''45'refl_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> T__'8621'__16
d_'8621''45'refl_50 ~v0 ~v1 ~v2 = du_'8621''45'refl_50
du_'8621''45'refl_50 :: T__'8621'__16
du_'8621''45'refl_50 = coe C_refl_20
-- Data.List.Relation.Binary.Permutation.Propositional.↭-sym
d_'8621''45'sym_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> T__'8621'__16 -> T__'8621'__16
d_'8621''45'sym_56 ~v0 ~v1 v2 v3 v4 = du_'8621''45'sym_56 v2 v3 v4
du_'8621''45'sym_56 ::
  [AgdaAny] -> [AgdaAny] -> T__'8621'__16 -> T__'8621'__16
du_'8621''45'sym_56 v0 v1 v2
  = case coe v2 of
      C_refl_20 -> coe C_refl_20
      C_prep_28 v6
        -> case coe v0 of
             (:) v7 v8
               -> case coe v1 of
                    (:) v9 v10
                      -> coe
                           C_prep_28 (coe du_'8621''45'sym_56 (coe v8) (coe v10) (coe v6))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_swap_38 v7
        -> case coe v0 of
             (:) v8 v9
               -> case coe v9 of
                    (:) v10 v11
                      -> case coe v1 of
                           (:) v12 v13
                             -> case coe v13 of
                                  (:) v14 v15
                                    -> coe
                                         C_swap_38
                                         (coe du_'8621''45'sym_56 (coe v11) (coe v15) (coe v7))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_trans_46 v4 v6 v7
        -> coe
             C_trans_46 v4 (coe du_'8621''45'sym_56 (coe v4) (coe v1) (coe v7))
             (coe du_'8621''45'sym_56 (coe v0) (coe v4) (coe v6))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.↭-trans
d_'8621''45'trans_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] -> T__'8621'__16 -> T__'8621'__16 -> T__'8621'__16
d_'8621''45'trans_72 ~v0 ~v1 ~v2 v3 ~v4 v5 v6
  = du_'8621''45'trans_72 v3 v5 v6
du_'8621''45'trans_72 ::
  [AgdaAny] -> T__'8621'__16 -> T__'8621'__16 -> T__'8621'__16
du_'8621''45'trans_72 v0 v1 v2
  = let v3
          = let v3 = coe C_trans_46 v0 v1 v2 in
            case coe v2 of
              C_refl_20 -> coe v1
              _ -> coe v3 in
    case coe v1 of
      C_refl_20 -> coe v2
      _ -> coe v3
-- Data.List.Relation.Binary.Permutation.Propositional.↭-isEquivalence
d_'8621''45'isEquivalence_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8621''45'isEquivalence_82 ~v0 ~v1
  = du_'8621''45'isEquivalence_82
du_'8621''45'isEquivalence_82 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_'8621''45'isEquivalence_82
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (\ v0 -> coe C_refl_20) (coe du_'8621''45'sym_56)
      (\ v0 v1 v2 v3 v4 -> coe du_'8621''45'trans_72 v1 v3 v4)
-- Data.List.Relation.Binary.Permutation.Propositional.↭-setoid
d_'8621''45'setoid_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8621''45'setoid_84 ~v0 ~v1 = du_'8621''45'setoid_84
du_'8621''45'setoid_84 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_'8621''45'setoid_84
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe du_'8621''45'isEquivalence_82)
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning._._IsRelatedTo_
d__IsRelatedTo__114 a0 a1 a2 a3 = ()
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning._._∎
d__'8718'_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d__'8718'_116 ~v0 ~v1 = du__'8718'_116
du__'8718'_116 ::
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du__'8718'_116
  = let v0 = coe du_'8621''45'setoid_84 in
    coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning._._≡⟨⟩_
d__'8801''10216''10217'__118 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d__'8801''10216''10217'__118 v0 = coe v0
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning._.begin_
d_begin__120 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16
d_begin__120
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning._.step-≡
d_step'45''8801'_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45''8801'_124 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_step'45''8801'_124 v5
du_step'45''8801'_124 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45''8801'_124 v0 = coe v0
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning._.step-≡˘
d_step'45''8801''728'_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45''8801''728'_126 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_step'45''8801''728'_126 v5
du_step'45''8801''728'_126 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45''8801''728'_126 v0 = coe v0
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning.step-↭
d_step'45''8621'_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45''8621'_132 ~v0 ~v1 = du_step'45''8621'_132
du_step'45''8621'_132 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45''8621'_132
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
      (coe du_'8621''45'setoid_84)
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning.step-↭˘
d_step'45''8621''728'_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45''8621''728'_134 ~v0 ~v1 = du_step'45''8621''728'_134
du_step'45''8621''728'_134 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45''8621''728'_134
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
      (coe du_'8621''45'setoid_84)
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning.step-prep
d_step'45'prep_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45'prep_144 ~v0 ~v1 v2 ~v3 v4 ~v5 v6 v7
  = du_step'45'prep_144 v2 v4 v6 v7
du_step'45'prep_144 ::
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45'prep_144 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.C_relTo_34
      (coe
         C_trans_46
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v1))
         (coe C_prep_28 v3)
         (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
            (coe v2)))
-- Data.List.Relation.Binary.Permutation.Propositional.PermutationReasoning.step-swap
d_step'45'swap_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45'swap_164 ~v0 ~v1 v2 v3 ~v4 v5 ~v6 v7 v8
  = du_step'45'swap_164 v2 v3 v5 v7 v8
du_step'45'swap_164 ::
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  T__'8621'__16 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45'swap_164 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.C_relTo_34
      (coe
         C_trans_46
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v2)))
         (coe C_swap_38 v4)
         (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
            (coe v3)))
