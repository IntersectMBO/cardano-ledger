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

module MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Membership.Setoid.Properties
import qualified MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid
import qualified MAlonzo.Code.Data.List.Relation.Binary.Pointwise
import qualified MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Induction.WellFounded
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.List.Relation.Binary.Permutation.Setoid.Properties._._↭_
d__'8621'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] -> [AgdaAny] -> ()
d__'8621'__44 = erased
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.steps
d_steps_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  Integer
d_steps_46 ~v0 ~v1 ~v2 = du_steps_46
du_steps_46 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  Integer
du_steps_46
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_steps_130
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._._∈_
d__'8712'__108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> [AgdaAny] -> ()
d__'8712'__108 = erased
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.AllPairs
d_AllPairs_126 a0 a1 a2 a3 = ()
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋._≋_
d__'8779'__136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] -> [AgdaAny] -> ()
d__'8779'__136 = erased
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.++-cancelʳ
d_'43''43''45'cancel'691'_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_'43''43''45'cancel'691'_138 ~v0 ~v1 ~v2
  = du_'43''43''45'cancel'691'_138
du_'43''43''45'cancel'691'_138 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_'43''43''45'cancel'691'_138 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'43''43''45'cancel'691'_164
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.++-cancelˡ
d_'43''43''45'cancel'737'_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_'43''43''45'cancel'737'_140 ~v0 ~v1 ~v2
  = du_'43''43''45'cancel'737'_140
du_'43''43''45'cancel'737'_140 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_'43''43''45'cancel'737'_140 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'43''43''45'cancel'737'_154
      v0
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.++⁺
d_'43''43''8314'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_'43''43''8314'_142 ~v0 ~v1 ~v2 = du_'43''43''8314'_142
du_'43''43''8314'_142 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_'43''43''8314'_142 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'43''43''8314'_146
      v0 v1
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.Unique-resp-≋
d_Unique'45'resp'45''8779'_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
d_Unique'45'resp'45''8779'_144 ~v0 ~v1 v2
  = du_Unique'45'resp'45''8779'_144 v2
du_Unique'45'resp'45''8779'_144 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
du_Unique'45'resp'45''8779'_144 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_Unique'45'resp'45''8779'_72
      (coe v0)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.concat⁺
d_concat'8314'_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [[AgdaAny]] ->
  [[AgdaAny]] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_concat'8314'_146 ~v0 ~v1 ~v2 = du_concat'8314'_146
du_concat'8314'_146 ::
  [[AgdaAny]] ->
  [[AgdaAny]] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_concat'8314'_146
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_concat'8314'_170
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.filter⁺
d_filter'8314'_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_filter'8314'_148 ~v0 ~v1 ~v2 = du_filter'8314'_148
du_filter'8314'_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_filter'8314'_148 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_filter'8314'_206
      v2 v4 v5 v6
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.foldr⁺
d_foldr'8314'_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  AgdaAny
d_foldr'8314'_150 ~v0 ~v1 ~v2 = du_foldr'8314'_150
du_foldr'8314'_150 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  AgdaAny
du_foldr'8314'_150
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_foldr'8314'_130
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.map⁺
d_map'8314'_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_map'8314'_152 ~v0 ~v1 ~v2 = du_map'8314'_152
du_map'8314'_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_map'8314'_152 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_map'8314'_102
      v4 v5 v6 v7
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.reverse⁺
d_reverse'8314'_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_reverse'8314'_154 ~v0 ~v1 ~v2 = du_reverse'8314'_154
du_reverse'8314'_154 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_reverse'8314'_154
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_reverse'8314'_224
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.tabulate⁺
d_tabulate'8314'_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_tabulate'8314'_156 ~v0 ~v1 ~v2 = du_tabulate'8314'_156
du_tabulate'8314'_156 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_tabulate'8314'_156 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_tabulate'8314'_184
      v0
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.tabulate⁻
d_tabulate'8315'_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_tabulate'8315'_158 ~v0 ~v1 ~v2 = du_tabulate'8315'_158
du_tabulate'8315'_158 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_tabulate'8315'_158 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_tabulate'8315'_188
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.ʳ++⁺
d_'691''43''43''8314'_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_'691''43''43''8314'_160 ~v0 ~v1 ~v2 = du_'691''43''43''8314'_160
du_'691''43''43''8314'_160 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_'691''43''43''8314'_160 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'691''43''43''8314'_218
      v0 v1
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.≋-isEquivalence
d_'8779''45'isEquivalence_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8779''45'isEquivalence_162 ~v0 ~v1 v2
  = du_'8779''45'isEquivalence_162 v2
du_'8779''45'isEquivalence_162 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_'8779''45'isEquivalence_162 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'isEquivalence_68
      (coe v0)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.≋-length
d_'8779''45'length_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8779''45'length_164 = erased
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.≋-refl
d_'8779''45'refl_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_'8779''45'refl_166 ~v0 ~v1 v2 = du_'8779''45'refl_166 v2
du_'8779''45'refl_166 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_'8779''45'refl_166 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
      (coe v0)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.≋-reflexive
d_'8779''45'reflexive_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_'8779''45'reflexive_168 ~v0 ~v1 v2
  = du_'8779''45'reflexive_168 v2
du_'8779''45'reflexive_168 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_'8779''45'reflexive_168 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'reflexive_62
      (coe v0) v1
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.≋-setoid
d_'8779''45'setoid_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8779''45'setoid_170 ~v0 ~v1 v2 = du_'8779''45'setoid_170 v2
du_'8779''45'setoid_170 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_'8779''45'setoid_170 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'setoid_70
      (coe v0)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.≋-sym
d_'8779''45'sym_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_'8779''45'sym_172 ~v0 ~v1 v2 = du_'8779''45'sym_172 v2
du_'8779''45'sym_172 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_'8779''45'sym_172 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
      (coe v0)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.≋-trans
d_'8779''45'trans_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
d_'8779''45'trans_174 ~v0 ~v1 v2 = du_'8779''45'trans_174 v2
du_'8779''45'trans_174 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48
du_'8779''45'trans_174 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'trans_66
      (coe v0)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.Pointwise.All-resp-Pointwise
d_All'45'resp'45'Pointwise_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_All'45'resp'45'Pointwise_182 ~v0 ~v1 ~v2
  = du_All'45'resp'45'Pointwise_182
du_All'45'resp'45'Pointwise_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_All'45'resp'45'Pointwise_182 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Pointwise.du_All'45'resp'45'Pointwise_194
      v6 v7 v8 v9 v10
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.Pointwise.AllPairs-resp-Pointwise
d_AllPairs'45'resp'45'Pointwise_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
d_AllPairs'45'resp'45'Pointwise_184 ~v0 ~v1 ~v2
  = du_AllPairs'45'resp'45'Pointwise_184
du_AllPairs'45'resp'45'Pointwise_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
du_AllPairs'45'resp'45'Pointwise_184 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
                                     v10
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Pointwise.du_AllPairs'45'resp'45'Pointwise_228
      v6 v7 v8 v9 v10
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋.Pointwise.Any-resp-Pointwise
d_Any'45'resp'45'Pointwise_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
d_Any'45'resp'45'Pointwise_186 ~v0 ~v1 ~v2
  = du_Any'45'resp'45'Pointwise_186
du_Any'45'resp'45'Pointwise_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
du_Any'45'resp'45'Pointwise_186 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Pointwise.du_Any'45'resp'45'Pointwise_210
      v6 v7 v8 v9 v10
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.All-resp-↭
d_All'45'resp'45''8621'_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_All'45'resp'45''8621'_192 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_All'45'resp'45''8621'_192 v5 v6 v7 v8 v9
du_All'45'resp'45''8621'_192 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_All'45'resp'45''8621'_192 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v7
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Pointwise.du_All'45'resp'45'Pointwise_194
             (coe v0) (coe v1) (coe v2) (coe v7) (coe v4)
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v9 v10
        -> case coe v1 of
             (:) v11 v12
               -> case coe v2 of
                    (:) v13 v14
                      -> case coe v4 of
                           MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v17 v18
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60
                                  (coe v0 v11 v13 v9 v17)
                                  (coe
                                     du_All'45'resp'45''8621'_192 (coe v0) (coe v12) (coe v14)
                                     (coe v10) (coe v18))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v11 v12 v13
        -> case coe v1 of
             (:) v14 v15
               -> case coe v15 of
                    (:) v16 v17
                      -> case coe v2 of
                           (:) v18 v19
                             -> case coe v19 of
                                  (:) v20 v21
                                    -> case coe v4 of
                                         MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v24 v25
                                           -> case coe v25 of
                                                MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v28 v29
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60
                                                       (coe v0 v16 v18 v12 v28)
                                                       (coe
                                                          MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60
                                                          (coe v0 v14 v20 v11 v24)
                                                          (coe
                                                             du_All'45'resp'45''8621'_192 (coe v0)
                                                             (coe v17) (coe v21) (coe v13)
                                                             (coe v29)))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v6 v8 v9
        -> coe
             du_All'45'resp'45''8621'_192 (coe v0) (coe v6) (coe v2) (coe v9)
             (coe
                du_All'45'resp'45''8621'_192 (coe v0) (coe v1) (coe v6) (coe v8)
                (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.Any-resp-↭
d_Any'45'resp'45''8621'_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
d_Any'45'resp'45''8621'_234 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_Any'45'resp'45''8621'_234 v5 v6 v7 v8 v9
du_Any'45'resp'45''8621'_234 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
du_Any'45'resp'45''8621'_234 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v7
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Pointwise.du_Any'45'resp'45'Pointwise_210
             (coe v0) (coe v1) (coe v2) (coe v7) (coe v4)
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v9 v10
        -> case coe v1 of
             (:) v11 v12
               -> case coe v2 of
                    (:) v13 v14
                      -> case coe v4 of
                           MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v17
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46
                                  (coe v0 v11 v13 v9 v17)
                           MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v17
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                  (coe
                                     du_Any'45'resp'45''8621'_234 (coe v0) (coe v12) (coe v14)
                                     (coe v10) (coe v17))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v11 v12 v13
        -> case coe v1 of
             (:) v14 v15
               -> case coe v15 of
                    (:) v16 v17
                      -> case coe v2 of
                           (:) v18 v19
                             -> case coe v19 of
                                  (:) v20 v21
                                    -> case coe v4 of
                                         MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v24
                                           -> coe
                                                MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                                (coe
                                                   MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46
                                                   (coe v0 v14 v20 v11 v24))
                                         MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v24
                                           -> case coe v24 of
                                                MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v27
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46
                                                       (coe v0 v16 v18 v12 v27)
                                                MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v27
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                                       (coe
                                                          MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                                          (coe
                                                             du_Any'45'resp'45''8621'_234 (coe v0)
                                                             (coe v17) (coe v21) (coe v13)
                                                             (coe v27)))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v6 v8 v9
        -> coe
             du_Any'45'resp'45''8621'_234 (coe v0) (coe v6) (coe v2) (coe v9)
             (coe
                du_Any'45'resp'45''8621'_234 (coe v0) (coe v1) (coe v6) (coe v8)
                (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Permutation
d_Permutation_245 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.AllPairs-resp-↭
d_AllPairs'45'resp'45''8621'_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
d_AllPairs'45'resp'45''8621'_298 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
                                 v10
  = du_AllPairs'45'resp'45''8621'_298 v5 v6 v7 v8 v9 v10
du_AllPairs'45'resp'45''8621'_298 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
du_AllPairs'45'resp'45''8621'_298 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v8
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Pointwise.du_AllPairs'45'resp'45'Pointwise_228
             (coe v1) (coe v2) (coe v3) (coe v8) (coe v5)
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v10 v11
        -> case coe v2 of
             (:) v12 v13
               -> case coe v3 of
                    (:) v14 v15
                      -> case coe v5 of
                           MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C__'8759'__28 v18 v19
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C__'8759'__28
                                  (coe
                                     du_All'45'resp'45''8621'_192
                                     (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 v1 v14) (coe v13)
                                     (coe v15) (coe v11)
                                     (coe
                                        MAlonzo.Code.Data.List.Relation.Unary.All.du_map_166
                                        (coe
                                           (\ v20 ->
                                              coe
                                                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1 v20 v12
                                                v14 v10))
                                        (coe v13) (coe v18)))
                                  (coe
                                     du_AllPairs'45'resp'45''8621'_298 (coe v0) (coe v1) (coe v13)
                                     (coe v15) (coe v11) (coe v19))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v12 v13 v14
        -> case coe v2 of
             (:) v15 v16
               -> case coe v16 of
                    (:) v17 v18
                      -> case coe v3 of
                           (:) v19 v20
                             -> case coe v20 of
                                  (:) v21 v22
                                    -> case coe v1 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                           -> case coe v5 of
                                                MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C__'8759'__28 v27 v28
                                                  -> case coe v27 of
                                                       MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v31 v32
                                                         -> case coe v28 of
                                                              MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C__'8759'__28 v35 v36
                                                                -> coe
                                                                     MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C__'8759'__28
                                                                     (coe
                                                                        MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60
                                                                        (coe
                                                                           v0 v21 v19
                                                                           (coe
                                                                              v23 v21 v17 v19 v13
                                                                              (coe
                                                                                 v24 v17 v15 v21 v12
                                                                                 v31)))
                                                                        (coe
                                                                           du_All'45'resp'45''8621'_192
                                                                           (coe v23 v19) (coe v18)
                                                                           (coe v22) (coe v14)
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Relation.Unary.All.du_map_166
                                                                              (coe
                                                                                 (\ v37 ->
                                                                                    coe
                                                                                      v24 v37 v17
                                                                                      v19 v13))
                                                                              (coe v18) (coe v35))))
                                                                     (coe
                                                                        MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C__'8759'__28
                                                                        (coe
                                                                           du_All'45'resp'45''8621'_192
                                                                           (coe v23 v21) (coe v18)
                                                                           (coe v22) (coe v14)
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Relation.Unary.All.du_map_166
                                                                              (coe
                                                                                 (\ v37 ->
                                                                                    coe
                                                                                      v24 v37 v15
                                                                                      v21 v12))
                                                                              (coe v18) (coe v32)))
                                                                        (coe
                                                                           du_AllPairs'45'resp'45''8621'_298
                                                                           (coe v0) (coe v1)
                                                                           (coe v18) (coe v22)
                                                                           (coe v14) (coe v36)))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v7 v9 v10
        -> coe
             du_AllPairs'45'resp'45''8621'_298 (coe v0) (coe v1) (coe v7)
             (coe v3) (coe v10)
             (coe
                du_AllPairs'45'resp'45''8621'_298 (coe v0) (coe v1) (coe v2)
                (coe v7) (coe v9) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.∈-resp-↭
d_'8712''45'resp'45''8621'_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
d_'8712''45'resp'45''8621'_356 ~v0 ~v1 v2 v3 v4 v5
  = du_'8712''45'resp'45''8621'_356 v2 v3 v4 v5
du_'8712''45'resp'45''8621'_356 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
du_'8712''45'resp'45''8621'_356 v0 v1 v2 v3
  = coe
      du_Any'45'resp'45''8621'_234
      (coe
         (\ v4 v5 v6 v7 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_trans_38
              (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
              v1 v4 v5 v7 v6))
      (coe v2) (coe v3)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.Unique-resp-↭
d_Unique'45'resp'45''8621'_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
d_Unique'45'resp'45''8621'_358 ~v0 ~v1 v2 v3 v4
  = du_Unique'45'resp'45''8621'_358 v2 v3 v4
du_Unique'45'resp'45''8621'_358 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
du_Unique'45'resp'45''8621'_358 v0 v1 v2
  = coe
      du_AllPairs'45'resp'45''8621'_298
      (coe
         (\ v3 v4 v5 v6 ->
            coe
              v5
              (coe
                 MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                 (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                 v4 v3 v6)))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            (\ v3 v4 v5 v6 v7 v8 ->
               coe
                 v7
                 (coe
                    MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                    (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                    v3 v5 v4 v8
                    (coe
                       MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                       (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                       v4 v5 v6))))
         (coe
            (\ v3 v4 v5 v6 v7 v8 ->
               coe
                 v7
                 (coe
                    MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                    (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                    v4 v5 v3 v6 v8))))
      (coe v1) (coe v2)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.≋⇒↭
d_'8779''8658''8621'_362 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'8779''8658''8621'_362 ~v0 ~v1 ~v2 = du_'8779''8658''8621'_362
du_'8779''8658''8621'_362 ::
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'8779''8658''8621'_362
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.↭-respʳ-≋
d_'8621''45'resp'691''45''8779'_364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'8621''45'resp'691''45''8779'_364 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_'8621''45'resp'691''45''8779'_364 v2 v3 v4 v5 v6 v7
du_'8621''45'resp'691''45''8779'_364 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'8621''45'resp'691''45''8779'_364 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v8
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'trans_66
                v0 v1 v2 v3 v8 v4)
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v10 v11
        -> case coe v1 of
             (:) v12 v13
               -> case coe v2 of
                    (:) v14 v15
                      -> case coe v4 of
                           MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v20 v21
                             -> case coe v3 of
                                  (:) v22 v23
                                    -> coe
                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                         (coe
                                            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                            (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                               (coe v0))
                                            v12 v14 v22 v10 v20)
                                         (coe
                                            du_'8621''45'resp'691''45''8779'_364 (coe v0) (coe v13)
                                            (coe v15) (coe v23) (coe v21) (coe v11))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v12 v13 v14
        -> case coe v1 of
             (:) v15 v16
               -> case coe v16 of
                    (:) v17 v18
                      -> case coe v2 of
                           (:) v19 v20
                             -> case coe v20 of
                                  (:) v21 v22
                                    -> case coe v4 of
                                         MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v27 v28
                                           -> case coe v3 of
                                                (:) v29 v30
                                                  -> case coe v28 of
                                                       MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v35 v36
                                                         -> case coe v30 of
                                                              (:) v37 v38
                                                                -> coe
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                                                                     (coe
                                                                        MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                           (coe v0))
                                                                        v15 v21 v37 v12 v35)
                                                                     (coe
                                                                        MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                           (coe v0))
                                                                        v17 v19 v29 v13 v27)
                                                                     (coe
                                                                        du_'8621''45'resp'691''45''8779'_364
                                                                        (coe v0) (coe v18) (coe v22)
                                                                        (coe v38) (coe v36)
                                                                        (coe v14))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v7 v9 v10
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
             v7 v9
             (coe
                du_'8621''45'resp'691''45''8779'_364 (coe v0) (coe v7) (coe v2)
                (coe v3) (coe v4) (coe v10))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.↭-respˡ-≋
d_'8621''45'resp'737''45''8779'_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'8621''45'resp'737''45''8779'_396 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_'8621''45'resp'737''45''8779'_396 v2 v3 v4 v5 v6 v7
du_'8621''45'resp'737''45''8779'_396 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'8621''45'resp'737''45''8779'_396 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v8
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'trans_66
                v0 v3 v2 v1
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                   v0 v2 v3 v4)
                v8)
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v10 v11
        -> case coe v1 of
             (:) v12 v13
               -> case coe v2 of
                    (:) v14 v15
                      -> case coe v4 of
                           MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v20 v21
                             -> case coe v3 of
                                  (:) v22 v23
                                    -> coe
                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                         (coe
                                            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                            (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                               (coe v0))
                                            v22 v14 v12
                                            (coe
                                               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                  (coe v0))
                                               v14 v22 v20)
                                            v10)
                                         (coe
                                            du_'8621''45'resp'737''45''8779'_396 (coe v0) (coe v13)
                                            (coe v15) (coe v23) (coe v21) (coe v11))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v12 v13 v14
        -> case coe v1 of
             (:) v15 v16
               -> case coe v16 of
                    (:) v17 v18
                      -> case coe v2 of
                           (:) v19 v20
                             -> case coe v20 of
                                  (:) v21 v22
                                    -> case coe v4 of
                                         MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v27 v28
                                           -> case coe v3 of
                                                (:) v29 v30
                                                  -> case coe v28 of
                                                       MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v35 v36
                                                         -> case coe v30 of
                                                              (:) v37 v38
                                                                -> coe
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                                                                     (coe
                                                                        MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                           (coe v0))
                                                                        v29 v19 v17
                                                                        (coe
                                                                           MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                                                           (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                              (coe v0))
                                                                           v19 v29 v27)
                                                                        v12)
                                                                     (coe
                                                                        MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                           (coe v0))
                                                                        v37 v21 v15
                                                                        (coe
                                                                           MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                                                           (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                              (coe v0))
                                                                           v21 v37 v35)
                                                                        v13)
                                                                     (coe
                                                                        du_'8621''45'resp'737''45''8779'_396
                                                                        (coe v0) (coe v18) (coe v22)
                                                                        (coe v38) (coe v36)
                                                                        (coe v14))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v7 v9 v10
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
             v7
             (coe
                du_'8621''45'resp'737''45''8779'_396 (coe v0) (coe v7) (coe v2)
                (coe v3) (coe v4) (coe v9))
             v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.0<steps
d_0'60'steps_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_0'60'steps_434 ~v0 ~v1 ~v2 v3 v4 v5 = du_0'60'steps_434 v3 v4 v5
du_0'60'steps_434 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_0'60'steps_434 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v5
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v7 v8
        -> case coe v0 of
             (:) v9 v10
               -> case coe v1 of
                    (:) v11 v12
                      -> coe
                           MAlonzo.Code.Data.Nat.Properties.du_m'60'n'8658'm'60'1'43'n_2906
                           (coe du_0'60'steps_434 (coe v10) (coe v12) (coe v8))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v9 v10 v11
        -> case coe v0 of
             (:) v12 v13
               -> case coe v13 of
                    (:) v14 v15
                      -> case coe v1 of
                           (:) v16 v17
                             -> case coe v17 of
                                  (:) v18 v19
                                    -> coe
                                         MAlonzo.Code.Data.Nat.Properties.du_m'60'n'8658'm'60'1'43'n_2906
                                         (coe du_0'60'steps_434 (coe v15) (coe v19) (coe v11))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v4 v6 v7
        -> coe
             MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822
             (coe du_0'60'steps_434 (coe v0) (coe v4) (coe v6))
             (coe
                MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_steps_130
                   (coe v0) (coe v4) (coe v6)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.steps-respˡ
d_steps'45'resp'737'_460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_steps'45'resp'737'_460 = erased
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.steps-respʳ
d_steps'45'resp'691'_488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_steps'45'resp'691'_488 = erased
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._._._↭_
d__'8621'__520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] -> [AgdaAny] -> ()
d__'8621'__520 = erased
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.map⁺
d_map'8314'_528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_map'8314'_528 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_map'8314'_528 v5 v6 v7 v8 v9
du_map'8314'_528 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_map'8314'_528 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v7
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.du_map'8314'_382
                (coe v2) (coe v3)
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_map_120
                   (coe v1) (coe v2) (coe v3) (coe v7)))
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v9 v10
        -> case coe v2 of
             (:) v11 v12
               -> case coe v3 of
                    (:) v13 v14
                      -> coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                           (coe v1 v11 v13 v9)
                           (coe
                              du_map'8314'_528 (coe v0) (coe v1) (coe v12) (coe v14) (coe v10))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v11 v12 v13
        -> case coe v2 of
             (:) v14 v15
               -> case coe v15 of
                    (:) v16 v17
                      -> case coe v3 of
                           (:) v18 v19
                             -> case coe v19 of
                                  (:) v20 v21
                                    -> coe
                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                                         (coe v1 v14 v20 v11) (coe v1 v16 v18 v12)
                                         (coe
                                            du_map'8314'_528 (coe v0) (coe v1) (coe v17) (coe v21)
                                            (coe v13))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v6 v8 v9
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
             (coe MAlonzo.Code.Data.List.Base.du_map_22 (coe v0) (coe v6))
             (coe du_map'8314'_528 (coe v0) (coe v1) (coe v2) (coe v6) (coe v8))
             (coe du_map'8314'_528 (coe v0) (coe v1) (coe v6) (coe v3) (coe v9))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.shift
d_shift_562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_shift_562 ~v0 ~v1 v2 ~v3 v4 v5 v6 v7
  = du_shift_562 v2 v4 v5 v6 v7
du_shift_562 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_shift_562 v0 v1 v2 v3 v4
  = case coe v3 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
             v2
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
                (coe v0)
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v3) (coe v4)))
      (:) v5 v6
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5)
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v6) (coe v4))))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                   (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                   v5)
                (coe du_shift_562 (coe v0) (coe v1) (coe v2) (coe v6) (coe v4)))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
                   (coe
                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5)
                      (coe
                         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v6) (coe v4))))
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                      (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                      v5)
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                      (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                      v1)
                   (coe
                      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
                      (coe v0)
                      (coe
                         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v6) (coe v4))))
                (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                   (let v7
                          = coe
                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                              (coe v0) in
                    coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v7)))
                      (coe
                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
                         (coe
                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5)
                            (coe
                               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v6)
                               (coe v4)))))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.↭-shift
d_'8621''45'shift_590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'8621''45'shift_590 ~v0 ~v1 v2 v3 = du_'8621''45'shift_590 v2 v3
du_'8621''45'shift_590 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'8621''45'shift_590 v0 v1
  = coe
      du_shift_562 (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
         v1)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++⁺ˡ
d_'43''43''8314''737'_598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'43''43''8314''737'_598 ~v0 ~v1 v2 v3 ~v4 ~v5 v6
  = du_'43''43''8314''737'_598 v2 v3 v6
du_'43''43''8314''737'_598 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'43''43''8314''737'_598 v0 v1 v2
  = case coe v1 of
      [] -> coe v2
      (:) v3 v4
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                v3)
             (coe du_'43''43''8314''737'_598 (coe v0) (coe v4) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++⁺ʳ
d_'43''43''8314''691'_614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'43''43''8314''691'_614 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_'43''43''8314''691'_614 v2 v3 v4 v5 v6
du_'43''43''8314''691'_614 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'43''43''8314''691'_614 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v7
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.du_'43''43''8314'_290
                (coe v1) (coe v2) (coe v7)
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
                   (coe v0) (coe v3)))
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v9 v10
        -> case coe v1 of
             (:) v11 v12
               -> case coe v2 of
                    (:) v13 v14
                      -> coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                           v9
                           (coe
                              du_'43''43''8314''691'_614 (coe v0) (coe v12) (coe v14) (coe v3)
                              (coe v10))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v11 v12 v13
        -> case coe v1 of
             (:) v14 v15
               -> case coe v15 of
                    (:) v16 v17
                      -> case coe v2 of
                           (:) v18 v19
                             -> case coe v19 of
                                  (:) v20 v21
                                    -> coe
                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                                         v11 v12
                                         (coe
                                            du_'43''43''8314''691'_614 (coe v0) (coe v17) (coe v21)
                                            (coe v3) (coe v13))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v6 v8 v9
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v6) (coe v3))
             (coe
                du_'43''43''8314''691'_614 (coe v0) (coe v1) (coe v6) (coe v3)
                (coe v8))
             (coe
                du_'43''43''8314''691'_614 (coe v0) (coe v6) (coe v2) (coe v3)
                (coe v9))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++⁺
d_'43''43''8314'_640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'43''43''8314'_640 ~v0 ~v1 v2 v3 v4 v5 ~v6 v7 v8
  = du_'43''43''8314'_640 v2 v3 v4 v5 v7 v8
du_'43''43''8314'_640 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'43''43''8314'_640 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v3))
      (coe
         du_'43''43''8314''691'_614 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe v4))
      (coe du_'43''43''8314''737'_598 (coe v0) (coe v2) (coe v5))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-identityˡ
d_'43''43''45'identity'737'_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'43''43''45'identity'737'_646 ~v0 ~v1 v2 v3
  = du_'43''43''45'identity'737'_646 v2 v3
du_'43''43''45'identity'737'_646 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'43''43''45'identity'737'_646 v0 v1
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
      (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62
         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v1))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-identityʳ
d_'43''43''45'identity'691'_650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'43''43''45'identity'691'_650 ~v0 ~v1 v2 v3
  = du_'43''43''45'identity'691'_650 v2 v3
du_'43''43''45'identity'691'_650 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'43''43''45'identity'691'_650 v0 v1
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'reflexive_140
      (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-identity
d_'43''43''45'identity_654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''43''45'identity_654 ~v0 ~v1 v2
  = du_'43''43''45'identity_654 v2
du_'43''43''45'identity_654 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'43''43''45'identity_654 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'43''43''45'identity'737'_646 (coe v0))
      (coe du_'43''43''45'identity'691'_650 (coe v0))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-assoc
d_'43''43''45'assoc_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'43''43''45'assoc_656 ~v0 ~v1 v2 v3 v4 v5
  = du_'43''43''45'assoc_656 v2 v3 v4 v5
du_'43''43''45'assoc_656 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'43''43''45'assoc_656 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'reflexive_140
      (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
         (coe v3))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-comm
d_'43''43''45'comm_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'43''43''45'comm_664 ~v0 ~v1 v2 v3 v4
  = du_'43''43''45'comm_664 v2 v3 v4
du_'43''43''45'comm_664 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'43''43''45'comm_664 v0 v1 v2
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
             v0
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v1))
             v2 (coe du_'43''43''45'identity'691'_650 (coe v0) (coe v2))
      (:) v3 v4
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v4)))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                   (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                   v3)
                (coe du_'43''43''45'comm_664 (coe v0) (coe v4) (coe v2)))
             (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621''728'_200
                   v0
                   (coe
                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                      (coe
                         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v4)))
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v1))
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v1))
                   (let v5
                          = coe
                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                              (coe v0) in
                    coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v5)))
                      (coe
                         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v1)))
                   (coe du_'8621''45'shift_590 v0 v3 v2 v4)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-isMagma
d_'43''43''45'isMagma_674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'43''43''45'isMagma_674 ~v0 ~v1 v2
  = du_'43''43''45'isMagma_674 v2
du_'43''43''45'isMagma_674 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'43''43''45'isMagma_674 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'isEquivalence_148
         (coe v0))
      (\ v1 v2 v3 v4 v5 v6 ->
         coe du_'43''43''8314'_640 (coe v0) v1 v2 v3 v5 v6)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-isSemigroup
d_'43''43''45'isSemigroup_676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'43''43''45'isSemigroup_676 ~v0 ~v1 v2
  = du_'43''43''45'isSemigroup_676 v2
du_'43''43''45'isSemigroup_676 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'43''43''45'isSemigroup_676 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'43''43''45'isMagma_674 (coe v0))
      (coe du_'43''43''45'assoc_656 (coe v0))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-isMonoid
d_'43''43''45'isMonoid_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''43''45'isMonoid_678 ~v0 ~v1 v2
  = du_'43''43''45'isMonoid_678 v2
du_'43''43''45'isMonoid_678 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'43''43''45'isMonoid_678 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'43''43''45'isSemigroup_676 (coe v0))
      (coe du_'43''43''45'identity_654 (coe v0))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-isCommutativeMonoid
d_'43''43''45'isCommutativeMonoid_680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''43''45'isCommutativeMonoid_680 ~v0 ~v1 v2
  = du_'43''43''45'isCommutativeMonoid_680 v2
du_'43''43''45'isCommutativeMonoid_680 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'43''43''45'isCommutativeMonoid_680 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe du_'43''43''45'isMonoid_678 (coe v0))
      (coe du_'43''43''45'comm_664 (coe v0))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-magma
d_'43''43''45'magma_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'43''43''45'magma_682 ~v0 ~v1 v2 = du_'43''43''45'magma_682 v2
du_'43''43''45'magma_682 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'43''43''45'magma_682 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe du_'43''43''45'isMagma_674 (coe v0))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-semigroup
d_'43''43''45'semigroup_684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'43''43''45'semigroup_684 ~v0 ~v1 v2
  = du_'43''43''45'semigroup_684 v2
du_'43''43''45'semigroup_684 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'43''43''45'semigroup_684 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe du_'43''43''45'isSemigroup_676 (coe v0))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-monoid
d_'43''43''45'monoid_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'43''43''45'monoid_686 ~v0 ~v1 v2 = du_'43''43''45'monoid_686 v2
du_'43''43''45'monoid_686 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'43''43''45'monoid_686 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe du_'43''43''45'isMonoid_678 (coe v0))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++-commutativeMonoid
d_'43''43''45'commutativeMonoid_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''43''45'commutativeMonoid_688 ~v0 ~v1 v2
  = du_'43''43''45'commutativeMonoid_688 v2
du_'43''43''45'commutativeMonoid_688 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''43''45'commutativeMonoid_688 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe du_'43''43''45'isCommutativeMonoid_680 (coe v0))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.zoom
d_zoom_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_zoom_698 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_zoom_698 v2 v3 v4 v5 v6 v7
du_zoom_698 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_zoom_698 v0 v1 v2 v3 v4 v5
  = coe
      du_'43''43''8314''737'_598 (coe v0) (coe v1)
      (coe
         du_'43''43''8314''691'_614 (coe v0) (coe v3) (coe v4) (coe v2)
         (coe v5))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.inject
d_inject_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_inject_714 ~v0 ~v1 v2 v3 v4 ~v5 v6 v7 v8 v9
  = du_inject_714 v2 v3 v4 v6 v7 v8 v9
du_inject_714 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_inject_714 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v4)))
      (coe
         du_'43''43''8314''737'_598 (coe v0) (coe v2)
         (coe
            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
               v1)
            v6))
      (coe
         du_'43''43''8314''691'_614 (coe v0) (coe v2) (coe v3)
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v4))
         (coe v5))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.shifts
d_shifts_728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_shifts_728 ~v0 ~v1 v2 v3 v4 v5 = du_shifts_728 v2 v3 v4 v5
du_shifts_728 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_shifts_728 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621''728'_200
         v0
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v3)))
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
            (coe v3))
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v3)))
         (coe
            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
            v0
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
               (coe v3))
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v1))
               (coe v3))
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v3)))
            (coe
               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
               v0
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v1))
                  (coe v3))
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v3)))
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v3)))
               (let v4
                      = coe
                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                          (coe v0) in
                coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4)))
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
                     (coe
                        MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v3))))
               (coe du_'43''43''45'assoc_656 (coe v0) (coe v2) (coe v1) (coe v3)))
            (coe
               du_'43''43''8314''691'_614 (coe v0)
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v1))
               (coe v3) (coe du_'43''43''45'comm_664 (coe v0) (coe v1) (coe v2))))
         (coe du_'43''43''45'assoc_656 (coe v0) (coe v1) (coe v2) (coe v3)))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.dropMiddleElement-≋
d_dropMiddleElement'45''8779'_746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_dropMiddleElement'45''8779'_746 ~v0 ~v1 v2 v3 v4 v5 v6 v7 v8
  = du_dropMiddleElement'45''8779'_746 v2 v3 v4 v5 v6 v7 v8
du_dropMiddleElement'45''8779'_746 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_dropMiddleElement'45''8779'_746 v0 v1 v2 v3 v4 v5 v6
  = case coe v2 of
      []
        -> case coe v3 of
             []
               -> case coe v6 of
                    MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v11 v12
                      -> coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38
                           v12
                    _ -> MAlonzo.RTE.mazUnreachableError
             (:) v7 v8
               -> case coe v6 of
                    MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v13 v14
                      -> coe
                           du_'8621''45'resp'737''45''8779'_396 (coe v0)
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v7)
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8) (coe v5)))
                           (coe
                              MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8)
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                 (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                                 (coe v5)))
                           (coe
                              MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v4))
                           (coe
                              MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                              v0
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v4))
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                    (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                                    (coe v5)))
                              v14)
                           (coe du_shift_562 (coe v0) (coe v7) (coe v13) (coe v8) (coe v5))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v7 v8
        -> case coe v3 of
             []
               -> case coe v6 of
                    MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v13 v14
                      -> coe
                           du_'8621''45'resp'691''45''8779'_364 (coe v0)
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v7)
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8) (coe v4)))
                           (coe
                              MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8)
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                 (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                                 (coe v4)))
                           (coe
                              MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v3) (coe v5))
                           (coe v14)
                           (coe
                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
                              v0
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                    (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                                    (coe v4)))
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v7)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8) (coe v4)))
                              (coe
                                 du_shift_562 (coe v0) (coe v7)
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                    (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                       (coe v0))
                                    v7 v1 v13)
                                 (coe v8) (coe v4)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             (:) v9 v10
               -> case coe v6 of
                    MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v15 v16
                      -> coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                           v15
                           (coe
                              du_dropMiddleElement'45''8779'_746 (coe v0) (coe v1) (coe v8)
                              (coe v10) (coe v4) (coe v5) (coe v16))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.dropMiddleElement
d_dropMiddleElement_788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_dropMiddleElement_788 ~v0 ~v1 v2 v3 v4 v5 v6 v7 v8
  = du_dropMiddleElement_788 v2 v3 v4 v5 v6 v7 v8
du_dropMiddleElement_788 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_dropMiddleElement_788 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_helper_834 (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62
            (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
            (coe v4)))
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v3)
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62
            (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
            (coe v5)))
      (coe v6) (coe v2) (coe v3) (coe v4) (coe v5)
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
         (coe v0)
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62
               (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
               (coe v4))))
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
         (coe v0)
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v3)
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62
               (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
               (coe v5))))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.lemma
d_lemma_814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lemma_814 ~v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10 v11 v12 v13
            v14 v15
  = du_lemma_814 v2 v9 v10 v11 v12 v13 v14 v15
du_lemma_814 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lemma_814 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      v1 v3 v4
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
         v1 v2 v3 v5 v6)
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
         v4 v3 v7)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.helper
d_helper_834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_helper_834 ~v0 ~v1 v2 v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10 v11 v12 v13
             v14 v15 v16 v17
  = du_helper_834 v2 v3 v9 v10 v11 v12 v13 v14 v15 v16 v17
du_helper_834 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_helper_834 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v4 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v13
        -> coe
             du_dropMiddleElement'45''8779'_746 (coe v0) (coe v1) (coe v5)
             (coe v6) (coe v7) (coe v8)
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'trans_66
                v0
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62
                      (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                      (coe v7)))
                v3
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v6)
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62
                      (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                      (coe v8)))
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'trans_66
                   v0
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                      (coe
                         MAlonzo.Code.Data.List.Base.du__'43''43'__62
                         (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                         (coe v7)))
                   v2 v3 v9 v13)
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                   v0
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v6)
                      (coe
                         MAlonzo.Code.Data.List.Base.du__'43''43'__62
                         (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                         (coe v8)))
                   v3 v10))
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v15 v16
        -> case coe v2 of
             (:) v17 v18
               -> case coe v3 of
                    (:) v19 v20
                      -> case coe v5 of
                           []
                             -> case coe v6 of
                                  []
                                    -> case coe v9 of
                                         MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v25 v26
                                           -> case coe v10 of
                                                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v31 v32
                                                  -> coe
                                                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                       (coe
                                                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                          (coe v18)
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                             v0 v18 v20 v8
                                                             (coe
                                                                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779''728'_222
                                                                (coe v0) (coe v20) (coe v8)
                                                                (let v33
                                                                       = coe
                                                                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                           (coe v0) in
                                                                 coe
                                                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                   (coe
                                                                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                      (coe
                                                                         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                         (coe v33)))
                                                                   (coe v8))
                                                                (coe v32))
                                                             v16)
                                                          (coe v26))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  (:) v21 v22
                                    -> case coe v9 of
                                         MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v27 v28
                                           -> case coe v10 of
                                                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v33 v34
                                                  -> coe
                                                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                       (coe
                                                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                          (coe v18)
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                             v0 v18 v20
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                (coe v21)
                                                                (coe
                                                                   MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                   (coe v22) (coe v8)))
                                                             (coe
                                                                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779''728'_222
                                                                (coe v0) (coe v20)
                                                                (coe
                                                                   MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                   (coe v22)
                                                                   (coe
                                                                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                      (coe v1) (coe v8)))
                                                                (coe
                                                                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                   v0
                                                                   (coe
                                                                      MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                      (coe v22)
                                                                      (coe
                                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                         (coe v1) (coe v8)))
                                                                   (coe
                                                                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                      (coe v21)
                                                                      (coe
                                                                         MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                         (coe v22) (coe v8)))
                                                                   (coe
                                                                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                      (coe v21)
                                                                      (coe
                                                                         MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                         (coe v22) (coe v8)))
                                                                   (let v35
                                                                          = coe
                                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                              (coe v0) in
                                                                    coe
                                                                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                      (coe
                                                                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                            (coe v35)))
                                                                      (coe
                                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                         (coe v21)
                                                                         (coe
                                                                            MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                            (coe v22) (coe v8))))
                                                                   (coe
                                                                      du_shift_562 (coe v0)
                                                                      (coe v21)
                                                                      (coe
                                                                         du_lemma_814 (coe v0)
                                                                         (coe v1) (coe v17)
                                                                         (coe v19) (coe v21)
                                                                         (coe v27) (coe v15)
                                                                         (coe v33))
                                                                      (coe v22) (coe v8)))
                                                                (coe v34))
                                                             v16)
                                                          (coe v28))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           (:) v21 v22
                             -> case coe v6 of
                                  []
                                    -> case coe v9 of
                                         MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v27 v28
                                           -> case coe v10 of
                                                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v33 v34
                                                  -> coe
                                                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                       (coe
                                                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                          v0
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                             (coe v21)
                                                             (coe
                                                                MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                (coe v22) (coe v7)))
                                                          (coe
                                                             MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                             (coe v22)
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                (coe v1) (coe v7)))
                                                          v8
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                             (coe v18)
                                                             (coe
                                                                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                v0 v18 v20 v8
                                                                (coe
                                                                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779''728'_222
                                                                   (coe v0) (coe v20) (coe v8)
                                                                   (let v35
                                                                          = coe
                                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                              (coe v0) in
                                                                    coe
                                                                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                      (coe
                                                                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                            (coe v35)))
                                                                      (coe v8))
                                                                   (coe v34))
                                                                v16)
                                                             (coe v28))
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
                                                             v0
                                                             (coe
                                                                MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                (coe v22)
                                                                (coe
                                                                   MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                   (coe
                                                                      MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                      (coe v1))
                                                                   (coe v7)))
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                (coe v21)
                                                                (coe
                                                                   MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                   (coe v22) (coe v7)))
                                                             (coe
                                                                du_shift_562 (coe v0) (coe v21)
                                                                (coe
                                                                   du_lemma_814 (coe v0) (coe v1)
                                                                   (coe v19) (coe v17) (coe v21)
                                                                   (coe v33)
                                                                   (coe
                                                                      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                                                      (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                         (coe v0))
                                                                      v17 v19 v15)
                                                                   (coe v27))
                                                                (coe v22) (coe v7))))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  (:) v23 v24
                                    -> case coe v9 of
                                         MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v29 v30
                                           -> case coe v10 of
                                                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v35 v36
                                                  -> coe
                                                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                       (coe
                                                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                          v0
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                             (coe v21)
                                                             (coe
                                                                MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                (coe v22) (coe v7)))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                             (coe v23)
                                                             (coe
                                                                MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                (coe v24) (coe v8)))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                             (coe v23)
                                                             (coe
                                                                MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                (coe v24) (coe v8)))
                                                          (let v37
                                                                 = coe
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                     (coe v0) in
                                                           coe
                                                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                             (coe
                                                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                (coe
                                                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                   (coe v37)))
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                (coe v23)
                                                                (coe
                                                                   MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                   (coe v24) (coe v8))))
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                             (coe
                                                                du_lemma_814 (coe v0) (coe v21)
                                                                (coe v17) (coe v19) (coe v23)
                                                                (coe v29) (coe v15) (coe v35))
                                                             (coe
                                                                du_helper_834 (coe v0) (coe v1)
                                                                (coe v18) (coe v20) (coe v16)
                                                                (coe v22) (coe v24) (coe v7)
                                                                (coe v8) (coe v30) (coe v36))))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v17 v18 v19
        -> case coe v2 of
             (:) v20 v21
               -> case coe v21 of
                    (:) v22 v23
                      -> case coe v3 of
                           (:) v24 v25
                             -> case coe v25 of
                                  (:) v26 v27
                                    -> case coe v5 of
                                         []
                                           -> case coe v6 of
                                                []
                                                  -> case coe v9 of
                                                       MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v32 v33
                                                         -> case coe v10 of
                                                              MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v38 v39
                                                                -> coe
                                                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                     (coe
                                                                        MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                        (coe v21)
                                                                        (coe
                                                                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                           v0 v21 v25 v8
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779''728'_222
                                                                              (coe v0) (coe v25)
                                                                              (coe v8)
                                                                              (let v40
                                                                                     = coe
                                                                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                         (coe v0) in
                                                                               coe
                                                                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                 (coe
                                                                                    MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                    (coe
                                                                                       MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                       (coe v40)))
                                                                                 (coe v8))
                                                                              (coe v39))
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                              (coe
                                                                                 MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                                 (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                    (coe v0))
                                                                                 v22 v20 v26
                                                                                 (coe
                                                                                    MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                                    (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                       (coe v0))
                                                                                    v22 v1 v20
                                                                                    (coe
                                                                                       MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                                       (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                          (coe v0))
                                                                                       v22 v24 v1
                                                                                       v18
                                                                                       (coe
                                                                                          MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                                                                          (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                             (coe
                                                                                                v0))
                                                                                          v1 v24
                                                                                          v38))
                                                                                    v32)
                                                                                 v17)
                                                                              v19))
                                                                        (coe v33))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                (:) v28 v29
                                                  -> case coe v29 of
                                                       []
                                                         -> case coe v9 of
                                                              MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v34 v35
                                                                -> case coe v10 of
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v40 v41
                                                                       -> coe
                                                                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                            (coe
                                                                               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                               (coe v21)
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                  v0 v21
                                                                                  (coe
                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                     (coe v24)
                                                                                     (coe v27))
                                                                                  (coe
                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                     (coe v28)
                                                                                     (coe v8))
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779''728'_222
                                                                                     (coe v0)
                                                                                     (coe
                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                        (coe v24)
                                                                                        (coe v27))
                                                                                     (coe
                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                        (coe v28)
                                                                                        (coe v8))
                                                                                     (let v42
                                                                                            = coe
                                                                                                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                                (coe
                                                                                                   v0) in
                                                                                      coe
                                                                                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                        (coe
                                                                                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                           (coe
                                                                                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                              (coe
                                                                                                 v42)))
                                                                                        (coe
                                                                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                           (coe v28)
                                                                                           (coe
                                                                                              v8)))
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                                        v40
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_tail_70
                                                                                           (coe
                                                                                              v41))))
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                     v18 v19))
                                                                               (coe v35))
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       (:) v30 v31
                                                         -> case coe v9 of
                                                              MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v36 v37
                                                                -> case coe v10 of
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v42 v43
                                                                       -> coe
                                                                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                            (coe
                                                                               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                               (coe v21)
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                  v0 v21
                                                                                  (coe
                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                     (coe v24)
                                                                                     (coe v27))
                                                                                  (coe
                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                     (coe v28)
                                                                                     (coe
                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                        (coe v30)
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                           (coe v31)
                                                                                           (coe
                                                                                              v8))))
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                     (coe
                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                        (coe v28)
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                           (coe v31)
                                                                                           (coe
                                                                                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                              (coe
                                                                                                 v1)
                                                                                              (coe
                                                                                                 v8))))
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                        v0
                                                                                        (coe
                                                                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                           (coe v28)
                                                                                           (coe
                                                                                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                              (coe
                                                                                                 v31)
                                                                                              (coe
                                                                                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                 (coe
                                                                                                    v1)
                                                                                                 (coe
                                                                                                    v8))))
                                                                                        (coe
                                                                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                           (coe v28)
                                                                                           (coe
                                                                                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                              (coe
                                                                                                 v30)
                                                                                              (coe
                                                                                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                 (coe
                                                                                                    v31)
                                                                                                 (coe
                                                                                                    v8))))
                                                                                        (coe
                                                                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                           (coe v28)
                                                                                           (coe
                                                                                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                              (coe
                                                                                                 v30)
                                                                                              (coe
                                                                                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                 (coe
                                                                                                    v31)
                                                                                                 (coe
                                                                                                    v8))))
                                                                                        (let v44
                                                                                               = coe
                                                                                                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                                   (coe
                                                                                                      v0) in
                                                                                         coe
                                                                                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                           (coe
                                                                                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                              (coe
                                                                                                 MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                 (coe
                                                                                                    v44)))
                                                                                           (coe
                                                                                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                              (coe
                                                                                                 v28)
                                                                                              (coe
                                                                                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                 (coe
                                                                                                    v30)
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                    (coe
                                                                                                       v31)
                                                                                                    (coe
                                                                                                       v8)))))
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                           (coe
                                                                                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                              (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                 (coe
                                                                                                    v0))
                                                                                              v28)
                                                                                           (coe
                                                                                              du_shift_562
                                                                                              (coe
                                                                                                 v0)
                                                                                              (coe
                                                                                                 v30)
                                                                                              (coe
                                                                                                 du_lemma_814
                                                                                                 (coe
                                                                                                    v0)
                                                                                                 (coe
                                                                                                    v1)
                                                                                                 (coe
                                                                                                    v20)
                                                                                                 (coe
                                                                                                    v26)
                                                                                                 (coe
                                                                                                    v30)
                                                                                                 (coe
                                                                                                    v36)
                                                                                                 (coe
                                                                                                    v17)
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_head_64
                                                                                                    (coe
                                                                                                       v43)))
                                                                                              (coe
                                                                                                 v31)
                                                                                              (coe
                                                                                                 v8))))
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                                                                                        v0
                                                                                        (coe
                                                                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                           (coe v28)
                                                                                           (coe
                                                                                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                              (coe
                                                                                                 v31)
                                                                                              (coe
                                                                                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                                                    (coe
                                                                                                       v1))
                                                                                                 (coe
                                                                                                    v8))))
                                                                                        (coe
                                                                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                           (coe v24)
                                                                                           (coe
                                                                                              v27))
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                                           v42
                                                                                           (coe
                                                                                              MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_tail_70
                                                                                              (coe
                                                                                                 v43)))))
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                     v18 v19))
                                                                               (coe v37))
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         (:) v28 v29
                                           -> case coe v29 of
                                                []
                                                  -> case coe v6 of
                                                       []
                                                         -> case coe v9 of
                                                              MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v34 v35
                                                                -> case coe v10 of
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v40 v41
                                                                       -> coe
                                                                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                            (coe
                                                                               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                               (coe
                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                  (coe v20)
                                                                                  (coe v23))
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                  v0
                                                                                  (coe
                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                     (coe v20)
                                                                                     (coe v23))
                                                                                  v25 v8
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                     (coe v8)
                                                                                     (let v42
                                                                                            = coe
                                                                                                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                                (coe
                                                                                                   v0) in
                                                                                      coe
                                                                                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                        (coe
                                                                                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                           (coe
                                                                                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                              (coe
                                                                                                 v42)))
                                                                                        (coe v8))
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                                                                                        v0
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                           (coe v6)
                                                                                           (coe v8))
                                                                                        v25 v41))
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                     v17 v19))
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                                  v34
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_tail_70
                                                                                     (coe v35))))
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       (:) v30 v31
                                                         -> case coe v31 of
                                                              []
                                                                -> case coe v9 of
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v36 v37
                                                                       -> case coe v10 of
                                                                            MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v42 v43
                                                                              -> coe
                                                                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                                   (coe
                                                                                      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                         (coe v20)
                                                                                         (coe v23))
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                         v0
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v20)
                                                                                            (coe
                                                                                               v23))
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v24)
                                                                                            (coe
                                                                                               v27))
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v30)
                                                                                            (coe
                                                                                               v8))
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                            (coe
                                                                                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                               (coe
                                                                                                  v30)
                                                                                               (coe
                                                                                                  v8))
                                                                                            (let v44
                                                                                                   = coe
                                                                                                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                                       (coe
                                                                                                          v0) in
                                                                                             coe
                                                                                               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                               (coe
                                                                                                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                     (coe
                                                                                                        v44)))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v30)
                                                                                                  (coe
                                                                                                     v8)))
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                                                                                               v0
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v30)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                     (coe
                                                                                                        v31)
                                                                                                     (coe
                                                                                                        v8)))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v24)
                                                                                                  (coe
                                                                                                     v27))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                                                  v42
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_tail_70
                                                                                                     (coe
                                                                                                        v43)))))
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                            (coe
                                                                                               MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                                               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                  (coe
                                                                                                     v0))
                                                                                               v20
                                                                                               v26
                                                                                               v24
                                                                                               v17
                                                                                               (coe
                                                                                                  MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                                                  (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                     (coe
                                                                                                        v0))
                                                                                                  v26
                                                                                                  v1
                                                                                                  v24
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                                                                                     (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                        (coe
                                                                                                           v0))
                                                                                                     v1
                                                                                                     v26
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_head_64
                                                                                                        (coe
                                                                                                           v43)))
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                                                                                                     (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                        (coe
                                                                                                           v0))
                                                                                                     v1
                                                                                                     v22
                                                                                                     v24
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_head_64
                                                                                                        (coe
                                                                                                           v37))
                                                                                                     v18)))
                                                                                            v19))
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                                         v36
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_tail_70
                                                                                            (coe
                                                                                               v37))))
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              (:) v32 v33
                                                                -> case coe v9 of
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v38 v39
                                                                       -> case coe v10 of
                                                                            MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v44 v45
                                                                              -> coe
                                                                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                                   (coe
                                                                                      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                         (coe v20)
                                                                                         (coe v23))
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                         v0
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v20)
                                                                                            (coe
                                                                                               v23))
                                                                                         v25
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v30)
                                                                                            (coe
                                                                                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                               (coe
                                                                                                  v32)
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                  (coe
                                                                                                     v33)
                                                                                                  (coe
                                                                                                     v8))))
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                            (coe
                                                                                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                               (coe
                                                                                                  v32)
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                  (coe
                                                                                                     v33)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                     (coe
                                                                                                        v1)
                                                                                                     (coe
                                                                                                        v8))))
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                               v0
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v32)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                     (coe
                                                                                                        v33)
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                        (coe
                                                                                                           v1)
                                                                                                        (coe
                                                                                                           v8))))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v32)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                     (coe
                                                                                                        v1)
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                        (coe
                                                                                                           v33)
                                                                                                        (coe
                                                                                                           v8))))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v30)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                     (coe
                                                                                                        v32)
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                        (coe
                                                                                                           v33)
                                                                                                        (coe
                                                                                                           v8))))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                                  v0
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                     (coe
                                                                                                        v32)
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                        (coe
                                                                                                           v1)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                           (coe
                                                                                                              v33)
                                                                                                           (coe
                                                                                                              v8))))
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                     (coe
                                                                                                        v30)
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                        (coe
                                                                                                           v32)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                           (coe
                                                                                                              v33)
                                                                                                           (coe
                                                                                                              v8))))
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                     (coe
                                                                                                        v30)
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                        (coe
                                                                                                           v32)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                           (coe
                                                                                                              v33)
                                                                                                           (coe
                                                                                                              v8))))
                                                                                                  (let v46
                                                                                                         = coe
                                                                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                                             (coe
                                                                                                                v0) in
                                                                                                   coe
                                                                                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                           (coe
                                                                                                              v46)))
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                        (coe
                                                                                                           v30)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                           (coe
                                                                                                              v32)
                                                                                                           (coe
                                                                                                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                              (coe
                                                                                                                 v33)
                                                                                                              (coe
                                                                                                                 v8)))))
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                           (coe
                                                                                                              v0))
                                                                                                        v32)
                                                                                                     (coe
                                                                                                        du_lemma_814
                                                                                                        (coe
                                                                                                           v0)
                                                                                                        (coe
                                                                                                           v1)
                                                                                                        (coe
                                                                                                           v22)
                                                                                                        (coe
                                                                                                           v24)
                                                                                                        (coe
                                                                                                           v30)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_head_64
                                                                                                           (coe
                                                                                                              v39))
                                                                                                        (coe
                                                                                                           v18)
                                                                                                        (coe
                                                                                                           v44))
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
                                                                                                        (coe
                                                                                                           v0)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                           (coe
                                                                                                              v33)
                                                                                                           (coe
                                                                                                              v8)))))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                                     (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                        (coe
                                                                                                           v0))
                                                                                                     v32)
                                                                                                  (coe
                                                                                                     du_'8621''45'shift_590
                                                                                                     v0
                                                                                                     v1
                                                                                                     v33
                                                                                                     v8)))
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                                                                                               v0
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                  (coe
                                                                                                     v31)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                                                        (coe
                                                                                                           v1))
                                                                                                     (coe
                                                                                                        v8)))
                                                                                               v25
                                                                                               v45))
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                            v17
                                                                                            v19))
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                                         v38
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_tail_70
                                                                                            (coe
                                                                                               v39))))
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                (:) v30 v31
                                                  -> case coe v6 of
                                                       []
                                                         -> case coe v9 of
                                                              MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v36 v37
                                                                -> case coe v10 of
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v42 v43
                                                                       -> coe
                                                                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                            (coe
                                                                               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                               v0
                                                                               (coe
                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                  (coe v28)
                                                                                  (coe
                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                     (coe v30)
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                        (coe v31)
                                                                                        (coe v7))))
                                                                               (coe
                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                  (coe v28)
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                     (coe v31)
                                                                                     (coe
                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                        (coe v1)
                                                                                        (coe v7))))
                                                                               v8
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                  (coe
                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                     (coe v20)
                                                                                     (coe v23))
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                     v0
                                                                                     (coe
                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                        (coe v20)
                                                                                        (coe v23))
                                                                                     v25 v8
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                        (coe v8)
                                                                                        (let v44
                                                                                               = coe
                                                                                                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                                   (coe
                                                                                                      v0) in
                                                                                         coe
                                                                                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                           (coe
                                                                                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                              (coe
                                                                                                 MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                 (coe
                                                                                                    v44)))
                                                                                           (coe v8))
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                                                                                           v0
                                                                                           (coe
                                                                                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                              (coe
                                                                                                 v6)
                                                                                              (coe
                                                                                                 v8))
                                                                                           v25 v43))
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                        v17 v19))
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                                     v36
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_tail_70
                                                                                        (coe v37))))
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                  (coe
                                                                                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                     (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                        (coe v0))
                                                                                     v28)
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
                                                                                     v0
                                                                                     (coe
                                                                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                        (coe v31)
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                           (coe
                                                                                              MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                                              (coe
                                                                                                 v1))
                                                                                           (coe
                                                                                              v7)))
                                                                                     (coe
                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                        (coe v30)
                                                                                        (coe
                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                           (coe v31)
                                                                                           (coe
                                                                                              v7)))
                                                                                     (coe
                                                                                        du_shift_562
                                                                                        (coe v0)
                                                                                        (coe v30)
                                                                                        (coe
                                                                                           du_lemma_814
                                                                                           (coe v0)
                                                                                           (coe v1)
                                                                                           (coe v24)
                                                                                           (coe v22)
                                                                                           (coe v30)
                                                                                           (coe v42)
                                                                                           (coe
                                                                                              MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                                                                              (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                 (coe
                                                                                                    v0))
                                                                                              v22
                                                                                              v24
                                                                                              v18)
                                                                                           (coe
                                                                                              MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_head_64
                                                                                              (coe
                                                                                                 v37)))
                                                                                        (coe v31)
                                                                                        (coe v7)))))
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       (:) v32 v33
                                                         -> case coe v33 of
                                                              []
                                                                -> case coe v9 of
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v38 v39
                                                                       -> case coe v10 of
                                                                            MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v44 v45
                                                                              -> coe
                                                                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                                   (coe
                                                                                      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                      v0
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                         (coe v28)
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v30)
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                               (coe
                                                                                                  v31)
                                                                                               (coe
                                                                                                  v7))))
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                         (coe v30)
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe v1)
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                               (coe
                                                                                                  v31)
                                                                                               (coe
                                                                                                  v7))))
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                         (coe v32)
                                                                                         (coe v8))
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                         v0
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v30)
                                                                                            (coe
                                                                                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                               (coe
                                                                                                  v1)
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                  (coe
                                                                                                     v31)
                                                                                                  (coe
                                                                                                     v7))))
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v30)
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                               (coe
                                                                                                  v31)
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v1)
                                                                                                  (coe
                                                                                                     v7))))
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                            (coe
                                                                                               v32)
                                                                                            (coe
                                                                                               v8))
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                            (coe
                                                                                               v21)
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                               v0
                                                                                               v21
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v24)
                                                                                                  (coe
                                                                                                     v27))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v32)
                                                                                                  (coe
                                                                                                     v8))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8779'_208
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                     (coe
                                                                                                        v32)
                                                                                                     (coe
                                                                                                        v8))
                                                                                                  (let v46
                                                                                                         = coe
                                                                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                                             (coe
                                                                                                                v0) in
                                                                                                   coe
                                                                                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                           (coe
                                                                                                              v46)))
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                        (coe
                                                                                                           v32)
                                                                                                        (coe
                                                                                                           v8)))
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                                                                                                     v0
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                        (coe
                                                                                                           v32)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                           (coe
                                                                                                              v33)
                                                                                                           (coe
                                                                                                              v8)))
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                        (coe
                                                                                                           v24)
                                                                                                        (coe
                                                                                                           v27))
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                                                        v44
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_tail_70
                                                                                                           (coe
                                                                                                              v45)))))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                                  v18
                                                                                                  v19))
                                                                                            (coe
                                                                                               v39))
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                            (coe
                                                                                               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                  (coe
                                                                                                     v0))
                                                                                               v30)
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
                                                                                               v0
                                                                                               (coe
                                                                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                  (coe
                                                                                                     v31)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                                                        (coe
                                                                                                           v1))
                                                                                                     (coe
                                                                                                        v7)))
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                  (coe
                                                                                                     v1)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                     (coe
                                                                                                        v31)
                                                                                                     (coe
                                                                                                        v7)))
                                                                                               (coe
                                                                                                  du_'8621''45'shift_590
                                                                                                  v0
                                                                                                  v1
                                                                                                  v31
                                                                                                  v7))))
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                                                                                         (coe
                                                                                            du_lemma_814
                                                                                            (coe v0)
                                                                                            (coe
                                                                                               v28)
                                                                                            (coe
                                                                                               v20)
                                                                                            (coe
                                                                                               v26)
                                                                                            (coe v1)
                                                                                            (coe
                                                                                               v38)
                                                                                            (coe
                                                                                               v17)
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_head_64
                                                                                               (coe
                                                                                                  v45)))
                                                                                         (coe
                                                                                            MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                            (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                               (coe
                                                                                                  v0))
                                                                                            v30)
                                                                                         (coe
                                                                                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
                                                                                            (coe v0)
                                                                                            (coe
                                                                                               MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                               (coe
                                                                                                  v31)
                                                                                               (coe
                                                                                                  v7)))))
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              (:) v34 v35
                                                                -> case coe v9 of
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v40 v41
                                                                       -> case coe v41 of
                                                                            MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v46 v47
                                                                              -> case coe v10 of
                                                                                   MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v52 v53
                                                                                     -> case coe
                                                                                               v53 of
                                                                                          MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62 v58 v59
                                                                                            -> coe
                                                                                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
                                                                                                    v0
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                       (coe
                                                                                                          v28)
                                                                                                       (coe
                                                                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                          (coe
                                                                                                             v30)
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                             (coe
                                                                                                                v31)
                                                                                                             (coe
                                                                                                                v7))))
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                       (coe
                                                                                                          v32)
                                                                                                       (coe
                                                                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                          (coe
                                                                                                             v34)
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                             (coe
                                                                                                                v35)
                                                                                                             (coe
                                                                                                                v8))))
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                       (coe
                                                                                                          v32)
                                                                                                       (coe
                                                                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                          (coe
                                                                                                             v34)
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                             (coe
                                                                                                                v35)
                                                                                                             (coe
                                                                                                                v8))))
                                                                                                    (let v60
                                                                                                           = coe
                                                                                                               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                                                                                               (coe
                                                                                                                  v0) in
                                                                                                     coe
                                                                                                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                                       (coe
                                                                                                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                                                             (coe
                                                                                                                v60)))
                                                                                                       (coe
                                                                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                          (coe
                                                                                                             v32)
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                             (coe
                                                                                                                v34)
                                                                                                             (coe
                                                                                                                MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                                                (coe
                                                                                                                   v35)
                                                                                                                (coe
                                                                                                                   v8)))))
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                                                                                                       (coe
                                                                                                          du_lemma_814
                                                                                                          (coe
                                                                                                             v0)
                                                                                                          (coe
                                                                                                             v28)
                                                                                                          (coe
                                                                                                             v20)
                                                                                                          (coe
                                                                                                             v26)
                                                                                                          (coe
                                                                                                             v34)
                                                                                                          (coe
                                                                                                             v40)
                                                                                                          (coe
                                                                                                             v17)
                                                                                                          (coe
                                                                                                             v58))
                                                                                                       (coe
                                                                                                          du_lemma_814
                                                                                                          (coe
                                                                                                             v0)
                                                                                                          (coe
                                                                                                             v30)
                                                                                                          (coe
                                                                                                             v22)
                                                                                                          (coe
                                                                                                             v24)
                                                                                                          (coe
                                                                                                             v32)
                                                                                                          (coe
                                                                                                             v46)
                                                                                                          (coe
                                                                                                             v18)
                                                                                                          (coe
                                                                                                             v52))
                                                                                                       (coe
                                                                                                          du_helper_834
                                                                                                          (coe
                                                                                                             v0)
                                                                                                          (coe
                                                                                                             v1)
                                                                                                          (coe
                                                                                                             v23)
                                                                                                          (coe
                                                                                                             v27)
                                                                                                          (coe
                                                                                                             v19)
                                                                                                          (coe
                                                                                                             v31)
                                                                                                          (coe
                                                                                                             v35)
                                                                                                          (coe
                                                                                                             v7)
                                                                                                          (coe
                                                                                                             v8)
                                                                                                          (coe
                                                                                                             v47)
                                                                                                          (coe
                                                                                                             v59))))
                                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v12 v14 v15
        -> let v16
                 = coe
                     MAlonzo.Code.Data.List.Membership.Setoid.Properties.du_'8712''45''8707''43''43'_850
                     (coe v0) (coe v12)
                     (coe
                        du_'8712''45'resp'45''8621'_356 v0 v1
                        (coe
                           MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                           (coe
                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                              (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                              (coe v7)))
                        v12
                        (coe
                           du_'8621''45'resp'737''45''8779'_396 (coe v0) (coe v12) (coe v2)
                           (coe
                              MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                 (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                                 (coe v7)))
                           (coe
                              MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                              v0
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                    (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                                    (coe v7)))
                              v2 v9)
                           (coe v14))
                        (coe
                           MAlonzo.Code.Data.List.Membership.Setoid.Properties.du_'8712''45'insert_836
                           v5 v1
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                              v1))) in
           case coe v16 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v17 v18
               -> case coe v18 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v19 v20
                      -> case coe v20 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v21 v22
                             -> case coe v22 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                    -> coe
                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
                                         (coe
                                            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v17)
                                            (coe v19))
                                         (coe
                                            du_helper_834 (coe v0) (coe v1) (coe v2) (coe v12)
                                            (coe v14) (coe v5) (coe v17) (coe v7) (coe v19) (coe v9)
                                            (coe
                                               MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'trans_66
                                               v0
                                               (coe
                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                  (coe v17)
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                     (coe v1) (coe v19)))
                                               (coe
                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                  (coe v17)
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                     (coe v21) (coe v19)))
                                               v12
                                               (coe
                                                  MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'43''43''8314'_146
                                                  v17 v17
                                                  (coe
                                                     MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
                                                     (coe v0) (coe v17))
                                                  (coe
                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                     v23
                                                     (coe
                                                        MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
                                                        (coe v0) (coe v19))))
                                               (coe
                                                  MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                                                  v0 v12
                                                  (coe
                                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                     (coe v17)
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                        (coe v21) (coe v19)))
                                                  v24)))
                                         (coe
                                            du_helper_834 (coe v0) (coe v1) (coe v12) (coe v3)
                                            (coe v15) (coe v17) (coe v6) (coe v19) (coe v8)
                                            (coe
                                               MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'trans_66
                                               v0
                                               (coe
                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                  (coe v17)
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                     (coe v1) (coe v19)))
                                               (coe
                                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                  (coe v17)
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                     (coe v21) (coe v19)))
                                               v12
                                               (coe
                                                  MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'43''43''8314'_146
                                                  v17 v17
                                                  (coe
                                                     MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
                                                     (coe v0) (coe v17))
                                                  (coe
                                                     MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                     v23
                                                     (coe
                                                        MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
                                                        (coe v0) (coe v19))))
                                               (coe
                                                  MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'sym_64
                                                  v0 v12
                                                  (coe
                                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                     (coe v17)
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                        (coe v21) (coe v19)))
                                                  v24))
                                            (coe v10))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Pointwise
d_Pointwise_955 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.dropMiddle
d_dropMiddle_1276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_dropMiddle_1276 ~v0 ~v1 v2 v3 v4 v5 v6 v7 v8
  = du_dropMiddle_1276 v2 v3 v4 v5 v6 v7 v8
du_dropMiddle_1276 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_dropMiddle_1276 v0 v1 v2 v3 v4 v5 v6
  = case coe v1 of
      [] -> coe v6
      (:) v7 v8
        -> coe
             du_dropMiddle_1276 (coe v0) (coe v8) (coe v2) (coe v3) (coe v4)
             (coe v5)
             (coe
                du_dropMiddleElement_788 (coe v0) (coe v7) (coe v2) (coe v3)
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8) (coe v4))
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v8) (coe v5))
                (coe v6))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.split
d_split_1306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_split_1306 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_split_1306 v2 v3 v4 v5 v6 v7
du_split_1306 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_split_1306 v0 v1 v2 v3 v4 v5
  = coe
      du_helper_1332 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
      (coe v5)
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.helper
d_helper_1332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_helper_1332 ~v0 ~v1 v2 v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10 v11 ~v12
  = du_helper_1332 v2 v3 v8 v9 v10 v11
du_helper_1332 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_helper_1332 v0 v1 v2 v3 v4 v5
  = let v6
          = case coe v5 of
              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v7 v9 v10
                -> let v11
                         = coe
                             du_helper_1332 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7)
                             (coe v10) in
                   case coe v11 of
                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v12 v13
                       -> case coe v13 of
                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v14 v15
                              -> coe
                                   du_helper_1332 (coe v0) (coe v1) (coe v12) (coe v14) (coe v4)
                                   (coe
                                      du_'8621''45'resp'691''45''8779'_364 (coe v0) (coe v4)
                                      (coe v7)
                                      (coe
                                         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v12)
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
                                            (coe v14)))
                                      (coe v15) (coe v9))
                            _ -> MAlonzo.RTE.mazUnreachableError
                     _ -> MAlonzo.RTE.mazUnreachableError
              _ -> MAlonzo.RTE.mazUnreachableError in
    case coe v2 of
      []
        -> case coe v5 of
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v9
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                    (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v9))
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v11 v12
               -> case coe v4 of
                    (:) v13 v14
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v14)
                              (coe
                                 MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                 v11
                                 (coe
                                    MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
                                    (coe v0) (coe v14))))
                    _ -> coe v6
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v13 v14 v15
               -> case coe v3 of
                    (:) v16 v17
                      -> case coe v4 of
                           (:) v18 v19
                             -> case coe v19 of
                                  (:) v20 v21
                                    -> coe
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                         (coe
                                            MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v16))
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v21)
                                            (coe
                                               MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                               v13
                                               (coe
                                                  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                  v14
                                                  (coe
                                                     MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
                                                     (coe v0) (coe v21)))))
                                  _ -> coe v6
                           _ -> coe v6
                    _ -> coe v6
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v8 v10 v11
               -> let v12
                        = coe
                            du_helper_1332 (coe v0) (coe v1) (coe v2) (coe v3) (coe v8)
                            (coe v11) in
                  case coe v12 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v13 v14
                      -> case coe v14 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v15 v16
                             -> coe
                                  du_helper_1332 (coe v0) (coe v1) (coe v13) (coe v15) (coe v4)
                                  (coe
                                     du_'8621''45'resp'691''45''8779'_364 (coe v0) (coe v4) (coe v8)
                                     (coe
                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v13)
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
                                           (coe v15)))
                                     (coe v16) (coe v10))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v7 v8
        -> let v9
                 = case coe v5 of
                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v13 v14
                       -> case coe v4 of
                            (:) v15 v16
                              -> let v17
                                       = coe
                                           du_helper_1332 (coe v0) (coe v1) (coe v8) (coe v3)
                                           (coe v16) (coe v14) in
                                 case coe v17 of
                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v18 v19
                                     -> case coe v19 of
                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v20 v21
                                            -> coe
                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                    (coe v7) (coe v18))
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                    (coe v20)
                                                    (coe
                                                       MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                       v13 v21))
                                          _ -> MAlonzo.RTE.mazUnreachableError
                                   _ -> MAlonzo.RTE.mazUnreachableError
                            _ -> coe v6
                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v10 v12 v13
                       -> let v14
                                = coe
                                    du_helper_1332 (coe v0) (coe v1) (coe v2) (coe v3) (coe v10)
                                    (coe v13) in
                          case coe v14 of
                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v15 v16
                              -> case coe v16 of
                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v17 v18
                                     -> coe
                                          du_helper_1332 (coe v0) (coe v1) (coe v15) (coe v17)
                                          (coe v4)
                                          (coe
                                             du_'8621''45'resp'691''45''8779'_364 (coe v0) (coe v4)
                                             (coe v10)
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                (coe v15)
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                   (coe v1) (coe v17)))
                                             (coe v18) (coe v12))
                                   _ -> MAlonzo.RTE.mazUnreachableError
                            _ -> MAlonzo.RTE.mazUnreachableError
                     _ -> MAlonzo.RTE.mazUnreachableError in
           case coe v8 of
             []
               -> case coe v5 of
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v12
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                           (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v7))
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v12))
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v14 v15
                      -> case coe v4 of
                           (:) v16 v17
                             -> let v18
                                      = coe
                                          du_helper_1332 (coe v0) (coe v1) (coe v8) (coe v3)
                                          (coe v17) (coe v15) in
                                case coe v18 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v19 v20
                                    -> case coe v20 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v21 v22
                                           -> coe
                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                   (coe v7) (coe v19))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                   (coe v21)
                                                   (coe
                                                      MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                      v14 v22))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v9
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v16 v17 v18
                      -> case coe v4 of
                           (:) v19 v20
                             -> case coe v20 of
                                  (:) v21 v22
                                    -> coe
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v8)
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v7)
                                               (coe v22))
                                            (coe
                                               MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                               v16
                                               (coe
                                                  MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                  v17
                                                  (coe
                                                     MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_'8779''45'refl_60
                                                     (coe v0) (coe v22)))))
                                  _ -> coe v9
                           _ -> coe v9
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v11 v13 v14
                      -> let v15
                               = coe
                                   du_helper_1332 (coe v0) (coe v1) (coe v2) (coe v3) (coe v11)
                                   (coe v14) in
                         case coe v15 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v16 v17
                             -> case coe v17 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v18 v19
                                    -> coe
                                         du_helper_1332 (coe v0) (coe v1) (coe v16) (coe v18)
                                         (coe v4)
                                         (coe
                                            du_'8621''45'resp'691''45''8779'_364 (coe v0) (coe v4)
                                            (coe v11)
                                            (coe
                                               MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                               (coe v16)
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                  (coe v1) (coe v18)))
                                            (coe v19) (coe v13))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             (:) v10 v11
               -> case coe v5 of
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v14
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v14))
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v16 v17
                      -> case coe v4 of
                           (:) v18 v19
                             -> let v20
                                      = coe
                                          du_helper_1332 (coe v0) (coe v1) (coe v8) (coe v3)
                                          (coe v19) (coe v17) in
                                case coe v20 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v21 v22
                                    -> case coe v22 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                           -> coe
                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                   (coe v7) (coe v21))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                   (coe v23)
                                                   (coe
                                                      MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                      v16 v24))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v9
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v18 v19 v20
                      -> case coe v4 of
                           (:) v21 v22
                             -> case coe v22 of
                                  (:) v23 v24
                                    -> let v25
                                             = coe
                                                 du_helper_1332 (coe v0) (coe v1) (coe v11) (coe v3)
                                                 (coe v24) (coe v20) in
                                       case coe v25 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v26 v27
                                           -> case coe v27 of
                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v28 v29
                                                  -> coe
                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                          (coe v10)
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                             (coe v7) (coe v26)))
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                          (coe v28)
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                             v18
                                                             (coe
                                                                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.C__'8759'__62
                                                                v19 v29)))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> coe v9
                           _ -> coe v9
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v13 v15 v16
                      -> let v17
                               = coe
                                   du_helper_1332 (coe v0) (coe v1) (coe v2) (coe v3) (coe v13)
                                   (coe v16) in
                         case coe v17 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v18 v19
                             -> case coe v19 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v20 v21
                                    -> coe
                                         du_helper_1332 (coe v0) (coe v1) (coe v18) (coe v20)
                                         (coe v4)
                                         (coe
                                            du_'8621''45'resp'691''45''8779'_364 (coe v0) (coe v4)
                                            (coe v13)
                                            (coe
                                               MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                               (coe v18)
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                  (coe v1) (coe v20)))
                                            (coe v21) (coe v15))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.filter⁺
d_filter'8314'_1500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_filter'8314'_1500 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 v7 v8 v9
  = du_filter'8314'_1500 v5 v7 v8 v9
du_filter'8314'_1500 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_filter'8314'_1500 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38 v6
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_refl_38
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Equality.Setoid.du_filter'8314'_206
                (coe v0) (coe v1) (coe v2) (coe v6))
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50 v8 v9
        -> case coe v1 of
             (:) v10 v11
               -> case coe v2 of
                    (:) v12 v13
                      -> let v14 = coe v0 v10 in
                         let v15 = coe v0 v12 in
                         case coe v14 of
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v16 v17
                             -> if coe v16
                                  then coe
                                         seq (coe v17)
                                         (case coe v15 of
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v18 v19
                                              -> if coe v18
                                                   then coe
                                                          seq (coe v19)
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                             v8
                                                             (coe
                                                                du_filter'8314'_1500 (coe v0)
                                                                (coe v11) (coe v13) (coe v9)))
                                                   else coe
                                                          seq (coe v19)
                                                          (coe
                                                             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38)
                                            _ -> MAlonzo.RTE.mazUnreachableError)
                                  else coe
                                         seq (coe v17)
                                         (case coe v15 of
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v18 v19
                                              -> if coe v18
                                                   then coe
                                                          seq (coe v19)
                                                          (coe
                                                             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38)
                                                   else coe
                                                          seq (coe v19)
                                                          (coe
                                                             du_filter'8314'_1500 (coe v0) (coe v11)
                                                             (coe v13) (coe v9))
                                            _ -> MAlonzo.RTE.mazUnreachableError)
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68 v10 v11 v12
        -> case coe v1 of
             (:) v13 v14
               -> case coe v14 of
                    (:) v15 v16
                      -> case coe v2 of
                           (:) v17 v18
                             -> case coe v18 of
                                  (:) v19 v20
                                    -> let v21 = coe v0 v13 in
                                       let v22 = coe v0 v17 in
                                       case coe v21 of
                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v23 v24
                                           -> if coe v23
                                                then coe
                                                       seq (coe v24)
                                                       (case coe v22 of
                                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v25 v26
                                                            -> if coe v25
                                                                 then case coe v26 of
                                                                        MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v27
                                                                          -> let v28 = coe v0 v19 in
                                                                             let v29 = coe v0 v15 in
                                                                             let v30
                                                                                   = case coe v29 of
                                                                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v30 v31
                                                                                         -> coe
                                                                                              seq
                                                                                              (coe
                                                                                                 v30)
                                                                                              (coe
                                                                                                 seq
                                                                                                 (coe
                                                                                                    v31)
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38))
                                                                                       _ -> MAlonzo.RTE.mazUnreachableError in
                                                                             case coe v28 of
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v31 v32
                                                                                 -> let v33
                                                                                          = case coe
                                                                                                   v29 of
                                                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v33 v34
                                                                                                -> case coe
                                                                                                          v33 of
                                                                                                     MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                                                                                                       -> case coe
                                                                                                                 v34 of
                                                                                                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                              -> coe
                                                                                                                   MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                                            _ -> coe
                                                                                                                   v30
                                                                                                     _ -> coe
                                                                                                            v30
                                                                                              _ -> MAlonzo.RTE.mazUnreachableError in
                                                                                    if coe v31
                                                                                      then case coe
                                                                                                  v29 of
                                                                                             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v34 v35
                                                                                               -> if coe
                                                                                                       v34
                                                                                                    then case coe
                                                                                                                v32 of
                                                                                                           MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v36
                                                                                                             -> case coe
                                                                                                                       v35 of
                                                                                                                  MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v37
                                                                                                                    -> coe
                                                                                                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_swap_68
                                                                                                                         v10
                                                                                                                         v11
                                                                                                                         (coe
                                                                                                                            du_filter'8314'_1500
                                                                                                                            (coe
                                                                                                                               v0)
                                                                                                                            (coe
                                                                                                                               v16)
                                                                                                                            (coe
                                                                                                                               v20)
                                                                                                                            (coe
                                                                                                                               v12))
                                                                                                                  _ -> coe
                                                                                                                         v33
                                                                                                           _ -> coe
                                                                                                                  v33
                                                                                                    else (case coe
                                                                                                                 v35 of
                                                                                                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                              -> coe
                                                                                                                   MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                                            _ -> coe
                                                                                                                   v33)
                                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                                      else (case coe
                                                                                                   v32 of
                                                                                              MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                -> coe
                                                                                                     MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                              _ -> coe
                                                                                                     v33)
                                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                                        _ -> MAlonzo.RTE.mazUnreachableError
                                                                 else (case coe v26 of
                                                                         MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                           -> let v28
                                                                                    = coe v0 v19 in
                                                                              let v29
                                                                                    = coe v0 v15 in
                                                                              let v30
                                                                                    = case coe
                                                                                             v29 of
                                                                                        MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v30 v31
                                                                                          -> coe
                                                                                               seq
                                                                                               (coe
                                                                                                  v30)
                                                                                               (coe
                                                                                                  seq
                                                                                                  (coe
                                                                                                     v31)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38))
                                                                                        _ -> MAlonzo.RTE.mazUnreachableError in
                                                                              case coe v28 of
                                                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v31 v32
                                                                                  -> let v33
                                                                                           = case coe
                                                                                                    v29 of
                                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v33 v34
                                                                                                 -> case coe
                                                                                                           v33 of
                                                                                                      MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                                                                                                        -> case coe
                                                                                                                  v34 of
                                                                                                             MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v35
                                                                                                               -> coe
                                                                                                                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                                             _ -> coe
                                                                                                                    v30
                                                                                                      _ -> coe
                                                                                                             v30
                                                                                               _ -> MAlonzo.RTE.mazUnreachableError in
                                                                                     if coe v31
                                                                                       then case coe
                                                                                                   v29 of
                                                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v34 v35
                                                                                                -> if coe
                                                                                                        v34
                                                                                                     then case coe
                                                                                                                 v35 of
                                                                                                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v36
                                                                                                              -> coe
                                                                                                                   MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                                            _ -> coe
                                                                                                                   v33
                                                                                                     else (case coe
                                                                                                                  v32 of
                                                                                                             MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v36
                                                                                                               -> case coe
                                                                                                                         v35 of
                                                                                                                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                                      -> coe
                                                                                                                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                                                           v10
                                                                                                                           (coe
                                                                                                                              du_filter'8314'_1500
                                                                                                                              (coe
                                                                                                                                 v0)
                                                                                                                              (coe
                                                                                                                                 v16)
                                                                                                                              (coe
                                                                                                                                 v20)
                                                                                                                              (coe
                                                                                                                                 v12))
                                                                                                                    _ -> coe
                                                                                                                           v33
                                                                                                             _ -> coe
                                                                                                                    v33)
                                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                                       else (case coe
                                                                                                    v32 of
                                                                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                 -> coe
                                                                                                      MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                               _ -> coe
                                                                                                      v33)
                                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                                         _ -> MAlonzo.RTE.mazUnreachableError)
                                                          _ -> MAlonzo.RTE.mazUnreachableError)
                                                else coe
                                                       seq (coe v24)
                                                       (case coe v22 of
                                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v25 v26
                                                            -> if coe v25
                                                                 then case coe v26 of
                                                                        MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v27
                                                                          -> let v28 = coe v0 v19 in
                                                                             let v29 = coe v0 v15 in
                                                                             let v30
                                                                                   = case coe v28 of
                                                                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v30 v31
                                                                                         -> coe
                                                                                              seq
                                                                                              (coe
                                                                                                 v30)
                                                                                              (coe
                                                                                                 seq
                                                                                                 (coe
                                                                                                    v31)
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38))
                                                                                       _ -> MAlonzo.RTE.mazUnreachableError in
                                                                             case coe v29 of
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v31 v32
                                                                                 -> let v33
                                                                                          = case coe
                                                                                                   v28 of
                                                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v33 v34
                                                                                                -> case coe
                                                                                                          v33 of
                                                                                                     MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                                                                                                       -> case coe
                                                                                                                 v34 of
                                                                                                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v35
                                                                                                              -> coe
                                                                                                                   MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                                            _ -> coe
                                                                                                                   v30
                                                                                                     _ -> coe
                                                                                                            v30
                                                                                              _ -> MAlonzo.RTE.mazUnreachableError in
                                                                                    if coe v31
                                                                                      then case coe
                                                                                                  v28 of
                                                                                             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v34 v35
                                                                                               -> if coe
                                                                                                       v34
                                                                                                    then case coe
                                                                                                                v35 of
                                                                                                           MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v36
                                                                                                             -> coe
                                                                                                                  MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                                           _ -> coe
                                                                                                                  v33
                                                                                                    else (case coe
                                                                                                                 v35 of
                                                                                                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                              -> case coe
                                                                                                                        v32 of
                                                                                                                   MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v37
                                                                                                                     -> coe
                                                                                                                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                                                                                                                          v11
                                                                                                                          (coe
                                                                                                                             du_filter'8314'_1500
                                                                                                                             (coe
                                                                                                                                v0)
                                                                                                                             (coe
                                                                                                                                v16)
                                                                                                                             (coe
                                                                                                                                v20)
                                                                                                                             (coe
                                                                                                                                v12))
                                                                                                                   _ -> coe
                                                                                                                          v33
                                                                                                            _ -> coe
                                                                                                                   v33)
                                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                                      else (case coe
                                                                                                   v32 of
                                                                                              MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                -> coe
                                                                                                     MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                              _ -> coe
                                                                                                     v33)
                                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                                        _ -> MAlonzo.RTE.mazUnreachableError
                                                                 else (case coe v26 of
                                                                         MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                           -> let v28
                                                                                    = coe v0 v19 in
                                                                              let v29
                                                                                    = coe v0 v15 in
                                                                              let v30
                                                                                    = case coe
                                                                                             v28 of
                                                                                        MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v30 v31
                                                                                          -> coe
                                                                                               seq
                                                                                               (coe
                                                                                                  v30)
                                                                                               (coe
                                                                                                  seq
                                                                                                  (coe
                                                                                                     v31)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38))
                                                                                        _ -> MAlonzo.RTE.mazUnreachableError in
                                                                              case coe v29 of
                                                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v31 v32
                                                                                  -> let v33
                                                                                           = case coe
                                                                                                    v28 of
                                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v33 v34
                                                                                                 -> case coe
                                                                                                           v33 of
                                                                                                      MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                                                                                                        -> case coe
                                                                                                                  v34 of
                                                                                                             MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v35
                                                                                                               -> coe
                                                                                                                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                                             _ -> coe
                                                                                                                    v30
                                                                                                      _ -> coe
                                                                                                             v30
                                                                                               _ -> MAlonzo.RTE.mazUnreachableError in
                                                                                     if coe v31
                                                                                       then case coe
                                                                                                   v32 of
                                                                                              MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v34
                                                                                                -> coe
                                                                                                     MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                              _ -> coe
                                                                                                     v33
                                                                                       else (case coe
                                                                                                    v28 of
                                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v34 v35
                                                                                                 -> if coe
                                                                                                         v34
                                                                                                      then case coe
                                                                                                                  v35 of
                                                                                                             MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v36
                                                                                                               -> coe
                                                                                                                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
                                                                                                             _ -> coe
                                                                                                                    v33
                                                                                                      else (case coe
                                                                                                                   v35 of
                                                                                                              MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                                -> case coe
                                                                                                                          v32 of
                                                                                                                     MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                                                                                       -> coe
                                                                                                                            du_filter'8314'_1500
                                                                                                                            (coe
                                                                                                                               v0)
                                                                                                                            (coe
                                                                                                                               v16)
                                                                                                                            (coe
                                                                                                                               v20)
                                                                                                                            (coe
                                                                                                                               v12)
                                                                                                                     _ -> coe
                                                                                                                            v33
                                                                                                              _ -> coe
                                                                                                                     v33)
                                                                                               _ -> MAlonzo.RTE.mazUnreachableError)
                                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                                         _ -> MAlonzo.RTE.mazUnreachableError)
                                                          _ -> MAlonzo.RTE.mazUnreachableError)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76 v5 v7 v8
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
             (coe MAlonzo.Code.Data.List.Base.du_filter_792 v0 v5)
             (coe du_filter'8314'_1500 (coe v0) (coe v1) (coe v5) (coe v7))
             (coe du_filter'8314'_1500 (coe v0) (coe v5) (coe v2) (coe v8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.partition-↭
d_partition'45''8621'_2002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_partition'45''8621'_2002 ~v0 ~v1 v2 ~v3 ~v4 v5 v6
  = du_partition'45''8621'_2002 v2 v5 v6
du_partition'45''8621'_2002 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_partition'45''8621'_2002 v0 v1 v2
  = case coe v2 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
             (coe v0) (coe v2)
      (:) v3 v4
        -> let v5
                 = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                     (coe v1 v3) in
           if coe v5
             then coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                    (coe
                       MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                       (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                       v3)
                    (coe du_partition'45''8621'_2002 (coe v0) (coe v1) (coe v4))
             else coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                       (coe
                          MAlonzo.Code.Data.List.Base.du__'43''43'__62
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                             (coe MAlonzo.Code.Data.List.Base.du_partition_798 v1 v4))
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                             (coe MAlonzo.Code.Data.List.Base.du_partition_798 v1 v4))))
                    (coe
                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                       (coe
                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                          (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                          v3)
                       (coe du_partition'45''8621'_2002 (coe v0) (coe v1) (coe v4)))
                    (coe
                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
                       v0
                       (coe
                          MAlonzo.Code.Data.List.Base.du__'43''43'__62
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                             (coe
                                MAlonzo.Code.Data.List.Base.du_partition'7495'_644
                                (coe
                                   (\ v6 ->
                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                        (coe v1 v6)))
                                (coe v4)))
                          (coe
                             MAlonzo.Code.Data.List.Base.du__'43''43'__62
                             (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v3))
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                (coe
                                   MAlonzo.Code.Data.List.Base.du_partition'7495'_644
                                   (coe
                                      (\ v6 ->
                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                           (coe v1 v6)))
                                   (coe v4)))))
                       (coe
                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                          (coe
                             MAlonzo.Code.Data.List.Base.du__'43''43'__62
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                (coe
                                   MAlonzo.Code.Data.List.Base.du_partition'7495'_644
                                   (coe
                                      (\ v6 ->
                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                           (coe v1 v6)))
                                   (coe v4)))
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                (coe
                                   MAlonzo.Code.Data.List.Base.du_partition'7495'_644
                                   (coe
                                      (\ v6 ->
                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                           (coe v1 v6)))
                                   (coe v4)))))
                       (coe
                          du_'8621''45'shift_590 v0 v3
                          (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                             (coe
                                MAlonzo.Code.Data.List.Base.du_partition'7495'_644
                                (coe
                                   (\ v6 ->
                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                        (coe v1 v6)))
                                (coe v4)))
                          (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                             (coe
                                MAlonzo.Code.Data.List.Base.du_partition'7495'_644
                                (coe
                                   (\ v6 ->
                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                        (coe v1 v6)))
                                (coe v4)))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties._.merge-↭
d_merge'45''8621'_2038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_merge'45''8621'_2038 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_merge'45''8621'_2038 v2 v5 v6 v7
du_merge'45''8621'_2038 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_merge'45''8621'_2038 v0 v1 v2 v3
  = case coe v2 of
      []
        -> case coe v3 of
             []
               -> coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
                    (coe v0)
                    (coe
                       MAlonzo.Code.Data.List.Base.du_merge_222 (coe v1) (coe v3)
                       (coe v3))
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
                    (coe v0)
                    (coe
                       MAlonzo.Code.Data.List.Base.du_merge_222 (coe v1) (coe v2)
                       (coe v3))
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v4 v5
        -> case coe v3 of
             []
               -> coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
                    v0
                    (coe
                       MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v3))
                    v2 (coe du_'43''43''45'identity'691'_650 (coe v0) (coe v2))
             (:) v6 v7
               -> let v8
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v1 v4 v6) in
                  let v9
                        = coe
                            du_merge'45''8621'_2038 (coe v0) (coe v1) (coe v5) (coe v3) in
                  let v10
                        = coe
                            du_merge'45''8621'_2038 (coe v0) (coe v1) (coe v2) (coe v7) in
                  if coe v8
                    then coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                              v4)
                           v9
                    else coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v6)
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v4)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                                    (coe v7))))
                           (coe
                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_prep_50
                              (coe
                                 MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                 (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                                 v6)
                              v10)
                           (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                              (coe
                                 MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621''728'_200
                                 v0
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v6)
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v4)
                                       (coe
                                          MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                                          (coe v7))))
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v3))
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v4)
                                    (coe
                                       MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                                       (coe v3)))
                                 (let v11
                                        = coe
                                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                                            (coe v0) in
                                  coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                          (coe v11)))
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v4)
                                       (coe
                                          MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                                          (coe v3))))
                                 (coe du_'8621''45'shift_590 v0 v6 v2 v7)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.∷↭∷ʳ
d_'8759''8621''8759''691'_2088 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'8759''8621''8759''691'_2088 ~v0 ~v1 v2 v3 v4
  = du_'8759''8621''8759''691'_2088 v2 v3 v4
du_'8759''8621''8759''691'_2088 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'8759''8621''8759''691'_2088 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
      v0
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
         (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1)))
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v2))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_step'45''8621'_198
            v0
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
               (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1)))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v2))
            (let v3
                   = coe
                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'setoid_150
                       (coe v0) in
             coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3)))
               (coe
                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v2)))
            (coe
               du_'8621''45'shift_590 v0 v1 v2
               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
-- Data.List.Relation.Binary.Permutation.Setoid.Properties.++↭ʳ++
d_'43''43''8621''691''43''43'_2102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
d_'43''43''8621''691''43''43'_2102 ~v0 ~v1 v2 v3 v4
  = du_'43''43''8621''691''43''43'_2102 v2 v3 v4
du_'43''43''8621''691''43''43'_2102 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.T_Permutation_28
du_'43''43''8621''691''43''43'_2102 v0 v1 v2
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'refl_142
             (coe v0)
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
      (:) v3 v4
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous.C_trans_76
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62
                   (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v3))
                   (coe v2)))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Setoid.du_'8621''45'sym_144
                v0
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62
                      (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v3))
                      (coe v2)))
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4) (coe v2)))
                (coe du_'8621''45'shift_590 v0 v3 v4 v2))
             (coe
                du_'43''43''8621''691''43''43'_2102 (coe v0) (coe v4)
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3) (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
