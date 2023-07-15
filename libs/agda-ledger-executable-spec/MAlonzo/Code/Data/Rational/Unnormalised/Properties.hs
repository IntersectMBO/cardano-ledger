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

module MAlonzo.Code.Data.Rational.Unnormalised.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Consequences.Setoid
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.Base
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MaxOp
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp
import qualified MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.Integer.Properties
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Rational.Unnormalised.Base
import qualified MAlonzo.Code.Data.Sign.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Construct.Converse
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Core
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core

-- Data.Rational.Unnormalised.Properties._.xy∙z≈xz∙y
d_xy'8729'z'8776'xz'8729'y_30 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_xy'8729'z'8776'xz'8729'y_30 = erased
-- Data.Rational.Unnormalised.Properties._.xy∙z≈y∙xz
d_xy'8729'z'8776'y'8729'xz_38 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_xy'8729'z'8776'y'8729'xz_38 = erased
-- Data.Rational.Unnormalised.Properties.↥↧≡⇒≡
d_'8613''8615''8801''8658''8801'_82 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''8615''8801''8658''8801'_82 = erased
-- Data.Rational.Unnormalised.Properties./-cong
d_'47''45'cong_96 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''45'cong_96 = erased
-- Data.Rational.Unnormalised.Properties.↥[n/d]≡n
d_'8613''91'n'47'd'93''8801'n_104 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''91'n'47'd'93''8801'n_104 = erased
-- Data.Rational.Unnormalised.Properties.↧[n/d]≡d
d_'8615''91'n'47'd'93''8801'd_116 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''91'n'47'd'93''8801'd_116 = erased
-- Data.Rational.Unnormalised.Properties.drop-*≡*
d_drop'45''42''8801''42'_126 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_drop'45''42''8801''42'_126 = erased
-- Data.Rational.Unnormalised.Properties.≃-refl
d_'8771''45'refl_130 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8771''45'refl_130 ~v0 = du_'8771''45'refl_130
du_'8771''45'refl_130 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'8771''45'refl_130
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30
-- Data.Rational.Unnormalised.Properties.≃-reflexive
d_'8771''45'reflexive_132 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8771''45'reflexive_132 ~v0 ~v1 ~v2 = du_'8771''45'reflexive_132
du_'8771''45'reflexive_132 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'8771''45'reflexive_132
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30
-- Data.Rational.Unnormalised.Properties.≃-sym
d_'8771''45'sym_134 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8771''45'sym_134 ~v0 ~v1 v2 = du_'8771''45'sym_134 v2
du_'8771''45'sym_134 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'8771''45'sym_134 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
-- Data.Rational.Unnormalised.Properties.≃-trans
d_'8771''45'trans_138 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8771''45'trans_138 ~v0 ~v1 ~v2 v3 v4
  = du_'8771''45'trans_138 v3 v4
du_'8771''45'trans_138 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'8771''45'trans_138 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))
-- Data.Rational.Unnormalised.Properties._≃?_
d__'8771''63'__158 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8771''63'__158 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (\ v2 ->
         coe
           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
      (coe
         MAlonzo.Code.Data.Integer.Properties.d__'8799'__2476
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v0))))
-- Data.Rational.Unnormalised.Properties.≃-isEquivalence
d_'8771''45'isEquivalence_164 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8771''45'isEquivalence_164
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (\ v0 -> coe du_'8771''45'refl_130)
      (\ v0 v1 v2 -> coe du_'8771''45'sym_134 v2)
      (\ v0 v1 v2 v3 v4 -> coe du_'8771''45'trans_138 v3 v4)
-- Data.Rational.Unnormalised.Properties.≃-isDecEquivalence
d_'8771''45'isDecEquivalence_166 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_'8771''45'isDecEquivalence_166
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecEquivalence'46'constructor_2293
      (coe d_'8771''45'isEquivalence_164) (coe d__'8771''63'__158)
-- Data.Rational.Unnormalised.Properties.≃-setoid
d_'8771''45'setoid_168 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8771''45'setoid_168
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      d_'8771''45'isEquivalence_164
-- Data.Rational.Unnormalised.Properties.≃-decSetoid
d_'8771''45'decSetoid_170 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8771''45'decSetoid_170
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecSetoid'46'constructor_1373
      d_'8771''45'isDecEquivalence_166
-- Data.Rational.Unnormalised.Properties.neg-involutive-≡
d_neg'45'involutive'45''8801'_172 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'involutive'45''8801'_172 = erased
-- Data.Rational.Unnormalised.Properties.neg-involutive
d_neg'45'involutive_180 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_neg'45'involutive_180 ~v0 = du_neg'45'involutive_180
du_neg'45'involutive_180 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_neg'45'involutive_180 = coe du_'8771''45'refl_130
-- Data.Rational.Unnormalised.Properties.-‿cong
d_'45''8255'cong_188 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'45''8255'cong_188 v0 v1 v2
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            seq (coe v2)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)))
-- Data.Rational.Unnormalised.Properties.neg-mono-<
d_neg'45'mono'45''60'_210 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_neg'45'mono'45''60'_210 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v5 v6
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v9
                      -> coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                                 (\ v10 v11 v12 v13 v14 ->
                                    coe
                                      MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770 v13
                                      v14)
                                 (coe
                                    MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                                 (\ v10 v11 v12 v13 v14 ->
                                    coe
                                      MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                      v13 v14)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v5)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v0))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v3)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v1))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v0)))
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v1))))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                    (coe
                                       MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v0)))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v1)))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Properties.d_neg'45'mono'45''60'_3064
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v3)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v1)))
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v5)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v0)))
                                    (coe v9))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.neg-cancel-<
d_neg'45'cancel'45''60'_226 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_neg'45'cancel'45''60'_226 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v5 v6
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v9
                      -> coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                                 (\ v10 v11 v12 v13 v14 ->
                                    coe
                                      MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770 v13
                                      v14)
                                 (coe
                                    MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                                 (\ v10 v11 v12 v13 v14 ->
                                    coe
                                      MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                      v13 v14)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v5))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v0))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v3))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v1))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v3)
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                       (coe v1)))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                    (coe
                                       MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v3)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v1))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Properties.d_neg'45'mono'45''60'_3064
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v0)))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v1))))
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v1)))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v0))))
                                    (coe v9))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.drop-*≤*
d_drop'45''42''8804''42'_238 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_drop'45''42''8804''42'_238 ~v0 ~v1 v2
  = du_drop'45''42''8804''42'_238 v2
du_drop'45''42''8804''42'_238 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_drop'45''42''8804''42'_238 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v3
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.≤-reflexive
d_'8804''45'reflexive_242 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8804''45'reflexive_242 v0 v1 v2
  = coe
      seq (coe v2)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
         (coe
            MAlonzo.Code.Data.Integer.Properties.du_'8804''45'reflexive_2506
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                  (coe v0))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe v1)))))
-- Data.Rational.Unnormalised.Properties.≤-refl
d_'8804''45'refl_246 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8804''45'refl_246 v0
  = coe
      d_'8804''45'reflexive_242 (coe v0) (coe v0)
      (coe du_'8771''45'refl_130)
-- Data.Rational.Unnormalised.Properties.≤-reflexive-≡
d_'8804''45'reflexive'45''8801'_248 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8804''45'reflexive'45''8801'_248 v0 ~v1 ~v2
  = du_'8804''45'reflexive'45''8801'_248 v0
du_'8804''45'reflexive'45''8801'_248 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'8804''45'reflexive'45''8801'_248 v0
  = coe d_'8804''45'refl_246 (coe v0)
-- Data.Rational.Unnormalised.Properties.≤-trans
d_'8804''45'trans_250 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8804''45'trans_250 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v7
        -> case coe v4 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v10
               -> coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                    (coe
                       MAlonzo.Code.Data.Integer.Properties.du_'42''45'cancel'691''45''8804''45'pos_5758
                       (coe
                          MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                             (coe v0))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                             (coe v2)))
                       (coe
                          MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                             (coe v2))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                             (coe v0)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                          (coe
                             MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                          (\ v11 v12 v13 ->
                             coe
                               MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630 v13)
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'42'__308
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                   (coe v0))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                (coe v1)))
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'42'__308
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                   (coe v2))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe v0)))
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                (coe v1)))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                             (\ v11 v12 v13 v14 v15 ->
                                coe
                                  MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                  v14 v15)
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v1)))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v1))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0)))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v2))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0)))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe v1)))
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                (coe
                                   MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                (\ v11 v12 v13 v14 v15 ->
                                   coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                     v14 v15)
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v1))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v2))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v0)))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v1)))
                                (coe
                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                   (coe
                                      MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                         (coe
                                            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                            (coe v2))
                                         (coe
                                            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                            (coe v0)))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'737''45''8804''45'nonNeg_5856
                                   (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0))
                                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v1))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v2)))
                                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v1)))
                                   v10))
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''8804''45'nonNeg_5814
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe v2))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v1)))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v1))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0)))
                                (coe v7)))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.≤-antisym
d_'8804''45'antisym_284 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8804''45'antisym_284 ~v0 ~v1 v2 v3
  = du_'8804''45'antisym_284 v2 v3
du_'8804''45'antisym_284 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'8804''45'antisym_284 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))
-- Data.Rational.Unnormalised.Properties.≤-total
d_'8804''45'total_290 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8804''45'total_290 v0 v1
  = coe
      MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
      (\ v2 ->
         coe
           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
           (coe
              MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
              v2))
      (\ v2 ->
         coe
           MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
           (coe
              MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
              v2))
      (MAlonzo.Code.Data.Integer.Properties.d_'8804''45'total_2538
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v0))))
-- Data.Rational.Unnormalised.Properties.≤-respˡ-≃
d_'8804''45'resp'737''45''8771'_296 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8804''45'resp'737''45''8771'_296 v0 v1 v2 v3
  = coe
      d_'8804''45'trans_250 (coe v2) (coe v1) (coe v0)
      (coe
         d_'8804''45'reflexive_242 (coe v2) (coe v1)
         (coe du_'8771''45'sym_134 (coe v3)))
-- Data.Rational.Unnormalised.Properties.≤-respʳ-≃
d_'8804''45'resp'691''45''8771'_300 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8804''45'resp'691''45''8771'_300 v0 v1 v2 v3 v4
  = coe
      d_'8804''45'trans_250 (coe v0) (coe v1) (coe v2) (coe v4)
      (coe d_'8804''45'reflexive_242 (coe v1) (coe v2) (coe v3))
-- Data.Rational.Unnormalised.Properties.≤-resp₂-≃
d_'8804''45'resp'8322''45''8771'_306 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8804''45'resp'8322''45''8771'_306
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_'8804''45'resp'691''45''8771'_300)
      (coe d_'8804''45'resp'737''45''8771'_296)
-- Data.Rational.Unnormalised.Properties._≤?_
d__'8804''63'__308 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__308 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44)
      (coe
         MAlonzo.Code.Data.Integer.Properties.d__'8804''63'__2556
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v0))))
-- Data.Rational.Unnormalised.Properties._≥?_
d__'8805''63'__314 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8805''63'__314 v0 v1 = coe d__'8804''63'__308 (coe v1) (coe v0)
-- Data.Rational.Unnormalised.Properties.≤-irrelevant
d_'8804''45'irrelevant_316 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'irrelevant_316 = erased
-- Data.Rational.Unnormalised.Properties.≤-isPreorder
d_'8804''45'isPreorder_322 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8804''45'isPreorder_322
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe d_'8771''45'isEquivalence_164) (coe d_'8804''45'reflexive_242)
      (coe d_'8804''45'trans_250)
-- Data.Rational.Unnormalised.Properties.≤-isTotalPreorder
d_'8804''45'isTotalPreorder_324 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_'8804''45'isTotalPreorder_324
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalPreorder'46'constructor_7157
      (coe d_'8804''45'isPreorder_322) (coe d_'8804''45'total_290)
-- Data.Rational.Unnormalised.Properties.≤-isPartialOrder
d_'8804''45'isPartialOrder_326 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8804''45'isPartialOrder_326
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe d_'8804''45'isPreorder_322)
      (\ v0 v1 v2 v3 -> coe du_'8804''45'antisym_284 v2 v3)
-- Data.Rational.Unnormalised.Properties.≤-isTotalOrder
d_'8804''45'isTotalOrder_328 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_'8804''45'isTotalOrder_328
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe d_'8804''45'isPartialOrder_326) (coe d_'8804''45'total_290)
-- Data.Rational.Unnormalised.Properties.≤-isDecTotalOrder
d_'8804''45'isDecTotalOrder_330 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_'8804''45'isDecTotalOrder_330
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe d_'8804''45'isTotalOrder_328) (coe d__'8771''63'__158)
      (coe d__'8804''63'__308)
-- Data.Rational.Unnormalised.Properties.≤-preorder
d_'8804''45'preorder_332 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8804''45'preorder_332
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      d_'8804''45'isPreorder_322
-- Data.Rational.Unnormalised.Properties.≤-totalPreorder
d_'8804''45'totalPreorder_334 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
d_'8804''45'totalPreorder_334
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalPreorder'46'constructor_3645
      d_'8804''45'isTotalPreorder_324
-- Data.Rational.Unnormalised.Properties.≤-poset
d_'8804''45'poset_336 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8804''45'poset_336
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      d_'8804''45'isPartialOrder_326
-- Data.Rational.Unnormalised.Properties.≤-totalOrder
d_'8804''45'totalOrder_338 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_'8804''45'totalOrder_338
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      d_'8804''45'isTotalOrder_328
-- Data.Rational.Unnormalised.Properties.≤-decTotalOrder
d_'8804''45'decTotalOrder_340 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_'8804''45'decTotalOrder_340
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      d_'8804''45'isDecTotalOrder_330
-- Data.Rational.Unnormalised.Properties.≤-isPreorder-≡
d_'8804''45'isPreorder'45''8801'_342 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8804''45'isPreorder'45''8801'_342
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'8804''45'reflexive'45''8801'_248 v0)
      (coe d_'8804''45'trans_250)
-- Data.Rational.Unnormalised.Properties.≤-isTotalPreorder-≡
d_'8804''45'isTotalPreorder'45''8801'_344 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_'8804''45'isTotalPreorder'45''8801'_344
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalPreorder'46'constructor_7157
      (coe d_'8804''45'isPreorder'45''8801'_342)
      (coe d_'8804''45'total_290)
-- Data.Rational.Unnormalised.Properties.≤-preorder-≡
d_'8804''45'preorder'45''8801'_346 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8804''45'preorder'45''8801'_346
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      d_'8804''45'isPreorder'45''8801'_342
-- Data.Rational.Unnormalised.Properties.≤-totalPreorder-≡
d_'8804''45'totalPreorder'45''8801'_348 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
d_'8804''45'totalPreorder'45''8801'_348
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalPreorder'46'constructor_3645
      d_'8804''45'isTotalPreorder'45''8801'_344
-- Data.Rational.Unnormalised.Properties.mono⇒cong
d_mono'8658'cong_352 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_mono'8658'cong_352 v0
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_mono'8658'cong_276
      (\ v1 v2 v3 -> coe du_'8771''45'sym_134 v3)
      (coe d_'8804''45'reflexive_242)
      (\ v1 v2 v3 v4 -> coe du_'8804''45'antisym_284 v3 v4) (coe v0)
-- Data.Rational.Unnormalised.Properties.antimono⇒cong
d_antimono'8658'cong_356 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_antimono'8658'cong_356 v0
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_antimono'8658'cong_290
      (\ v1 v2 v3 -> coe du_'8771''45'sym_134 v3)
      (coe d_'8804''45'reflexive_242)
      (\ v1 v2 v3 v4 -> coe du_'8804''45'antisym_284 v3 v4) (coe v0)
-- Data.Rational.Unnormalised.Properties.≤ᵇ⇒≤
d_'8804''7495''8658''8804'_358 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  AgdaAny ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8804''7495''8658''8804'_358 v0 v1 ~v2
  = du_'8804''7495''8658''8804'_358 v0 v1
du_'8804''7495''8658''8804'_358 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'8804''7495''8658''8804'_358 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8804''7495''8658''8804'_2604
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'9667'__230
            (coe
               MAlonzo.Code.Data.Sign.Base.d__'42'__14
               (coe
                  MAlonzo.Code.Data.Integer.Base.d_sign_24
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                     (coe v0)))
               (coe
                  MAlonzo.Code.Data.Integer.Base.d_sign_24
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                     (coe v1))))
            (coe
               mulInt
               (coe
                  MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                     (coe v0)))
               (coe
                  MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                     (coe v1)))))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'9667'__230
            (coe
               MAlonzo.Code.Data.Sign.Base.d__'42'__14
               (coe
                  MAlonzo.Code.Data.Integer.Base.d_sign_24
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                     (coe v1)))
               (coe
                  MAlonzo.Code.Data.Integer.Base.d_sign_24
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                     (coe v0))))
            (coe
               mulInt
               (coe
                  MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                     (coe v1)))
               (coe
                  MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                     (coe v0))))))
-- Data.Rational.Unnormalised.Properties.≤⇒≤ᵇ
d_'8804''8658''8804''7495'_360 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  AgdaAny
d_'8804''8658''8804''7495'_360 ~v0 ~v1 v2
  = du_'8804''8658''8804''7495'_360 v2
du_'8804''8658''8804''7495'_360 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  AgdaAny
du_'8804''8658''8804''7495'_360 v0
  = coe
      MAlonzo.Code.Data.Integer.Properties.du_'8804''8658''8804''7495'_2612
      (coe du_drop'45''42''8804''42'_238 (coe v0))
-- Data.Rational.Unnormalised.Properties.drop-*<*
d_drop'45''42''60''42'_362 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_drop'45''42''60''42'_362 ~v0 ~v1 v2
  = du_drop'45''42''60''42'_362 v2
du_drop'45''42''60''42'_362 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_drop'45''42''60''42'_362 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v3
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.<⇒≤
d_'60''8658''8804'_366 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'60''8658''8804'_366 ~v0 ~v1 v2 = du_'60''8658''8804'_366 v2
du_'60''8658''8804'_366 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'60''8658''8804'_366 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v3
        -> coe
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
             (coe
                MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630
                (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.<⇒≢
d_'60''8658''8802'_370 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8802'_370 = erased
-- Data.Rational.Unnormalised.Properties.<⇒≱
d_'60''8658''8817'_374 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8817'_374 = erased
-- Data.Rational.Unnormalised.Properties.≰⇒>
d_'8816''8658''62'_378 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'8816''8658''62'_378 v0 v1 ~v2 = du_'8816''8658''62'_378 v0 v1
du_'8816''8658''62'_378 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'8816''8658''62'_378 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8816''8658''62'_2658
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v0))))
-- Data.Rational.Unnormalised.Properties.≮⇒≥
d_'8814''8658''8805'_382 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8814''8658''8805'_382 v0 v1 ~v2
  = du_'8814''8658''8805'_382 v0 v1
du_'8814''8658''8805'_382 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'8814''8658''8805'_382 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8814''8658''8805'_2684
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v0))))
-- Data.Rational.Unnormalised.Properties.≰⇒≥
d_'8816''8658''8805'_386 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8816''8658''8805'_386 v0 v1 ~v2
  = du_'8816''8658''8805'_386 v0 v1
du_'8816''8658''8805'_386 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'8816''8658''8805'_386 v0 v1
  = coe
      du_'60''8658''8804'_366
      (coe du_'8816''8658''62'_378 (coe v0) (coe v1))
-- Data.Rational.Unnormalised.Properties.<-irrefl-≡
d_'60''45'irrefl'45''8801'_388 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'irrefl'45''8801'_388 = erased
-- Data.Rational.Unnormalised.Properties.<-irrefl
d_'60''45'irrefl_392 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'irrefl_392 = erased
-- Data.Rational.Unnormalised.Properties.<-asym
d_'60''45'asym_398 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'asym_398 = erased
-- Data.Rational.Unnormalised.Properties.≤-<-trans
d_'8804''45''60''45'trans_402 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'8804''45''60''45'trans_402 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v7
        -> case coe v4 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v10
               -> coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                    (coe
                       MAlonzo.Code.Data.Integer.Properties.du_'42''45'cancel'691''45''60''45'nonNeg_6064
                       (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                             (coe v0))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                             (coe v2)))
                       (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                             (coe v2))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                             (coe v0)))
                       (addInt
                          (coe (1 :: Integer))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator'45'1_16
                             (coe v1)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                             (\ v11 v12 v13 v14 v15 ->
                                coe
                                  MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                  v14 v15)
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_n'8321'_418 (coe v0)) (coe du_d'8322'_426 (coe v1)))
                                (coe du_d'8323'_428 (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_n'8322'_420 (coe v1)) (coe du_d'8321'_424 (coe v0)))
                                (coe du_d'8323'_428 (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_n'8323'_422 (coe v2)) (coe du_d'8321'_424 (coe v0)))
                                (coe du_d'8322'_426 (coe v1)))
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                                (\ v11 v12 v13 v14 v15 ->
                                   coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770 v14
                                     v15)
                                (coe
                                   MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                                (\ v11 v12 v13 v14 v15 ->
                                   coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                     v14 v15)
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_d'8321'_424 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe du_n'8322'_420 (coe v1)) (coe du_d'8323'_428 (coe v2))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_d'8321'_424 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe du_n'8323'_422 (coe v2)) (coe du_d'8322'_426 (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe du_n'8323'_422 (coe v2)) (coe du_d'8321'_424 (coe v0)))
                                   (coe du_d'8322'_426 (coe v1)))
                                (coe
                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                   (coe
                                      MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                         (coe du_n'8323'_422 (coe v2))
                                         (coe du_d'8321'_424 (coe v0)))
                                      (coe du_d'8322'_426 (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'737''45''60''45'pos_5974
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v1))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v2)))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v1)))
                                   (coe v10)))
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''8804''45'nonNeg_5814
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe v2))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v1)))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v1))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0)))
                                (coe v7)))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties._.n₁
d_n'8321'_418 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_n'8321'_418 v0 ~v1 ~v2 ~v3 ~v4 = du_n'8321'_418 v0
du_n'8321'_418 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8321'_418 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.n₂
d_n'8322'_420 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_n'8322'_420 ~v0 v1 ~v2 ~v3 ~v4 = du_n'8322'_420 v1
du_n'8322'_420 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8322'_420 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.n₃
d_n'8323'_422 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_n'8323'_422 ~v0 ~v1 v2 ~v3 ~v4 = du_n'8323'_422 v2
du_n'8323'_422 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8323'_422 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₁
d_d'8321'_424 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_d'8321'_424 v0 ~v1 ~v2 ~v3 ~v4 = du_d'8321'_424 v0
du_d'8321'_424 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8321'_424 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₂
d_d'8322'_426 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_d'8322'_426 ~v0 v1 ~v2 ~v3 ~v4 = du_d'8322'_426 v1
du_d'8322'_426 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8322'_426 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₃
d_d'8323'_428 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_d'8323'_428 ~v0 ~v1 v2 ~v3 ~v4 = du_d'8323'_428 v2
du_d'8323'_428 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8323'_428 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties.<-≤-trans
d_'60''45''8804''45'trans_436 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'60''45''8804''45'trans_436 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v7
        -> case coe v4 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v10
               -> coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                    (coe
                       MAlonzo.Code.Data.Integer.Properties.du_'42''45'cancel'691''45''60''45'nonNeg_6064
                       (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                             (coe v0))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                             (coe v2)))
                       (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                             (coe v2))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                             (coe v0)))
                       (addInt
                          (coe (1 :: Integer))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator'45'1_16
                             (coe v1)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                             (\ v11 v12 v13 v14 v15 ->
                                coe
                                  MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770 v14
                                  v15)
                             (coe
                                MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                             (\ v11 v12 v13 v14 v15 ->
                                coe
                                  MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                  v14 v15)
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_n'8321'_452 (coe v0)) (coe du_d'8322'_460 (coe v1)))
                                (coe du_d'8323'_462 (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_n'8322'_454 (coe v1)) (coe du_d'8321'_458 (coe v0)))
                                (coe du_d'8323'_462 (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_n'8323'_456 (coe v2)) (coe du_d'8321'_458 (coe v0)))
                                (coe du_d'8322'_460 (coe v1)))
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                (coe
                                   MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                (\ v11 v12 v13 v14 v15 ->
                                   coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                     v14 v15)
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_d'8321'_458 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe du_n'8322'_454 (coe v1)) (coe du_d'8323'_462 (coe v2))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe du_d'8321'_458 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe du_n'8323'_456 (coe v2)) (coe du_d'8322'_460 (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe du_n'8323'_456 (coe v2)) (coe du_d'8321'_458 (coe v0)))
                                   (coe du_d'8322'_460 (coe v1)))
                                (coe
                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                   (coe
                                      MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                         (coe du_n'8323'_456 (coe v2))
                                         (coe du_d'8321'_458 (coe v0)))
                                      (coe du_d'8322'_460 (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'737''45''8804''45'nonNeg_5856
                                   (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0))
                                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v1))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v2)))
                                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                         (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                         (coe v1)))
                                   v10))
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''60''45'pos_6006
                                (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe v2))
                                (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v1)))
                                (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                      (coe v1))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                      (coe v0)))
                                v7))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties._.n₁
d_n'8321'_452 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_n'8321'_452 v0 ~v1 ~v2 ~v3 ~v4 = du_n'8321'_452 v0
du_n'8321'_452 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8321'_452 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.n₂
d_n'8322'_454 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_n'8322'_454 ~v0 v1 ~v2 ~v3 ~v4 = du_n'8322'_454 v1
du_n'8322'_454 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8322'_454 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.n₃
d_n'8323'_456 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_n'8323'_456 ~v0 ~v1 v2 ~v3 ~v4 = du_n'8323'_456 v2
du_n'8323'_456 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8323'_456 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₁
d_d'8321'_458 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_d'8321'_458 v0 ~v1 ~v2 ~v3 ~v4 = du_d'8321'_458 v0
du_d'8321'_458 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8321'_458 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₂
d_d'8322'_460 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_d'8322'_460 ~v0 v1 ~v2 ~v3 ~v4 = du_d'8322'_460 v1
du_d'8322'_460 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8322'_460 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₃
d_d'8323'_462 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_d'8323'_462 ~v0 ~v1 v2 ~v3 ~v4 = du_d'8323'_462 v2
du_d'8323'_462 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8323'_462 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties.<-trans
d_'60''45'trans_470 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'60''45'trans_470 v0 v1 v2 v3
  = coe
      d_'8804''45''60''45'trans_402 (coe v0) (coe v1) (coe v2)
      (coe du_'60''8658''8804'_366 (coe v3))
-- Data.Rational.Unnormalised.Properties.<-cmp
d_'60''45'cmp_472 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_'60''45'cmp_472 v0 v1
  = let v2
          = MAlonzo.Code.Data.Integer.Properties.d_'60''45'cmp_2776
              (coe
                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                 (coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                    (coe v0))
                 (coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                    (coe v1)))
              (coe
                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                 (coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                    (coe v1))
                 (coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                    (coe v0))) in
    case coe v2 of
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v3
        -> coe
             MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v3)
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v4
        -> coe
             MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v5
        -> coe
             MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties._<?_
d__'60''63'__512 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__512 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52)
      (coe
         MAlonzo.Code.Data.Integer.Properties.d__'60''63'__2866
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
               (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe v0))))
-- Data.Rational.Unnormalised.Properties._>?_
d__'62''63'__518 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'62''63'__518 v0 v1 = coe d__'60''63'__512 (coe v1) (coe v0)
-- Data.Rational.Unnormalised.Properties.<-irrelevant
d_'60''45'irrelevant_520 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''45'irrelevant_520 = erased
-- Data.Rational.Unnormalised.Properties.<-respʳ-≃
d_'60''45'resp'691''45''8771'_526 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'60''45'resp'691''45''8771'_526 v0 v1 v2 v3 v4
  = coe
      seq (coe v3)
      (case coe v4 of
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v7
           -> coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                (coe
                   MAlonzo.Code.Data.Integer.Properties.du_'42''45'cancel'691''45''60''45'nonNeg_6064
                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                      (coe
                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                         (coe v0))
                      (coe
                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                         (coe v2)))
                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                      (coe
                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                         (coe v2))
                      (coe
                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                         (coe v0)))
                   (addInt
                      (coe (1 :: Integer))
                      (coe
                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator'45'1_16
                         (coe v1)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                         (\ v8 v9 v10 v11 v12 ->
                            coe
                              MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770 v11 v12)
                         (coe
                            MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                         (\ v8 v9 v10 v11 v12 ->
                            coe
                              MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                              v11 v12)
                         (coe
                            MAlonzo.Code.Data.Integer.Base.d__'42'__308
                            (coe
                               MAlonzo.Code.Data.Integer.Base.d__'42'__308
                               (coe du_n'8321'_542 (coe v0)) (coe du_d'8322'_550 (coe v1)))
                            (coe du_d'8323'_552 (coe v2)))
                         (coe
                            MAlonzo.Code.Data.Integer.Base.d__'42'__308
                            (coe
                               MAlonzo.Code.Data.Integer.Base.d__'42'__308
                               (coe du_n'8322'_544 (coe v1)) (coe du_d'8321'_548 (coe v0)))
                            (coe du_d'8323'_552 (coe v2)))
                         (coe
                            MAlonzo.Code.Data.Integer.Base.d__'42'__308
                            (coe
                               MAlonzo.Code.Data.Integer.Base.d__'42'__308
                               (coe du_n'8323'_546 (coe v2)) (coe du_d'8321'_548 (coe v0)))
                            (coe du_d'8322'_550 (coe v1)))
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                            (coe
                               MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                            (coe
                               MAlonzo.Code.Data.Integer.Base.d__'42'__308
                               (coe
                                  MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                  (coe du_n'8323'_546 (coe v2)) (coe du_d'8321'_548 (coe v0)))
                               (coe du_d'8322'_550 (coe v1))))
                         (coe
                            MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''60''45'pos_6006
                            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                               (coe v2))
                            (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                               (coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                  (coe v0))
                               (coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                  (coe v1)))
                            (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                               (coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                  (coe v1))
                               (coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                  (coe v0)))
                            v7))))
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Data.Rational.Unnormalised.Properties._.n₁
d_n'8321'_542 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_n'8321'_542 v0 ~v1 ~v2 ~v3 ~v4 = du_n'8321'_542 v0
du_n'8321'_542 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8321'_542 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.n₂
d_n'8322'_544 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_n'8322'_544 ~v0 v1 ~v2 ~v3 ~v4 = du_n'8322'_544 v1
du_n'8322'_544 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8322'_544 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.n₃
d_n'8323'_546 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_n'8323'_546 ~v0 ~v1 v2 ~v3 ~v4 = du_n'8323'_546 v2
du_n'8323'_546 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_n'8323'_546 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₁
d_d'8321'_548 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_d'8321'_548 v0 ~v1 ~v2 ~v3 ~v4 = du_d'8321'_548 v0
du_d'8321'_548 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8321'_548 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₂
d_d'8322'_550 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_d'8322'_550 ~v0 v1 ~v2 ~v3 ~v4 = du_d'8322'_550 v1
du_d'8322'_550 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8322'_550 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties._.d₃
d_d'8323'_552 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_d'8323'_552 ~v0 ~v1 v2 ~v3 ~v4 = du_d'8323'_552 v2
du_d'8323'_552 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 -> Integer
du_d'8323'_552 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe v0)
-- Data.Rational.Unnormalised.Properties.<-respˡ-≃
d_'60''45'resp'737''45''8771'_562 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'60''45'resp'737''45''8771'_562 v0 v1 v2 v3 v4
  = coe
      d_neg'45'mono'45''60'_210
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v5 v6 -> v6)
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 v1 v0)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v5 v6 -> v6)
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 v1 v2)
      (coe
         d_'60''45'resp'691''45''8771'_526
         (coe
            MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
            (\ v5 v6 -> v6)
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 v1 v0)
         (coe
            MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
            (\ v5 v6 -> v5) v1 v2)
         (coe
            MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
            (\ v5 v6 -> v6)
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 v1 v2)
         (coe d_'45''8255'cong_188 (coe v1) (coe v2) (coe v3))
         (coe d_neg'45'mono'45''60'_210 (coe v1) (coe v0) (coe v4)))
-- Data.Rational.Unnormalised.Properties.<-resp-≃
d_'60''45'resp'45''8771'_572 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8771'_572
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_'60''45'resp'691''45''8771'_526)
      (coe d_'60''45'resp'737''45''8771'_562)
-- Data.Rational.Unnormalised.Properties.<-isStrictPartialOrder-≡
d_'60''45'isStrictPartialOrder'45''8801'_574 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder'45''8801'_574
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      d_'60''45'trans_470
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe (\ v0 v1 v2 v3 v4 -> v4)) (coe (\ v0 v1 v2 v3 v4 -> v4)))
-- Data.Rational.Unnormalised.Properties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_580 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_580
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      d_'8771''45'isEquivalence_164 d_'60''45'trans_470
      d_'60''45'resp'45''8771'_572
-- Data.Rational.Unnormalised.Properties.<-isStrictTotalOrder
d_'60''45'isStrictTotalOrder_582 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder_582
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe d_'8771''45'isEquivalence_164) (coe d_'60''45'trans_470)
      (coe d_'60''45'cmp_472)
-- Data.Rational.Unnormalised.Properties.<-strictPartialOrder-≡
d_'60''45'strictPartialOrder'45''8801'_584 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder'45''8801'_584
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      d_'60''45'isStrictPartialOrder'45''8801'_574
-- Data.Rational.Unnormalised.Properties.<-strictPartialOrder
d_'60''45'strictPartialOrder_586 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_586
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      d_'60''45'isStrictPartialOrder_580
-- Data.Rational.Unnormalised.Properties.<-strictTotalOrder
d_'60''45'strictTotalOrder_588 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder_588
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      d_'60''45'isStrictTotalOrder_582
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple._IsRelatedTo_
d__IsRelatedTo__594 a0 a1 = ()
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple._∎
d__'8718'_596 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d__'8718'_596
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
      (coe d_'8804''45'isPreorder_322)
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple._≡⟨⟩_
d__'8801''10216''10217'__598 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d__'8801''10216''10217'__598 v0 = coe v0
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.IsEquality
d_IsEquality_600 a0 a1 a2 = ()
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.IsEquality?
d_IsEquality'63'_602 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsEquality'63'_602 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_IsEquality'63'_142
      v2
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.IsStrict
d_IsStrict_604 a0 a1 a2 = ()
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.IsStrict?
d_IsStrict'63'_606 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsStrict'63'_606 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_IsStrict'63'_108
      v2
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.begin_
d_begin__608 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_begin__608
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v0 v1 v2 -> coe du_'60''8658''8804'_366 v2)
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.begin-equality_
d_begin'45'equality__610 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  AgdaAny ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_begin'45'equality__610 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
      v2
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.begin-strict_
d_begin'45'strict__612 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  AgdaAny -> MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_begin'45'strict__612 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      v2
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.extractEquality
d_extractEquality_616 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T_IsEquality_126 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_extractEquality_616 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_extractEquality_152
      v2 v3
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.extractStrict
d_extractStrict_618 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T_IsStrict_92 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_extractStrict_618 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_extractStrict_118
      v2 v3
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.step-<
d_step'45''60'_626 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''60'_626
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
      (coe d_'60''45'trans_470) (coe d_'60''45'resp'45''8771'_572)
      (coe d_'60''45''8804''45'trans_436)
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.step-≈
d_step'45''8776'_628 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8776'_628
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8776'_254
      (coe d_'8804''45'isPreorder_322) (coe d_'60''45'resp'45''8771'_572)
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.step-≈˘
d_step'45''8776''728'_630 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8776''728'_630
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8776''728'_280
      (coe d_'8804''45'isPreorder_322) (coe d_'60''45'resp'45''8771'_572)
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.step-≡
d_step'45''8801'_632 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8801'_632 ~v0 ~v1 ~v2 v3 ~v4 = du_step'45''8801'_632 v3
du_step'45''8801'_632 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
du_step'45''8801'_632 v0 = coe v0
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.step-≡˘
d_step'45''8801''728'_634 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8801''728'_634 ~v0 ~v1 ~v2 v3 ~v4
  = du_step'45''8801''728'_634 v3
du_step'45''8801''728'_634 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
du_step'45''8801''728'_634 v0 = coe v0
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.Triple.step-≤
d_step'45''8804'_636 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8804'_636
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
      (coe d_'8804''45'isPreorder_322)
      (coe d_'8804''45''60''45'trans_402)
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.step-≃
d_step'45''8771'_656 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8771'_656
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8776'_254
      (coe d_'8804''45'isPreorder_322) (coe d_'60''45'resp'45''8771'_572)
-- Data.Rational.Unnormalised.Properties.≤-Reasoning.step-≃˘
d_step'45''8771''728'_658 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8771''728'_658
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8776''728'_280
      (coe d_'8804''45'isPreorder_322) (coe d_'60''45'resp'45''8771'_572)
-- Data.Rational.Unnormalised.Properties.≥0⇒↥≥0
d_'8805'0'8658''8613''8805'0_664 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8805'0'8658''8613''8805'0_664 v0 ~v1 v2
  = du_'8805'0'8658''8613''8805'0_664 v0 v2
du_'8805'0'8658''8613''8805'0_664 ::
  Integer ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'8805'0'8658''8613''8805'0_664 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.Properties.du_'8804''45'trans_2514
      (coe du_drop'45''42''8804''42'_238 (coe v1))
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8804''45'reflexive_2506
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0)
            (coe MAlonzo.Code.Data.Integer.Base.d_1ℤ_16)))
-- Data.Rational.Unnormalised.Properties.>0⇒↥>0
d_'62'0'8658''8613''62'0_676 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'62'0'8658''8613''62'0_676 v0 ~v1 v2
  = du_'62'0'8658''8613''62'0_676 v0 v2
du_'62'0'8658''8613''62'0_676 ::
  Integer ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'62'0'8658''8613''62'0_676 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
      (coe du_drop'45''42''60''42'_362 (coe v1))
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8804''45'reflexive_2506
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0)
            (coe MAlonzo.Code.Data.Integer.Base.d_1ℤ_16)))
-- Data.Rational.Unnormalised.Properties.positive⁻¹
d_positive'8315''185'_686 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_positive'8315''185'_686 v0 ~v1 = du_positive'8315''185'_686 v0
du_positive'8315''185'_686 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_positive'8315''185'_686 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
         (coe
            MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
            (coe
               MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
               (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))))
-- Data.Rational.Unnormalised.Properties.nonNegative⁻¹
d_nonNegative'8315''185'_692 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_nonNegative'8315''185'_692 v0 ~v1
  = du_nonNegative'8315''185'_692 v0
du_nonNegative'8315''185'_692 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_nonNegative'8315''185'_692 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                (coe
                   MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                   (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.negative⁻¹
d_negative'8315''185'_698 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_negative'8315''185'_698 v0 ~v1 = du_negative'8315''185'_698 v0
du_negative'8315''185'_698 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_negative'8315''185'_698 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
         (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64))
-- Data.Rational.Unnormalised.Properties.nonPositive⁻¹
d_nonPositive'8315''185'_704 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_nonPositive'8315''185'_704 v0 ~v1
  = du_nonPositive'8315''185'_704 v0
du_nonPositive'8315''185'_704 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_nonPositive'8315''185'_704 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> case coe v1 of
             0 -> coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                    (coe
                       MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                       (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
             _ -> coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                    (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.pos⇒nonNeg
d_pos'8658'nonNeg_710 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_pos'8658'nonNeg_710 v0 ~v1 = du_pos'8658'nonNeg_710 v0
du_pos'8658'nonNeg_710 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_pos'8658'nonNeg_710 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Integer.Base.C_NonNegative'46'constructor_1353
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.neg⇒nonPos
d_neg'8658'nonPos_716 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154
d_neg'8658'nonPos_716 v0 ~v1 = du_neg'8658'nonPos_716 v0
du_neg'8658'nonPos_716 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154
du_neg'8658'nonPos_716 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Integer.Base.C_NonPositive'46'constructor_1411
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.neg<pos
d_neg'60'pos_724 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_neg'60'pos_724 v0 v1 ~v2 ~v3 = du_neg'60'pos_724 v0 v1
du_neg'60'pos_724 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_neg'60'pos_724 v0 v1
  = coe
      d_'60''45'trans_470 v0
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108 v1
      (coe du_negative'8315''185'_698 (coe v0))
      (coe du_positive'8315''185'_686 (coe v1))
-- Data.Rational.Unnormalised.Properties.pos⇒nonZero
d_pos'8658'nonZero_732 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_pos'8658'nonZero_732 v0 ~v1 = du_pos'8658'nonZero_732 v0
du_pos'8658'nonZero_732 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_pos'8658'nonZero_732 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Unnormalised.Properties.nonNeg∧nonPos⇒0
d_nonNeg'8743'nonPos'8658'0_736 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_nonNeg'8743'nonPos'8658'0_736 v0 ~v1 ~v2
  = du_nonNeg'8743'nonPos'8658'0_736 v0
du_nonNeg'8743'nonPos'8658'0_736 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_nonNeg'8743'nonPos'8658'0_736 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
-- Data.Rational.Unnormalised.Properties.nonNeg<⇒pos
d_nonNeg'60''8658'pos_744 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_nonNeg'60''8658'pos_744 v0 v1 ~v2 v3
  = du_nonNeg'60''8658'pos_744 v0 v1 v3
du_nonNeg'60''8658'pos_744 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_nonNeg'60''8658'pos_744 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_positive_162
      (coe v1)
      (coe
         d_'8804''45''60''45'trans_402
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v0) (coe v1) (coe du_nonNegative'8315''185'_692 (coe v0))
         (coe v2))
-- Data.Rational.Unnormalised.Properties.nonNeg≤⇒nonNeg
d_nonNeg'8804''8658'nonNeg_756 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_nonNeg'8804''8658'nonNeg_756 v0 v1 ~v2 v3
  = du_nonNeg'8804''8658'nonNeg_756 v0 v1 v3
du_nonNeg'8804''8658'nonNeg_756 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_nonNeg'8804''8658'nonNeg_756 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_nonNegative_186
      (coe v1)
      (coe
         d_'8804''45'trans_250
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v0) (coe v1) (coe du_nonNegative'8315''185'_692 (coe v0))
         (coe v2))
-- Data.Rational.Unnormalised.Properties.neg⇒nonZero
d_neg'8658'nonZero_764 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_neg'8658'nonZero_764 v0 ~v1 = du_neg'8658'nonZero_764 v0
du_neg'8658'nonZero_764 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_neg'8658'nonZero_764 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Unnormalised.Properties.+-cong
d_'43''45'cong_766 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'cong_766 v0 v1 v2 v3 v4 v5
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            seq (coe v2)
            (coe
               seq (coe v3)
               (coe
                  seq (coe v4)
                  (coe
                     seq (coe v5)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))))))
-- Data.Rational.Unnormalised.Properties._.↥x
d_'8613'x_784 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_'8613'x_784 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_'8613'x_784 v0
du_'8613'x_784 :: Integer -> Integer
du_'8613'x_784 v0 = coe v0
-- Data.Rational.Unnormalised.Properties._.↧x
d_'8615'x_786 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_'8615'x_786 v0 v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_'8615'x_786 v0 v1
du_'8615'x_786 :: Integer -> Integer -> Integer
du_'8615'x_786 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
         (coe v0) (coe v1))
-- Data.Rational.Unnormalised.Properties._.↥y
d_'8613'y_788 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_'8613'y_788 ~v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_'8613'y_788 v2
du_'8613'y_788 :: Integer -> Integer
du_'8613'y_788 v0 = coe v0
-- Data.Rational.Unnormalised.Properties._.↧y
d_'8615'y_790 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_'8615'y_790 ~v0 ~v1 v2 v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_'8615'y_790 v2 v3
du_'8615'y_790 :: Integer -> Integer -> Integer
du_'8615'y_790 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
         (coe v0) (coe v1))
-- Data.Rational.Unnormalised.Properties._.↥u
d_'8613'u_792 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_'8613'u_792 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_'8613'u_792 v4
du_'8613'u_792 :: Integer -> Integer
du_'8613'u_792 v0 = coe v0
-- Data.Rational.Unnormalised.Properties._.↧u
d_'8615'u_794 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_'8615'u_794 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 ~v8 ~v9
  = du_'8615'u_794 v4 v5
du_'8615'u_794 :: Integer -> Integer -> Integer
du_'8615'u_794 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
         (coe v0) (coe v1))
-- Data.Rational.Unnormalised.Properties._.↥v
d_'8613'v_796 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_'8613'v_796 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 ~v9
  = du_'8613'v_796 v6
du_'8613'v_796 :: Integer -> Integer
du_'8613'v_796 v0 = coe v0
-- Data.Rational.Unnormalised.Properties._.↧v
d_'8615'v_798 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_'8615'v_798 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 ~v8 ~v9
  = du_'8615'v_798 v6 v7
du_'8615'v_798 :: Integer -> Integer -> Integer
du_'8615'v_798 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
         (coe v0) (coe v1))
-- Data.Rational.Unnormalised.Properties.+-congʳ
d_'43''45'cong'691'_830 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'cong'691'_830 v0 v1 v2 v3
  = coe
      d_'43''45'cong_766 (coe v2) (coe v2) (coe v0) (coe v1)
      (coe du_'8771''45'refl_130) (coe v3)
-- Data.Rational.Unnormalised.Properties.+-congˡ
d_'43''45'cong'737'_838 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'cong'737'_838 v0 v1 v2 v3
  = coe
      d_'43''45'cong_766 (coe v0) (coe v1) (coe v2) (coe v2) (coe v3)
      (coe du_'8771''45'refl_130)
-- Data.Rational.Unnormalised.Properties.+-assoc-↥
d_'43''45'assoc'45''8613'_844 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'assoc'45''8613'_844 = erased
-- Data.Rational.Unnormalised.Properties.+-assoc-↧
d_'43''45'assoc'45''8615'_868 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'assoc'45''8615'_868 = erased
-- Data.Rational.Unnormalised.Properties.+-assoc-≡
d_'43''45'assoc'45''8801'_876 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'assoc'45''8801'_876 = erased
-- Data.Rational.Unnormalised.Properties.+-assoc
d_'43''45'assoc_884 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'assoc_884 ~v0 ~v1 ~v2 = du_'43''45'assoc_884
du_'43''45'assoc_884 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'43''45'assoc_884 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.+-comm-↥
d_'43''45'comm'45''8613'_892 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'comm'45''8613'_892 = erased
-- Data.Rational.Unnormalised.Properties.+-comm-↧
d_'43''45'comm'45''8615'_898 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'comm'45''8615'_898 = erased
-- Data.Rational.Unnormalised.Properties.+-comm-≡
d_'43''45'comm'45''8801'_904 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'comm'45''8801'_904 = erased
-- Data.Rational.Unnormalised.Properties.+-comm
d_'43''45'comm_910 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'comm_910 ~v0 ~v1 = du_'43''45'comm_910
du_'43''45'comm_910 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'43''45'comm_910 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.+-identityˡ-↥
d_'43''45'identity'737''45''8613'_916 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'737''45''8613'_916 = erased
-- Data.Rational.Unnormalised.Properties.+-identityˡ-↧
d_'43''45'identity'737''45''8615'_924 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'737''45''8615'_924 = erased
-- Data.Rational.Unnormalised.Properties.+-identityˡ-≡
d_'43''45'identity'737''45''8801'_928 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'737''45''8801'_928 = erased
-- Data.Rational.Unnormalised.Properties.+-identityˡ
d_'43''45'identity'737'_932 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'identity'737'_932 ~v0 = du_'43''45'identity'737'_932
du_'43''45'identity'737'_932 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'43''45'identity'737'_932 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.+-identityʳ-≡
d_'43''45'identity'691''45''8801'_936 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'691''45''8801'_936 = erased
-- Data.Rational.Unnormalised.Properties.+-identityʳ
d_'43''45'identity'691'_938 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'identity'691'_938 ~v0 = du_'43''45'identity'691'_938
du_'43''45'identity'691'_938 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'43''45'identity'691'_938 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.+-identity-≡
d_'43''45'identity'45''8801'_942 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'identity'45''8801'_942
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Rational.Unnormalised.Properties.+-identity
d_'43''45'identity_944 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'identity_944
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'43''45'identity'737'_932)
      (\ v0 -> coe du_'43''45'identity'691'_938)
-- Data.Rational.Unnormalised.Properties.+-inverseˡ
d_'43''45'inverse'737'_946 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'inverse'737'_946 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
-- Data.Rational.Unnormalised.Properties.+-inverseʳ
d_'43''45'inverse'691'_960 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'inverse'691'_960 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
-- Data.Rational.Unnormalised.Properties.+-inverse
d_'43''45'inverse_974 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_974
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_'43''45'inverse'737'_946) (coe d_'43''45'inverse'691'_960)
-- Data.Rational.Unnormalised.Properties.+-cancelˡ
d_'43''45'cancel'737'_982 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'cancel'737'_982 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
      (coe
         d_step'45''8771''728'_658 v1
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe v1)
            (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
         v2
         (coe
            d_step'45''8771''728'_658
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe v1)
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe v1)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                  (coe v0)))
            v2
            (coe
               d_step'45''8771''728'_658
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe v1)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                     (coe v0)))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v1)
                     (coe v0))
                  (coe v0))
               v2
               (coe
                  d_step'45''8771'_656
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v1)
                        (coe v0))
                     (coe v0))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                        (coe v1))
                     (coe v0))
                  v2
                  (coe
                     d_step'45''8771'_656
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                           (coe v1))
                        (coe v0))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                           (coe v2))
                        (coe v0))
                     v2
                     (coe
                        d_step'45''8771'_656
                        (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                              (coe v2))
                           (coe v0))
                        (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v2)
                              (coe v0))
                           (coe v0))
                        v2
                        (coe
                           d_step'45''8771'_656
                           (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                              (coe
                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v2)
                                 (coe v0))
                              (coe v0))
                           (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                              (coe v2)
                              (coe
                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                                 (coe v0)))
                           v2
                           (coe
                              d_step'45''8771'_656
                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                 (coe v2)
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                                    (coe v0) (coe v0)))
                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                 (coe v2)
                                 (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
                              v2
                              (coe
                                 d_step'45''8771'_656
                                 (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                    (coe v2)
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
                                 v2 v2
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                    (coe d_'8804''45'isPreorder_322) (coe v2))
                                 (coe du_'43''45'identity'691'_938))
                              (d_'43''45'cong'691'_830
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                                    (coe v0) (coe v0))
                                 (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
                                 (coe v2) (coe d_'43''45'inverse'691'_960 (coe v0))))
                           (coe du_'43''45'assoc_884))
                        (d_'43''45'cong'737'_838
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                              (coe v2))
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v2)
                              (coe v0))
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                           (coe du_'43''45'comm_910)))
                     (d_'43''45'cong'737'_838
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                           (coe v1))
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                           (coe v2))
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                        (coe v3)))
                  (d_'43''45'cong'737'_838
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v1)
                        (coe v0))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                        (coe v1))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                     (coe du_'43''45'comm_910)))
               (coe du_'43''45'assoc_884))
            (d_'43''45'cong'691'_830
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                  (coe v0))
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
               (coe v1) (coe d_'43''45'inverse'691'_960 (coe v0))))
         (coe du_'43''45'identity'691'_938))
-- Data.Rational.Unnormalised.Properties.+-cancelʳ
d_'43''45'cancel'691'_1002 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'cancel'691'_1002 v0 v1 v2 v3
  = coe
      d_'43''45'cancel'737'_982 (coe v0) (coe v1) (coe v2)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe v0) (coe v1))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe v1) (coe v0))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe v0) (coe v2))
            (coe
               d_step'45''8771'_656
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe v1) (coe v0))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe v2) (coe v0))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe v0) (coe v2))
               (coe
                  d_step'45''8771'_656
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe v2) (coe v0))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe v0) (coe v2))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe v0) (coe v2))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                        (coe v2)))
                  (coe du_'43''45'comm_910))
               v3)
            (coe du_'43''45'comm_910)))
-- Data.Rational.Unnormalised.Properties.p+p≃0⇒p≃0
d_p'43'p'8771'0'8658'p'8771'0_1018 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_p'43'p'8771'0'8658'p'8771'0_1018 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))
-- Data.Rational.Unnormalised.Properties.neg-distrib-+
d_neg'45'distrib'45''43'_1024 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'45''43'_1024 = erased
-- Data.Rational.Unnormalised.Properties.p≃-p⇒p≃0
d_p'8771''45'p'8658'p'8771'0_1036 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_p'8771''45'p'8658'p'8771'0_1036 v0 v1
  = coe
      d_p'43'p'8771'0'8658'p'8771'0_1018 (coe v0)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe v0) (coe v0))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
               (coe v0) (coe v0))
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
            (coe
               d_step'45''8771'_656
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                  (coe v0) (coe v0))
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                  (coe d_'8804''45'isPreorder_322)
                  (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
               (d_'43''45'inverse'691'_960 (coe v0)))
            (d_'43''45'cong'691'_830
               (coe v0)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
               (coe v0) (coe v1))))
-- Data.Rational.Unnormalised.Properties.lemma
d_lemma_1052 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lemma_1052 = erased
-- Data.Rational.Unnormalised.Properties.+-monoʳ-≤
d_'43''45'mono'691''45''8804'_1078 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'43''45'mono'691''45''8804'_1078 v0 v1 v2 v3
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v6 v7
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v8 v9
                      -> case coe v3 of
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v12
                             -> coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                                  (coe
                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                                     (coe
                                        MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                     (\ v13 v14 v15 ->
                                        coe
                                          MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630
                                          v15)
                                     (coe
                                        MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                           (coe
                                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                 (coe v0))
                                              (\ v13 v14 -> v13) v1 v2))
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe
                                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                              (\ v13 v14 -> v14)
                                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                 (coe v0))
                                              v1 v2)))
                                     (coe
                                        MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                           (coe
                                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                              (\ v13 v14 -> v14)
                                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                 (coe v0))
                                              v1 v2))
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe
                                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                 (coe v0))
                                              (\ v13 v14 -> v13) v1 v2)))
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                        (coe
                                           MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                        (\ v13 v14 v15 v16 v17 ->
                                           coe
                                             MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                             v16 v17)
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'43'__276
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe du_r'8322'_1092 (coe v4) (coe v5))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0)))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2)))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'43'__276
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe du_r'8322'_1092 (coe v4) (coe v5))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0)))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1)))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe
                                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                              (coe
                                                 MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                                 (\ v13 v14 -> v14)
                                                 (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                    (coe v0))
                                                 v1 v2))
                                           (coe
                                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                              (coe
                                                 MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                                 (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                    (coe v0))
                                                 (\ v13 v14 -> v13) v1 v2)))
                                        (coe
                                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                           (coe
                                              MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                    (coe v0) (coe v2)))
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                    (coe v0) (coe v1)))))
                                        (coe
                                           d_leq_1094 (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                           (coe v9) (coe v12))))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties._.r₂
d_r'8322'_1092 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_r'8322'_1092 v0 v1 ~v2 ~v3 ~v4 ~v5 ~v6 = du_r'8322'_1092 v0 v1
du_r'8322'_1092 :: Integer -> Integer -> Integer
du_r'8322'_1092 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
-- Data.Rational.Unnormalised.Properties._.leq
d_leq_1094 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_leq_1094 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Data.Integer.Properties.d_'43''45'mono'45''8804'_4310
      (coe
         MAlonzo.Code.Data.Integer.Base.d__'42'__308
         (coe du_r'8322'_1092 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5)))))
      (coe
         MAlonzo.Code.Data.Integer.Base.d__'42'__308
         (coe du_r'8322'_1092 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3)))))
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                     (coe v0) (coe v1)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                     (coe v0) (coe v1)))))
         (\ v7 v8 -> v7)
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v2)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5))))
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v4)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3)))))
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v7 v8 -> v8)
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                     (coe v0) (coe v1)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                     (coe v0) (coe v1)))))
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v2)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5))))
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v4)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3)))))
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8804''45'reflexive_2506
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe du_r'8322'_1092 (coe v0) (coe v1))
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                     (coe v2) (coe v3)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                     (coe v4) (coe v5))))))
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'737''45''8804''45'nonNeg_5856
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v0) (coe v1)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v0) (coe v1))))
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v2)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5))))
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v4)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3))))
         v6)
-- Data.Rational.Unnormalised.Properties.+-monoˡ-≤
d_'43''45'mono'737''45''8804'_1102 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'43''45'mono'737''45''8804'_1102 v0 v1 v2
  = coe d_'43''45'mono'691''45''8804'_1078 (coe v0) (coe v1) (coe v2)
-- Data.Rational.Unnormalised.Properties.+-mono-≤
d_'43''45'mono'45''8804'_1118 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'43''45'mono'45''8804'_1118 v0 v1 v2 v3 v4 v5
  = coe
      d_'8804''45'trans_250
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
         (coe v2))
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v6 v7 -> v7)
         (\ v6 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
              (coe v6) (coe v2))
         v0 v1)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v1)
         (coe v3))
      (coe d_'43''45'mono'737''45''8804'_1102 v2 v0 v1 v4)
      (coe
         d_'43''45'mono'691''45''8804'_1078 (coe v1) (coe v2) (coe v3)
         (coe v5))
-- Data.Rational.Unnormalised.Properties.p≤q⇒p≤r+q
d_p'8804'q'8658'p'8804'r'43'q_1136 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_p'8804'q'8658'p'8804'r'43'q_1136 v0 v1 v2 ~v3 v4
  = du_p'8804'q'8658'p'8804'r'43'q_1136 v0 v1 v2 v4
du_p'8804'q'8658'p'8804'r'43'q_1136 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_p'8804'q'8658'p'8804'r'43'q_1136 v0 v1 v2 v3
  = coe
      d_'43''45'mono'45''8804'_1118
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
      (coe v2) (coe v0) (coe v1)
      (coe du_nonNegative'8315''185'_692 (coe v2)) (coe v3)
-- Data.Rational.Unnormalised.Properties.p≤q+p
d_p'8804'q'43'p_1154 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_p'8804'q'43'p_1154 v0 v1 ~v2 = du_p'8804'q'43'p_1154 v0 v1
du_p'8804'q'43'p_1154 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_p'8804'q'43'p_1154 v0 v1
  = coe
      du_p'8804'q'8658'p'8804'r'43'q_1136 (coe v0) (coe v0) (coe v1)
      (coe d_'8804''45'refl_246 (coe v0))
-- Data.Rational.Unnormalised.Properties.p≤p+q
d_p'8804'p'43'q_1166 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_p'8804'p'43'q_1166 v0 v1 ~v2 = du_p'8804'p'43'q_1166 v0 v1
du_p'8804'p'43'q_1166 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_p'8804'p'43'q_1166 v0 v1
  = coe du_p'8804'q'43'p_1154 (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.+-monoʳ-<
d_'43''45'mono'691''45''60'_1180 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'43''45'mono'691''45''60'_1180 v0 v1 v2 v3
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v6 v7
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v8 v9
                      -> case coe v3 of
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v12
                             -> coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                                  (coe
                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                                        (\ v13 v14 v15 v16 v17 ->
                                           coe
                                             MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770
                                             v16 v17)
                                        (coe
                                           MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                                        (\ v13 v14 v15 v16 v17 ->
                                           coe
                                             MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                             v16 v17)
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'43'__276
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe du_'8613'r'8615'r_1194 (coe v4) (coe v5))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe du_'8615'r'8615'r_1196 (coe v4) (coe v5))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2)))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'43'__276
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe du_'8613'r'8615'r_1194 (coe v4) (coe v5))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe du_'8615'r'8615'r_1196 (coe v4) (coe v5))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1)))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe
                                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                 (coe v0) (coe v2)))
                                           (coe
                                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                 (coe v0) (coe v1))))
                                        (coe
                                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                           (coe
                                              MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                    (coe v0) (coe v2)))
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                                    (coe v0) (coe v1)))))
                                        (coe
                                           d_leq_1198 (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                           (coe v9) (coe v12))))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties._.↥r↧r
d_'8613'r'8615'r_1194 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_'8613'r'8615'r_1194 v0 v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_'8613'r'8615'r_1194 v0 v1
du_'8613'r'8615'r_1194 :: Integer -> Integer -> Integer
du_'8613'r'8615'r_1194 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
-- Data.Rational.Unnormalised.Properties._.↧r↧r
d_'8615'r'8615'r_1196 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'60'__50 -> Integer
d_'8615'r'8615'r_1196 v0 v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_'8615'r'8615'r_1196 v0 v1
du_'8615'r'8615'r_1196 :: Integer -> Integer -> Integer
du_'8615'r'8615'r_1196 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
-- Data.Rational.Unnormalised.Properties._.leq
d_leq_1198 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_leq_1198 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Data.Integer.Properties.d_'43''45'mono'45''8804''45''60'_4432
      (coe
         MAlonzo.Code.Data.Integer.Base.d__'42'__308
         (coe du_'8613'r'8615'r_1194 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5)))))
      (coe
         MAlonzo.Code.Data.Integer.Base.d__'42'__308
         (coe du_'8613'r'8615'r_1194 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3)))))
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe du_'8615'r'8615'r_1196 (coe v0) (coe v1)))
         (\ v7 v8 -> v7)
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v2)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5))))
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v4)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3)))))
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v7 v8 -> v8)
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe du_'8615'r'8615'r_1196 (coe v0) (coe v1)))
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v2)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5))))
         (MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe v4)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3)))))
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8804''45'reflexive_2506
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe du_'8613'r'8615'r_1194 (coe v0) (coe v1))
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                     (coe v2) (coe v3)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                     (coe v4) (coe v5))))))
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'737''45''60''45'pos_5974
         (coe du_'8615'r'8615'r_1196 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v2)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v4) (coe v5))))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v4)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                  (coe v2) (coe v3))))
         (coe v6))
-- Data.Rational.Unnormalised.Properties.+-monoˡ-<
d_'43''45'mono'737''45''60'_1206 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'43''45'mono'737''45''60'_1206 v0 v1 v2
  = coe d_'43''45'mono'691''45''60'_1180 (coe v0) (coe v1) (coe v2)
-- Data.Rational.Unnormalised.Properties.+-mono-<
d_'43''45'mono'45''60'_1222 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'43''45'mono'45''60'_1222 v0 v1 v2 v3 v4 v5
  = coe
      d_'60''45'trans_470
      (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
         (coe v0) (coe v2))
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v6 v7 -> v7)
         (\ v6 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
              (coe v6) (coe v2))
         v0 v1)
      (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
         (coe v1) (coe v3))
      (coe d_'43''45'mono'737''45''60'_1206 v2 v0 v1 v4)
      (d_'43''45'mono'691''45''60'_1180
         (coe v1) (coe v2) (coe v3) (coe v5))
-- Data.Rational.Unnormalised.Properties.+-mono-≤-<
d_'43''45'mono'45''8804''45''60'_1236 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'43''45'mono'45''8804''45''60'_1236 v0 v1 v2 v3 v4 v5
  = coe
      d_'8804''45''60''45'trans_402
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
         (coe v2))
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v6 v7 -> v7)
         (\ v6 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
              (coe v6) (coe v2))
         v0 v1)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v1)
         (coe v3))
      (coe d_'43''45'mono'737''45''8804'_1102 v2 v0 v1 v4)
      (coe
         d_'43''45'mono'691''45''60'_1180 (coe v1) (coe v2) (coe v3)
         (coe v5))
-- Data.Rational.Unnormalised.Properties.+-mono-<-≤
d_'43''45'mono'45''60''45''8804'_1248 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'43''45'mono'45''60''45''8804'_1248 v0 v1 v2 v3 v4 v5
  = coe
      d_'60''45''8804''45'trans_436
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
         (coe v2))
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v6 v7 -> v7)
         (\ v6 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
              (coe v6) (coe v2))
         v0 v1)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v1)
         (coe v3))
      (coe d_'43''45'mono'737''45''60'_1206 v2 v0 v1 v4)
      (coe
         d_'43''45'mono'691''45''8804'_1078 (coe v1) (coe v2) (coe v3)
         (coe v5))
-- Data.Rational.Unnormalised.Properties.pos+pos⇒pos
d_pos'43'pos'8658'pos_1268 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_pos'43'pos'8658'pos_1268 v0 ~v1 v2 ~v3
  = du_pos'43'pos'8658'pos_1268 v0 v2
du_pos'43'pos'8658'pos_1268 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_pos'43'pos'8658'pos_1268 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_positive_162
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
         (coe v1))
      (coe
         d_'43''45'mono'45''60'_1222
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v1) (coe du_positive'8315''185'_686 (coe v0))
         (coe du_positive'8315''185'_686 (coe v1)))
-- Data.Rational.Unnormalised.Properties.nonNeg+nonNeg⇒nonNeg
d_nonNeg'43'nonNeg'8658'nonNeg_1282 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_nonNeg'43'nonNeg'8658'nonNeg_1282 v0 ~v1 v2 ~v3
  = du_nonNeg'43'nonNeg'8658'nonNeg_1282 v0 v2
du_nonNeg'43'nonNeg'8658'nonNeg_1282 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_nonNeg'43'nonNeg'8658'nonNeg_1282 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_nonNegative_186
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
         (coe v1))
      (coe
         d_'43''45'mono'45''8804'_1118
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v1) (coe du_nonNegative'8315''185'_692 (coe v0))
         (coe du_nonNegative'8315''185'_692 (coe v1)))
-- Data.Rational.Unnormalised.Properties.+-minus-telescope
d_'43''45'minus'45'telescope_1294 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'43''45'minus'45'telescope_1294 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
      (coe
         d_step'45''8771'_656
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
               (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v1)
               (coe v2)))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                  (coe v1))
               (coe v1))
            (coe v2))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
            (coe v0) (coe v2))
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                     (coe v1))
                  (coe v1))
               (coe v2))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                     (coe v1)))
               (coe v2))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
               (coe v0) (coe v2))
            (coe
               d_step'45''8771'_656
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                        (coe v1)))
                  (coe v2))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                     (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
                  (coe v2))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                  (coe v0) (coe v2))
               (coe
                  d_step'45''8771'_656
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                        (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
                     (coe v2))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                     (coe v0) (coe v2))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                     (coe v0) (coe v2))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                        (coe v2)))
                  (d_'43''45'cong'737'_838
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                        (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
                     (coe v0)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
                     (coe du_'43''45'identity'691'_938)))
               (d_'43''45'cong'737'_838
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                        (coe v1)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                     (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
                  (coe
                     d_'43''45'cong'691'_830
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                        (coe v1))
                     (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
                     (coe v0) (coe d_'43''45'inverse'737'_946 (coe v1)))))
            (d_'43''45'cong'737'_838
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1)))
                  (coe v1))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196 (coe v0)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                     (coe v1)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
               (coe du_'43''45'assoc_884)))
         (coe du_'8771''45'sym_134 (coe du_'43''45'assoc_884)))
-- Data.Rational.Unnormalised.Properties.p≃q⇒p-q≃0
d_p'8771'q'8658'p'45'q'8771'0_1310 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_p'8771'q'8658'p'45'q'8771'0_1310 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
      (coe
         d_step'45''8771'_656
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
            (coe v0) (coe v1))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
            (coe v1) (coe v1))
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
               (coe v1) (coe v1))
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_322)
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
            (d_'43''45'inverse'691'_960 (coe v1)))
         (d_'43''45'cong'737'_838
            (coe v0) (coe v1)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
            (coe v2)))
-- Data.Rational.Unnormalised.Properties.p-q≃0⇒p≃q
d_p'45'q'8771'0'8658'p'8771'q_1326 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_p'45'q'8771'0'8658'p'8771'q_1326 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
      (coe
         d_step'45''8771'_656
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe v0)
            (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe v0)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
               (coe v1)))
         v1
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                  (coe v1))
               (coe v1))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
               (coe v1))
            v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_322) (coe v1))
            (d_'43''45'cong'737'_838
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                  (coe v1))
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
               (coe v1) (coe v2)))
         (d_'43''45'cong'691'_830
            (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
               (coe v1))
            (coe v0)
            (coe
               du_'8771''45'sym_134 (coe d_'43''45'inverse'737'_946 (coe v1)))))
-- Data.Rational.Unnormalised.Properties.neg-mono-≤
d_neg'45'mono'45''8804'_1338 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_neg'45'mono'45''8804'_1338 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v5 v6
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v9
                      -> coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                              (coe
                                 MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                              (\ v10 v11 v12 ->
                                 coe
                                   MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630
                                   v12)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                    (coe
                                       MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                       (\ v10 v11 -> v11)
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 v0
                                       v1))
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                    (coe
                                       MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                       (\ v10 v11 -> v10) v0 v1)))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                    (coe
                                       MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                       (\ v10 v11 -> v10) v0 v1))
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                    (coe
                                       MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                       (\ v10 v11 -> v11)
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 v0
                                       v1)))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                 (coe
                                    MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                 (\ v10 v11 v12 v13 v14 ->
                                    coe
                                      MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                      v13 v14)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v5)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v0))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v3)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v1))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                       (coe
                                          MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (\ v10 v11 -> v10) v0 v1))
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                       (coe
                                          MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                          (\ v10 v11 -> v11)
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          v0 v1)))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                    (coe
                                       MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v3))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v1))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Properties.du_neg'45'mono'45''8804'_3034
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v5)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v0)))
                                    (coe v9))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.neg-cancel-≤
d_neg'45'cancel'45''8804'_1354 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_neg'45'cancel'45''8804'_1354 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v5 v6
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v9
                      -> coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                              (coe
                                 MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                              (\ v10 v11 v12 ->
                                 coe
                                   MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630
                                   v12)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v5)
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                    (coe v0)))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v3)
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                    (coe v1)))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                 (coe
                                    MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                 (\ v10 v11 v12 v13 v14 ->
                                    coe
                                      MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                      v13 v14)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v5))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v0))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v3))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v1))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v3)
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                       (coe v1)))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                    (coe
                                       MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v3)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe v1))))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Properties.du_neg'45'mono'45''8804'_3034
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v1)))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v0))))
                                    (coe v9))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.p≤q⇒p-q≤0
d_p'8804'q'8658'p'45'q'8804'0_1370 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_p'8804'q'8658'p'45'q'8804'0_1370 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v3 v4 v5 -> coe du_'60''8658''8804'_366 v5)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
         (coe v1))
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_322)
         (coe d_'8804''45''60''45'trans_402)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
            (coe v1))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v1)
            (coe v1))
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
               (coe v1) (coe v1))
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_322)
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
            (d_'43''45'inverse'691'_960 (coe v1)))
         (coe
            d_'43''45'mono'737''45''8804'_1102
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
            v0 v1 v2))
-- Data.Rational.Unnormalised.Properties.p-q≤0⇒p≤q
d_p'45'q'8804'0'8658'p'8804'q_1386 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_p'45'q'8804'0'8658'p'8804'q_1386 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v3 v4 v5 -> coe du_'60''8658''8804'_366 v5) (coe v0) (coe v1)
      (coe
         d_step'45''8771'_656
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe v0)
            (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe v0)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
               (coe v1)))
         v1
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
            (coe d_'8804''45'isPreorder_322)
            (coe d_'8804''45''60''45'trans_402)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
                  (coe v1))
               (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
               (coe v1))
            (coe v1)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_322) (coe v1))
            (coe
               d_'43''45'mono'737''45''8804'_1102 v1
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
                  (coe v0) (coe v1))
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108 v2))
         (d_'43''45'cong'691'_830
            (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
               (coe v1))
            (coe v0)
            (coe
               du_'8771''45'sym_134 (coe d_'43''45'inverse'737'_946 (coe v1)))))
-- Data.Rational.Unnormalised.Properties.p≤q⇒0≤q-p
d_p'8804'q'8658'0'8804'q'45'p_1402 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_p'8804'q'8658'0'8804'q'45'p_1402 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v3 v4 v5 -> coe du_'60''8658''8804'_366 v5)
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v1)
         (coe v0))
      (coe
         d_step'45''8771'_656
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
            (coe v0) (coe v0))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
            (coe v1) (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
            (coe d_'8804''45'isPreorder_322)
            (coe d_'8804''45''60''45'trans_402)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v1)
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v1)
               (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_322)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v1)
                  (coe v0)))
            (coe
               d_'43''45'mono'737''45''8804'_1102
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
               v0 v1 v2))
         (coe
            du_'8771''45'sym_134 (coe d_'43''45'inverse'691'_960 (coe v0))))
-- Data.Rational.Unnormalised.Properties.0≤q-p⇒p≤q
d_0'8804'q'45'p'8658'p'8804'q_1418 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_0'8804'q'45'p'8658'p'8804'q_1418 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v3 v4 v5 -> coe du_'60''8658''8804'_366 v5) (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_322)
         (coe d_'8804''45''60''45'trans_402)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
            (coe v0))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v1)
               (coe v0))
            (coe v0))
         (coe v1)
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe v1)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                  (coe v0)))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe v1)
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
            v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_322) (coe v1))
            (d_'43''45'cong'691'_830
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                  (coe v0))
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
               (coe v1) (coe d_'43''45'inverse'737'_946 (coe v0))))
         (coe
            d_'43''45'mono'737''45''8804'_1102 v0
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208
               (coe v1) (coe v0))
            v2))
-- Data.Rational.Unnormalised.Properties.+-isMagma
d_'43''45'isMagma_1430 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'43''45'isMagma_1430
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe d_'8771''45'isEquivalence_164) (coe d_'43''45'cong_766)
-- Data.Rational.Unnormalised.Properties.+-isSemigroup
d_'43''45'isSemigroup_1432 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'43''45'isSemigroup_1432
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'43''45'isMagma_1430)
      (\ v0 v1 v2 -> coe du_'43''45'assoc_884)
-- Data.Rational.Unnormalised.Properties.+-0-isMonoid
d_'43''45'0'45'isMonoid_1434 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'0'45'isMonoid_1434
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'43''45'isSemigroup_1432) (coe d_'43''45'identity_944)
-- Data.Rational.Unnormalised.Properties.+-0-isCommutativeMonoid
d_'43''45'0'45'isCommutativeMonoid_1436 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'0'45'isCommutativeMonoid_1436
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'43''45'0'45'isMonoid_1434)
      (\ v0 v1 -> coe du_'43''45'comm_910)
-- Data.Rational.Unnormalised.Properties.+-0-isGroup
d_'43''45'0'45'isGroup_1438 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_'43''45'0'45'isGroup_1438
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsGroup'46'constructor_22905
      (coe d_'43''45'0'45'isMonoid_1434) (coe d_'43''45'inverse_974)
      (coe d_'45''8255'cong_188)
-- Data.Rational.Unnormalised.Properties.+-0-isAbelianGroup
d_'43''45'0'45'isAbelianGroup_1440 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'0'45'isAbelianGroup_1440
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsAbelianGroup'46'constructor_27897
      (coe d_'43''45'0'45'isGroup_1438)
      (\ v0 v1 -> coe du_'43''45'comm_910)
-- Data.Rational.Unnormalised.Properties.+-magma
d_'43''45'magma_1442 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'43''45'magma_1442
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
      d_'43''45'isMagma_1430
-- Data.Rational.Unnormalised.Properties.+-semigroup
d_'43''45'semigroup_1444 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'43''45'semigroup_1444
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
      d_'43''45'isSemigroup_1432
-- Data.Rational.Unnormalised.Properties.+-0-monoid
d_'43''45'0'45'monoid_1446 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'43''45'0'45'monoid_1446
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
      d_'43''45'0'45'isMonoid_1434
-- Data.Rational.Unnormalised.Properties.+-0-commutativeMonoid
d_'43''45'0'45'commutativeMonoid_1448 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'0'45'commutativeMonoid_1448
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
      d_'43''45'0'45'isCommutativeMonoid_1436
-- Data.Rational.Unnormalised.Properties.+-0-group
d_'43''45'0'45'group_1450 ::
  MAlonzo.Code.Algebra.Bundles.T_Group_1266
d_'43''45'0'45'group_1450
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Group'46'constructor_21965
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
      d_'43''45'0'45'isGroup_1438
-- Data.Rational.Unnormalised.Properties.+-0-abelianGroup
d_'43''45'0'45'abelianGroup_1452 ::
  MAlonzo.Code.Algebra.Bundles.T_AbelianGroup_1378
d_'43''45'0'45'abelianGroup_1452
  = coe
      MAlonzo.Code.Algebra.Bundles.C_AbelianGroup'46'constructor_24425
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
      d_'43''45'0'45'isAbelianGroup_1440
-- Data.Rational.Unnormalised.Properties.*-cong
d_'42''45'cong_1454 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'cong_1454 v0 v1 v2 v3 v4 v5
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            seq (coe v2)
            (coe
               seq (coe v3)
               (coe
                  seq (coe v4)
                  (coe
                     seq (coe v5)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))))))
-- Data.Rational.Unnormalised.Properties.*-congˡ
d_'42''45'cong'737'_1488 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'cong'737'_1488 v0 v1 v2 v3
  = coe
      d_'42''45'cong_1454 (coe v0) (coe v0) (coe v1) (coe v2)
      (coe du_'8771''45'refl_130) (coe v3)
-- Data.Rational.Unnormalised.Properties.*-congʳ
d_'42''45'cong'691'_1494 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'cong'691'_1494 v0 v1 v2 v3
  = coe
      d_'42''45'cong_1454 (coe v1) (coe v2) (coe v0) (coe v0) (coe v3)
      (coe du_'8771''45'refl_130)
-- Data.Rational.Unnormalised.Properties.*-assoc-↥
d_'42''45'assoc'45''8613'_1500 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc'45''8613'_1500 = erased
-- Data.Rational.Unnormalised.Properties.*-assoc-↧
d_'42''45'assoc'45''8615'_1508 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc'45''8615'_1508 = erased
-- Data.Rational.Unnormalised.Properties.*-assoc-≡
d_'42''45'assoc'45''8801'_1516 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc'45''8801'_1516 = erased
-- Data.Rational.Unnormalised.Properties.*-assoc
d_'42''45'assoc_1524 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'assoc_1524 ~v0 ~v1 ~v2 = du_'42''45'assoc_1524
du_'42''45'assoc_1524 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'assoc_1524 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.*-comm-↥
d_'42''45'comm'45''8613'_1532 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm'45''8613'_1532 = erased
-- Data.Rational.Unnormalised.Properties.*-comm-↧
d_'42''45'comm'45''8615'_1538 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm'45''8615'_1538 = erased
-- Data.Rational.Unnormalised.Properties.*-comm-≡
d_'42''45'comm'45''8801'_1544 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm'45''8801'_1544 = erased
-- Data.Rational.Unnormalised.Properties.*-comm
d_'42''45'comm_1550 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'comm_1550 ~v0 ~v1 = du_'42''45'comm_1550
du_'42''45'comm_1550 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'comm_1550 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.*-identityˡ-≡
d_'42''45'identity'737''45''8801'_1556 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'737''45''8801'_1556 = erased
-- Data.Rational.Unnormalised.Properties.*-identityʳ-≡
d_'42''45'identity'691''45''8801'_1560 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'691''45''8801'_1560 = erased
-- Data.Rational.Unnormalised.Properties.*-identity-≡
d_'42''45'identity'45''8801'_1562 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity'45''8801'_1562
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Rational.Unnormalised.Properties.*-identityˡ
d_'42''45'identity'737'_1564 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'identity'737'_1564 ~v0 = du_'42''45'identity'737'_1564
du_'42''45'identity'737'_1564 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'identity'737'_1564 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.*-identityʳ
d_'42''45'identity'691'_1568 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'identity'691'_1568 ~v0 = du_'42''45'identity'691'_1568
du_'42''45'identity'691'_1568 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'identity'691'_1568 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.*-identity
d_'42''45'identity_1572 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1572
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'42''45'identity'737'_1564)
      (\ v0 -> coe du_'42''45'identity'691'_1568)
-- Data.Rational.Unnormalised.Properties.*-inverseˡ
d_'42''45'inverse'737'_1578 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'inverse'737'_1578 v0 ~v1
  = du_'42''45'inverse'737'_1578 v0
du_'42''45'inverse'737'_1578 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'inverse'737'_1578 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (1 :: Integer)) ->
                 coe
                   MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30
             _ -> coe
                    du_'42''45'inverse'737'_1578
                    (coe
                       MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                       (coe subInt (coe (0 :: Integer)) (coe v1)) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-inverseʳ
d_'42''45'inverse'691'_1608 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'inverse'691'_1608 v0 ~v1
  = du_'42''45'inverse'691'_1608 v0
du_'42''45'inverse'691'_1608 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'inverse'691'_1608 v0
  = coe
      du_'8771''45'trans_138 (coe du_'42''45'comm_1550)
      (coe du_'42''45'inverse'737'_1578 (coe v0))
-- Data.Rational.Unnormalised.Properties.*-zeroˡ
d_'42''45'zero'737'_1612 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'zero'737'_1612 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
-- Data.Rational.Unnormalised.Properties.*-zeroʳ
d_'42''45'zero'691'_1616 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'zero'691'_1616
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'ze'737''8658'ze'691'_246
      (coe d_'8771''45'setoid_168)
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202)
      (\ v0 v1 -> coe du_'42''45'comm_1550)
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
      (coe d_'42''45'zero'737'_1612)
-- Data.Rational.Unnormalised.Properties.*-zero
d_'42''45'zero_1618 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'zero_1618
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_'42''45'zero'737'_1612) (coe d_'42''45'zero'691'_1616)
-- Data.Rational.Unnormalised.Properties.*-distribˡ-+
d_'42''45'distrib'737''45''43'_1620 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'737''45''43'_1620 v0 v1 v2
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            seq (coe v2)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)))
-- Data.Rational.Unnormalised.Properties.*-distribʳ-+
d_'42''45'distrib'691''45''43'_1658 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'691''45''43'_1658
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'distr'737''8658'distr'691'_390
      (coe d_'8771''45'setoid_168)
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202)
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196)
      (coe d_'43''45'cong_766) (\ v0 v1 -> coe du_'42''45'comm_1550)
      (coe d_'42''45'distrib'737''45''43'_1620)
-- Data.Rational.Unnormalised.Properties.*-distrib-+
d_'42''45'distrib'45''43'_1660 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'distrib'45''43'_1660
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_'42''45'distrib'737''45''43'_1620)
      (coe d_'42''45'distrib'691''45''43'_1658)
-- Data.Rational.Unnormalised.Properties.neg-distribˡ-*
d_neg'45'distrib'737''45''42'_1666 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_neg'45'distrib'737''45''42'_1666 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))
-- Data.Rational.Unnormalised.Properties.neg-distribʳ-*
d_neg'45'distrib'691''45''42'_1678 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_neg'45'distrib'691''45''42'_1678 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))
-- Data.Rational.Unnormalised.Properties.*-cancelˡ-/
d_'42''45'cancel'737''45''47'_1696 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'cancel'737''45''47'_1696 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_'42''45'cancel'737''45''47'_1696
du_'42''45'cancel'737''45''47'_1696 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'cancel'737''45''47'_1696
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30
-- Data.Rational.Unnormalised.Properties.*-cancelʳ-/
d_'42''45'cancel'691''45''47'_1728 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'cancel'691''45''47'_1728 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_'42''45'cancel'691''45''47'_1728
du_'42''45'cancel'691''45''47'_1728 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'cancel'691''45''47'_1728
  = coe du_'42''45'cancel'737''45''47'_1696
-- Data.Rational.Unnormalised.Properties.reorder₁
d_reorder'8321'_1752 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reorder'8321'_1752 = erased
-- Data.Rational.Unnormalised.Properties.reorder₂
d_reorder'8322'_1774 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reorder'8322'_1774 = erased
-- Data.Rational.Unnormalised.Properties.+▹-nonNeg
d_'43''9657''45'nonNeg_1790 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_'43''9657''45'nonNeg_1790 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Integer.Base.C_NonNegative'46'constructor_1353
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Unnormalised.Properties.*-cancelʳ-≤-pos
d_'42''45'cancel'691''45''8804''45'pos_1796 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'cancel'691''45''8804''45'pos_1796 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'691''45''8804''45'pos_1796 v0 v1 v2 v4
du_'42''45'cancel'691''45''8804''45'pos_1796 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'cancel'691''45''8804''45'pos_1796 v0 v1 v2 v3
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v6 v7
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v8 v9
                      -> case coe v3 of
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v12
                             -> coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                                  (coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'42''45'cancel'691''45''8804''45'pos_5758
                                     (coe
                                        MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v4)
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe v1)))
                                     (coe
                                        MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v6)
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe v0)))
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                                        (coe
                                           MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                        (\ v13 v14 v15 ->
                                           coe
                                             MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630
                                             v15)
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v4)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe v1)))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v8)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe v2))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v6)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe v0)))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v8)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe v2))))
                                        (coe
                                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                           (coe
                                              MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                           (\ v13 v14 v15 v16 v17 ->
                                              coe
                                                MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                                v16 v17)
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v4) (coe v8))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6) (coe v8))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0)))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))))
                                           (coe
                                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                    (coe v6)
                                                    (coe
                                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                       (coe v0)))
                                                 (coe
                                                    MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                    (coe v8)
                                                    (coe
                                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                       (coe v2)))))
                                           (coe v12))))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-cancelˡ-≤-pos
d_'42''45'cancel'737''45''8804''45'pos_1814 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'cancel'737''45''8804''45'pos_1814 v0 v1 v2 ~v3
  = du_'42''45'cancel'737''45''8804''45'pos_1814 v0 v1 v2
du_'42''45'cancel'737''45''8804''45'pos_1814 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'cancel'737''45''8804''45'pos_1814 v0 v1 v2
  = coe
      du_'42''45'cancel'691''45''8804''45'pos_1796 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Unnormalised.Properties.*-cancelʳ-≤-neg
d_'42''45'cancel'691''45''8804''45'neg_1834 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'cancel'691''45''8804''45'neg_1834 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'691''45''8804''45'neg_1834 v0 v1 v2 v4
du_'42''45'cancel'691''45''8804''45'neg_1834 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'cancel'691''45''8804''45'neg_1834 v0 v1 v2 v3
  = coe
      seq (coe v2)
      (coe
         d_neg'45'cancel'45''8804'_1354 (coe v0) (coe v1)
         (coe
            du_'42''45'cancel'691''45''8804''45'pos_1796
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
               (coe d_'8804''45'isPreorder_322)
               (\ v4 v5 v6 -> coe du_'60''8658''8804'_366 v6)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2)))
               (coe
                  d_step'45''8771''728'_658
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2)))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                           (coe v2))))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2)))
                  (coe
                     d_step'45''8771''728'_658
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                              (coe v2))))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
                              (coe v2))))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2)))
                     (coe
                        d_step'45''8771'_656
                        (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                              (coe
                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
                                 (coe v2))))
                        (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                           (coe v0) (coe v2))
                        (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                           (coe d_'8804''45'isPreorder_322)
                           (coe d_'8804''45''60''45'trans_402)
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
                              (coe v2))
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                              (coe v2))
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                              (coe
                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v1))
                              (coe
                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2)))
                           (coe
                              d_step'45''8771''728'_658
                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                 (coe v1) (coe v2))
                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                       (coe v1) (coe v2))))
                              (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                    (coe v1))
                                 (coe
                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                    (coe v2)))
                              (coe
                                 d_step'45''8771'_656
                                 (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                          (coe v1) (coe v2))))
                                 (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                       (coe v1)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v2))))
                                 (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                       (coe v1))
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                       (coe v2)))
                                 (coe
                                    d_step'45''8771'_656
                                    (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                          (coe v1)
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v2))))
                                    (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v1))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v2)))
                                    (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v1))
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v2)))
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                       (coe d_'8804''45'isPreorder_322)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v1))
                                          (coe
                                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                             (coe v2))))
                                    (d_neg'45'distrib'737''45''42'_1666
                                       (coe v1)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v2))))
                                 (d_'45''8255'cong_188
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                          (coe v1) (coe v2)))
                                    (coe
                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                       (coe v1)
                                       (coe
                                          MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                                          (coe v2)))
                                    (coe d_neg'45'distrib'691''45''42'_1678 (coe v1) (coe v2))))
                              (coe du_neg'45'involutive_180))
                           (coe v3))
                        (coe du_neg'45'involutive_180))
                     (d_'45''8255'cong_188
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
                              (coe v2)))
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2)))
                        (coe d_neg'45'distrib'691''45''42'_1678 (coe v0) (coe v2))))
                  (d_neg'45'distrib'737''45''42'_1666
                     (coe v0)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe v2)))))))
-- Data.Rational.Unnormalised.Properties.*-cancelˡ-≤-neg
d_'42''45'cancel'737''45''8804''45'neg_1852 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'cancel'737''45''8804''45'neg_1852 v0 v1 v2 ~v3
  = du_'42''45'cancel'737''45''8804''45'neg_1852 v0 v1 v2
du_'42''45'cancel'737''45''8804''45'neg_1852 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'cancel'737''45''8804''45'neg_1852 v0 v1 v2
  = coe
      du_'42''45'cancel'691''45''8804''45'neg_1834 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Unnormalised.Properties.*-monoˡ-≤-nonNeg
d_'42''45'mono'737''45''8804''45'nonNeg_1874 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'737''45''8804''45'nonNeg_1874 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''8804''45'nonNeg_1874 v0 v2 v3 v4
du_'42''45'mono'737''45''8804''45'nonNeg_1874 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'737''45''8804''45'nonNeg_1874 v0 v1 v2 v3
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v6 v7
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v8 v9
                      -> case coe v3 of
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v12
                             -> coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                                  (coe
                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                                     (coe
                                        MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                     (\ v13 v14 v15 ->
                                        coe
                                          MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630
                                          v15)
                                     (coe
                                        MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                           (coe
                                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                              (\ v13 ->
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                                   (coe v13) (coe v0))
                                              (\ v13 v14 -> v13) v1 v2))
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe
                                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                              (\ v13 v14 -> v14)
                                              (\ v13 ->
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                                   (coe v13) (coe v0))
                                              v1 v2)))
                                     (coe
                                        MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                           (coe
                                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                              (\ v13 v14 -> v14)
                                              (\ v13 ->
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                                   (coe v13) (coe v0))
                                              v1 v2))
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe
                                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                              (\ v13 ->
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                                   (coe v13) (coe v0))
                                              (\ v13 v14 -> v13) v1 v2)))
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                        (coe
                                           MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                        (\ v13 v14 v15 v16 v17 ->
                                           coe
                                             MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                             v16 v17)
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe du_l'8321'_1890 (coe v6) (coe v8) (coe v9))
                                           (coe
                                              mulInt (coe v4)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominatorℕ_18
                                                 (coe v0))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe du_l'8322'_1892 (coe v6) (coe v7) (coe v8))
                                           (coe
                                              mulInt (coe v4)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominatorℕ_18
                                                 (coe v0))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe
                                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                              (coe
                                                 MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                                 (\ v13 v14 -> v14)
                                                 (\ v13 ->
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                                      (coe v13) (coe v0))
                                                 v1 v2))
                                           (coe
                                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                              (coe
                                                 MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                                 (\ v13 ->
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                                                      (coe v13) (coe v0))
                                                 (\ v13 v14 -> v13) v1 v2)))
                                        (coe
                                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                           (coe
                                              MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8) (coe v4))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0)))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''8804''45'nonNeg_5814
                                           (coe
                                              mulInt (coe v4)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominatorℕ_18
                                                 (coe v0)))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v6)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe v2)))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v8)
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe v1)))
                                           (coe v12))))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties._.l₁
d_l'8321'_1890 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_l'8321'_1890 ~v0 ~v1 ~v2 v3 ~v4 v5 v6 ~v7
  = du_l'8321'_1890 v3 v5 v6
du_l'8321'_1890 :: Integer -> Integer -> Integer -> Integer
du_l'8321'_1890 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v1) (coe v2)))
-- Data.Rational.Unnormalised.Properties._.l₂
d_l'8322'_1892 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> Integer
d_l'8322'_1892 ~v0 ~v1 ~v2 v3 v4 v5 ~v6 ~v7
  = du_l'8322'_1892 v3 v4 v5
du_l'8322'_1892 :: Integer -> Integer -> Integer -> Integer
du_l'8322'_1892 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v2)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
-- Data.Rational.Unnormalised.Properties.*-monoʳ-≤-nonNeg
d_'42''45'mono'691''45''8804''45'nonNeg_1904 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'691''45''8804''45'nonNeg_1904 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''8804''45'nonNeg_1904 v0 v2 v3
du_'42''45'mono'691''45''8804''45'nonNeg_1904 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'691''45''8804''45'nonNeg_1904 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''8804''45'nonNeg_1874 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Unnormalised.Properties.*-mono-≤-nonNeg
d_'42''45'mono'45''8804''45'nonNeg_1932 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'45''8804''45'nonNeg_1932 v0 v1 v2 v3 ~v4 ~v5 v6 v7
  = du_'42''45'mono'45''8804''45'nonNeg_1932 v0 v1 v2 v3 v6 v7
du_'42''45'mono'45''8804''45'nonNeg_1932 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'45''8804''45'nonNeg_1932 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v6 v7 v8 -> coe du_'60''8658''8804'_366 v8)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
         (coe v2))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_322)
         (coe d_'8804''45''60''45'trans_402)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
            (coe v2))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
            (coe v2))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
            (coe v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
            (coe d_'8804''45'isPreorder_322)
            (coe d_'8804''45''60''45'trans_402)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
               (coe v2))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
               (coe v3))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
               (coe v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_322)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                  (coe v3)))
            (coe du_'42''45'mono'691''45''8804''45'nonNeg_1904 v1 v2 v3 v5))
         (coe
            du_'42''45'mono'737''45''8804''45'nonNeg_1874 (coe v2) (coe v0)
            (coe v1) (coe v4)))
-- Data.Rational.Unnormalised.Properties.*-monoˡ-≤-nonPos
d_'42''45'mono'737''45''8804''45'nonPos_1956 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'737''45''8804''45'nonPos_1956 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''8804''45'nonPos_1956 v0 v2 v3 v4
du_'42''45'mono'737''45''8804''45'nonPos_1956 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'737''45''8804''45'nonPos_1956 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v4 v5 v6 -> coe du_'60''8658''8804'_366 v6)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5)
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
              (coe v4) (coe v0))
         v1 v2)
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
              (coe v4) (coe v0))
         (\ v4 v5 -> v4) v1 v2)
      (coe
         d_step'45''8771''728'_658
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
            (coe v2) (coe v0))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                  (coe v0))))
         (coe
            MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
            (\ v4 ->
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                 (coe v4) (coe v0))
            (\ v4 v5 -> v4) v1 v2)
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe v0))))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                     (coe v0))))
            (coe
               MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
               (\ v4 ->
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                    (coe v4) (coe v0))
               (\ v4 v5 -> v4) v1 v2)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
               (coe d_'8804''45'isPreorder_322)
               (coe d_'8804''45''60''45'trans_402)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe v0))))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe v0))))
               (coe
                  MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                  (\ v4 ->
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                       (coe v4) (coe v0))
                  (\ v4 v5 -> v4) v1 v2)
               (coe
                  d_step'45''8771''728'_658
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                           (coe v0))))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                           (coe v0))))
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (\ v4 ->
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                          (coe v4) (coe v0))
                     (\ v4 v5 -> v4) v1 v2)
                  (coe
                     d_step'45''8771'_656
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                              (coe v0))))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe v1) (coe v0))
                     (coe
                        MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                        (\ v4 ->
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                             (coe v4) (coe v0))
                        (\ v4 v5 -> v4) v1 v2)
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                        (coe d_'8804''45'isPreorder_322)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                           (coe v0)))
                     (coe du_neg'45'involutive_180))
                  (d_'45''8255'cong_188
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                           (coe v0)))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0)))
                     (coe d_neg'45'distrib'691''45''42'_1678 (coe v1) (coe v0))))
               (coe
                  d_neg'45'mono'45''8804'_1338
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (\ v4 ->
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                          (coe v4)
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0)))
                     (\ v4 v5 -> v4) v1 v2)
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v4 v5 -> v5)
                     (\ v4 ->
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                          (coe v4)
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0)))
                     v1 v2)
                  (coe
                     du_'42''45'mono'737''45''8804''45'nonNeg_1874
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                     (coe v1) (coe v2) (coe v3))))
            (d_'45''8255'cong_188
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0)))
               (coe d_neg'45'distrib'691''45''42'_1678 (coe v2) (coe v0))))
         (coe du_neg'45'involutive_180))
-- Data.Rational.Unnormalised.Properties._.-r≥0
d_'45'r'8805'0_1970 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_'45'r'8805'0_1970 v0 ~v1 ~v2 ~v3 ~v4 = du_'45'r'8805'0_1970 v0
du_'45'r'8805'0_1970 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_'45'r'8805'0_1970 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_nonNegative_186
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
         (\ v1 v2 -> v1) v0
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
      (coe
         d_neg'45'mono'45''8804'_1338 (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe du_nonPositive'8315''185'_704 (coe v0)))
-- Data.Rational.Unnormalised.Properties.*-monoʳ-≤-nonPos
d_'42''45'mono'691''45''8804''45'nonPos_1978 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'691''45''8804''45'nonPos_1978 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''8804''45'nonPos_1978 v0 v2 v3
du_'42''45'mono'691''45''8804''45'nonPos_1978 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'691''45''8804''45'nonPos_1978 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''8804''45'nonPos_1956 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Unnormalised.Properties.*-monoˡ-<-pos
d_'42''45'mono'737''45''60''45'pos_2000 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'mono'737''45''60''45'pos_2000 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''60''45'pos_2000 v0 v2 v3 v4
du_'42''45'mono'737''45''60''45'pos_2000 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'mono'737''45''60''45'pos_2000 v0 v1 v2 v3
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v6 v7
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v8 v9
                      -> case coe v3 of
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v12
                             -> coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                                  (coe
                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                                        (\ v13 v14 v15 v16 v17 ->
                                           coe
                                             MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770
                                             v16 v17)
                                        (coe
                                           MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                                        (\ v13 v14 v15 v16 v17 ->
                                           coe
                                             MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                             v16 v17)
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2)))
                                              (coe v4))
                                           (coe
                                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                              (coe v0)))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1)))
                                              (coe v4))
                                           (coe
                                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                              (coe v0)))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v8)
                                              (coe v4))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe v1))
                                              (coe
                                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                 (coe v0))))
                                        (coe
                                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                           (coe
                                              MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8) (coe v4))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0)))))
                                        (coe
                                           MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''60''45'pos_6006
                                           (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                              (coe v0))
                                           (coe
                                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                              (\ v13 ->
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                   (coe v13) (coe v4))
                                              (\ v13 v14 -> v13)
                                              (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2)))
                                              (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))))
                                           (coe
                                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                              (\ v13 v14 -> v14)
                                              (\ v13 ->
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                   (coe v13) (coe v4))
                                              (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2)))
                                              (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''60''45'pos_6006
                                              v4
                                              (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2)))
                                              (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1)))
                                              v12))))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-monoʳ-<-pos
d_'42''45'mono'691''45''60''45'pos_2020 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'mono'691''45''60''45'pos_2020 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''60''45'pos_2020 v0 v2 v3
du_'42''45'mono'691''45''60''45'pos_2020 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'mono'691''45''60''45'pos_2020 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''60''45'pos_2000 (coe v0) (coe v1) (coe v2)
-- Data.Rational.Unnormalised.Properties.*-mono-<-nonNeg
d_'42''45'mono'45''60''45'nonNeg_2048 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'mono'45''60''45'nonNeg_2048 v0 v1 v2 v3 ~v4 ~v5 v6 v7
  = du_'42''45'mono'45''60''45'nonNeg_2048 v0 v1 v2 v3 v6 v7
du_'42''45'mono'45''60''45'nonNeg_2048 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'mono'45''60''45'nonNeg_2048 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_322)
         (coe d_'8804''45''60''45'trans_402)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
            (coe v2))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
            (coe v2))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
            (coe v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
            (coe d_'60''45'trans_470) (coe d_'60''45'resp'45''8771'_572)
            (coe d_'60''45''8804''45'trans_436)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
               (coe v2))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
               (coe v3))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
               (coe v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_322)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                  (coe v3)))
            (coe du_'42''45'mono'691''45''60''45'pos_2020 v1 v2 v3 v5))
         (coe
            du_'42''45'mono'737''45''8804''45'nonNeg_1874 (coe v2) (coe v0)
            (coe v1) (coe du_'60''8658''8804'_366 (coe v4))))
-- Data.Rational.Unnormalised.Properties.*-cancelʳ-<-nonNeg
d_'42''45'cancel'691''45''60''45'nonNeg_2070 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'cancel'691''45''60''45'nonNeg_2070 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'691''45''60''45'nonNeg_2070 v0 v1 v2 v4
du_'42''45'cancel'691''45''60''45'nonNeg_2070 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'cancel'691''45''60''45'nonNeg_2070 v0 v1 v2 v3
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v6 v7
               -> case coe v2 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v8 v9
                      -> case coe v3 of
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v12
                             -> coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                                  (coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'42''45'cancel'691''45''60''45'nonNeg_6064
                                     (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                        (coe v4)
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe v1)))
                                     (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                        (coe v6)
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe v0)))
                                     (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                        (coe v8)
                                        (coe
                                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                           (coe v2)))
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                                        (coe
                                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                                           (\ v13 v14 v15 v16 v17 ->
                                              coe
                                                MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770
                                                v16 v17)
                                           (coe
                                              MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                                           (\ v13 v14 v15 v16 v17 ->
                                              coe
                                                MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                                v16 v17)
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v4) (coe v8))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v1))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6) (coe v8))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0))
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))))
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v6)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v0)))
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                    (coe v2))))
                                           (coe
                                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                              (coe
                                                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                 (coe
                                                    MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                    (coe v6)
                                                    (coe
                                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                       (coe v0)))
                                                 (coe
                                                    MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                                    (coe v8)
                                                    (coe
                                                       MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                                       (coe v2)))))
                                           (coe v12))))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-cancelˡ-<-nonNeg
d_'42''45'cancel'737''45''60''45'nonNeg_2088 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'cancel'737''45''60''45'nonNeg_2088 v0 v1 v2 ~v3
  = du_'42''45'cancel'737''45''60''45'nonNeg_2088 v0 v1 v2
du_'42''45'cancel'737''45''60''45'nonNeg_2088 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'cancel'737''45''60''45'nonNeg_2088 v0 v1 v2
  = coe
      du_'42''45'cancel'691''45''60''45'nonNeg_2070 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Unnormalised.Properties.*-monoˡ-<-neg
d_'42''45'mono'737''45''60''45'neg_2110 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'mono'737''45''60''45'neg_2110 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''60''45'neg_2110 v0 v2 v3 v4
du_'42''45'mono'737''45''60''45'neg_2110 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'mono'737''45''60''45'neg_2110 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         d_step'45''8771''728'_658
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
            (coe v2) (coe v0))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                  (coe v0))))
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
            (coe v1) (coe v0))
         (coe
            d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe v0))))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                     (coe v0))))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe v1) (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
               (coe d_'60''45'trans_470) (coe d_'60''45'resp'45''8771'_572)
               (coe d_'60''45''8804''45'trans_436)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe v0))))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe v0))))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                  (coe v0))
               (coe
                  d_step'45''8771''728'_658
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                           (coe v0))))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                           (coe v0))))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe v1) (coe v0))
                  (coe
                     d_step'45''8771'_656
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                              (coe v0))))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe v1) (coe v0))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe v1) (coe v0))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                        (coe d_'8804''45'isPreorder_322)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                           (coe v0)))
                     (coe du_neg'45'involutive_180))
                  (d_'45''8255'cong_188
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                           (coe v0)))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0)))
                     (coe d_neg'45'distrib'691''45''42'_1678 (coe v1) (coe v0))))
               (coe
                  d_neg'45'mono'45''60'_210
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (\ v4 ->
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                          (coe v4)
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0)))
                     (\ v4 v5 -> v4) v1 v2)
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v4 v5 -> v5)
                     (\ v4 ->
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                          (coe v4)
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0)))
                     v1 v2)
                  (coe
                     du_'42''45'mono'737''45''60''45'pos_2000
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0))
                     (coe v1) (coe v2) (coe v3))))
            (d_'45''8255'cong_188
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v0)))
               (coe d_neg'45'distrib'691''45''42'_1678 (coe v2) (coe v0))))
         (coe du_neg'45'involutive_180))
-- Data.Rational.Unnormalised.Properties._.-r>0
d_'45'r'62'0_2124 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_'45'r'62'0_2124 v0 ~v1 ~v2 ~v3 ~v4 = du_'45'r'62'0_2124 v0
du_'45'r'62'0_2124 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_'45'r'62'0_2124 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_positive_162
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
         (\ v1 v2 -> v1) v0
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
      (coe
         d_neg'45'mono'45''60'_210 (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe du_negative'8315''185'_698 (coe v0)))
-- Data.Rational.Unnormalised.Properties.*-monoʳ-<-neg
d_'42''45'mono'691''45''60''45'neg_2132 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'mono'691''45''60''45'neg_2132 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''60''45'neg_2132 v0 v2 v3
du_'42''45'mono'691''45''60''45'neg_2132 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'mono'691''45''60''45'neg_2132 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''60''45'neg_2110 (coe v0) (coe v1) (coe v2)
-- Data.Rational.Unnormalised.Properties.*-cancelˡ-<-nonPos
d_'42''45'cancel'737''45''60''45'nonPos_2152 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'cancel'737''45''60''45'nonPos_2152 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'737''45''60''45'nonPos_2152 v0 v1 v2 v4
du_'42''45'cancel'737''45''60''45'nonPos_2152 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'cancel'737''45''60''45'nonPos_2152 v0 v1 v2 v3
  = coe
      du_'42''45'cancel'737''45''60''45'nonNeg_2088 v1 v0
      (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
         (coe
            d_step'45''8771''728'_658
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
               (coe v1))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                  (coe v1)))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
               (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
               (coe d_'60''45'trans_470) (coe d_'60''45'resp'45''8771'_572)
               (coe d_'60''45''8804''45'trans_436)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe v1)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
                  (coe v0))
               (coe
                  d_step'45''8771'_656
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                        (coe v0)))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
                     (coe v0))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
                     (coe v0))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190 (coe v2))
                        (coe v0)))
                  (d_neg'45'distrib'737''45''42'_1666 (coe v2) (coe v0)))
               (coe
                  d_neg'45'mono'45''60'_210
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe v0))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v2)
                     (coe v1))
                  (coe v3)))
            (d_neg'45'distrib'737''45''42'_1666 (coe v2) (coe v1))))
-- Data.Rational.Unnormalised.Properties._.-r≥0
d_'45'r'8805'0_2166 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_'45'r'8805'0_2166 ~v0 ~v1 v2 ~v3 ~v4 = du_'45'r'8805'0_2166 v2
du_'45'r'8805'0_2166 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_'45'r'8805'0_2166 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_nonNegative_186
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
         (\ v1 v2 -> v1) v0
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
      (coe
         d_neg'45'mono'45''8804'_1338 (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe du_nonPositive'8315''185'_704 (coe v0)))
-- Data.Rational.Unnormalised.Properties.*-cancelʳ-<-nonPos
d_'42''45'cancel'691''45''60''45'nonPos_2172 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'cancel'691''45''60''45'nonPos_2172 v0 v1 v2 ~v3
  = du_'42''45'cancel'691''45''60''45'nonPos_2172 v0 v1 v2
du_'42''45'cancel'691''45''60''45'nonPos_2172 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'cancel'691''45''60''45'nonPos_2172 v0 v1 v2
  = coe
      du_'42''45'cancel'737''45''60''45'nonPos_2152 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Unnormalised.Properties.pos*pos⇒pos
d_pos'42'pos'8658'pos_2196 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_pos'42'pos'8658'pos_2196 v0 ~v1 v2 ~v3
  = du_pos'42'pos'8658'pos_2196 v0 v2
du_pos'42'pos'8658'pos_2196 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_pos'42'pos'8658'pos_2196 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_positive_162
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
         (coe v1))
      (coe
         du_'42''45'mono'45''60''45'nonNeg_2048
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v1) (coe du_positive'8315''185'_686 (coe v0))
         (coe du_positive'8315''185'_686 (coe v1)))
-- Data.Rational.Unnormalised.Properties.nonNeg*nonNeg⇒nonNeg
d_nonNeg'42'nonNeg'8658'nonNeg_2210 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_nonNeg'42'nonNeg'8658'nonNeg_2210 v0 ~v1 v2 ~v3
  = du_nonNeg'42'nonNeg'8658'nonNeg_2210 v0 v2
du_nonNeg'42'nonNeg'8658'nonNeg_2210 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_nonNeg'42'nonNeg'8658'nonNeg_2210 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_nonNegative_186
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0)
         (coe v1))
      (coe
         du_'42''45'mono'45''8804''45'nonNeg_1932
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v1) (coe du_nonNegative'8315''185'_692 (coe v0))
         (coe du_nonNegative'8315''185'_692 (coe v1)))
-- Data.Rational.Unnormalised.Properties.*-isMagma
d_'42''45'isMagma_2216 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_2216
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe d_'8771''45'isEquivalence_164) (coe d_'42''45'cong_1454)
-- Data.Rational.Unnormalised.Properties.*-isSemigroup
d_'42''45'isSemigroup_2218 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2218
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'42''45'isMagma_2216)
      (\ v0 v1 v2 -> coe du_'42''45'assoc_1524)
-- Data.Rational.Unnormalised.Properties.*-1-isMonoid
d_'42''45'1'45'isMonoid_2220 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'1'45'isMonoid_2220
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'42''45'isSemigroup_2218) (coe d_'42''45'identity_1572)
-- Data.Rational.Unnormalised.Properties.*-1-isCommutativeMonoid
d_'42''45'1'45'isCommutativeMonoid_2222 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'1'45'isCommutativeMonoid_2222
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'42''45'1'45'isMonoid_2220)
      (\ v0 v1 -> coe du_'42''45'comm_1550)
-- Data.Rational.Unnormalised.Properties.+-*-isRing
d_'43''45''42''45'isRing_2224 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_'43''45''42''45'isRing_2224
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsRing'46'constructor_80853
      (coe d_'43''45'0'45'isAbelianGroup_1440) (coe d_'42''45'cong_1454)
      (\ v0 v1 v2 -> coe du_'42''45'assoc_1524)
      (coe d_'42''45'identity_1572) (coe d_'42''45'distrib'45''43'_1660)
      (coe d_'42''45'zero_1618)
-- Data.Rational.Unnormalised.Properties.+-*-isCommutativeRing
d_'43''45''42''45'isCommutativeRing_2226 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
d_'43''45''42''45'isCommutativeRing_2226
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeRing'46'constructor_87819
      (coe d_'43''45''42''45'isRing_2224)
      (\ v0 v1 -> coe du_'42''45'comm_1550)
-- Data.Rational.Unnormalised.Properties.*-magma
d_'42''45'magma_2228 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'42''45'magma_2228
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
      d_'42''45'isMagma_2216
-- Data.Rational.Unnormalised.Properties.*-semigroup
d_'42''45'semigroup_2230 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'42''45'semigroup_2230
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
      d_'42''45'isSemigroup_2218
-- Data.Rational.Unnormalised.Properties.*-1-monoid
d_'42''45'1'45'monoid_2232 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'1'45'monoid_2232
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110
      d_'42''45'1'45'isMonoid_2220
-- Data.Rational.Unnormalised.Properties.*-1-commutativeMonoid
d_'42''45'1'45'commutativeMonoid_2234 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'1'45'commutativeMonoid_2234
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110
      d_'42''45'1'45'isCommutativeMonoid_2222
-- Data.Rational.Unnormalised.Properties.+-*-ring
d_'43''45''42''45'ring_2236 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432
d_'43''45''42''45'ring_2236
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Ring'46'constructor_60565
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110
      d_'43''45''42''45'isRing_2224
-- Data.Rational.Unnormalised.Properties.+-*-commutativeRing
d_'43''45''42''45'commutativeRing_2238 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634
d_'43''45''42''45'commutativeRing_2238
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeRing'46'constructor_64147
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110
      d_'43''45''42''45'isCommutativeRing_2226
-- Data.Rational.Unnormalised.Properties.p>1⇒p≢0
d_p'62'1'8658'p'8802'0_2240 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_p'62'1'8658'p'8802'0_2240 v0 ~v1
  = du_p'62'1'8658'p'8802'0_2240 v0
du_p'62'1'8658'p'8802'0_2240 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_p'62'1'8658'p'8802'0_2240 v0
  = coe du_pos'8658'nonZero_732 (coe v0)
-- Data.Rational.Unnormalised.Properties.1/nonZero⇒nonZero
d_1'47'nonZero'8658'nonZero_2250 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_1'47'nonZero'8658'nonZero_2250 v0 ~v1
  = du_1'47'nonZero'8658'nonZero_2250 v0
du_1'47'nonZero'8658'nonZero_2250 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_1'47'nonZero'8658'nonZero_2250 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.1/-involutive-≡
d_1'47''45'involutive'45''8801'_2256 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_1'47''45'involutive'45''8801'_2256 = erased
-- Data.Rational.Unnormalised.Properties.1/-involutive
d_1'47''45'involutive_2270 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_1'47''45'involutive_2270 ~v0 ~v1 = du_1'47''45'involutive_2270
du_1'47''45'involutive_2270 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_1'47''45'involutive_2270 = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.1/pos⇒pos
d_1'47'pos'8658'pos_2278 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_1'47'pos'8658'pos_2278 v0 ~v1 = du_1'47'pos'8658'pos_2278 v0
du_1'47'pos'8658'pos_2278 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_1'47'pos'8658'pos_2278 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Integer.Base.C_Positive'46'constructor_1295
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Unnormalised.Properties.1/neg⇒neg
d_1'47'neg'8658'neg_2288 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164
d_1'47'neg'8658'neg_2288 v0 ~v1 = du_1'47'neg'8658'neg_2288 v0
du_1'47'neg'8658'neg_2288 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164
du_1'47'neg'8658'neg_2288 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Integer.Base.C_Negative'46'constructor_1469
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Unnormalised.Properties.p>1⇒1/p<1
d_p'62'1'8658'1'47'p'60'1_2298 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_p'62'1'8658'1'47'p'60'1_2298 v0 v1
  = coe du_lemma'8242'_2312 (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties._.lemma′
d_lemma'8242'_2312 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_lemma'8242'_2312 ~v0 ~v1 v2 ~v3 v4 = du_lemma'8242'_2312 v2 v4
du_lemma'8242'_2312 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_lemma'8242'_2312 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v6
               -> coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                          (\ v7 v8 v9 v10 v11 ->
                             coe
                               MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770 v10 v11)
                          (coe
                             MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                          (\ v7 v8 v9 v10 v11 ->
                             coe
                               MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                               v10 v11)
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'42'__308
                             (coe MAlonzo.Code.Data.Integer.Base.d_1ℤ_16)
                             (coe addInt (coe (1 :: Integer)) (coe v3)))
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v2)
                             (coe MAlonzo.Code.Data.Integer.Base.d_1ℤ_16))
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'42'__308
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110))
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
                                   (coe v0))))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
                                      (coe v0)))))
                          (coe v6)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.1/-antimono-≤-pos
d_1'47''45'antimono'45''8804''45'pos_2332 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_1'47''45'antimono'45''8804''45'pos_2332 v0 v1 ~v2 ~v3 v4
  = du_1'47''45'antimono'45''8804''45'pos_2332 v0 v1 v4
du_1'47''45'antimono'45''8804''45'pos_2332 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_1'47''45'antimono'45''8804''45'pos_2332 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v3 v4 v5 -> coe du_'60''8658''8804'_366 v5)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
         (coe v1))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
         (coe v0))
      (coe
         d_step'45''8771''728'_658 (coe du_1'47'q_2350 v1 erased)
         (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
            (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110)
            (coe du_1'47'q_2350 v1 erased))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
            (coe v0))
         (coe
            d_step'45''8771''728'_658
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110)
               (coe du_1'47'q_2350 v1 erased))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe du_1'47'p_2348 v0 erased) (coe v0))
               (coe du_1'47'q_2350 v1 erased))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
               (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
               (coe d_'8804''45'isPreorder_322)
               (coe d_'8804''45''60''45'trans_402)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe du_1'47'p_2348 v0 erased) (coe v0))
                  (coe du_1'47'q_2350 v1 erased))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe du_1'47'p_2348 v0 erased) (coe v1))
                  (coe du_1'47'q_2350 v1 erased))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
                  (coe v0))
               (coe
                  d_step'45''8771'_656
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe du_1'47'p_2348 v0 erased) (coe v1))
                     (coe du_1'47'q_2350 v1 erased))
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe du_1'47'p_2348 v0 erased)
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                        (coe du_1'47'q_2350 v1 erased)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
                     (coe v0))
                  (coe
                     d_step'45''8771'_656
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe du_1'47'p_2348 v0 erased)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                           (coe du_1'47'q_2350 v1 erased)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe du_1'47'p_2348 v0 erased)
                        (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
                        (coe v0))
                     (coe
                        d_step'45''8771'_656
                        (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                           (coe du_1'47'p_2348 v0 erased)
                           (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110))
                        (coe du_1'47'p_2348 v0 erased)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
                           (coe v0))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                           (coe d_'8804''45'isPreorder_322) (coe du_1'47'p_2348 v0 erased))
                        (coe du_'42''45'identity'691'_1568))
                     (d_'42''45'cong'737'_1488
                        (coe du_1'47'p_2348 v0 erased)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v1)
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
                              (coe v1)))
                        (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110)
                        (coe du_'42''45'inverse'691'_1608 (coe v1))))
                  (coe du_'42''45'assoc_1524))
               (coe
                  du_'42''45'mono'737''45''8804''45'nonNeg_1874
                  (coe du_1'47'q_2350 v1 erased)
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe du_1'47'p_2348 v0 erased))
                     (\ v3 v4 -> v3) v0 v1)
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v3 v4 -> v4)
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe du_1'47'p_2348 v0 erased))
                     v0 v1)
                  (coe
                     du_'42''45'mono'691''45''8804''45'nonNeg_1904
                     (coe du_1'47'p_2348 v0 erased) v0 v1 v2)))
            (d_'42''45'cong'691'_1494
               (coe du_1'47'q_2350 v1 erased)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218
                     (coe v0))
                  (coe v0))
               (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_1ℚ'7512'_110)
               (coe du_'42''45'inverse'737'_1578 (coe v0))))
         (coe du_'42''45'identity'737'_1564))
-- Data.Rational.Unnormalised.Properties._.1/p
d_1'47'p_2348 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8
d_1'47'p_2348 v0 ~v1 ~v2 ~v3 ~v4 = du_1'47'p_2348 v0
du_1'47'p_2348 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8
du_1'47'p_2348 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218 (coe v0)
-- Data.Rational.Unnormalised.Properties._.1/q
d_1'47'q_2350 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8
d_1'47'q_2350 ~v0 v1 ~v2 ~v3 ~v4 = du_1'47'q_2350 v1
du_1'47'q_2350 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8
du_1'47'q_2350 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.du_1'47'__218 (coe v0)
-- Data.Rational.Unnormalised.Properties.p≤q⇒p⊔q≃q
d_p'8804'q'8658'p'8852'q'8771'q_2356 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_p'8804'q'8658'p'8852'q'8771'q_2356 v0 v1 ~v2
  = du_p'8804'q'8658'p'8852'q'8771'q_2356 v0 v1
du_p'8804'q'8658'p'8852'q'8771'q_2356 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_p'8804'q'8658'p'8852'q'8771'q_2356 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (let v2
                = MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8804''7495'__90
                    (coe v0) (coe v1) in
          if coe v2
            then coe du_'8771''45'refl_130
            else coe
                   MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38))
-- Data.Rational.Unnormalised.Properties.p≥q⇒p⊔q≃p
d_p'8805'q'8658'p'8852'q'8771'p_2384 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_p'8805'q'8658'p'8852'q'8771'p_2384 v0 v1 v2
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (let v3
                = MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8804''7495'__90
                    (coe v0) (coe v1) in
          if coe v3
            then coe
                   du_'8804''45'antisym_284 (coe v2)
                   (coe
                      du_'8804''7495''8658''8804'_358 (coe v0)
                      (coe
                         MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v3) (coe v1)
                         (coe v0)))
            else coe du_'8771''45'refl_130))
-- Data.Rational.Unnormalised.Properties.p≤q⇒p⊓q≃p
d_p'8804'q'8658'p'8851'q'8771'p_2414 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_p'8804'q'8658'p'8851'q'8771'p_2414 v0 v1 ~v2
  = du_p'8804'q'8658'p'8851'q'8771'p_2414 v0 v1
du_p'8804'q'8658'p'8851'q'8771'p_2414 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_p'8804'q'8658'p'8851'q'8771'p_2414 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (let v2
                = MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8804''7495'__90
                    (coe v0) (coe v1) in
          if coe v2
            then coe du_'8771''45'refl_130
            else coe
                   MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38))
-- Data.Rational.Unnormalised.Properties.p≥q⇒p⊓q≃q
d_p'8805'q'8658'p'8851'q'8771'q_2442 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_p'8805'q'8658'p'8851'q'8771'q_2442 v0 v1 v2
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (let v3
                = MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8804''7495'__90
                    (coe v0) (coe v1) in
          if coe v3
            then coe
                   du_'8804''45'antisym_284
                   (coe
                      du_'8804''7495''8658''8804'_358
                      (coe
                         MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v3) (coe v0)
                         (coe v1))
                      (coe v1))
                   (coe v2)
            else coe du_'8771''45'refl_130))
-- Data.Rational.Unnormalised.Properties.⊓-operator
d_'8851''45'operator_2472 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84
d_'8851''45'operator_2472
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MinOperator'46'constructor_973
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8851'__254)
      (\ v0 v1 v2 -> coe du_p'8804'q'8658'p'8851'q'8771'p_2414 v0 v1)
      (coe d_p'8805'q'8658'p'8851'q'8771'q_2442)
-- Data.Rational.Unnormalised.Properties.⊔-operator
d_'8852''45'operator_2474 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MaxOperator_114
d_'8852''45'operator_2474
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MaxOperator'46'constructor_1501
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8852'__244)
      (\ v0 v1 v2 -> coe du_p'8804'q'8658'p'8852'q'8771'q_2356 v0 v1)
      (coe d_p'8805'q'8658'p'8852'q'8771'p_2384)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.antimono-≤-distrib-⊓
d_antimono'45''8804''45'distrib'45''8851'_2478 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_antimono'45''8804''45'distrib'45''8851'_2478
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_antimono'45''8804''45'distrib'45''8851'_2906
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.antimono-≤-distrib-⊔
d_antimono'45''8804''45'distrib'45''8852'_2480 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_antimono'45''8804''45'distrib'45''8852'_2480
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_antimono'45''8804''45'distrib'45''8852'_2952
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_2482 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_mono'45''8804''45'distrib'45''8851'_2482
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_mono'45''8804''45'distrib'45''8851'_2862
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_2484 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_mono'45''8804''45'distrib'45''8852'_2484
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MaxOp.du_mono'45''8804''45'distrib'45''8852'_168
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≤x
d_x'8851'y'8804'x_2486 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8804'x_2486
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_2488 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8804'y'8658'x'8851'z'8804'y_2488
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_2490 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8804'y'8658'z'8851'x'8804'y_2490
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_2492 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8804'y'8658'x'8851'z'8804'y_2492
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_2494 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8804'y'8658'z'8851'x'8804'y_2494
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_2496 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8804'y'8851'z'8658'x'8804'y_2496
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_2498 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8804'y'8851'z'8658'x'8804'z_2498
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≤y
d_x'8851'y'8804'y_2500 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8804'y_2500
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_2502 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8776'x'8658'x'8804'y_2502
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_2504 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8776'y'8658'y'8804'x_2504
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≤x
d_x'8851'y'8804'x_2506 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8804'x_2506
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≤x⊔y
d_x'8851'y'8804'x'8852'y_2508 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8804'x'8852'y_2508
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_x'8851'y'8804'x'8852'y_2996
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≤y
d_x'8851'y'8804'y_2510 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8804'y_2510
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_2512 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8776'x'8658'x'8804'y_2512
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_2514 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8851'y'8776'y'8658'y'8804'x_2514
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_2516 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8804'y'8851'z'8658'x'8804'y_2516
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_2518 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_x'8804'y'8851'z'8658'x'8804'z_2518
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-absorbs-⊔
d_'8851''45'absorbs'45''8852'_2520 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'absorbs'45''8852'_2520
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45'absorbs'45''8852'_2850
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-assoc
d_'8851''45'assoc_2522 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'assoc_2522
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'assoc_2692
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-band
d_'8851''45'band_2524 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_2524
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-comm
d_'8851''45'comm_2526 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'comm_2526
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'comm_2604
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_2528 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_2528
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-cong
d_'8851''45'cong_2530 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'cong_2530
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong_2678
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-congʳ
d_'8851''45'cong'691'_2532 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'cong'691'_2532
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong'691'_2668
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-congˡ
d_'8851''45'cong'737'_2534 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'cong'737'_2534
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong'737'_2630
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-distrib-⊔
d_'8851''45'distrib'45''8852'_2536 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'distrib'45''8852'_2536
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45'distrib'45''8852'_2816
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-distribʳ-⊔
d_'8851''45'distrib'691''45''8852'_2538 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'distrib'691''45''8852'_2538
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45'distrib'691''45''8852'_2814
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-distribˡ-⊔
d_'8851''45'distrib'737''45''8852'_2540 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'distrib'737''45''8852'_2540
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45'distrib'737''45''8852'_2786
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-glb
d_'8851''45'glb_2542 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8851''45'glb_2542
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-idem
d_'8851''45'idem_2544 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'idem_2544
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'idem_2732
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isBand
d_'8851''45'isBand_2552 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_2552
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_2554 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_2554
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isMagma
d_'8851''45'isMagma_2556 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_2556
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_2560 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_2560
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isSemigroup
d_'8851''45'isSemigroup_2562 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_2562
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-magma
d_'8851''45'magma_2564 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_2564
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-mono-≤
d_'8851''45'mono'45''8804'_2566 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8851''45'mono'45''8804'_2566
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_2570 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8851''45'mono'691''45''8804'_2570
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_2572 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8851''45'mono'737''45''8804'_2572
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-sel
d_'8851''45'sel_2576 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_2576
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-selectiveMagma
d_'8851''45'selectiveMagma_2578 ::
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_2578
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-semigroup
d_'8851''45'semigroup_2580 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_2580
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-triangulate
d_'8851''45'triangulate_2582 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'triangulate_2582
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'triangulate_3040
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-⊔-absorptive
d_'8851''45''8852''45'absorptive_2590 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45''8852''45'absorptive_2590
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'absorptive_2896
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊔-absorbs-⊓
d_'8852''45'absorbs'45''8851'_2592 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8852''45'absorbs'45''8851'_2592
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45'absorbs'45''8851'_2872
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-assoc
d_'8851''45'assoc_2594 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'assoc_2594
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'assoc_2692
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-band
d_'8851''45'band_2596 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_2596
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-comm
d_'8851''45'comm_2598 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'comm_2598
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'comm_2604
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_2600 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_2600
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-cong
d_'8851''45'cong_2602 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'cong_2602
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong_2678
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-congʳ
d_'8851''45'cong'691'_2604 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'cong'691'_2604
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong'691'_2668
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-congˡ
d_'8851''45'cong'737'_2606 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'cong'737'_2606
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong'737'_2630
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊔-distrib-⊓
d_'8852''45'distrib'45''8851'_2608 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45'distrib'45''8851'_2608
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45'distrib'45''8851'_2848
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊔-distribʳ-⊓
d_'8852''45'distrib'691''45''8851'_2610 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8852''45'distrib'691''45''8851'_2610
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45'distrib'691''45''8851'_2846
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊔-distribˡ-⊓
d_'8852''45'distrib'737''45''8851'_2612 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8852''45'distrib'737''45''8851'_2612
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45'distrib'737''45''8851'_2818
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-idem
d_'8851''45'idem_2614 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'idem_2614
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'idem_2732
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isBand
d_'8851''45'isBand_2622 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_2622
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_2624 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_2624
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isMagma
d_'8851''45'isMagma_2626 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_2626
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_2630 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_2630
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-isSemigroup
d_'8851''45'isSemigroup_2632 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_2632
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-glb
d_'8851''45'glb_2634 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8851''45'glb_2634
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-magma
d_'8851''45'magma_2636 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_2636
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-mono-≤
d_'8851''45'mono'45''8804'_2638 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8851''45'mono'45''8804'_2638
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_2642 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8851''45'mono'691''45''8804'_2642
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_2644 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8851''45'mono'737''45''8804'_2644
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-sel
d_'8851''45'sel_2646 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_2646
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-selectiveMagma
d_'8851''45'selectiveMagma_2648 ::
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_2648
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-semigroup
d_'8851''45'semigroup_2650 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_2650
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊓-triangulate
d_'8851''45'triangulate_2652 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8851''45'triangulate_2652
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'triangulate_3040
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-properties.⊔-⊓-absorptive
d_'8852''45''8851''45'absorptive_2660 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45''8851''45'absorptive_2660
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'absorptive_2894
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊓-isSemilattice
d_'8851''45'isSemilattice_2664 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8851''45'isSemilattice_2664
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'isSemilattice_586
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊓-semilattice
d_'8851''45'semilattice_2666 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8851''45'semilattice_2666
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8851''45'operator_2472 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'semilattice_588
      (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊓-⊔-distributiveLattice
d_'8851''45''8852''45'distributiveLattice_2668 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8851''45''8852''45'distributiveLattice_2668
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'distributiveLattice_770
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊓-⊔-isDistributiveLattice
d_'8851''45''8852''45'isDistributiveLattice_2670 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8851''45''8852''45'isDistributiveLattice_2670
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'isDistributiveLattice_760
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊓-⊔-isLattice
d_'8851''45''8852''45'isLattice_2672 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8851''45''8852''45'isLattice_2672
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'isLattice_758
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊓-⊔-lattice
d_'8851''45''8852''45'lattice_2674 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8851''45''8852''45'lattice_2674
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'lattice_766
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊓-isSemilattice
d_'8851''45'isSemilattice_2676 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8851''45'isSemilattice_2676
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'isSemilattice_586
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊓-semilattice
d_'8851''45'semilattice_2678 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8851''45'semilattice_2678
  = let v0 = d_'8804''45'totalPreorder_334 in
    let v1 = d_'8852''45'operator_2474 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'semilattice_588
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊔-⊓-distributiveLattice
d_'8852''45''8851''45'distributiveLattice_2680 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8852''45''8851''45'distributiveLattice_2680
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'distributiveLattice_768
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊔-⊓-isDistributiveLattice
d_'8852''45''8851''45'isDistributiveLattice_2682 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8852''45''8851''45'isDistributiveLattice_2682
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'isDistributiveLattice_762
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊔-⊓-isLattice
d_'8852''45''8851''45'isLattice_2684 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8852''45''8851''45'isLattice_2684
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'isLattice_756
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-⊔-latticeProperties.⊔-⊓-lattice
d_'8852''45''8851''45'lattice_2686 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8852''45''8851''45'lattice_2686
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'lattice_764
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474)
-- Data.Rational.Unnormalised.Properties.⊓-rawMagma
d_'8851''45'rawMagma_2688 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8851''45'rawMagma_2688
  = coe
      MAlonzo.Code.Algebra.Bundles.du_rawMagma_52
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
         (coe d_'8804''45'totalPreorder_334)
         (coe d_'8851''45'operator_2472))
-- Data.Rational.Unnormalised.Properties.⊔-rawMagma
d_'8852''45'rawMagma_2690 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8852''45'rawMagma_2690
  = coe
      MAlonzo.Code.Algebra.Bundles.du_rawMagma_52
      (let v0 = d_'8804''45'totalPreorder_334 in
       let v1 = d_'8852''45'operator_2474 in
       coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
         (coe
            MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
            (coe v1)))
-- Data.Rational.Unnormalised.Properties.⊔-⊓-rawLattice
d_'8852''45''8851''45'rawLattice_2692 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12
d_'8852''45''8851''45'rawLattice_2692
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.du_rawLattice_564
      (coe
         MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'lattice_764
         (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
         (coe d_'8852''45'operator_2474))
-- Data.Rational.Unnormalised.Properties.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_2700 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_mono'45''8804''45'distrib'45''8852'_2700 v0 v1
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MaxOp.du_mono'45''8804''45'distrib'45''8852'_168
      (coe d_'8804''45'totalPreorder_334) (coe d_'8852''45'operator_2474)
      (coe v0) (coe d_mono'8658'cong_352 v0 v1) (coe v1)
-- Data.Rational.Unnormalised.Properties.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_2710 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_mono'45''8804''45'distrib'45''8851'_2710 v0 v1
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_mono'45''8804''45'distrib'45''8851'_2862
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe v0) (coe d_mono'8658'cong_352 v0 v1) (coe v1)
-- Data.Rational.Unnormalised.Properties.antimono-≤-distrib-⊓
d_antimono'45''8804''45'distrib'45''8851'_2720 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_antimono'45''8804''45'distrib'45''8851'_2720 v0 v1
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_antimono'45''8804''45'distrib'45''8851'_2906
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474) (coe v0)
      (coe d_antimono'8658'cong_356 v0 v1) (coe v1)
-- Data.Rational.Unnormalised.Properties.antimono-≤-distrib-⊔
d_antimono'45''8804''45'distrib'45''8852'_2730 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_antimono'45''8804''45'distrib'45''8852'_2730 v0 v1
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_antimono'45''8804''45'distrib'45''8852'_2952
      (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
      (coe d_'8852''45'operator_2474) (coe v0)
      (coe d_antimono'8658'cong_356 v0 v1) (coe v1)
-- Data.Rational.Unnormalised.Properties.neg-distrib-⊔-⊓
d_neg'45'distrib'45''8852''45''8851'_2738 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_neg'45'distrib'45''8852''45''8851'_2738
  = coe
      d_antimono'45''8804''45'distrib'45''8852'_2730
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190)
      (coe d_neg'45'mono'45''8804'_1338)
-- Data.Rational.Unnormalised.Properties.neg-distrib-⊓-⊔
d_neg'45'distrib'45''8851''45''8852'_2744 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_neg'45'distrib'45''8851''45''8852'_2744
  = coe
      d_antimono'45''8804''45'distrib'45''8851'_2720
      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190)
      (coe d_neg'45'mono'45''8804'_1338)
-- Data.Rational.Unnormalised.Properties.*-distribˡ-⊓-nonNeg
d_'42''45'distrib'737''45''8851''45'nonNeg_2754 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'737''45''8851''45'nonNeg_2754 v0 ~v1
  = du_'42''45'distrib'737''45''8851''45'nonNeg_2754 v0
du_'42''45'distrib'737''45''8851''45'nonNeg_2754 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'distrib'737''45''8851''45'nonNeg_2754 v0
  = coe
      d_mono'45''8804''45'distrib'45''8851'_2710
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0))
      (coe du_'42''45'mono'691''45''8804''45'nonNeg_1904 (coe v0))
-- Data.Rational.Unnormalised.Properties.*-distribʳ-⊓-nonNeg
d_'42''45'distrib'691''45''8851''45'nonNeg_2766 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'691''45''8851''45'nonNeg_2766 v0 ~v1
  = du_'42''45'distrib'691''45''8851''45'nonNeg_2766 v0
du_'42''45'distrib'691''45''8851''45'nonNeg_2766 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'distrib'691''45''8851''45'nonNeg_2766 v0
  = coe
      d_mono'45''8804''45'distrib'45''8851'_2710
      (coe
         (\ v1 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
              (coe v1) (coe v0)))
      (coe du_'42''45'mono'737''45''8804''45'nonNeg_1874 (coe v0))
-- Data.Rational.Unnormalised.Properties.*-distribˡ-⊔-nonNeg
d_'42''45'distrib'737''45''8852''45'nonNeg_2778 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'737''45''8852''45'nonNeg_2778 v0 ~v1
  = du_'42''45'distrib'737''45''8852''45'nonNeg_2778 v0
du_'42''45'distrib'737''45''8852''45'nonNeg_2778 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'distrib'737''45''8852''45'nonNeg_2778 v0
  = coe
      d_mono'45''8804''45'distrib'45''8852'_2700
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0))
      (coe du_'42''45'mono'691''45''8804''45'nonNeg_1904 (coe v0))
-- Data.Rational.Unnormalised.Properties.*-distribʳ-⊔-nonNeg
d_'42''45'distrib'691''45''8852''45'nonNeg_2790 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'691''45''8852''45'nonNeg_2790 v0 ~v1
  = du_'42''45'distrib'691''45''8852''45'nonNeg_2790 v0
du_'42''45'distrib'691''45''8852''45'nonNeg_2790 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'distrib'691''45''8852''45'nonNeg_2790 v0
  = coe
      d_mono'45''8804''45'distrib'45''8852'_2700
      (coe
         (\ v1 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
              (coe v1) (coe v0)))
      (coe du_'42''45'mono'737''45''8804''45'nonNeg_1874 (coe v0))
-- Data.Rational.Unnormalised.Properties.*-distribˡ-⊔-nonPos
d_'42''45'distrib'737''45''8852''45'nonPos_2802 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'737''45''8852''45'nonPos_2802 v0 ~v1
  = du_'42''45'distrib'737''45''8852''45'nonPos_2802 v0
du_'42''45'distrib'737''45''8852''45'nonPos_2802 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'distrib'737''45''8852''45'nonPos_2802 v0
  = coe
      d_antimono'45''8804''45'distrib'45''8852'_2730
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0))
      (coe du_'42''45'mono'691''45''8804''45'nonPos_1978 (coe v0))
-- Data.Rational.Unnormalised.Properties.*-distribʳ-⊔-nonPos
d_'42''45'distrib'691''45''8852''45'nonPos_2814 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'691''45''8852''45'nonPos_2814 v0 ~v1
  = du_'42''45'distrib'691''45''8852''45'nonPos_2814 v0
du_'42''45'distrib'691''45''8852''45'nonPos_2814 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'distrib'691''45''8852''45'nonPos_2814 v0
  = coe
      d_antimono'45''8804''45'distrib'45''8852'_2730
      (coe
         (\ v1 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
              (coe v1) (coe v0)))
      (coe du_'42''45'mono'737''45''8804''45'nonPos_1956 (coe v0))
-- Data.Rational.Unnormalised.Properties.*-distribˡ-⊓-nonPos
d_'42''45'distrib'737''45''8851''45'nonPos_2826 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'737''45''8851''45'nonPos_2826 v0 ~v1
  = du_'42''45'distrib'737''45''8851''45'nonPos_2826 v0
du_'42''45'distrib'737''45''8851''45'nonPos_2826 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'distrib'737''45''8851''45'nonPos_2826 v0
  = coe
      d_antimono'45''8804''45'distrib'45''8851'_2720
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202 (coe v0))
      (coe du_'42''45'mono'691''45''8804''45'nonPos_1978 (coe v0))
-- Data.Rational.Unnormalised.Properties.*-distribʳ-⊓-nonPos
d_'42''45'distrib'691''45''8851''45'nonPos_2838 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'42''45'distrib'691''45''8851''45'nonPos_2838 v0 ~v1
  = du_'42''45'distrib'691''45''8851''45'nonPos_2838 v0
du_'42''45'distrib'691''45''8851''45'nonPos_2838 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'42''45'distrib'691''45''8851''45'nonPos_2838 v0
  = coe
      d_antimono'45''8804''45'distrib'45''8851'_2720
      (coe
         (\ v1 ->
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
              (coe v1) (coe v0)))
      (coe du_'42''45'mono'737''45''8804''45'nonPos_1956 (coe v0))
-- Data.Rational.Unnormalised.Properties.⊓-mono-<
d_'8851''45'mono'45''60'_2842 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'8851''45'mono'45''60'_2842 v0 v1 v2 v3 v4 v5
  = let v6
          = let v6 = d_'8804''45'totalPreorder_334 in
            let v7 = d_'8851''45'operator_2472 in
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
              (coe v6) (coe v7) (coe v1) (coe v3) in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7
        -> coe
             d_'60''45'resp'691''45''8771'_526
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8851'__254
                (coe v0) (coe v2))
             (coe v1)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8851'__254
                (coe v1) (coe v3))
             (coe du_'8771''45'sym_134 (coe v7))
             (coe
                d_'8804''45''60''45'trans_402
                (coe
                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8851'__254
                   (coe v0) (coe v2))
                (coe v0) (coe v1)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
                   (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
                   (coe v0) (coe v2))
                (coe v4))
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7
        -> coe
             d_'60''45'resp'691''45''8771'_526
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8851'__254
                (coe v0) (coe v2))
             (coe v3)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8851'__254
                (coe v1) (coe v3))
             (coe du_'8771''45'sym_134 (coe v7))
             (coe
                d_'8804''45''60''45'trans_402
                (coe
                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8851'__254
                   (coe v0) (coe v2))
                (coe v2) (coe v3)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
                   (coe d_'8804''45'totalPreorder_334) (coe d_'8851''45'operator_2472)
                   (coe v0) (coe v2))
                (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.⊔-mono-<
d_'8852''45'mono'45''60'_2888 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'8852''45'mono'45''60'_2888 v0 v1 v2 v3 v4 v5
  = let v6
          = let v6 = d_'8804''45'totalPreorder_334 in
            let v7 = d_'8852''45'operator_2474 in
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
              (coe
                 MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
                 (coe v6))
              (coe
                 MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
                 (coe v7))
              (coe v0) (coe v2) in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7
        -> coe
             d_'60''45'resp'737''45''8771'_562
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8852'__244
                (coe v1) (coe v3))
             (coe v0)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8852'__244
                (coe v0) (coe v2))
             (coe du_'8771''45'sym_134 (coe v7))
             (coe
                d_'60''45''8804''45'trans_436 (coe v0) (coe v1)
                (coe
                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8852'__244
                   (coe v1) (coe v3))
                (coe v4)
                (let v8 = d_'8804''45'totalPreorder_334 in
                 let v9 = d_'8852''45'operator_2474 in
                 coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
                   (coe
                      MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
                      (coe v8))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
                      (coe v9))
                   (coe v1) (coe v3)))
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7
        -> coe
             d_'60''45'resp'737''45''8771'_562
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8852'__244
                (coe v1) (coe v3))
             (coe v2)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8852'__244
                (coe v0) (coe v2))
             (coe du_'8771''45'sym_134 (coe v7))
             (coe
                d_'60''45''8804''45'trans_436 (coe v2) (coe v3)
                (coe
                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8852'__244
                   (coe v1) (coe v3))
                (coe v5)
                (let v8 = d_'8804''45'totalPreorder_334 in
                 let v9 = d_'8852''45'operator_2474 in
                 coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
                   (coe
                      MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
                      (coe v8))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
                      (coe v9))
                   (coe v1) (coe v3)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.pos⊓pos⇒pos
d_pos'8851'pos'8658'pos_2942 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_pos'8851'pos'8658'pos_2942 v0 ~v1 v2 ~v3
  = du_pos'8851'pos'8658'pos_2942 v0 v2
du_pos'8851'pos'8658'pos_2942 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_pos'8851'pos'8658'pos_2942 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_positive_162
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8851'__254
         (coe v0) (coe v1))
      (coe
         d_'8851''45'mono'45''60'_2842
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v1) (coe du_positive'8315''185'_686 (coe v0))
         (coe du_positive'8315''185'_686 (coe v1)))
-- Data.Rational.Unnormalised.Properties.pos⊔pos⇒pos
d_pos'8852'pos'8658'pos_2956 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_pos'8852'pos'8658'pos_2956 v0 ~v1 v2 ~v3
  = du_pos'8852'pos'8658'pos_2956 v0 v2
du_pos'8852'pos'8658'pos_2956 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_pos'8852'pos'8658'pos_2956 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_positive_162
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'8852'__244
         (coe v0) (coe v1))
      (coe
         d_'8852''45'mono'45''60'_2888
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v0)
         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)
         (coe v1) (coe du_positive'8315''185'_686 (coe v0))
         (coe du_positive'8315''185'_686 (coe v1)))
-- Data.Rational.Unnormalised.Properties.∣-∣-cong
d_'8739''45''8739''45'cong_2962 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8739''45''8739''45'cong_2962 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> coe
             seq (coe v3)
             (coe
                seq (coe v1)
                (coe
                   seq (coe v2)
                   (coe
                      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.∣p∣≃0⇒p≃0
d_'8739'p'8739''8771'0'8658'p'8771'0_2990 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8739'p'8739''8771'0'8658'p'8771'0_2990 v0 v1
  = coe seq (coe v0) (coe v1)
-- Data.Rational.Unnormalised.Properties.0≤∣p∣
d_0'8804''8739'p'8739'_3004 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_0'8804''8739'p'8739'_3004 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                (coe
                   MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                   (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.∣-p∣≡∣p∣
d_'8739''45'p'8739''8801''8739'p'8739'_3008 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45'p'8739''8801''8739'p'8739'_3008 = erased
-- Data.Rational.Unnormalised.Properties.∣-p∣≃∣p∣
d_'8739''45'p'8739''8771''8739'p'8739'_3022 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8739''45'p'8739''8771''8739'p'8739'_3022 ~v0
  = du_'8739''45'p'8739''8771''8739'p'8739'_3022
du_'8739''45'p'8739''8771''8739'p'8739'_3022 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'8739''45'p'8739''8771''8739'p'8739'_3022
  = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.0≤p⇒∣p∣≡p
d_0'8804'p'8658''8739'p'8739''8801'p_3024 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'8804'p'8658''8739'p'8739''8801'p_3024 = erased
-- Data.Rational.Unnormalised.Properties.0≤p⇒∣p∣≃p
d_0'8804'p'8658''8739'p'8739''8771'p_3038 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_0'8804'p'8658''8739'p'8739''8771'p_3038 ~v0 ~v1
  = du_0'8804'p'8658''8739'p'8739''8771'p_3038
du_0'8804'p'8658''8739'p'8739''8771'p_3038 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_0'8804'p'8658''8739'p'8739''8771'p_3038
  = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.∣p∣≡p⇒0≤p
d_'8739'p'8739''8801'p'8658'0'8804'p_3042 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8739'p'8739''8801'p'8658'0'8804'p_3042 v0 ~v1
  = du_'8739'p'8739''8801'p'8658'0'8804'p_3042 v0
du_'8739'p'8739''8801'p'8658'0'8804'p_3042 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'8739'p'8739''8801'p'8658'0'8804'p_3042 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> coe
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                (coe
                   MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                (\ v3 v4 v5 ->
                   coe
                     MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630 v5)
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                   (coe
                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108))
                   (coe
                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                      (coe v0)))
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v1)
                   (coe
                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                      (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                   (coe
                      MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                   (\ v3 v4 v5 v6 v7 ->
                      coe
                        MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                        v6 v7)
                   (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12) (coe v1)
                   (coe
                      MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v1)
                      (coe
                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                         (coe MAlonzo.Code.Data.Rational.Unnormalised.Base.d_0ℚ'7512'_108)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                      (coe
                         MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                      (coe
                         MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v1)
                         (coe MAlonzo.Code.Data.Integer.Base.d_1ℤ_16)))
                   (coe
                      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                      (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.∣p∣≡p∨∣p∣≡-p
d_'8739'p'8739''8801'p'8744''8739'p'8739''8801''45'p_3056 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8739'p'8739''8801'p'8744''8739'p'8739''8801''45'p_3056 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
             _ -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.∣p∣≃p⇒0≤p
d_'8739'p'8739''8771'p'8658'0'8804'p_3066 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8739'p'8739''8771'p'8658'0'8804'p_3066 v0 v1
  = let v2
          = d_'8739'p'8739''8801'p'8744''8739'p'8739''8801''45'p_3056
              (coe v0) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v3
        -> coe du_'8739'p'8739''8801'p'8658'0'8804'p_3042 (coe v0)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v3
        -> coe
             d_'8804''45'reflexive_242
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
                (coe (0 :: Integer)) (coe (0 :: Integer)))
             (coe v0)
             (coe
                du_'8771''45'sym_134
                (coe
                   d_p'8771''45'p'8658'p'8771'0_1036 (coe v0)
                   (coe du_'8771''45'sym_134 (coe v1))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.∣p+q∣≤∣p∣+∣q∣
d_'8739'p'43'q'8739''8804''8739'p'8739''43''8739'q'8739'_3096 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8739'p'43'q'8739''8804''8739'p'8739''43''8739'q'8739'_3096 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
               -> coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                       (coe
                          MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                       (\ v6 v7 v8 ->
                          coe
                            MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630 v8)
                       (coe
                          MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                   (coe v0) (coe v1))))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                   (coe v0))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                   (coe v1)))))
                       (coe
                          MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                   (coe v0))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                   (coe v1))))
                          (coe
                             MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                   (coe v0) (coe v1)))))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                          (coe
                             MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                          (\ v6 v7 v8 v9 v10 ->
                             coe
                               MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                               v9 v10)
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'42'__308
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'43'__276
                                   (coe du_'8613'p'8615'q_3106 (coe v2) (coe v4) (coe v5))
                                   (coe du_'8613'q'8615'p_3108 (coe v2) (coe v3) (coe v4))))
                             (coe d_'8615'p'8615'q_3118 (coe v2) (coe v3) (coe v4) (coe v5)))
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'42'__308
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'43'__276
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                                   (coe du_'8613'p'8615'q_3106 (coe v2) (coe v4) (coe v5)))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                                   (coe du_'8613'q'8615'p_3108 (coe v2) (coe v3) (coe v4))))
                             (coe d_'8615'p'8615'q_3118 (coe v2) (coe v3) (coe v4) (coe v5)))
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'42'__308
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                      (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                      (coe v1))))
                             (coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                      (coe v0) (coe v1)))))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                         (coe v0))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                         (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
                                   (coe
                                      MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                                      (coe
                                         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                                         (coe v0) (coe v1))))))
                          (coe
                             MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''8804''45'nonNeg_5814
                             (coe d_'8615'p'8615'q_3118 (coe v2) (coe v3) (coe v4) (coe v5))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'43'__276
                                   (coe du_'8613'p'8615'q_3106 (coe v2) (coe v4) (coe v5))
                                   (coe du_'8613'q'8615'p_3108 (coe v2) (coe v3) (coe v4))))
                             (coe
                                addInt
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                                   (coe du_'8613'q'8615'p_3108 (coe v2) (coe v3) (coe v4)))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                                   (coe du_'8613'p'8615'q_3106 (coe v2) (coe v4) (coe v5))))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                                (MAlonzo.Code.Data.Integer.Properties.d_'8739'i'43'j'8739''8804''8739'i'8739''43''8739'j'8739'_3160
                                   (coe du_'8613'p'8615'q_3106 (coe v2) (coe v4) (coe v5))
                                   (coe du_'8613'q'8615'p_3108 (coe v2) (coe v3) (coe v4)))))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties._.↥p↧q
d_'8613'p'8615'q_3106 ::
  Integer -> Integer -> Integer -> Integer -> Integer
d_'8613'p'8615'q_3106 v0 ~v1 v2 v3
  = du_'8613'p'8615'q_3106 v0 v2 v3
du_'8613'p'8615'q_3106 :: Integer -> Integer -> Integer -> Integer
du_'8613'p'8615'q_3106 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v1) (coe v2)))
-- Data.Rational.Unnormalised.Properties._.↥q↧p
d_'8613'q'8615'p_3108 ::
  Integer -> Integer -> Integer -> Integer -> Integer
d_'8613'q'8615'p_3108 v0 v1 v2 ~v3
  = du_'8613'q'8615'p_3108 v0 v1 v2
du_'8613'q'8615'p_3108 :: Integer -> Integer -> Integer -> Integer
du_'8613'q'8615'p_3108 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v2)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
-- Data.Rational.Unnormalised.Properties._.↥∣p∣↧q
d_'8613''8739'p'8739''8615'q_3110 ::
  Integer -> Integer -> Integer -> Integer -> Integer
d_'8613''8739'p'8739''8615'q_3110 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
               (coe v0) (coe v1))))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v2) (coe v3)))
-- Data.Rational.Unnormalised.Properties._.↥∣q∣↧p
d_'8613''8739'q'8739''8615'p_3112 ::
  Integer -> Integer -> Integer -> Integer -> Integer
d_'8613''8739'q'8739''8615'p_3112 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_numerator_14
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
               (coe v2) (coe v3))))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
-- Data.Rational.Unnormalised.Properties._.∣↥p∣↧q
d_'8739''8613'p'8739''8615'q_3114 ::
  Integer -> Integer -> Integer -> Integer -> Integer
d_'8739''8613'p'8739''8615'q_3114 v0 ~v1 v2 v3
  = du_'8739''8613'p'8739''8615'q_3114 v0 v2 v3
du_'8739''8613'p'8739''8615'q_3114 ::
  Integer -> Integer -> Integer -> Integer
du_'8739''8613'p'8739''8615'q_3114 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v1) (coe v2)))
-- Data.Rational.Unnormalised.Properties._.∣↥q∣↧p
d_'8739''8613'q'8739''8615'p_3116 ::
  Integer -> Integer -> Integer -> Integer -> Integer
d_'8739''8613'q'8739''8615'p_3116 v0 v1 v2 ~v3
  = du_'8739''8613'q'8739''8615'p_3116 v0 v1 v2
du_'8739''8613'q'8739''8615'p_3116 ::
  Integer -> Integer -> Integer -> Integer
du_'8739''8613'q'8739''8615'p_3116 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v2))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominator_20
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
-- Data.Rational.Unnormalised.Properties._.↧p↧q
d_'8615'p'8615'q_3118 ::
  Integer -> Integer -> Integer -> Integer -> Integer
d_'8615'p'8615'q_3118 v0 v1 v2 v3
  = coe
      mulInt
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominatorℕ_18
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_denominatorℕ_18
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22
            (coe v2) (coe v3)))
-- Data.Rational.Unnormalised.Properties._.∣m∣n≡∣mn∣
d_'8739'm'8739'n'8801''8739'mn'8739'_3124 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'm'8739'n'8801''8739'mn'8739'_3124 = erased
-- Data.Rational.Unnormalised.Properties._.∣↥p∣↧q≡∣↥p↧q∣
d_'8739''8613'p'8739''8615'q'8801''8739''8613'p'8615'q'8739'_3130 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''8613'p'8739''8615'q'8801''8739''8613'p'8615'q'8739'_3130
  = erased
-- Data.Rational.Unnormalised.Properties._.∣↥q∣↧p≡∣↥q↧p∣
d_'8739''8613'q'8739''8615'p'8801''8739''8613'q'8615'p'8739'_3132 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''8613'q'8739''8615'p'8801''8739''8613'q'8615'p'8739'_3132
  = erased
-- Data.Rational.Unnormalised.Properties.∣p-q∣≤∣p∣+∣q∣
d_'8739'p'45'q'8739''8804''8739'p'8739''43''8739'q'8739'_3146 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8739'p'45'q'8739''8804''8739'p'8739''43''8739'q'8739'_3146 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_322)
      (\ v2 v3 v4 -> coe du_'60''8658''8804'_366 v4)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
            (coe v1)))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
            (coe v0))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
            (coe v1)))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_322)
         (coe d_'8804''45''60''45'trans_402)
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'45'__208 (coe v0)
               (coe v1)))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
                  (coe v1))))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
               (coe v0))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
               (coe v1)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe d_'8804''45'isPreorder_322)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                  (coe v0))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                  (coe v1))))
         (coe
            d_'8739'p'43'q'8739''8804''8739'p'8739''43''8739'q'8739'_3096
            (coe v0)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'45'__190
               (coe v1))))
-- Data.Rational.Unnormalised.Properties.∣p*q∣≡∣p∣*∣q∣
d_'8739'p'42'q'8739''8801''8739'p'8739''42''8739'q'8739'_3162 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'p'42'q'8739''8801''8739'p'8739''42''8739'q'8739'_3162
  = erased
-- Data.Rational.Unnormalised.Properties.∣p*q∣≃∣p∣*∣q∣
d_'8739'p'42'q'8739''8771''8739'p'8739''42''8739'q'8739'_3180 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8739'p'42'q'8739''8771''8739'p'8739''42''8739'q'8739'_3180 ~v0
                                                              ~v1
  = du_'8739'p'42'q'8739''8771''8739'p'8739''42''8739'q'8739'_3180
du_'8739'p'42'q'8739''8771''8739'p'8739''42''8739'q'8739'_3180 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'8739'p'42'q'8739''8771''8739'p'8739''42''8739'q'8739'_3180
  = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.∣∣p∣∣≡∣p∣
d_'8739''8739'p'8739''8739''8801''8739'p'8739'_3188 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''8739'p'8739''8739''8801''8739'p'8739'_3188 = erased
-- Data.Rational.Unnormalised.Properties.∣∣p∣∣≃∣p∣
d_'8739''8739'p'8739''8739''8771''8739'p'8739'_3194 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'8739''8739'p'8739''8739''8771''8739'p'8739'_3194 ~v0
  = du_'8739''8739'p'8739''8739''8771''8739'p'8739'_3194
du_'8739''8739'p'8739''8739''8771''8739'p'8739'_3194 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'8739''8739'p'8739''8739''8771''8739'p'8739'_3194
  = coe du_'8771''45'reflexive_132
-- Data.Rational.Unnormalised.Properties.∣-∣-nonNeg
d_'8739''45''8739''45'nonNeg_3200 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_'8739''45''8739''45'nonNeg_3200 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Integer.Base.C_NonNegative'46'constructor_1353
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.neg-mono-<->
d_neg'45'mono'45''60''45''62'_3202 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_neg'45'mono'45''60''45''62'_3202 = coe d_neg'45'mono'45''60'_210
-- Data.Rational.Unnormalised.Properties.↥[p/q]≡p
d_'8613''91'p'47'q'93''8801'p_3204 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''91'p'47'q'93''8801'p_3204 = erased
-- Data.Rational.Unnormalised.Properties.↧[p/q]≡q
d_'8615''91'p'47'q'93''8801'q_3206 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''91'p'47'q'93''8801'q_3206 = erased
-- Data.Rational.Unnormalised.Properties.*-monoʳ-≤-pos
d_'42''45'mono'691''45''8804''45'pos_3212 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'691''45''8804''45'pos_3212 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''8804''45'pos_3212 v0 v2 v3
du_'42''45'mono'691''45''8804''45'pos_3212 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'691''45''8804''45'pos_3212 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v3 of
             0 -> coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   du_'42''45'mono'691''45''8804''45'nonNeg_1904 (coe v0) (coe v1)
                   (coe v2)
             _ -> coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-monoˡ-≤-pos
d_'42''45'mono'737''45''8804''45'pos_3220 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'737''45''8804''45'pos_3220 v0 ~v1 v2 v3
  = du_'42''45'mono'737''45''8804''45'pos_3220 v0 v2 v3
du_'42''45'mono'737''45''8804''45'pos_3220 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'737''45''8804''45'pos_3220 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v3 of
             0 -> coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   du_'42''45'mono'737''45''8804''45'nonNeg_1874 (coe v0) (coe v1)
                   (coe v2)
             _ -> coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.≤-steps
d_'8804''45'steps_3224 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'8804''45'steps_3224 v0 v1 v2 v3 v4
  = coe du_p'8804'q'8658'p'8804'r'43'q_1136 v0 v1 v2 v4
-- Data.Rational.Unnormalised.Properties.*-monoˡ-≤-neg
d_'42''45'mono'737''45''8804''45'neg_3230 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'737''45''8804''45'neg_3230 v0 ~v1 v2 v3
  = du_'42''45'mono'737''45''8804''45'neg_3230 v0 v2 v3
du_'42''45'mono'737''45''8804''45'neg_3230 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'737''45''8804''45'neg_3230 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v3 of
             _ | coe geqInt (coe v3) (coe (0 :: Integer)) ->
                 coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
             _ -> coe
                    du_'42''45'mono'737''45''8804''45'nonPos_1956 (coe v0) (coe v1)
                    (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-monoʳ-≤-neg
d_'42''45'mono'691''45''8804''45'neg_3238 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_'42''45'mono'691''45''8804''45'neg_3238 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''8804''45'neg_3238 v0 v2 v3
du_'42''45'mono'691''45''8804''45'neg_3238 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
du_'42''45'mono'691''45''8804''45'neg_3238 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v3 of
             _ | coe geqInt (coe v3) (coe (0 :: Integer)) ->
                 coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
             _ -> coe
                    du_'42''45'mono'691''45''8804''45'nonPos_1978 (coe v0) (coe v1)
                    (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-cancelˡ-<-pos
d_'42''45'cancel'737''45''60''45'pos_3248 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'cancel'737''45''60''45'pos_3248 v0 ~v1 v2 v3
  = du_'42''45'cancel'737''45''60''45'pos_3248 v0 v2 v3
du_'42''45'cancel'737''45''60''45'pos_3248 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'cancel'737''45''60''45'pos_3248 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v3 of
             0 -> coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   du_'42''45'cancel'737''45''60''45'nonNeg_2088 (coe v1) (coe v2)
                   (coe v0)
             _ -> coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-cancelʳ-<-pos
d_'42''45'cancel'691''45''60''45'pos_3260 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'cancel'691''45''60''45'pos_3260 v0 ~v1 v2 v3
  = du_'42''45'cancel'691''45''60''45'pos_3260 v0 v2 v3
du_'42''45'cancel'691''45''60''45'pos_3260 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'cancel'691''45''60''45'pos_3260 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v3 of
             0 -> coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   du_'42''45'cancel'691''45''60''45'nonNeg_2070 (coe v1) (coe v2)
                   (coe v0)
             _ -> coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-cancelˡ-<-neg
d_'42''45'cancel'737''45''60''45'neg_3272 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'cancel'737''45''60''45'neg_3272 v0 ~v1 v2 v3
  = du_'42''45'cancel'737''45''60''45'neg_3272 v0 v2 v3
du_'42''45'cancel'737''45''60''45'neg_3272 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'cancel'737''45''60''45'neg_3272 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v3 of
             _ | coe geqInt (coe v3) (coe (0 :: Integer)) ->
                 coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
             _ -> coe
                    du_'42''45'cancel'737''45''60''45'nonPos_2152 (coe v1) (coe v2)
                    (coe v0)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.*-cancelʳ-<-neg
d_'42''45'cancel'691''45''60''45'neg_3282 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_'42''45'cancel'691''45''60''45'neg_3282 v0 ~v1 v2 v3
  = du_'42''45'cancel'691''45''60''45'neg_3282 v0 v2 v3
du_'42''45'cancel'691''45''60''45'neg_3282 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_'42''45'cancel'691''45''60''45'neg_3282 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v3 v4
        -> case coe v3 of
             _ | coe geqInt (coe v3) (coe (0 :: Integer)) ->
                 coe (\ v5 -> MAlonzo.RTE.mazUnreachableError)
             _ -> coe
                    du_'42''45'cancel'691''45''60''45'nonPos_2172 (coe v1) (coe v2)
                    (coe v0)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Unnormalised.Properties.positive⇒nonNegative
d_positive'8658'nonNegative_3288 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_positive'8658'nonNegative_3288 v0 ~v1
  = du_positive'8658'nonNegative_3288 v0
du_positive'8658'nonNegative_3288 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_positive'8658'nonNegative_3288 v0
  = coe du_pos'8658'nonNeg_710 (coe v0)
-- Data.Rational.Unnormalised.Properties.negative⇒nonPositive
d_negative'8658'nonPositive_3296 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154
d_negative'8658'nonPositive_3296 v0 ~v1
  = du_negative'8658'nonPositive_3296 v0
du_negative'8658'nonPositive_3296 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154
du_negative'8658'nonPositive_3296 v0
  = coe du_neg'8658'nonPos_716 (coe v0)
-- Data.Rational.Unnormalised.Properties.negative<positive
d_negative'60'positive_3306 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_negative'60'positive_3306 v0 v1 ~v2 ~v3
  = du_negative'60'positive_3306 v0 v1
du_negative'60'positive_3306 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
du_negative'60'positive_3306 v0 v1
  = coe du_neg'60'pos_724 (coe v0) (coe v1)
