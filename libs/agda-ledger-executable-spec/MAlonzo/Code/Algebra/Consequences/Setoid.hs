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

module MAlonzo.Code.Algebra.Consequences.Setoid where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Consequences.Setoid._._Absorbs_
d__Absorbs__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__Absorbs__36 = erased
-- Algebra.Consequences.Setoid._._DistributesOver_
d__DistributesOver__38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver__38 = erased
-- Algebra.Consequences.Setoid._._DistributesOverʳ_
d__DistributesOver'691'__40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'691'__40 = erased
-- Algebra.Consequences.Setoid._._DistributesOverˡ_
d__DistributesOver'737'__42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'737'__42 = erased
-- Algebra.Consequences.Setoid._.AlmostLeftCancellative
d_AlmostLeftCancellative_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_AlmostLeftCancellative_50 = erased
-- Algebra.Consequences.Setoid._.AlmostRightCancellative
d_AlmostRightCancellative_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_AlmostRightCancellative_52 = erased
-- Algebra.Consequences.Setoid._.Associative
d_Associative_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Associative_56 = erased
-- Algebra.Consequences.Setoid._.Commutative
d_Commutative_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Commutative_60 = erased
-- Algebra.Consequences.Setoid._.Congruent₂
d_Congruent'8322'_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Congruent'8322'_64 = erased
-- Algebra.Consequences.Setoid._.Identity
d_Identity_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Identity_76 = erased
-- Algebra.Consequences.Setoid._.Inverse
d_Inverse_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Inverse_80 = erased
-- Algebra.Consequences.Setoid._.LeftCancellative
d_LeftCancellative_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftCancellative_90 = erased
-- Algebra.Consequences.Setoid._.LeftIdentity
d_LeftIdentity_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftIdentity_102 = erased
-- Algebra.Consequences.Setoid._.LeftInverse
d_LeftInverse_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftInverse_104 = erased
-- Algebra.Consequences.Setoid._.LeftZero
d_LeftZero_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftZero_110 = erased
-- Algebra.Consequences.Setoid._.RightCancellative
d_RightCancellative_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightCancellative_120 = erased
-- Algebra.Consequences.Setoid._.RightIdentity
d_RightIdentity_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightIdentity_132 = erased
-- Algebra.Consequences.Setoid._.RightInverse
d_RightInverse_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightInverse_134 = erased
-- Algebra.Consequences.Setoid._.RightZero
d_RightZero_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightZero_140 = erased
-- Algebra.Consequences.Setoid._.Zero
d_Zero_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Zero_158 = erased
-- Algebra.Consequences.Setoid._.comm+cancelˡ⇒cancelʳ
d_comm'43'cancel'737''8658'cancel'691'_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_comm'43'cancel'737''8658'cancel'691'_192 ~v0 ~v1 v2 v3 v4 v5 v6
                                           v7 v8 v9
  = du_comm'43'cancel'737''8658'cancel'691'_192
      v2 v3 v4 v5 v6 v7 v8 v9
du_comm'43'cancel'737''8658'cancel'691'_192 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_comm'43'cancel'737''8658'cancel'691'_192 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      v3 v4 v5 v6
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v4 v5) (coe v1 v5 v4) (coe v1 v4 v6)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0 (coe v1 v5 v4) (coe v1 v6 v4) (coe v1 v4 v6)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  v0 (coe v1 v6 v4) (coe v1 v4 v6) (coe v1 v4 v6)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                     (coe v1 v4 v6))
                  (coe v2 v6 v4))
               v7)
            (coe v2 v4 v5)))
-- Algebra.Consequences.Setoid._.comm+cancelʳ⇒cancelˡ
d_comm'43'cancel'691''8658'cancel'737'_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_comm'43'cancel'691''8658'cancel'737'_204 ~v0 ~v1 v2 v3 v4 v5 v6
                                           v7 v8 v9
  = du_comm'43'cancel'691''8658'cancel'737'_204
      v2 v3 v4 v5 v6 v7 v8 v9
du_comm'43'cancel'691''8658'cancel'737'_204 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_comm'43'cancel'691''8658'cancel'737'_204 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      v3 v4 v5 v6
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v5 v4) (coe v1 v4 v5) (coe v1 v6 v4)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0 (coe v1 v4 v5) (coe v1 v4 v6) (coe v1 v6 v4)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  v0 (coe v1 v4 v6) (coe v1 v6 v4) (coe v1 v6 v4)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                     (coe v1 v6 v4))
                  (coe v2 v4 v6))
               v7)
            (coe v2 v5 v4)))
-- Algebra.Consequences.Setoid._.comm+idˡ⇒idʳ
d_comm'43'id'737''8658'id'691'_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_comm'43'id'737''8658'id'691'_226 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_comm'43'id'737''8658'id'691'_226 v2 v3 v4 v5 v6 v7
du_comm'43'id'737''8658'id'691'_226 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_comm'43'id'737''8658'id'691'_226 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v1 v5 v3) (coe v1 v3 v5) v5
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v3 v5) v5 v5
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
               (coe v5))
            (coe v4 v5))
         (coe v2 v5 v3))
-- Algebra.Consequences.Setoid._.comm+idʳ⇒idˡ
d_comm'43'id'691''8658'id'737'_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_comm'43'id'691''8658'id'737'_232 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_comm'43'id'691''8658'id'737'_232 v2 v3 v4 v5 v6 v7
du_comm'43'id'691''8658'id'737'_232 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_comm'43'id'691''8658'id'737'_232 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v1 v3 v5) (coe v1 v5 v3) v5
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v5 v3) v5 v5
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
               (coe v5))
            (coe v4 v5))
         (coe v2 v3 v5))
-- Algebra.Consequences.Setoid._.comm+idˡ⇒id
d_comm'43'id'737''8658'id_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_comm'43'id'737''8658'id_238 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_comm'43'id'737''8658'id_238 v2 v3 v4 v5 v6
du_comm'43'id'737''8658'id_238 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_comm'43'id'737''8658'id_238 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
      (coe
         du_comm'43'id'737''8658'id'691'_226 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v4))
-- Algebra.Consequences.Setoid._.comm+idʳ⇒id
d_comm'43'id'691''8658'id_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_comm'43'id'691''8658'id_242 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_comm'43'id'691''8658'id_242 v2 v3 v4 v5 v6
du_comm'43'id'691''8658'id_242 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_comm'43'id'691''8658'id_242 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_comm'43'id'691''8658'id'737'_232 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v4))
      (coe v4)
-- Algebra.Consequences.Setoid._.comm+zeˡ⇒zeʳ
d_comm'43'ze'737''8658'ze'691'_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_comm'43'ze'737''8658'ze'691'_246 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_comm'43'ze'737''8658'ze'691'_246 v2 v3 v4 v5 v6 v7
du_comm'43'ze'737''8658'ze'691'_246 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_comm'43'ze'737''8658'ze'691'_246 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v1 v5 v3) (coe v1 v3 v5) v3
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v3 v5) v3 v3
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
               (coe v3))
            (coe v4 v5))
         (coe v2 v5 v3))
-- Algebra.Consequences.Setoid._.comm+zeʳ⇒zeˡ
d_comm'43'ze'691''8658'ze'737'_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_comm'43'ze'691''8658'ze'737'_252 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_comm'43'ze'691''8658'ze'737'_252 v2 v3 v4 v5 v6 v7
du_comm'43'ze'691''8658'ze'737'_252 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_comm'43'ze'691''8658'ze'737'_252 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v1 v3 v5) (coe v1 v5 v3) v3
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v5 v3) v3 v3
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
               (coe v3))
            (coe v4 v5))
         (coe v2 v3 v5))
-- Algebra.Consequences.Setoid._.comm+zeˡ⇒ze
d_comm'43'ze'737''8658'ze_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_comm'43'ze'737''8658'ze_258 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_comm'43'ze'737''8658'ze_258 v2 v3 v4 v5 v6
du_comm'43'ze'737''8658'ze_258 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_comm'43'ze'737''8658'ze_258 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
      (coe
         du_comm'43'ze'737''8658'ze'691'_246 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v4))
-- Algebra.Consequences.Setoid._.comm+zeʳ⇒ze
d_comm'43'ze'691''8658'ze_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_comm'43'ze'691''8658'ze_262 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_comm'43'ze'691''8658'ze_262 v2 v3 v4 v5 v6
du_comm'43'ze'691''8658'ze_262 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_comm'43'ze'691''8658'ze_262 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_comm'43'ze'691''8658'ze'737'_252 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v4))
      (coe v4)
-- Algebra.Consequences.Setoid._.comm+almostCancelˡ⇒almostCancelʳ
d_comm'43'almostCancel'737''8658'almostCancel'691'_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> AgdaAny
d_comm'43'almostCancel'737''8658'almostCancel'691'_266 ~v0 ~v1 v2
                                                       v3 v4 ~v5 v6 v7 v8 v9 v10 v11
  = du_comm'43'almostCancel'737''8658'almostCancel'691'_266
      v2 v3 v4 v6 v7 v8 v9 v10 v11
du_comm'43'almostCancel'737''8658'almostCancel'691'_266 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> AgdaAny
du_comm'43'almostCancel'737''8658'almostCancel'691'_266 v0 v1 v2 v3
                                                        v4 v5 v6 v7 v8
  = coe
      v3 v4 v5 v6 v7
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v4 v5) (coe v1 v5 v4) (coe v1 v4 v6)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0 (coe v1 v5 v4) (coe v1 v6 v4) (coe v1 v4 v6)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  v0 (coe v1 v6 v4) (coe v1 v4 v6) (coe v1 v4 v6)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                     (coe v1 v4 v6))
                  (coe v2 v6 v4))
               v8)
            (coe v2 v4 v5)))
-- Algebra.Consequences.Setoid._.comm+almostCancelʳ⇒almostCancelˡ
d_comm'43'almostCancel'691''8658'almostCancel'737'_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> AgdaAny
d_comm'43'almostCancel'691''8658'almostCancel'737'_280 ~v0 ~v1 v2
                                                       v3 v4 ~v5 v6 v7 v8 v9 v10 v11
  = du_comm'43'almostCancel'691''8658'almostCancel'737'_280
      v2 v3 v4 v6 v7 v8 v9 v10 v11
du_comm'43'almostCancel'691''8658'almostCancel'737'_280 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> AgdaAny
du_comm'43'almostCancel'691''8658'almostCancel'737'_280 v0 v1 v2 v3
                                                        v4 v5 v6 v7 v8
  = coe
      v3 v4 v5 v6 v7
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v5 v4) (coe v1 v4 v5) (coe v1 v6 v4)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0 (coe v1 v4 v5) (coe v1 v4 v6) (coe v1 v6 v4)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  v0 (coe v1 v4 v6) (coe v1 v6 v4) (coe v1 v6 v4)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                     (coe v1 v6 v4))
                  (coe v2 v4 v6))
               v8)
            (coe v2 v5 v4)))
-- Algebra.Consequences.Setoid._.comm+invˡ⇒invʳ
d_comm'43'inv'737''8658'inv'691'_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_comm'43'inv'737''8658'inv'691'_306 ~v0 ~v1 v2 v3 v4 v5 v6 v7 v8
  = du_comm'43'inv'737''8658'inv'691'_306 v2 v3 v4 v5 v6 v7 v8
du_comm'43'inv'737''8658'inv'691'_306 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_comm'43'inv'737''8658'inv'691'_306 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v1 v6 (coe v2 v6)) (coe v1 (coe v2 v6) v6) v3
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 (coe v2 v6) v6) v3 v3
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
               (coe v3))
            (coe v5 v6))
         (coe v4 v6 (coe v2 v6)))
-- Algebra.Consequences.Setoid._.comm+invˡ⇒inv
d_comm'43'inv'737''8658'inv_312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_comm'43'inv'737''8658'inv_312 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_comm'43'inv'737''8658'inv_312 v2 v3 v4 v5 v6 v7
du_comm'43'inv'737''8658'inv_312 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_comm'43'inv'737''8658'inv_312 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5)
      (coe
         du_comm'43'inv'737''8658'inv'691'_306 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v4) (coe v5))
-- Algebra.Consequences.Setoid._.comm+invʳ⇒invˡ
d_comm'43'inv'691''8658'inv'737'_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_comm'43'inv'691''8658'inv'737'_316 ~v0 ~v1 v2 v3 v4 v5 v6 v7 v8
  = du_comm'43'inv'691''8658'inv'737'_316 v2 v3 v4 v5 v6 v7 v8
du_comm'43'inv'691''8658'inv'737'_316 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_comm'43'inv'691''8658'inv'737'_316 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v1 (coe v2 v6) v6) (coe v1 v6 (coe v2 v6)) v3
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v6 (coe v2 v6)) v3 v3
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
               (coe v3))
            (coe v5 v6))
         (coe v4 (coe v2 v6) v6))
-- Algebra.Consequences.Setoid._.comm+invʳ⇒inv
d_comm'43'inv'691''8658'inv_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_comm'43'inv'691''8658'inv_322 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_comm'43'inv'691''8658'inv_322 v2 v3 v4 v5 v6 v7
du_comm'43'inv'691''8658'inv_322 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_comm'43'inv'691''8658'inv_322 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_comm'43'inv'691''8658'inv'737'_316 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v4) (coe v5))
      (coe v5)
-- Algebra.Consequences.Setoid._.assoc+id+invʳ⇒invˡ-unique
d_assoc'43'id'43'inv'691''8658'inv'737''45'unique_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc'43'id'43'inv'691''8658'inv'737''45'unique_342 ~v0 ~v1 v2 v3
                                                      v4 v5 v6 v7 v8 v9 v10 v11 v12
  = du_assoc'43'id'43'inv'691''8658'inv'737''45'unique_342
      v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
du_assoc'43'id'43'inv'691''8658'inv'737''45'unique_342 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc'43'id'43'inv'691''8658'inv'737''45'unique_342 v0 v1 v2 v3
                                                       v4 v5 v6 v7 v8 v9 v10
  = case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                v0 v8 (coe v1 v8 v3) (coe v2 v9)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   v0 (coe v1 v8 v3) (coe v1 v8 (coe v1 v9 (coe v2 v9))) (coe v2 v9)
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                      v0 (coe v1 v8 (coe v1 v9 (coe v2 v9)))
                      (coe v1 (coe v1 v8 v9) (coe v2 v9)) (coe v2 v9)
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                         v0 (coe v1 (coe v1 v8 v9) (coe v2 v9)) (coe v1 v3 (coe v2 v9))
                         (coe v2 v9)
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                            v0 (coe v1 v3 (coe v2 v9)) (coe v2 v9) (coe v2 v9)
                            (coe
                               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                               (coe
                                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                  (coe
                                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                     (coe v0)))
                               (coe v2 v9))
                            (coe v11 (coe v2 v9)))
                         (coe
                            v4 (coe v1 v8 v9) v3 (coe v2 v9) (coe v2 v9) v10
                            (coe
                               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                               (coe v2 v9))))
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                         (coe v1 (coe v1 v8 v9) (coe v2 v9))
                         (coe v1 v8 (coe v1 v9 (coe v2 v9))) (coe v5 v8 v9 (coe v2 v9))))
                   (coe
                      v4 v8 v8 v3 (coe v1 v9 (coe v2 v9))
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                         v8)
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                         (coe v1 v9 (coe v2 v9)) v3 (coe v7 v9))))
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                   (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                   (coe v1 v8 v3) v8 (coe v12 v8)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Consequences.Setoid._.assoc+id+invˡ⇒invʳ-unique
d_assoc'43'id'43'inv'737''8658'inv'691''45'unique_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc'43'id'43'inv'737''8658'inv'691''45'unique_362 ~v0 ~v1 v2 v3
                                                      v4 v5 v6 v7 v8 v9 v10 v11 v12
  = du_assoc'43'id'43'inv'737''8658'inv'691''45'unique_362
      v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
du_assoc'43'id'43'inv'737''8658'inv'691''45'unique_362 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc'43'id'43'inv'737''8658'inv'691''45'unique_362 v0 v1 v2 v3
                                                       v4 v5 v6 v7 v8 v9 v10
  = case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                v0 v9 (coe v1 v3 v9) (coe v2 v8)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   v0 (coe v1 v3 v9) (coe v1 (coe v1 (coe v2 v8) v8) v9) (coe v2 v8)
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                      v0 (coe v1 (coe v1 (coe v2 v8) v8) v9)
                      (coe v1 (coe v2 v8) (coe v1 v8 v9)) (coe v2 v8)
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                         v0 (coe v1 (coe v2 v8) (coe v1 v8 v9)) (coe v1 (coe v2 v8) v3)
                         (coe v2 v8)
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                            v0 (coe v1 (coe v2 v8) v3) (coe v2 v8) (coe v2 v8)
                            (coe
                               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                               (coe
                                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                  (coe
                                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                     (coe v0)))
                               (coe v2 v8))
                            (coe v12 (coe v2 v8)))
                         (coe
                            v4 (coe v2 v8) (coe v2 v8) (coe v1 v8 v9) v3
                            (coe
                               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                               (coe v2 v8))
                            v10))
                      (coe v5 (coe v2 v8) v8 v9))
                   (coe
                      v4 v3 (coe v1 (coe v2 v8) v8) v9 v9
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                         (coe v1 (coe v2 v8) v8) v3 (coe v7 v8))
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                         v9)))
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                   (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                   (coe v1 v3 v9) v9 (coe v11 v9)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Consequences.Setoid._.comm+distrˡ⇒distrʳ
d_comm'43'distr'737''8658'distr'691'_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_comm'43'distr'737''8658'distr'691'_390 ~v0 ~v1 v2 v3 v4 v5 v6 v7
                                         v8 v9 v10
  = du_comm'43'distr'737''8658'distr'691'_390
      v2 v3 v4 v5 v6 v7 v8 v9 v10
du_comm'43'distr'737''8658'distr'691'_390 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_comm'43'distr'737''8658'distr'691'_390 v0 v1 v2 v3 v4 v5 v6 v7
                                          v8
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v1 (coe v2 v7 v8) v6) (coe v1 v6 (coe v2 v7 v8))
         (coe v2 (coe v1 v7 v6) (coe v1 v8 v6))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 v6 (coe v2 v7 v8))
            (coe v2 (coe v1 v6 v7) (coe v1 v6 v8))
            (coe v2 (coe v1 v7 v6) (coe v1 v8 v6))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0 (coe v2 (coe v1 v6 v7) (coe v1 v6 v8))
               (coe v2 (coe v1 v7 v6) (coe v1 v8 v6))
               (coe v2 (coe v1 v7 v6) (coe v1 v8 v6))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                  (coe v2 (coe v1 v7 v6) (coe v1 v8 v6)))
               (coe
                  v3 (coe v1 v6 v7) (coe v1 v7 v6) (coe v1 v6 v8) (coe v1 v8 v6)
                  (coe v4 v6 v7) (coe v4 v6 v8)))
            (coe v5 v6 v7 v8))
         (coe v4 (coe v2 v7 v8) v6))
-- Algebra.Consequences.Setoid._.comm+distrʳ⇒distrˡ
d_comm'43'distr'691''8658'distr'737'_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_comm'43'distr'691''8658'distr'737'_400 ~v0 ~v1 v2 v3 v4 v5 v6 v7
                                         v8 v9 v10
  = du_comm'43'distr'691''8658'distr'737'_400
      v2 v3 v4 v5 v6 v7 v8 v9 v10
du_comm'43'distr'691''8658'distr'737'_400 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_comm'43'distr'691''8658'distr'737'_400 v0 v1 v2 v3 v4 v5 v6 v7
                                          v8
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v1 v6 (coe v2 v7 v8)) (coe v1 (coe v2 v7 v8) v6)
         (coe v2 (coe v1 v6 v7) (coe v1 v6 v8))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 (coe v2 v7 v8) v6)
            (coe v2 (coe v1 v7 v6) (coe v1 v8 v6))
            (coe v2 (coe v1 v6 v7) (coe v1 v6 v8))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0 (coe v2 (coe v1 v7 v6) (coe v1 v8 v6))
               (coe v2 (coe v1 v6 v7) (coe v1 v6 v8))
               (coe v2 (coe v1 v6 v7) (coe v1 v6 v8))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                  (coe v2 (coe v1 v6 v7) (coe v1 v6 v8)))
               (coe
                  v3 (coe v1 v7 v6) (coe v1 v6 v7) (coe v1 v8 v6) (coe v1 v6 v8)
                  (coe v4 v7 v6) (coe v4 v8 v6)))
            (coe v5 v6 v7 v8))
         (coe v4 v6 (coe v2 v7 v8)))
-- Algebra.Consequences.Setoid._.comm+distrˡ⇒distr
d_comm'43'distr'737''8658'distr_410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_comm'43'distr'737''8658'distr_410 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_comm'43'distr'737''8658'distr_410 v2 v3 v4 v5 v6 v7
du_comm'43'distr'737''8658'distr_410 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_comm'43'distr'737''8658'distr_410 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5)
      (coe
         du_comm'43'distr'737''8658'distr'691'_390 (coe v0) (coe v1)
         (coe v2) (coe v3) (coe v4) (coe v5))
-- Algebra.Consequences.Setoid._.comm+distrʳ⇒distr
d_comm'43'distr'691''8658'distr_414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_comm'43'distr'691''8658'distr_414 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_comm'43'distr'691''8658'distr_414 v2 v3 v4 v5 v6 v7
du_comm'43'distr'691''8658'distr_414 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_comm'43'distr'691''8658'distr_414 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_comm'43'distr'691''8658'distr'737'_400 (coe v0) (coe v1)
         (coe v2) (coe v3) (coe v4) (coe v5))
      (coe v5)
-- Algebra.Consequences.Setoid._.comm⇒sym[distribˡ]
d_comm'8658'sym'91'distrib'737''93'_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_comm'8658'sym'91'distrib'737''93'_424 ~v0 ~v1 v2 v3 v4 v5 v6 v7
                                        v8 v9 v10
  = du_comm'8658'sym'91'distrib'737''93'_424
      v2 v3 v4 v5 v6 v7 v8 v9 v10
du_comm'8658'sym'91'distrib'737''93'_424 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_comm'8658'sym'91'distrib'737''93'_424 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v2 v5 (coe v1 v7 v6)) (coe v2 v5 (coe v1 v6 v7))
         (coe v1 (coe v2 v5 v7) (coe v2 v5 v6))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v2 v5 (coe v1 v6 v7))
            (coe v1 (coe v2 v5 v6) (coe v2 v5 v7))
            (coe v1 (coe v2 v5 v7) (coe v2 v5 v6))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0 (coe v1 (coe v2 v5 v6) (coe v2 v5 v7))
               (coe v1 (coe v2 v5 v7) (coe v2 v5 v6))
               (coe v1 (coe v2 v5 v7) (coe v2 v5 v6))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                  (coe v1 (coe v2 v5 v7) (coe v2 v5 v6)))
               (coe v4 (coe v2 v5 v6) (coe v2 v5 v7)))
            v8)
         (coe
            v3 v5 v5 (coe v1 v7 v6) (coe v1 v6 v7)
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
               v5)
            (coe v4 v7 v6)))
-- Algebra.Consequences.Setoid._.distrib+absorbs⇒distribˡ
d_distrib'43'absorbs'8658'distrib'737'_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'43'absorbs'8658'distrib'737'_448 ~v0 ~v1 v2 v3 v4 v5 v6
                                           v7 v8 v9 v10 v11 v12 v13
  = du_distrib'43'absorbs'8658'distrib'737'_448
      v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
du_distrib'43'absorbs'8658'distrib'737'_448 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'43'absorbs'8658'distrib'737'_448 v0 v1 v2 v3 v4 v5 v6 v7
                                            v8 v9 v10 v11
  = case coe v8 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v12 v13
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                (coe v0) (coe v1 v9 (coe v2 v10 v11))
                (coe v1 (coe v1 v9 (coe v2 v9 v10)) (coe v2 v10 v11))
                (coe v2 (coe v1 v9 v10) (coe v1 v9 v11))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   v0 (coe v1 (coe v1 v9 (coe v2 v9 v10)) (coe v2 v10 v11))
                   (coe v1 (coe v1 v9 (coe v2 v10 v9)) (coe v2 v10 v11))
                   (coe v2 (coe v1 v9 v10) (coe v1 v9 v11))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                      v0 (coe v1 (coe v1 v9 (coe v2 v10 v9)) (coe v2 v10 v11))
                      (coe v1 v9 (coe v1 (coe v2 v10 v9) (coe v2 v10 v11)))
                      (coe v2 (coe v1 v9 v10) (coe v1 v9 v11))
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                         (coe v0) (coe v1 v9 (coe v1 (coe v2 v10 v9) (coe v2 v10 v11)))
                         (coe v1 v9 (coe v2 v10 (coe v1 v9 v11)))
                         (coe v2 (coe v1 v9 v10) (coe v1 v9 v11))
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                            (coe v0) (coe v1 v9 (coe v2 v10 (coe v1 v9 v11)))
                            (coe v1 (coe v2 v9 (coe v1 v9 v11)) (coe v2 v10 (coe v1 v9 v11)))
                            (coe v2 (coe v1 v9 v10) (coe v1 v9 v11))
                            (coe
                               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                               (coe v0)
                               (coe v1 (coe v2 v9 (coe v1 v9 v11)) (coe v2 v10 (coe v1 v9 v11)))
                               (coe v2 (coe v1 v9 v10) (coe v1 v9 v11))
                               (coe v2 (coe v1 v9 v10) (coe v1 v9 v11))
                               (coe
                                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                  (coe
                                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                        (coe v0)))
                                  (coe v2 (coe v1 v9 v10) (coe v1 v9 v11)))
                               (coe v13 (coe v1 v9 v11) v9 v10))
                            (coe
                               v3 (coe v2 v9 (coe v1 v9 v11)) v9 (coe v2 v10 (coe v1 v9 v11))
                               (coe v2 v10 (coe v1 v9 v11)) (coe v7 v9 v11)
                               (coe
                                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                  (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                                  (coe v2 v10 (coe v1 v9 v11)))))
                         (coe
                            v3 v9 v9 (coe v2 v10 (coe v1 v9 v11))
                            (coe v1 (coe v2 v10 v9) (coe v2 v10 v11))
                            (coe
                               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                               v9)
                            (coe v12 v10 v9 v11)))
                      (coe v4 v9 (coe v2 v10 v9) (coe v2 v10 v11)))
                   (coe
                      v3 (coe v1 v9 (coe v2 v9 v10)) (coe v1 v9 (coe v2 v10 v9))
                      (coe v2 v10 v11) (coe v2 v10 v11)
                      (coe
                         v3 v9 v9 (coe v2 v9 v10) (coe v2 v10 v9)
                         (coe
                            MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                            (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                            v9)
                         (coe v5 v9 v10))
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                         (coe v2 v10 v11))))
                (coe
                   v3 (coe v1 v9 (coe v2 v9 v10)) v9 (coe v2 v10 v11) (coe v2 v10 v11)
                   (coe v6 v9 v10)
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                      (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                      (coe v2 v10 v11))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Consequences.Setoid._.assoc+distribʳ+idʳ+invʳ⇒zeˡ
d_assoc'43'distrib'691''43'id'691''43'inv'691''8658'ze'737'_480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_assoc'43'distrib'691''43'id'691''43'inv'691''8658'ze'737'_480 ~v0
                                                                ~v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
                                                                v12 v13
  = du_assoc'43'distrib'691''43'id'691''43'inv'691''8658'ze'737'_480
      v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
du_assoc'43'distrib'691''43'id'691''43'inv'691''8658'ze'737'_480 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_assoc'43'distrib'691''43'id'691''43'inv'691''8658'ze'737'_480 v0
                                                                 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v2 v4 v11) (coe v1 (coe v2 v4 v11) v4) v4
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 (coe v2 v4 v11) v4)
            (coe
               v1 (coe v2 v4 v11)
               (coe v1 (coe v2 v4 v11) (coe v3 (coe v2 v4 v11))))
            v4
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0
               (coe
                  v1 (coe v2 v4 v11)
                  (coe v1 (coe v2 v4 v11) (coe v3 (coe v2 v4 v11))))
               (coe
                  v1 (coe v1 (coe v2 v4 v11) (coe v2 v4 v11))
                  (coe v3 (coe v2 v4 v11)))
               v4
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  v0
                  (coe
                     v1 (coe v1 (coe v2 v4 v11) (coe v2 v4 v11))
                     (coe v3 (coe v2 v4 v11)))
                  (coe v1 (coe v2 (coe v1 v4 v4) v11) (coe v3 (coe v2 v4 v11))) v4
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     v0 (coe v1 (coe v2 (coe v1 v4 v4) v11) (coe v3 (coe v2 v4 v11)))
                     (coe v1 (coe v2 v4 v11) (coe v3 (coe v2 v4 v11))) v4
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        v0 (coe v1 (coe v2 v4 v11) (coe v3 (coe v2 v4 v11))) v4 v4
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe
                                 MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                           (coe v4))
                        (coe v10 (coe v2 v4 v11)))
                     (coe
                        v5 (coe v2 (coe v1 v4 v4) v11) (coe v2 v4 v11)
                        (coe v3 (coe v2 v4 v11)) (coe v3 (coe v2 v4 v11))
                        (coe
                           v6 (coe v1 v4 v4) v4 v11 v11 (coe v9 v4)
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                              v11))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                           (coe v3 (coe v2 v4 v11)))))
                  (coe
                     v5 (coe v1 (coe v2 v4 v11) (coe v2 v4 v11))
                     (coe v2 (coe v1 v4 v4) v11) (coe v3 (coe v2 v4 v11))
                     (coe v3 (coe v2 v4 v11))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                        (coe v2 (coe v1 v4 v4) v11)
                        (coe v1 (coe v2 v4 v11) (coe v2 v4 v11)) (coe v8 v11 v4 v4))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                        (coe v3 (coe v2 v4 v11)))))
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                  (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                  (coe
                     v1 (coe v1 (coe v2 v4 v11) (coe v2 v4 v11))
                     (coe v3 (coe v2 v4 v11)))
                  (coe
                     v1 (coe v2 v4 v11)
                     (coe v1 (coe v2 v4 v11) (coe v3 (coe v2 v4 v11))))
                  (coe v7 (coe v2 v4 v11) (coe v2 v4 v11) (coe v3 (coe v2 v4 v11)))))
            (coe
               v5 (coe v2 v4 v11) (coe v2 v4 v11) v4
               (coe v1 (coe v2 v4 v11) (coe v3 (coe v2 v4 v11)))
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                  (coe v2 v4 v11))
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                  (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                  (coe v1 (coe v2 v4 v11) (coe v3 (coe v2 v4 v11))) v4
                  (coe v10 (coe v2 v4 v11)))))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
            (coe v1 (coe v2 v4 v11) v4) (coe v2 v4 v11)
            (coe v9 (coe v2 v4 v11))))
-- Algebra.Consequences.Setoid._.assoc+distribˡ+idʳ+invʳ⇒zeʳ
d_assoc'43'distrib'737''43'id'691''43'inv'691''8658'ze'691'_492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_assoc'43'distrib'737''43'id'691''43'inv'691''8658'ze'691'_492 ~v0
                                                                ~v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
                                                                v12 v13
  = du_assoc'43'distrib'737''43'id'691''43'inv'691''8658'ze'691'_492
      v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
du_assoc'43'distrib'737''43'id'691''43'inv'691''8658'ze'691'_492 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_assoc'43'distrib'737''43'id'691''43'inv'691''8658'ze'691'_492 v0
                                                                 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 (coe v2 v11 v4) (coe v1 (coe v2 v11 v4) v4) v4
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0 (coe v1 (coe v2 v11 v4) v4)
            (coe
               v1 (coe v2 v11 v4)
               (coe v1 (coe v2 v11 v4) (coe v3 (coe v2 v11 v4))))
            v4
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0
               (coe
                  v1 (coe v2 v11 v4)
                  (coe v1 (coe v2 v11 v4) (coe v3 (coe v2 v11 v4))))
               (coe
                  v1 (coe v1 (coe v2 v11 v4) (coe v2 v11 v4))
                  (coe v3 (coe v2 v11 v4)))
               v4
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  v0
                  (coe
                     v1 (coe v1 (coe v2 v11 v4) (coe v2 v11 v4))
                     (coe v3 (coe v2 v11 v4)))
                  (coe v1 (coe v2 v11 (coe v1 v4 v4)) (coe v3 (coe v2 v11 v4))) v4
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     v0 (coe v1 (coe v2 v11 (coe v1 v4 v4)) (coe v3 (coe v2 v11 v4)))
                     (coe v1 (coe v2 v11 v4) (coe v3 (coe v2 v11 v4))) v4
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        v0 (coe v1 (coe v2 v11 v4) (coe v3 (coe v2 v11 v4))) v4 v4
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe
                                 MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                           (coe v4))
                        (coe v10 (coe v2 v11 v4)))
                     (coe
                        v5 (coe v2 v11 (coe v1 v4 v4)) (coe v2 v11 v4)
                        (coe v3 (coe v2 v11 v4)) (coe v3 (coe v2 v11 v4))
                        (coe
                           v6 v11 v11 (coe v1 v4 v4) v4
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                              v11)
                           (coe v9 v4))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                           (coe v3 (coe v2 v11 v4)))))
                  (coe
                     v5 (coe v1 (coe v2 v11 v4) (coe v2 v11 v4))
                     (coe v2 v11 (coe v1 v4 v4)) (coe v3 (coe v2 v11 v4))
                     (coe v3 (coe v2 v11 v4))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                        (coe v2 v11 (coe v1 v4 v4))
                        (coe v1 (coe v2 v11 v4) (coe v2 v11 v4)) (coe v8 v11 v4 v4))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                        (coe v3 (coe v2 v11 v4)))))
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                  (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                  (coe
                     v1 (coe v1 (coe v2 v11 v4) (coe v2 v11 v4))
                     (coe v3 (coe v2 v11 v4)))
                  (coe
                     v1 (coe v2 v11 v4)
                     (coe v1 (coe v2 v11 v4) (coe v3 (coe v2 v11 v4))))
                  (coe v7 (coe v2 v11 v4) (coe v2 v11 v4) (coe v3 (coe v2 v11 v4)))))
            (coe
               v5 (coe v2 v11 v4) (coe v2 v11 v4) v4
               (coe v1 (coe v2 v11 v4) (coe v3 (coe v2 v11 v4)))
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                  (coe v2 v11 v4))
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                  (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                  (coe v1 (coe v2 v11 v4) (coe v3 (coe v2 v11 v4))) v4
                  (coe v10 (coe v2 v11 v4)))))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
            (coe v1 (coe v2 v11 v4) v4) (coe v2 v11 v4)
            (coe v9 (coe v2 v11 v4))))
-- Algebra.Consequences.Setoid._.subst+comm⇒sym
d_subst'43'comm'8658'sym_522 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  ((AgdaAny -> ()) ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_subst'43'comm'8658'sym_522 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_subst'43'comm'8658'sym_522 v2 v3 v4 v5 v6 v7
du_subst'43'comm'8658'sym_522 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  ((AgdaAny -> ()) ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_subst'43'comm'8658'sym_522 v0 v1 v2 v3 v4 v5
  = coe v2 v1 (coe v0 v4 v5) (coe v0 v5 v4) (coe v3 v4 v5)
-- Algebra.Consequences.Setoid._.wlog
d_wlog_536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  ((AgdaAny -> ()) ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
d_wlog_536 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 ~v8 ~v9 v10
  = du_wlog_536 v4 v5 v6 v7 v10
du_wlog_536 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  ((AgdaAny -> ()) ->
   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
du_wlog_536 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_wlog_748 (coe v4)
      (coe
         (\ v5 v6 ->
            coe v2 v1 (coe v0 v5 v6) (coe v0 v6 v5) (coe v3 v5 v6)))
