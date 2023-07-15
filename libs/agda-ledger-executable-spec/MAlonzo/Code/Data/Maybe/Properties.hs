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

module MAlonzo.Code.Data.Maybe.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Maybe.Relation.Unary.All
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.Maybe.Properties.just-injective
d_just'45'injective_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_just'45'injective_22 = erased
-- Data.Maybe.Properties.≡-dec
d_'8801''45'dec_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Maybe AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_'8801''45'dec_24 ~v0 ~v1 v2 v3 v4 = du_'8801''45'dec_24 v2 v3 v4
du_'8801''45'dec_24 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Maybe AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_'8801''45'dec_24 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    erased (coe v0 v3 v4)
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Maybe.Properties.map-id
d_map'45'id_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'id_42 = erased
-- Data.Maybe.Properties.map-id-local
d_map'45'id'45'local_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny ->
  MAlonzo.Code.Data.Maybe.Relation.Unary.All.T_All_18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'id'45'local_52 = erased
-- Data.Maybe.Properties.map-<∣>
d_map'45''60''8739''62'_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''60''8739''62'_62 = erased
-- Data.Maybe.Properties.map-cong
d_map'45'cong_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'cong_78 = erased
-- Data.Maybe.Properties.map-cong-local
d_map'45'cong'45'local_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny ->
  MAlonzo.Code.Data.Maybe.Relation.Unary.All.T_All_18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'cong'45'local_94 = erased
-- Data.Maybe.Properties.map-injective
d_map'45'injective_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Maybe AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'injective_100 = erased
-- Data.Maybe.Properties.map-∘
d_map'45''8728'_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''8728'_118 = erased
-- Data.Maybe.Properties.map-nothing
d_map'45'nothing_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'nothing_126 = erased
-- Data.Maybe.Properties.map-just
d_map'45'just_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'just_134 = erased
-- Data.Maybe.Properties.maybe-map
d_maybe'45'map_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (Maybe AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_maybe'45'map_148 = erased
-- Data.Maybe.Properties.maybe′-map
d_maybe'8242''45'map_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_maybe'8242''45'map_172 = erased
-- Data.Maybe.Properties._._.Associative
d_Associative_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny) -> ()
d_Associative_204 = erased
-- Data.Maybe.Properties._._.Idempotent
d_Idempotent_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny) -> ()
d_Idempotent_218 = erased
-- Data.Maybe.Properties._._.Identity
d_Identity_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny ->
  (Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny) -> ()
d_Identity_224 = erased
-- Data.Maybe.Properties._._.LeftIdentity
d_LeftIdentity_250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny ->
  (Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny) -> ()
d_LeftIdentity_250 = erased
-- Data.Maybe.Properties._._.RightIdentity
d_RightIdentity_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny ->
  (Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny) -> ()
d_RightIdentity_280 = erased
-- Data.Maybe.Properties._.<∣>-assoc
d_'60''8739''62''45'assoc_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny ->
  Maybe AgdaAny ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''8739''62''45'assoc_308 = erased
-- Data.Maybe.Properties._.<∣>-identityˡ
d_'60''8739''62''45'identity'737'_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''8739''62''45'identity'737'_320 = erased
-- Data.Maybe.Properties._.<∣>-identityʳ
d_'60''8739''62''45'identity'691'_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''8739''62''45'identity'691'_324 = erased
-- Data.Maybe.Properties._.<∣>-identity
d_'60''8739''62''45'identity_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''8739''62''45'identity_328 ~v0 ~v1
  = du_'60''8739''62''45'identity_328
du_'60''8739''62''45'identity_328 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''8739''62''45'identity_328
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Maybe.Properties._.<∣>-idem
d_'60''8739''62''45'idem_330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''8739''62''45'idem_330 = erased
-- Data.Maybe.Properties._._.IsMagma
d_IsMagma_384 a0 a1 a2 = ()
-- Data.Maybe.Properties._._.IsMonoid
d_IsMonoid_390 a0 a1 a2 a3 = ()
-- Data.Maybe.Properties._._.IsSemigroup
d_IsSemigroup_412 a0 a1 a2 = ()
-- Data.Maybe.Properties._.<∣>-isMagma
d_'60''8739''62''45'isMagma_2644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'60''8739''62''45'isMagma_2644 ~v0 ~v1
  = du_'60''8739''62''45'isMagma_2644
du_'60''8739''62''45'isMagma_2644 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'60''8739''62''45'isMagma_2644
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.Maybe.Properties._.<∣>-isSemigroup
d_'60''8739''62''45'isSemigroup_2646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'60''8739''62''45'isSemigroup_2646 ~v0 ~v1
  = du_'60''8739''62''45'isSemigroup_2646
du_'60''8739''62''45'isSemigroup_2646 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'60''8739''62''45'isSemigroup_2646
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'60''8739''62''45'isMagma_2644) erased
-- Data.Maybe.Properties._.<∣>-isMonoid
d_'60''8739''62''45'isMonoid_2648 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'60''8739''62''45'isMonoid_2648 ~v0 ~v1
  = du_'60''8739''62''45'isMonoid_2648
du_'60''8739''62''45'isMonoid_2648 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'60''8739''62''45'isMonoid_2648
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'60''8739''62''45'isSemigroup_2646)
      (coe du_'60''8739''62''45'identity_328)
-- Data.Maybe.Properties._.<∣>-semigroup
d_'60''8739''62''45'semigroup_2650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'60''8739''62''45'semigroup_2650 ~v0 ~v1
  = du_'60''8739''62''45'semigroup_2650
du_'60''8739''62''45'semigroup_2650 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'60''8739''62''45'semigroup_2650
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      (coe MAlonzo.Code.Data.Maybe.Base.du__'60''8739''62'__84)
      (coe du_'60''8739''62''45'isSemigroup_2646)
-- Data.Maybe.Properties._.<∣>-monoid
d_'60''8739''62''45'monoid_2652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'60''8739''62''45'monoid_2652 ~v0 ~v1
  = du_'60''8739''62''45'monoid_2652
du_'60''8739''62''45'monoid_2652 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'60''8739''62''45'monoid_2652
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      (coe MAlonzo.Code.Data.Maybe.Base.du__'60''8739''62'__84)
      (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
      (coe du_'60''8739''62''45'isMonoid_2648)
-- Data.Maybe.Properties.map-id₂
d_map'45'id'8322'_2654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny ->
  MAlonzo.Code.Data.Maybe.Relation.Unary.All.T_All_18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'id'8322'_2654 = erased
-- Data.Maybe.Properties.map-cong₂
d_map'45'cong'8322'_2656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny ->
  MAlonzo.Code.Data.Maybe.Relation.Unary.All.T_All_18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'cong'8322'_2656 = erased
-- Data.Maybe.Properties.map-compose
d_map'45'compose_2658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'compose_2658 = erased
-- Data.Maybe.Properties.map-<∣>-commute
d_map'45''60''8739''62''45'commute_2660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Maybe AgdaAny ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''60''8739''62''45'commute_2660 = erased
