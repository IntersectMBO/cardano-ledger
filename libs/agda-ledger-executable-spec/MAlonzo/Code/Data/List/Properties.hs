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

module MAlonzo.Code.Data.List.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Divisibility
import qualified MAlonzo.Code.Data.Nat.Divisibility.Core
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.These.Base
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.List.Properties._.∷-injective
d_'8759''45'injective_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8759''45'injective_42 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_'8759''45'injective_42
du_'8759''45'injective_42 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8759''45'injective_42
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.List.Properties._.∷-injectiveˡ
d_'8759''45'injective'737'_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8759''45'injective'737'_44 = erased
-- Data.List.Properties._.∷-injectiveʳ
d_'8759''45'injective'691'_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8759''45'injective'691'_46 = erased
-- Data.List.Properties._.∷-dec
d_'8759''45'dec_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_'8759''45'dec_48 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_'8759''45'dec_48 v6 v7
du_'8759''45'dec_48 ::
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_'8759''45'dec_48 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (coe MAlonzo.Code.Data.Product.Base.du_uncurry_220 erased)
      (coe
         MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
         (coe v0) (coe v1))
-- Data.List.Properties.≡-dec
d_'8801''45'dec_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_'8801''45'dec_54 ~v0 ~v1 v2 v3 v4 = du_'8801''45'dec_54 v2 v3 v4
du_'8801''45'dec_54 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_'8801''45'dec_54 v0 v1 v2
  = case coe v1 of
      []
        -> case coe v2 of
             []
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             (:) v3 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v3 v4
        -> case coe v2 of
             []
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             (:) v5 v6
               -> coe
                    du_'8759''45'dec_48 (coe v0 v3 v5)
                    (coe du_'8801''45'dec_54 (coe v0) (coe v4) (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties.map-id
d_map'45'id_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'id_80 = erased
-- Data.List.Properties.map-id-local
d_map'45'id'45'local_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'id'45'local_94 = erased
-- Data.List.Properties.map-++
d_map'45''43''43'_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''43''43'_106 = erased
-- Data.List.Properties.map-cong
d_map'45'cong_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'cong_126 = erased
-- Data.List.Properties.map-cong-local
d_map'45'cong'45'local_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'cong'45'local_144 = erased
-- Data.List.Properties.length-map
d_length'45'map_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'map_154 = erased
-- Data.List.Properties.map-∘
d_map'45''8728'_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''8728'_168 = erased
-- Data.List.Properties.map-injective
d_map'45'injective_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'injective_178 = erased
-- Data.List.Properties.mapMaybe-just
d_mapMaybe'45'just_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mapMaybe'45'just_202 = erased
-- Data.List.Properties.mapMaybe-nothing
d_mapMaybe'45'nothing_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mapMaybe'45'nothing_214 = erased
-- Data.List.Properties._.mapMaybe-concatMap
d_mapMaybe'45'concatMap_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> Maybe AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mapMaybe'45'concatMap_230 = erased
-- Data.List.Properties._.length-mapMaybe
d_length'45'mapMaybe_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> Maybe AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_length'45'mapMaybe_254 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_length'45'mapMaybe_254 v4 v5
du_length'45'mapMaybe_254 ::
  (AgdaAny -> Maybe AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_length'45'mapMaybe_254 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      (:) v2 v3
        -> let v4 = coe v0 v2 in
           case coe v4 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_length'45'mapMaybe_254 (coe v0) (coe v3))
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe du_length'45'mapMaybe_254 (coe v0) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties.length-++
d_length'45''43''43'_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45''43''43'_278 = erased
-- Data.List.Properties._._.Associative
d_Associative_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> ([AgdaAny] -> [AgdaAny] -> [AgdaAny]) -> ()
d_Associative_314 = erased
-- Data.List.Properties._._.Cancellative
d_Cancellative_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> ([AgdaAny] -> [AgdaAny] -> [AgdaAny]) -> ()
d_Cancellative_316 = erased
-- Data.List.Properties._._.Conical
d_Conical_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> ([AgdaAny] -> [AgdaAny] -> [AgdaAny]) -> ()
d_Conical_324 = erased
-- Data.List.Properties._._.Identity
d_Identity_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> ([AgdaAny] -> [AgdaAny] -> [AgdaAny]) -> ()
d_Identity_334 = erased
-- Data.List.Properties._._.LeftCancellative
d_LeftCancellative_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> ([AgdaAny] -> [AgdaAny] -> [AgdaAny]) -> ()
d_LeftCancellative_348 = erased
-- Data.List.Properties._._.LeftIdentity
d_LeftIdentity_360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> ([AgdaAny] -> [AgdaAny] -> [AgdaAny]) -> ()
d_LeftIdentity_360 = erased
-- Data.List.Properties._._.RightCancellative
d_RightCancellative_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> ([AgdaAny] -> [AgdaAny] -> [AgdaAny]) -> ()
d_RightCancellative_378 = erased
-- Data.List.Properties._._.RightIdentity
d_RightIdentity_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> ([AgdaAny] -> [AgdaAny] -> [AgdaAny]) -> ()
d_RightIdentity_390 = erased
-- Data.List.Properties._._.IsMagma
d_IsMagma_460 a0 a1 a2 = ()
-- Data.List.Properties._._.IsMonoid
d_IsMonoid_466 a0 a1 a2 a3 = ()
-- Data.List.Properties._._.IsSemigroup
d_IsSemigroup_488 a0 a1 a2 = ()
-- Data.List.Properties._.++-assoc
d_'43''43''45'assoc_2720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'assoc_2720 = erased
-- Data.List.Properties._.++-identityˡ
d_'43''43''45'identity'737'_2736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'identity'737'_2736 = erased
-- Data.List.Properties._.++-identityʳ
d_'43''43''45'identity'691'_2740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'identity'691'_2740 = erased
-- Data.List.Properties._.++-identity
d_'43''43''45'identity_2748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''43''45'identity_2748 ~v0 ~v1 = du_'43''43''45'identity_2748
du_'43''43''45'identity_2748 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'43''43''45'identity_2748
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.List.Properties._.++-identityʳ-unique
d_'43''43''45'identity'691''45'unique_2754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'identity'691''45'unique_2754 = erased
-- Data.List.Properties._.++-identityˡ-unique
d_'43''43''45'identity'737''45'unique_2766 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'identity'737''45'unique_2766 = erased
-- Data.List.Properties._.++-cancelˡ
d_'43''43''45'cancel'737'_2798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'cancel'737'_2798 = erased
-- Data.List.Properties._.++-cancelʳ
d_'43''43''45'cancel'691'_2808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'cancel'691'_2808 = erased
-- Data.List.Properties._.++-cancel
d_'43''43''45'cancel_2836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''43''45'cancel_2836 ~v0 ~v1 = du_'43''43''45'cancel_2836
du_'43''43''45'cancel_2836 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'43''43''45'cancel_2836
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.List.Properties._.++-conicalˡ
d_'43''43''45'conical'737'_2842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'conical'737'_2842 = erased
-- Data.List.Properties._.++-conicalʳ
d_'43''43''45'conical'691'_2848 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'conical'691'_2848 = erased
-- Data.List.Properties._.++-conical
d_'43''43''45'conical_2850 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''43''45'conical_2850 ~v0 ~v1 = du_'43''43''45'conical_2850
du_'43''43''45'conical_2850 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'43''43''45'conical_2850
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.List.Properties._.++-isMagma
d_'43''43''45'isMagma_2852 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'43''43''45'isMagma_2852 ~v0 ~v1 = du_'43''43''45'isMagma_2852
du_'43''43''45'isMagma_2852 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'43''43''45'isMagma_2852
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.List.Properties._.++-isSemigroup
d_'43''43''45'isSemigroup_2854 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'43''43''45'isSemigroup_2854 ~v0 ~v1
  = du_'43''43''45'isSemigroup_2854
du_'43''43''45'isSemigroup_2854 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'43''43''45'isSemigroup_2854
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'43''43''45'isMagma_2852) erased
-- Data.List.Properties._.++-isMonoid
d_'43''43''45'isMonoid_2856 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''43''45'isMonoid_2856 ~v0 ~v1 = du_'43''43''45'isMonoid_2856
du_'43''43''45'isMonoid_2856 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'43''43''45'isMonoid_2856
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'43''43''45'isSemigroup_2854)
      (coe du_'43''43''45'identity_2748)
-- Data.List.Properties._.++-semigroup
d_'43''43''45'semigroup_2866 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'43''43''45'semigroup_2866 ~v0 ~v1
  = du_'43''43''45'semigroup_2866
du_'43''43''45'semigroup_2866 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'43''43''45'semigroup_2866
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe du_'43''43''45'isSemigroup_2854)
-- Data.List.Properties._.++-monoid
d_'43''43''45'monoid_2868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'43''43''45'monoid_2868 ~v0 ~v1 = du_'43''43''45'monoid_2868
du_'43''43''45'monoid_2868 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'43''43''45'monoid_2868
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe du_'43''43''45'isMonoid_2856)
-- Data.List.Properties._.length-isMagmaHomomorphism
d_length'45'isMagmaHomomorphism_2878 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_length'45'isMagmaHomomorphism_2878 ~v0 ~v1
  = du_length'45'isMagmaHomomorphism_2878
du_length'45'isMagmaHomomorphism_2878 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
du_length'45'isMagmaHomomorphism_2878
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaHomomorphism'46'constructor_1049
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsRelHomomorphism'46'constructor_587
         erased)
      erased
-- Data.List.Properties._.length-isMonoidHomomorphism
d_length'45'isMonoidHomomorphism_2884 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidHomomorphism_266
d_length'45'isMonoidHomomorphism_2884 ~v0 ~v1
  = du_length'45'isMonoidHomomorphism_2884
du_length'45'isMonoidHomomorphism_2884 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidHomomorphism_266
du_length'45'isMonoidHomomorphism_2884
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMonoidHomomorphism'46'constructor_5533
      (coe du_length'45'isMagmaHomomorphism_2878) erased
-- Data.List.Properties._.prod
d_prod_2898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d_prod_2898 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_prod_2898 v6
du_prod_2898 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du_prod_2898 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_cartesianProductWith_100 (coe v0)
-- Data.List.Properties._.cartesianProductWith-zeroˡ
d_cartesianProductWith'45'zero'737'_2902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cartesianProductWith'45'zero'737'_2902 = erased
-- Data.List.Properties._.cartesianProductWith-zeroʳ
d_cartesianProductWith'45'zero'691'_2906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cartesianProductWith'45'zero'691'_2906 = erased
-- Data.List.Properties._.cartesianProductWith-distribʳ-++
d_cartesianProductWith'45'distrib'691''45''43''43'_2918 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cartesianProductWith'45'distrib'691''45''43''43'_2918 = erased
-- Data.List.Properties._.alignWith-cong
d_alignWith'45'cong_2950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  (MAlonzo.Code.Data.These.Base.T_These_38 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_alignWith'45'cong_2950 = erased
-- Data.List.Properties._.length-alignWith
d_length'45'alignWith_2974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'alignWith_2974 = erased
-- Data.List.Properties._.alignWith-map
d_alignWith'45'map_2996 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_alignWith'45'map_2996 = erased
-- Data.List.Properties._.map-alignWith
d_map'45'alignWith_3028 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'alignWith_3028 = erased
-- Data.List.Properties._.zipWith-comm
d_zipWith'45'comm_3066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zipWith'45'comm_3066 = erased
-- Data.List.Properties._.zipWith-zeroˡ
d_zipWith'45'zero'737'_3106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zipWith'45'zero'737'_3106 = erased
-- Data.List.Properties._.zipWith-zeroʳ
d_zipWith'45'zero'691'_3114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zipWith'45'zero'691'_3114 = erased
-- Data.List.Properties._.length-zipWith
d_length'45'zipWith_3124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'zipWith_3124 = erased
-- Data.List.Properties._.zipWith-map
d_zipWith'45'map_3162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zipWith'45'map_3162 = erased
-- Data.List.Properties._.map-zipWith
d_map'45'zipWith_3210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'zipWith_3210 = erased
-- Data.List.Properties.unalignWith-this
d_unalignWith'45'this_3238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unalignWith'45'this_3238 = erased
-- Data.List.Properties.unalignWith-that
d_unalignWith'45'that_3248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unalignWith'45'that_3248 = erased
-- Data.List.Properties._.unalignWith-cong
d_unalignWith'45'cong_3270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unalignWith'45'cong_3270 = erased
-- Data.List.Properties._.unalignWith-map
d_unalignWith'45'map_3334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unalignWith'45'map_3334 = erased
-- Data.List.Properties._.map-unalignWith
d_map'45'unalignWith_3386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'unalignWith_3386 = erased
-- Data.List.Properties._.unalignWith-alignWith
d_unalignWith'45'alignWith_3450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  (MAlonzo.Code.Data.These.Base.T_These_38 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unalignWith'45'alignWith_3450 = erased
-- Data.List.Properties._.length-unzipWith₁
d_length'45'unzipWith'8321'_3498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'unzipWith'8321'_3498 = erased
-- Data.List.Properties._.length-unzipWith₂
d_length'45'unzipWith'8322'_3506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'unzipWith'8322'_3506 = erased
-- Data.List.Properties._.zipWith-unzipWith
d_zipWith'45'unzipWith_3514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zipWith'45'unzipWith_3514 = erased
-- Data.List.Properties.foldr-universal
d_foldr'45'universal_3538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  ([AgdaAny] -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  (AgdaAny ->
   [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldr'45'universal_3538 = erased
-- Data.List.Properties.foldr-cong
d_foldr'45'cong_3576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldr'45'cong_3576 = erased
-- Data.List.Properties.foldr-fusion
d_foldr'45'fusion_3604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldr'45'fusion_3604 = erased
-- Data.List.Properties.id-is-foldr
d_id'45'is'45'foldr_3620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_id'45'is'45'foldr_3620 = erased
-- Data.List.Properties.++-is-foldr
d_'43''43''45'is'45'foldr_3630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''43''45'is'45'foldr_3630 = erased
-- Data.List.Properties.foldr-++
d_foldr'45''43''43'_3652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldr'45''43''43'_3652 = erased
-- Data.List.Properties.map-is-foldr
d_map'45'is'45'foldr_3676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'is'45'foldr_3676 = erased
-- Data.List.Properties.foldr-∷ʳ
d_foldr'45''8759''691'_3698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldr'45''8759''691'_3698 = erased
-- Data.List.Properties.foldr-map
d_foldr'45'map_3724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldr'45'map_3724 = erased
-- Data.List.Properties._.foldr-forcesᵇ
d_foldr'45'forces'7495'_3762 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_foldr'45'forces'7495'_3762 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_foldr'45'forces'7495'_3762 v4 v5 v6 v7 v8
du_foldr'45'forces'7495'_3762 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_foldr'45'forces'7495'_3762 v0 v1 v2 v3 v4
  = case coe v3 of
      [] -> coe MAlonzo.Code.Data.List.Relation.Unary.All.C_'91''93'_50
      (:) v5 v6
        -> let v7
                 = coe
                     v1 v5
                     (coe
                        MAlonzo.Code.Data.List.Base.du_foldr_242 (coe v0) (coe v2)
                        (coe v6))
                     v4 in
           case coe v7 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
               -> coe
                    MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v8
                    (coe
                       du_foldr'45'forces'7495'_3762 (coe v0) (coe v1) (coe v2) (coe v6)
                       (coe v9))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._.foldr-preservesᵇ
d_foldr'45'preserves'7495'_3796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_foldr'45'preserves'7495'_3796 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
  = du_foldr'45'preserves'7495'_3796 v4 v5 v6 v7 v8 v9
du_foldr'45'preserves'7495'_3796 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_foldr'45'preserves'7495'_3796 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      MAlonzo.Code.Data.List.Relation.Unary.All.C_'91''93'_50 -> coe v4
      MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v8 v9
        -> case coe v3 of
             (:) v10 v11
               -> coe
                    v1 v10
                    (coe
                       MAlonzo.Code.Data.List.Base.du_foldr_242 (coe v0) (coe v2)
                       (coe v11))
                    v8
                    (coe
                       du_foldr'45'preserves'7495'_3796 (coe v0) (coe v1) (coe v2)
                       (coe v11) (coe v4) (coe v9))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._.foldr-preservesʳ
d_foldr'45'preserves'691'_3816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> [AgdaAny] -> AgdaAny
d_foldr'45'preserves'691'_3816 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_foldr'45'preserves'691'_3816 v4 v5 v6 v7 v8
du_foldr'45'preserves'691'_3816 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> [AgdaAny] -> AgdaAny
du_foldr'45'preserves'691'_3816 v0 v1 v2 v3 v4
  = case coe v4 of
      [] -> coe v3
      (:) v5 v6
        -> coe
             v1 v5
             (coe
                MAlonzo.Code.Data.List.Base.du_foldr_242 (coe v0) (coe v2)
                (coe v6))
             (coe
                du_foldr'45'preserves'691'_3816 (coe v0) (coe v1) (coe v2) (coe v3)
                (coe v6))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._.foldr-preservesᵒ
d_foldr'45'preserves'7506'_3836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_foldr'45'preserves'7506'_3836 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_foldr'45'preserves'7506'_3836 v4 v5 v6 v7 v8
du_foldr'45'preserves'7506'_3836 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_foldr'45'preserves'7506'_3836 v0 v1 v2 v3 v4
  = case coe v3 of
      []
        -> case coe v4 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5 -> coe v5
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v5 v6
        -> case coe v4 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7
               -> coe
                    v1 v5
                    (coe
                       MAlonzo.Code.Data.List.Base.du_foldr_242 (coe v0) (coe v2)
                       (coe v6))
                    (coe
                       MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                       (coe
                          du_foldr'45'preserves'7506'_3836 (coe v0) (coe v1) (coe v2)
                          (coe v6) (coe v4)))
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7
               -> case coe v7 of
                    MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v10
                      -> coe
                           v1 v5
                           (coe
                              MAlonzo.Code.Data.List.Base.du_foldr_242 (coe v0) (coe v2)
                              (coe v6))
                           (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v10))
                    MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v10
                      -> coe
                           v1 v5
                           (coe
                              MAlonzo.Code.Data.List.Base.du_foldr_242 (coe v0) (coe v2)
                              (coe v6))
                           (coe
                              MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                              (coe
                                 du_foldr'45'preserves'7506'_3836 (coe v0) (coe v1) (coe v2)
                                 (coe v6)
                                 (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 (coe v10))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties.foldl-++
d_foldl'45''43''43'_3882 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldl'45''43''43'_3882 = erased
-- Data.List.Properties.foldl-∷ʳ
d_foldl'45''8759''691'_3908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldl'45''8759''691'_3908 = erased
-- Data.List.Properties.foldl-map
d_foldl'45'map_3934 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldl'45'map_3934 = erased
-- Data.List.Properties.concat-map
d_concat'45'map_3954 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [[AgdaAny]] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_concat'45'map_3954 = erased
-- Data.List.Properties.concat-++
d_concat'45''43''43'_3976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [[AgdaAny]] ->
  [[AgdaAny]] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_concat'45''43''43'_3976 = erased
-- Data.List.Properties.concat-concat
d_concat'45'concat_3994 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [[[AgdaAny]]] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_concat'45'concat_3994 = erased
-- Data.List.Properties.concat-[-]
d_concat'45''91''45''93'_4002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_concat'45''91''45''93'_4002 = erased
-- Data.List.Properties.concatMap-cong
d_concatMap'45'cong_4014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> [AgdaAny]) ->
  (AgdaAny -> [AgdaAny]) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_concatMap'45'cong_4014 = erased
-- Data.List.Properties.concatMap-pure
d_concatMap'45'pure_4020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_concatMap'45'pure_4020 = erased
-- Data.List.Properties.concatMap-map
d_concatMap'45'map_4028 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> [AgdaAny]) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_concatMap'45'map_4028 = erased
-- Data.List.Properties.map-concatMap
d_map'45'concatMap_4040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> [AgdaAny]) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'concatMap_4040 = erased
-- Data.List.Properties.sum-++
d_sum'45''43''43'_4052 ::
  [Integer] ->
  [Integer] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sum'45''43''43'_4052 = erased
-- Data.List.Properties.∈⇒∣product
d_'8712''8658''8739'product_4068 ::
  Integer ->
  [Integer] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_'8712''8658''8739'product_4068 ~v0 v1 v2
  = du_'8712''8658''8739'product_4068 v1 v2
du_'8712''8658''8739'product_4068 ::
  [Integer] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_'8712''8658''8739'product_4068 v0 v1
  = case coe v0 of
      (:) v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v6
               -> coe
                    MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26
                    (coe MAlonzo.Code.Data.List.Base.d_product_302 v3)
             MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v6
               -> coe
                    MAlonzo.Code.Data.Nat.Divisibility.du_'8739'n'8658''8739'm'42'n_310
                    v2 (coe du_'8712''8658''8739'product_4068 (coe v3) (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties.length-replicate
d_length'45'replicate_4086 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'replicate_4086 = erased
-- Data.List.Properties.scanr-defn
d_scanr'45'defn_4094 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_scanr'45'defn_4094 = erased
-- Data.List.Properties.scanl-defn
d_scanl'45'defn_4176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_scanl'45'defn_4176 = erased
-- Data.List.Properties.length-applyUpTo
d_length'45'applyUpTo_4200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (Integer -> AgdaAny) ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'applyUpTo_4200 = erased
-- Data.List.Properties.lookup-applyUpTo
d_lookup'45'applyUpTo_4214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (Integer -> AgdaAny) ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lookup'45'applyUpTo_4214 = erased
-- Data.List.Properties._.length-applyDownFrom
d_length'45'applyDownFrom_4236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (Integer -> AgdaAny) ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'applyDownFrom_4236 = erased
-- Data.List.Properties._.lookup-applyDownFrom
d_lookup'45'applyDownFrom_4244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (Integer -> AgdaAny) ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lookup'45'applyDownFrom_4244 = erased
-- Data.List.Properties.length-upTo
d_length'45'upTo_4254 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'upTo_4254 = erased
-- Data.List.Properties.lookup-upTo
d_lookup'45'upTo_4260 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lookup'45'upTo_4260 = erased
-- Data.List.Properties.length-downFrom
d_length'45'downFrom_4264 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'downFrom_4264 = erased
-- Data.List.Properties.lookup-downFrom
d_lookup'45'downFrom_4270 ::
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lookup'45'downFrom_4270 = erased
-- Data.List.Properties.tabulate-cong
d_tabulate'45'cong_4278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_tabulate'45'cong_4278 = erased
-- Data.List.Properties.tabulate-lookup
d_tabulate'45'lookup_4288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_tabulate'45'lookup_4288 = erased
-- Data.List.Properties.length-tabulate
d_length'45'tabulate_4300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'tabulate_4300 = erased
-- Data.List.Properties.lookup-tabulate
d_lookup'45'tabulate_4318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lookup'45'tabulate_4318 = erased
-- Data.List.Properties.map-tabulate
d_map'45'tabulate_4332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'tabulate_4332 = erased
-- Data.List.Properties.length-%=
d_length'45''37''61'_4352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45''37''61'_4352 = erased
-- Data.List.Properties.length-∷=
d_length'45''8759''61'_4374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45''8759''61'_4374 = erased
-- Data.List.Properties.map-∷=
d_map'45''8759''61'_4392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''8759''61'_4392 = erased
-- Data.List.Properties.length-─
d_length'45''9472'_4418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45''9472'_4418 = erased
-- Data.List.Properties.map-─
d_map'45''9472'_4440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''9472'_4440 = erased
-- Data.List.Properties.length-take
d_length'45'take_4462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'take_4462 = erased
-- Data.List.Properties.length-drop
d_length'45'drop_4478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'drop_4478 = erased
-- Data.List.Properties.take++drop
d_take'43''43'drop_4494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_take'43''43'drop_4494 = erased
-- Data.List.Properties.splitAt-defn
d_splitAt'45'defn_4510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_splitAt'45'defn_4510 = erased
-- Data.List.Properties._.takeWhile++dropWhile
d_takeWhile'43''43'dropWhile_4554 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_takeWhile'43''43'dropWhile_4554 = erased
-- Data.List.Properties._.span-defn
d_span'45'defn_4574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_span'45'defn_4574 = erased
-- Data.List.Properties._.length-filter
d_length'45'filter_4608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_length'45'filter_4608 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_length'45'filter_4608 v4 v5
du_length'45'filter_4608 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_length'45'filter_4608 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      (:) v2 v3
        -> let v4
                 = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                     (coe v0 v2) in
           if coe v4
             then coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_length'45'filter_4608 (coe v0) (coe v3))
             else coe du_length'45'filter_4608 (coe v0) (coe v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._.filter-all
d_filter'45'all_4628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_filter'45'all_4628 = erased
-- Data.List.Properties._.filter-notAll
d_filter'45'notAll_4664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_filter'45'notAll_4664 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_filter'45'notAll_4664 v4 v5 v6
du_filter'45'notAll_4664 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_filter'45'notAll_4664 v0 v1 v2
  = case coe v1 of
      (:) v3 v4
        -> case coe v2 of
             MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v7
               -> let v8 = coe v0 v3 in
                  case coe v8 of
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
                      -> if coe v9
                           then coe
                                  seq (coe v10)
                                  (coe
                                     MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38)
                           else coe
                                  MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                                  (coe du_length'45'filter_4608 (coe v0) (coe v4))
                    _ -> MAlonzo.RTE.mazUnreachableError
             MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v7
               -> let v8
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v3) in
                  if coe v8
                    then coe
                           MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                           (coe du_filter'45'notAll_4664 (coe v0) (coe v4) (coe v7))
                    else coe du_filter'45'notAll_4664 (coe v0) (coe v4) (coe v7)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._.filter-some
d_filter'45'some_4714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_filter'45'some_4714 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_filter'45'some_4714 v4 v5 v6
du_filter'45'some_4714 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_filter'45'some_4714 v0 v1 v2
  = case coe v1 of
      (:) v3 v4
        -> case coe v2 of
             MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v7
               -> let v8 = coe v0 v3 in
                  case coe v8 of
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
                      -> if coe v9
                           then coe
                                  MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                                  (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
                           else coe
                                  seq (coe v10)
                                  (coe
                                     MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38)
                    _ -> MAlonzo.RTE.mazUnreachableError
             MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v7
               -> let v8
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v3) in
                  coe
                    seq (coe v8)
                    (coe du_filter'45'some_4714 (coe v0) (coe v4) (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._.filter-none
d_filter'45'none_4764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_filter'45'none_4764 = erased
-- Data.List.Properties._.filter-complete
d_filter'45'complete_4798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_filter'45'complete_4798 = erased
-- Data.List.Properties._.filter-accept
d_filter'45'accept_4830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_filter'45'accept_4830 = erased
-- Data.List.Properties._.filter-reject
d_filter'45'reject_4854 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  [AgdaAny] ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_filter'45'reject_4854 = erased
-- Data.List.Properties._.filter-idem
d_filter'45'idem_4874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_filter'45'idem_4874 = erased
-- Data.List.Properties._.filter-++
d_filter'45''43''43'_4904 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_filter'45''43''43'_4904 = erased
-- Data.List.Properties._.length-derun
d_length'45'derun_4946 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_length'45'derun_4946 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_length'45'derun_4946 v4 v5
du_length'45'derun_4946 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_length'45'derun_4946 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
             (coe
                MAlonzo.Code.Data.List.Base.du_length_304
                (coe MAlonzo.Code.Data.List.Base.du_derun_828 v0 v1))
      (:) v2 v3
        -> case coe v3 of
             []
               -> coe
                    MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
                    (coe
                       MAlonzo.Code.Data.List.Base.du_length_304
                       (coe MAlonzo.Code.Data.List.Base.du_derun_828 v0 v1))
             (:) v4 v5
               -> let v6
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v2 v4) in
                  let v7 = coe du_length'45'derun_4946 (coe v0) (coe v3) in
                  if coe v6
                    then coe v7
                    else coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._.length-deduplicate
d_length'45'deduplicate_4978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_length'45'deduplicate_4978 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_length'45'deduplicate_4978 v4 v5
du_length'45'deduplicate_4978 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_length'45'deduplicate_4978 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      (:) v2 v3
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
             (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
             (\ v4 v5 v6 ->
                coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v6)
             (coe
                MAlonzo.Code.Data.List.Base.du_length_304
                (coe MAlonzo.Code.Data.List.Base.du_deduplicate_834 v0 v1))
             (coe MAlonzo.Code.Data.List.Base.du_length_304 v1)
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                (\ v4 v5 v6 v7 v8 ->
                   coe
                     MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v7 v8)
                (coe
                   addInt (coe (1 :: Integer))
                   (coe
                      MAlonzo.Code.Data.List.Base.du_length_304
                      (coe
                         MAlonzo.Code.Data.List.Base.du_filter_792
                         (\ v4 ->
                            coe
                              MAlonzo.Code.Relation.Nullary.Decidable.Core.du_'172''63'_56
                              (coe v0 v2 v4))
                         (coe du_r_4988 (coe v0) (coe v3)))))
                (coe
                   addInt (coe (1 :: Integer))
                   (coe
                      MAlonzo.Code.Data.List.Base.du_length_304
                      (coe du_r_4988 (coe v0) (coe v3))))
                (coe MAlonzo.Code.Data.List.Base.du_length_304 v1)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                   (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                   (\ v4 v5 v6 v7 v8 ->
                      coe
                        MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v7 v8)
                   (coe
                      addInt (coe (1 :: Integer))
                      (coe
                         MAlonzo.Code.Data.List.Base.du_length_304
                         (coe du_r_4988 (coe v0) (coe v3))))
                   (coe
                      addInt (coe (1 :: Integer))
                      (coe MAlonzo.Code.Data.List.Base.du_length_304 v3))
                   (coe MAlonzo.Code.Data.List.Base.du_length_304 v1)
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                      (coe
                         addInt (coe (1 :: Integer))
                         (coe MAlonzo.Code.Data.List.Base.du_length_304 v3)))
                   (coe
                      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                      (coe du_length'45'deduplicate_4978 (coe v0) (coe v3))))
                (coe
                   MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                   (coe
                      du_length'45'filter_4608
                      (coe
                         (\ v4 ->
                            coe
                              MAlonzo.Code.Relation.Nullary.Decidable.Core.du_'172''63'_56
                              (coe v0 v2 v4)))
                      (coe du_r_4988 (coe v0) (coe v3)))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._._.r
d_r_4988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny -> [AgdaAny] -> [AgdaAny]
d_r_4988 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 = du_r_4988 v4 v6
du_r_4988 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
du_r_4988 v0 v1
  = coe MAlonzo.Code.Data.List.Base.du_deduplicate_834 v0 v1
-- Data.List.Properties._.derun-reject
d_derun'45'reject_4996 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_derun'45'reject_4996 = erased
-- Data.List.Properties._.derun-accept
d_derun'45'accept_5034 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_derun'45'accept_5034 = erased
-- Data.List.Properties._.partition-defn
d_partition'45'defn_5078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_partition'45'defn_5078 = erased
-- Data.List.Properties._.length-partition
d_length'45'partition_5106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_length'45'partition_5106 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_length'45'partition_5106 v4 v5
du_length'45'partition_5106 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_length'45'partition_5106 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      (:) v2 v3
        -> let v4
                 = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                     (coe v0 v2) in
           let v5 = coe du_length'45'partition_5106 (coe v0) (coe v3) in
           if coe v4
             then coe
                    MAlonzo.Code.Data.Product.Base.du_map_104
                    (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30)
                    (coe (\ v6 v7 -> v7)) (coe v5)
             else coe
                    MAlonzo.Code.Data.Product.Base.du_map_104 (coe (\ v6 -> v6))
                    (coe (\ v6 -> coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30))
                    (coe v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties.ʳ++-defn
d_'691''43''43''45'defn_5132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'691''43''43''45'defn_5132 = erased
-- Data.List.Properties.ʳ++-++
d_'691''43''43''45''43''43'_5148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'691''43''43''45''43''43'_5148 = erased
-- Data.List.Properties.ʳ++-ʳ++
d_'691''43''43''45''691''43''43'_5164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'691''43''43''45''691''43''43'_5164 = erased
-- Data.List.Properties.length-ʳ++
d_length'45''691''43''43'_5178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45''691''43''43'_5178 = erased
-- Data.List.Properties.map-ʳ++
d_map'45''691''43''43'_5192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''691''43''43'_5192 = erased
-- Data.List.Properties.foldr-ʳ++
d_foldr'45''691''43''43'_5212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldr'45''691''43''43'_5212 = erased
-- Data.List.Properties.foldl-ʳ++
d_foldl'45''691''43''43'_5236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_foldl'45''691''43''43'_5236 = erased
-- Data.List.Properties.unfold-reverse
d_unfold'45'reverse_5256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unfold'45'reverse_5256 = erased
-- Data.List.Properties.reverse-++
d_reverse'45''43''43'_5266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reverse'45''43''43'_5266 = erased
-- Data.List.Properties.reverse-involutive
d_reverse'45'involutive_5272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reverse'45'involutive_5272 = erased
-- Data.List.Properties.reverse-injective
d_reverse'45'injective_5280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reverse'45'injective_5280 = erased
-- Data.List.Properties.length-reverse
d_length'45'reverse_5284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_length'45'reverse_5284 = erased
-- Data.List.Properties.reverse-map
d_reverse'45'map_5290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reverse'45'map_5290 = erased
-- Data.List.Properties.reverse-foldr
d_reverse'45'foldr_5300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reverse'45'foldr_5300 = erased
-- Data.List.Properties.reverse-foldl
d_reverse'45'foldl_5314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reverse'45'foldl_5314 = erased
-- Data.List.Properties._.∷ʳ-injective
d_'8759''691''45'injective_5336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8759''691''45'injective_5336 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6
  = du_'8759''691''45'injective_5336 v4 v5
du_'8759''691''45'injective_5336 ::
  [AgdaAny] -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8759''691''45'injective_5336 v0 v1
  = case coe v0 of
      []
        -> coe
             seq (coe v1)
             (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
      (:) v2 v3
        -> case coe v1 of
             (:) v4 v5
               -> let v6 = coe du_'8759''45'injective_42 in
                  coe
                    seq (coe v6)
                    (coe
                       MAlonzo.Code.Data.Product.Base.du_map_104 erased
                       (coe (\ v7 v8 -> v8))
                       (coe du_'8759''691''45'injective_5336 (coe v3) (coe v5)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Properties._.∷ʳ-injectiveˡ
d_'8759''691''45'injective'737'_5370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8759''691''45'injective'737'_5370 = erased
-- Data.List.Properties._.∷ʳ-injectiveʳ
d_'8759''691''45'injective'691'_5382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8759''691''45'injective'691'_5382 = erased
-- Data.List.Properties.∷ʳ-++
d_'8759''691''45''43''43'_5396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8759''691''45''43''43'_5396 = erased
-- Data.List.Properties.map-id₂
d_map'45'id'8322'_5404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'id'8322'_5404 = erased
-- Data.List.Properties.map-cong₂
d_map'45'cong'8322'_5406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'cong'8322'_5406 = erased
-- Data.List.Properties.map-compose
d_map'45'compose_5408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45'compose_5408 = erased
-- Data.List.Properties.map-++-commute
d_map'45''43''43''45'commute_5410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_map'45''43''43''45'commute_5410 = erased
-- Data.List.Properties.sum-++-commute
d_sum'45''43''43''45'commute_5412 ::
  [Integer] ->
  [Integer] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sum'45''43''43''45'commute_5412 = erased
-- Data.List.Properties.reverse-map-commute
d_reverse'45'map'45'commute_5414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reverse'45'map'45'commute_5414 = erased
-- Data.List.Properties.reverse-++-commute
d_reverse'45''43''43''45'commute_5416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reverse'45''43''43''45'commute_5416 = erased
-- Data.List.Properties.zipWith-identityˡ
d_zipWith'45'identity'737'_5418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zipWith'45'identity'737'_5418 = erased
-- Data.List.Properties.zipWith-identityʳ
d_zipWith'45'identity'691'_5420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zipWith'45'identity'691'_5420 = erased
