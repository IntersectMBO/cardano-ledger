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

module MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Membership.Propositional.Properties
import qualified MAlonzo.Code.Data.List.Properties
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.List.Relation.Binary.Permutation.Propositional.Properties.↭-empty-inv
d_'8621''45'empty'45'inv_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8621''45'empty'45'inv_26 = erased
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.¬x∷xs↭[]
d_'172'x'8759'xs'8621''91''93'_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'172'x'8759'xs'8621''91''93'_40 = erased
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.↭-singleton-inv
d_'8621''45'singleton'45'inv_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8621''45'singleton'45'inv_58 = erased
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.↭-sym-involutive
d_'8621''45'sym'45'involutive_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8621''45'sym'45'involutive_80 = erased
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.All-resp-↭
d_All'45'resp'45''8621'_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_All'45'resp'45''8621'_98 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_All'45'resp'45''8621'_98 v4 v5 v6 v7
du_All'45'resp'45''8621'_98 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_All'45'resp'45''8621'_98 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
        -> coe v3
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28 v7
        -> case coe v0 of
             (:) v8 v9
               -> case coe v1 of
                    (:) v10 v11
                      -> case coe v3 of
                           MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v14 v15
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v14
                                  (coe
                                     du_All'45'resp'45''8621'_98 (coe v9) (coe v11) (coe v7)
                                     (coe v15))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38 v8
        -> case coe v0 of
             (:) v9 v10
               -> case coe v10 of
                    (:) v11 v12
                      -> case coe v1 of
                           (:) v13 v14
                             -> case coe v14 of
                                  (:) v15 v16
                                    -> case coe v3 of
                                         MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v19 v20
                                           -> case coe v20 of
                                                MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v23 v24
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60
                                                       v23
                                                       (coe
                                                          MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60
                                                          v19
                                                          (coe
                                                             du_All'45'resp'45''8621'_98 (coe v12)
                                                             (coe v16) (coe v8) (coe v24)))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46 v5 v7 v8
        -> coe
             du_All'45'resp'45''8621'_98 (coe v5) (coe v1) (coe v8)
             (coe
                du_All'45'resp'45''8621'_98 (coe v0) (coe v5) (coe v7) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.Any-resp-↭
d_Any'45'resp'45''8621'_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
d_Any'45'resp'45''8621'_130 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_Any'45'resp'45''8621'_130 v4 v5 v6 v7
du_Any'45'resp'45''8621'_130 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
du_Any'45'resp'45''8621'_130 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
        -> coe v3
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28 v7
        -> case coe v0 of
             (:) v8 v9
               -> case coe v1 of
                    (:) v10 v11
                      -> case coe v3 of
                           MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v14
                             -> coe MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v14
                           MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v14
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                  (coe
                                     du_Any'45'resp'45''8621'_130 (coe v9) (coe v11) (coe v7)
                                     (coe v14))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38 v8
        -> case coe v0 of
             (:) v9 v10
               -> case coe v10 of
                    (:) v11 v12
                      -> case coe v1 of
                           (:) v13 v14
                             -> case coe v14 of
                                  (:) v15 v16
                                    -> case coe v3 of
                                         MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v19
                                           -> coe
                                                MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                                (coe
                                                   MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46
                                                   v19)
                                         MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v19
                                           -> case coe v19 of
                                                MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v22
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46
                                                       v22
                                                MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v22
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                                       (coe
                                                          MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                                          (coe
                                                             du_Any'45'resp'45''8621'_130 (coe v12)
                                                             (coe v16) (coe v8) (coe v22)))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46 v5 v7 v8
        -> coe
             du_Any'45'resp'45''8621'_130 (coe v5) (coe v1) (coe v8)
             (coe
                du_Any'45'resp'45''8621'_130 (coe v0) (coe v5) (coe v7) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.∈-resp-↭
d_'8712''45'resp'45''8621'_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
d_'8712''45'resp'45''8621'_180 ~v0 ~v1 ~v2 v3 v4
  = du_'8712''45'resp'45''8621'_180 v3 v4
du_'8712''45'resp'45''8621'_180 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
du_'8712''45'resp'45''8621'_180 v0 v1
  = coe du_Any'45'resp'45''8621'_130 (coe v0) (coe v1)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.Any-resp-[σ⁻¹∘σ]
d_Any'45'resp'45''91'σ'8315''185''8728'σ'93'_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_Any'45'resp'45''91'σ'8315''185''8728'σ'93'_192 = erased
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.∈-resp-[σ⁻¹∘σ]
d_'8712''45'resp'45''91'σ'8315''185''8728'σ'93'_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8712''45'resp'45''91'σ'8315''185''8728'σ'93'_236 = erased
-- Data.List.Relation.Binary.Permutation.Propositional.Properties._.map⁺
d_map'8314'_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_map'8314'_252 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_map'8314'_252 v4 v5 v6 v7
du_map'8314'_252 ::
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_map'8314'_252 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28 v7
        -> case coe v1 of
             (:) v8 v9
               -> case coe v2 of
                    (:) v10 v11
                      -> coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                           (coe du_map'8314'_252 (coe v0) (coe v9) (coe v11) (coe v7))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38 v8
        -> case coe v1 of
             (:) v9 v10
               -> case coe v10 of
                    (:) v11 v12
                      -> case coe v2 of
                           (:) v13 v14
                             -> case coe v14 of
                                  (:) v15 v16
                                    -> coe
                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38
                                         (coe
                                            du_map'8314'_252 (coe v0) (coe v12) (coe v16) (coe v8))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46 v5 v7 v8
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
             (coe MAlonzo.Code.Data.List.Base.du_map_22 (coe v0) (coe v5))
             (coe du_map'8314'_252 (coe v0) (coe v1) (coe v5) (coe v7))
             (coe du_map'8314'_252 (coe v0) (coe v5) (coe v2) (coe v8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties._.↭-map-inv
d_'8621''45'map'45'inv_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8621''45'map'45'inv_274 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7
  = du_'8621''45'map'45'inv_274 v5 v6 v7
du_'8621''45'map'45'inv_274 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8621''45'map'45'inv_274 v0 v1 v2
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Data.Product.Base.du_'45''44'__68 (coe v0)
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'refl_50))
      (:) v3 v4
        -> case coe v4 of
             []
               -> coe
                    MAlonzo.Code.Data.Product.Base.du_'45''44'__68 (coe v0)
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
                       (coe
                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'refl_50))
             (:) v5 v6
               -> case coe v2 of
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
                      -> coe
                           MAlonzo.Code.Data.Product.Base.du_'45''44'__68 (coe v0)
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
                              (coe
                                 MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'refl_50))
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28 v10
                      -> case coe v1 of
                           (:) v11 v12
                             -> let v13
                                      = coe
                                          du_'8621''45'map'45'inv_274 (coe v4) (coe v12)
                                          (coe v10) in
                                case coe v13 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v14 v15
                                    -> case coe v15 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v16 v17
                                           -> coe
                                                MAlonzo.Code.Data.Product.Base.du_'45''44'__68
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                   (coe v3) (coe v14))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                   erased
                                                   (coe
                                                      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                                                      v17))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38 v11
                      -> case coe v1 of
                           (:) v12 v13
                             -> case coe v13 of
                                  (:) v14 v15
                                    -> let v16
                                             = coe
                                                 du_'8621''45'map'45'inv_274 (coe v6) (coe v15)
                                                 (coe v11) in
                                       case coe v16 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v17 v18
                                           -> case coe v18 of
                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v19 v20
                                                  -> coe
                                                       MAlonzo.Code.Data.Product.Base.du_'45''44'__68
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                          (coe v5)
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                             (coe v3) (coe v17)))
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                          erased
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38
                                                             v20))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46 v8 v10 v11
                      -> let v12
                               = coe du_'8621''45'map'45'inv_274 (coe v0) (coe v8) (coe v10) in
                         case coe v12 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v13 v14
                             -> case coe v14 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v15 v16
                                    -> let v17
                                             = coe
                                                 du_'8621''45'map'45'inv_274 (coe v13) (coe v1)
                                                 (coe v11) in
                                       case coe v17 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v18 v19
                                           -> case coe v19 of
                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v20 v21
                                                  -> coe
                                                       MAlonzo.Code.Data.Product.Base.du_'45''44'__68
                                                       (coe v18)
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                          erased
                                                          (coe
                                                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
                                                             v13 v16 v21))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.↭-length
d_'8621''45'length_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8621''45'length_318 = erased
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++⁺ˡ
d_'43''43''8314''737'_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'43''43''8314''737'_340 ~v0 ~v1 v2 ~v3 ~v4 v5
  = du_'43''43''8314''737'_340 v2 v5
du_'43''43''8314''737'_340 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'43''43''8314''737'_340 v0 v1
  = case coe v0 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
             (coe du_'43''43''8314''737'_340 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++⁺ʳ
d_'43''43''8314''691'_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'43''43''8314''691'_356 ~v0 ~v1 v2 v3 v4 v5
  = du_'43''43''8314''691'_356 v2 v3 v4 v5
du_'43''43''8314''691'_356 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'43''43''8314''691'_356 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28 v7
        -> case coe v0 of
             (:) v8 v9
               -> case coe v1 of
                    (:) v10 v11
                      -> coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                           (coe
                              du_'43''43''8314''691'_356 (coe v9) (coe v11) (coe v2) (coe v7))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38 v8
        -> case coe v0 of
             (:) v9 v10
               -> case coe v10 of
                    (:) v11 v12
                      -> case coe v1 of
                           (:) v13 v14
                             -> case coe v14 of
                                  (:) v15 v16
                                    -> coe
                                         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38
                                         (coe
                                            du_'43''43''8314''691'_356 (coe v12) (coe v16) (coe v2)
                                            (coe v8))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46 v5 v7 v8
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5) (coe v2))
             (coe
                du_'43''43''8314''691'_356 (coe v0) (coe v5) (coe v2) (coe v7))
             (coe
                du_'43''43''8314''691'_356 (coe v5) (coe v1) (coe v2) (coe v8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++⁺
d_'43''43''8314'_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'43''43''8314'_380 ~v0 ~v1 v2 v3 v4 ~v5 v6 v7
  = du_'43''43''8314'_380 v2 v3 v4 v6 v7
du_'43''43''8314'_380 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'43''43''8314'_380 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
      (coe
         du_'43''43''8314''691'_356 (coe v0) (coe v1) (coe v2) (coe v3))
      (coe du_'43''43''8314''737'_340 (coe v1) (coe v4))
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.zoom
d_zoom_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_zoom_394 ~v0 ~v1 v2 v3 v4 v5 v6 = du_zoom_394 v2 v3 v4 v5 v6
du_zoom_394 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_zoom_394 v0 v1 v2 v3 v4
  = coe
      du_'43''43''8314''737'_340 (coe v0)
      (coe
         du_'43''43''8314''691'_356 (coe v2) (coe v3) (coe v1) (coe v4))
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.inject
d_inject_410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_inject_410 ~v0 ~v1 v2 v3 ~v4 v5 v6 v7 v8
  = du_inject_410 v2 v3 v5 v6 v7 v8
du_inject_410 ::
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_inject_410 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v3)))
      (coe
         du_'43''43''8314''737'_340 (coe v1)
         (coe
            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
            v5))
      (coe
         du_'43''43''8314''691'_356 (coe v1) (coe v2)
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v3))
         (coe v4))
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.shift
d_shift_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_shift_424 ~v0 ~v1 v2 v3 v4 = du_shift_424 v2 v3 v4
du_shift_424 ::
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_shift_424 v0 v1 v2
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
      (:) v3 v4
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'prep_144
                (coe v3)
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4) (coe v2)))
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'swap_164
                   (coe v3) (coe v0)
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4) (coe v2))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
                      (coe
                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
                         (coe
                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                            (coe
                               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4) (coe v2)))))
                   (coe
                      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20))
                (coe du_shift_424 (coe v0) (coe v4) (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.drop-mid-≡
d_drop'45'mid'45''8801'_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_drop'45'mid'45''8801'_448 ~v0 ~v1 ~v2 v3 v4 v5 v6 ~v7
  = du_drop'45'mid'45''8801'_448 v3 v4 v5 v6
du_drop'45'mid'45''8801'_448 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_drop'45'mid'45''8801'_448 v0 v1 v2 v3
  = case coe v0 of
      []
        -> case coe v1 of
             []
               -> coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
             (:) v4 v5 -> coe du_shift_424 (coe v4) (coe v5) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v4 v5
        -> case coe v1 of
             []
               -> coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
                    (coe
                       MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
                       (coe
                          MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                          (coe
                             MAlonzo.Code.Data.List.Base.du__'43''43'__62
                             (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v4))
                             (coe v2))))
                    (coe
                       MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v2))
                    (coe du_shift_424 (coe v4) (coe v5) (coe v2))
             (:) v6 v7
               -> let v8
                        = coe
                            MAlonzo.Code.Data.List.Properties.du_'8759''45'injective_42 in
                  coe
                    seq (coe v8)
                    (coe
                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                       (coe
                          du_drop'45'mid'45''8801'_448 (coe v5) (coe v7) (coe v2) (coe v3)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.drop-mid
d_drop'45'mid_502 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_drop'45'mid_502 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_drop'45'mid_502 v2 v3 v4 v5 v6 v7
du_drop'45'mid_502 ::
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_drop'45'mid_502 v0 v1 v2 v3 v4 v5
  = coe
      du_drop'45'mid'8242'_530 (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62
            (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v0))
            (coe v3)))
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62
            (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v0))
            (coe v4)))
      (coe v5) (coe v1) (coe v2) (coe v3) (coe v4)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties._.drop-mid′
d_drop'45'mid'8242'_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_drop'45'mid'8242'_530 ~v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
                        v11 v12 v13 v14 ~v15 ~v16
  = du_drop'45'mid'8242'_530 v2 v8 v9 v10 v11 v12 v13 v14
du_drop'45'mid'8242'_530 ::
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_drop'45'mid'8242'_530 v0 v1 v2 v3 v4 v5 v6 v7
  = case coe v3 of
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
        -> coe
             du_drop'45'mid'45''8801'_448 (coe v4) (coe v5) (coe v6) (coe v7)
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28 v11
        -> case coe v1 of
             (:) v12 v13
               -> case coe v4 of
                    []
                      -> case coe v5 of
                           [] -> coe v11
                           (:) v14 v15
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
                                  (coe
                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v15)
                                     (coe
                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                        (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v12))
                                        (coe v7)))
                                  v11 (coe du_shift_424 (coe v12) (coe v15) (coe v7))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    (:) v14 v15
                      -> case coe v5 of
                           []
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
                                  (coe
                                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v15)
                                     (coe
                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                        (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v12))
                                        (coe v6)))
                                  (coe
                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
                                     (coe
                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v15)
                                        (coe
                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                           (coe
                                              MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                              (coe v12))
                                           (coe v6)))
                                     (coe
                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v12)
                                           (coe v15))
                                        (coe v6))
                                     (coe du_shift_424 (coe v12) (coe v15) (coe v6)))
                                  v11
                           (:) v16 v17
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                                  (coe
                                     du_drop'45'mid'8242'_530 (coe v0)
                                     (coe
                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v15)
                                        (coe
                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                           (coe
                                              MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v0))
                                           (coe v6)))
                                     (coe
                                        MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v17)
                                        (coe
                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                           (coe
                                              MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v0))
                                           (coe v7)))
                                     (coe v11) (coe v15) (coe v17) (coe v6) (coe v7))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38 v12
        -> case coe v1 of
             (:) v13 v14
               -> case coe v14 of
                    (:) v15 v16
                      -> case coe v2 of
                           (:) v17 v18
                             -> case coe v18 of
                                  (:) v19 v20
                                    -> case coe v4 of
                                         []
                                           -> case coe v5 of
                                                []
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                                                       v12
                                                (:) v21 v22
                                                  -> case coe v22 of
                                                       []
                                                         -> coe
                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                                                              v12
                                                       (:) v23 v24
                                                         -> coe
                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                                                              (coe
                                                                 MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                    (coe v24)
                                                                    (coe
                                                                       MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                       (coe
                                                                          MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                          (coe v13))
                                                                       (coe v7)))
                                                                 v12
                                                                 (coe
                                                                    du_shift_424 (coe v13) (coe v24)
                                                                    (coe v7)))
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         (:) v21 v22
                                           -> case coe v22 of
                                                []
                                                  -> case coe v5 of
                                                       []
                                                         -> coe
                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                                                              v12
                                                       (:) v23 v24
                                                         -> case coe v24 of
                                                              []
                                                                -> coe
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                                                                     v12
                                                              (:) v25 v26
                                                                -> coe
                                                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                     (coe
                                                                        MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'prep_144
                                                                        (coe v13)
                                                                        (coe
                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                           (coe v26)
                                                                           (coe
                                                                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                              (coe v15)
                                                                              (coe
                                                                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                 (coe v22)
                                                                                 (coe v7))))
                                                                        (coe
                                                                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'prep_144
                                                                           (coe v13)
                                                                           (coe
                                                                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                              (coe v15)
                                                                              (coe
                                                                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                 (coe v26)
                                                                                 (coe v7)))
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'swap_164
                                                                              (coe v13) (coe v15)
                                                                              (coe
                                                                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                 (coe v26) (coe v7))
                                                                              (coe
                                                                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                 (coe
                                                                                    MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                    (coe
                                                                                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
                                                                                 (coe
                                                                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                    (coe v15)
                                                                                    (coe
                                                                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                       (coe v13)
                                                                                       (coe
                                                                                          MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                          (coe v26)
                                                                                          (coe
                                                                                             v7)))))
                                                                              (coe
                                                                                 MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20))
                                                                           (coe
                                                                              du_shift_424 (coe v15)
                                                                              (coe v26) (coe v7)))
                                                                        (coe v12))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                (:) v23 v24
                                                  -> case coe v5 of
                                                       []
                                                         -> coe
                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                                                              (coe
                                                                 MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                    (coe v24)
                                                                    (coe
                                                                       MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                       (coe
                                                                          MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                          (coe v15))
                                                                       (coe v6)))
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
                                                                    (coe
                                                                       MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                       (coe v24)
                                                                       (coe
                                                                          MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                          (coe
                                                                             MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                             (coe v15))
                                                                          (coe v6)))
                                                                    (coe
                                                                       MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                       (coe
                                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                          (coe v15) (coe v24))
                                                                       (coe v6))
                                                                    (coe
                                                                       du_shift_424 (coe v15)
                                                                       (coe v24) (coe v6)))
                                                                 v12)
                                                       (:) v25 v26
                                                         -> case coe v26 of
                                                              []
                                                                -> coe
                                                                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                                     (coe
                                                                        MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'swap_164
                                                                        (coe v13) (coe v15)
                                                                        (coe
                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                           (coe v24) (coe v6))
                                                                        (coe
                                                                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'prep_144
                                                                           (coe v15)
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                              (coe v24)
                                                                              (coe
                                                                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                 (coe v13)
                                                                                 (coe
                                                                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                    (coe v26)
                                                                                    (coe v6))))
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'prep_144
                                                                              (coe v15) (coe v20)
                                                                              (coe
                                                                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                                                 (coe
                                                                                    MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                                    (coe
                                                                                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
                                                                                 (coe
                                                                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                    (coe v15)
                                                                                    (coe v20)))
                                                                              (coe v12))
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
                                                                              (coe
                                                                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                 (coe v24)
                                                                                 (coe
                                                                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                    (coe v13)
                                                                                    (coe
                                                                                       MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                       (coe v26)
                                                                                       (coe v6))))
                                                                              (coe
                                                                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                 (coe v13)
                                                                                 (coe
                                                                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                                    (coe v24)
                                                                                    (coe v6)))
                                                                              (coe
                                                                                 du_shift_424
                                                                                 (coe v13) (coe v24)
                                                                                 (coe v6))))
                                                                        (coe
                                                                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20))
                                                              (:) v27 v28
                                                                -> coe
                                                                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_swap_38
                                                                     (coe
                                                                        du_drop'45'mid'8242'_530
                                                                        (coe v0)
                                                                        (coe
                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                           (coe v24)
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                              (coe
                                                                                 MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                                 (coe v0))
                                                                              (coe v6)))
                                                                        (coe
                                                                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                           (coe v28)
                                                                           (coe
                                                                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                                                              (coe
                                                                                 MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                                                 (coe v0))
                                                                              (coe v7)))
                                                                        (coe v12) (coe v24)
                                                                        (coe v28) (coe v6) (coe v7))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46 v9 v11 v12
        -> let v13
                 = coe
                     MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45''8707''43''43'_228
                     (coe v9)
                     (coe
                        du_'8712''45'resp'45''8621'_180
                        (coe
                           MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                           (coe
                              MAlonzo.Code.Data.List.Base.du__'43''43'__62
                              (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v0))
                              (coe v6)))
                        v9 v11
                        (coe
                           MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'insert_218
                           (coe v0) (coe v4))) in
           case coe v13 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v14 v15
               -> case coe v15 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v16 v17
                      -> coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_trans_46
                           (coe
                              MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v14) (coe v16))
                           (coe
                              du_drop'45'mid'8242'_530 (coe v0)
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                    (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v0))
                                    (coe v6)))
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v14)
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
                                    (coe v16)))
                              (coe v11) (coe v4) (coe v14) (coe v6) (coe v16))
                           (coe
                              du_drop'45'mid'8242'_530 (coe v0)
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v14)
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
                                    (coe v16)))
                              (coe
                                 MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5)
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v7)))
                              (coe v12) (coe v14) (coe v5) (coe v16) (coe v7))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-identityˡ
d_'43''43''45'identity'737'_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'43''43''45'identity'737'_714 ~v0 ~v1 ~v2
  = du_'43''43''45'identity'737'_714
du_'43''43''45'identity'737'_714 ::
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'43''43''45'identity'737'_714
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_refl_20
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-identityʳ
d_'43''43''45'identity'691'_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'43''43''45'identity'691'_718 ~v0 ~v1 ~v2
  = du_'43''43''45'identity'691'_718
du_'43''43''45'identity'691'_718 ::
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'43''43''45'identity'691'_718
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'reflexive_48
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-identity
d_'43''43''45'identity_722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''43''45'identity_722 ~v0 ~v1 = du_'43''43''45'identity_722
du_'43''43''45'identity_722 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'43''43''45'identity_722
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'43''43''45'identity'737'_714)
      (\ v0 -> coe du_'43''43''45'identity'691'_718)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-assoc
d_'43''43''45'assoc_724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'43''43''45'assoc_724 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_'43''43''45'assoc_724
du_'43''43''45'assoc_724 ::
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'43''43''45'assoc_724
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'reflexive_48
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-comm
d_'43''43''45'comm_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'43''43''45'comm_732 ~v0 ~v1 v2 v3
  = du_'43''43''45'comm_732 v2 v3
du_'43''43''45'comm_732 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'43''43''45'comm_732 v0 v1
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v0))
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v1))
             (coe du_'43''43''45'identity'691'_718)
      (:) v2 v3
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'prep_144
                (coe v2)
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v3))
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621''728'_134
                   (coe
                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                      (coe
                         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v3)))
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v0))
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v0))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
                      (coe
                         MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v0)))
                   (coe du_shift_424 (coe v2) (coe v1) (coe v3)))
                (coe du_'43''43''45'comm_732 (coe v3) (coe v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-isMagma
d_'43''43''45'isMagma_742 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'43''43''45'isMagma_742 ~v0 ~v1 = du_'43''43''45'isMagma_742
du_'43''43''45'isMagma_742 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'43''43''45'isMagma_742
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82)
      (\ v0 v1 v2 v3 v4 v5 -> coe du_'43''43''8314'_380 v0 v1 v2 v4 v5)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-isSemigroup
d_'43''43''45'isSemigroup_744 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'43''43''45'isSemigroup_744 ~v0 ~v1
  = du_'43''43''45'isSemigroup_744
du_'43''43''45'isSemigroup_744 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'43''43''45'isSemigroup_744
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'43''43''45'isMagma_742)
      (\ v0 v1 v2 -> coe du_'43''43''45'assoc_724)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-isMonoid
d_'43''43''45'isMonoid_746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''43''45'isMonoid_746 ~v0 ~v1 = du_'43''43''45'isMonoid_746
du_'43''43''45'isMonoid_746 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'43''43''45'isMonoid_746
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'43''43''45'isSemigroup_744)
      (coe du_'43''43''45'identity_722)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++-isCommutativeMonoid
d_'43''43''45'isCommutativeMonoid_748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''43''45'isCommutativeMonoid_748 ~v0 ~v1
  = du_'43''43''45'isCommutativeMonoid_748
du_'43''43''45'isCommutativeMonoid_748 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'43''43''45'isCommutativeMonoid_748
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe du_'43''43''45'isMonoid_746) (coe du_'43''43''45'comm_732)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties._.++-magma
d_'43''43''45'magma_758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'43''43''45'magma_758 ~v0 ~v1 = du_'43''43''45'magma_758
du_'43''43''45'magma_758 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'43''43''45'magma_758
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe du_'43''43''45'isMagma_742)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties._.++-semigroup
d_'43''43''45'semigroup_760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'43''43''45'semigroup_760 ~v0 ~v1 = du_'43''43''45'semigroup_760
du_'43''43''45'semigroup_760 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'43''43''45'semigroup_760
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe du_'43''43''45'isSemigroup_744)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties._.++-monoid
d_'43''43''45'monoid_762 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'43''43''45'monoid_762 ~v0 ~v1 = du_'43''43''45'monoid_762
du_'43''43''45'monoid_762 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'43''43''45'monoid_762
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe du_'43''43''45'isMonoid_746)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties._.++-commutativeMonoid
d_'43''43''45'commutativeMonoid_764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''43''45'commutativeMonoid_764 ~v0 ~v1
  = du_'43''43''45'commutativeMonoid_764
du_'43''43''45'commutativeMonoid_764 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''43''45'commutativeMonoid_764
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe du_'43''43''45'isCommutativeMonoid_748)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.shifts
d_shifts_772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_shifts_772 ~v0 ~v1 v2 v3 v4 = du_shifts_772 v2 v3 v4
du_shifts_772 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_shifts_772 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621''728'_134
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0)
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2)))
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v1))
            (coe v2))
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v2)))
         (coe
            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621'_132
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v1))
               (coe v2))
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v0))
               (coe v2))
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v2)))
            (coe
               MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621'_132
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v0))
                  (coe v2))
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v2)))
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v2)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
                     (coe
                        MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v2))))
               (coe du_'43''43''45'assoc_724))
            (coe
               du_'43''43''8314''691'_356
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v1))
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v0))
               (coe v2) (coe du_'43''43''45'comm_732 (coe v0) (coe v1))))
         (coe du_'43''43''45'assoc_724))
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.drop-∷
d_drop'45''8759'_786 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_drop'45''8759'_786 ~v0 ~v1 v2 v3 v4
  = du_drop'45''8759'_786 v2 v3 v4
du_drop'45''8759'_786 ::
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_drop'45''8759'_786 v0 v1 v2
  = coe
      du_drop'45'mid_502 (coe v0)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v1)
      (coe v2)
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.∷↭∷ʳ
d_'8759''8621''8759''691'_792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'8759''8621''8759''691'_792 ~v0 ~v1 v2 v3
  = du_'8759''8621''8759''691'_792 v2 v3
du_'8759''8621''8759''691'_792 ::
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'8759''8621''8759''691'_792 v0 v1
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
      (coe
         MAlonzo.Code.Data.List.Base.du__'8759''691'__494 (coe v1) (coe v0))
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v1))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621'_132
            (coe
               MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
               (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v0)))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
               (coe
                  MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1)
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v1))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
               (coe
                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v1)))
            (coe
               du_shift_424 (coe v0) (coe v1)
               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
-- Data.List.Relation.Binary.Permutation.Propositional.Properties.++↭ʳ++
d_'43''43''8621''691''43''43'_802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_'43''43''8621''691''43''43'_802 ~v0 ~v1 v2 v3
  = du_'43''43''8621''691''43''43'_802 v2 v3
du_'43''43''8621''691''43''43'_802 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_'43''43''8621''691''43''43'_802 v0 v1
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'refl_50
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'trans_72
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v3)
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62
                   (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v2))
                   (coe v1)))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v3)
                   (coe
                      MAlonzo.Code.Data.List.Base.du__'43''43'__62
                      (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v2))
                      (coe v1)))
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v0) (coe v1))
                (coe du_shift_424 (coe v2) (coe v3) (coe v1)))
             (coe
                du_'43''43''8621''691''43''43'_802 (coe v3)
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2) (coe v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Propositional.Properties._.merge-↭
d_merge'45''8621'_828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_merge'45''8621'_828 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_merge'45''8621'_828 v4 v5 v6
du_merge'45''8621'_828 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_merge'45''8621'_828 v0 v1 v2
  = case coe v1 of
      []
        -> coe
             seq (coe v2)
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'refl_50)
      (:) v3 v4
        -> case coe v2 of
             []
               -> coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
                    (coe
                       MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
                    (coe
                       MAlonzo.Code.Data.List.Base.du_merge_222 (coe v0) (coe v1)
                       (coe v2))
                    (coe du_'43''43''45'identity'691'_718)
             (:) v5 v6
               -> let v7
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v3 v5) in
                  let v8 = coe du_merge'45''8621'_828 (coe v0) (coe v4) (coe v2) in
                  let v9 = coe du_merge'45''8621'_828 (coe v0) (coe v1) (coe v6) in
                  if coe v7
                    then coe
                           MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.C_prep_28
                           v8
                    else coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                           (coe
                              MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45'prep_144
                              (coe v5)
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4) (coe v6)))
                              (coe
                                 MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621''728'_134
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5)
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                                       (coe
                                          MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                                          (coe v6))))
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                                    (coe
                                       MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                                       (coe v2)))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                       (coe
                                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                                       (coe
                                          MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                                          (coe v2))))
                                 (coe du_shift_424 (coe v5) (coe v1) (coe v6)))
                              (coe v9))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
