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

module MAlonzo.Code.Data.Container.Morphism.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core
import qualified MAlonzo.Code.Data.Container.Relation.Binary.Equality.Setoid
import qualified MAlonzo.Code.Data.Container.Relation.Binary.Pointwise
import qualified MAlonzo.Code.Relation.Binary.Bundles

-- Data.Container.Morphism.Properties._.id-correct
d_id'45'correct_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_id'45'correct_22 = erased
-- Data.Container.Morphism.Properties._.∘-correct
d_'8728''45'correct_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8728''45'correct_56 = erased
-- Data.Container.Morphism.Properties._.Natural
d_Natural_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  ()
d_Natural_86 = erased
-- Data.Container.Morphism.Properties._.natural
d_natural_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_natural_130 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 ~v9 v10 v11 v12
  = du_natural_130 v6 v10 v11 v12
du_natural_130 ::
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_natural_130 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Container.Relation.Binary.Equality.Setoid.du_refl_52
      (coe v1)
      (coe
         MAlonzo.Code.Data.Container.Core.du_'10218'_'10219'_104 v0
         (coe MAlonzo.Code.Data.Container.Core.du_map_56 v2 v3))
-- Data.Container.Morphism.Properties._.NT
d_NT_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> ()
d_NT_164 = erased
-- Data.Container.Morphism.Properties._.complete
d_complete_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_complete_222 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_complete_222 v7
du_complete_222 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_complete_222 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe du_m_232 (coe v1))
             (coe
                (\ v3 v4 ->
                   coe
                     v2 erased v3 (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v4))
                     (coe
                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                        (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v4))
                        (coe (\ v5 -> v5)))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Container.Morphism.Properties._._.m
d_m_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  (() ->
   MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
   (AgdaAny -> AgdaAny) ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36) ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74
d_m_232 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 ~v8 = du_m_232 v7
du_m_232 ::
  (() ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74
du_m_232 v0
  = coe
      MAlonzo.Code.Data.Container.Core.C__'9655'__108
      (coe
         (\ v1 ->
            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
              (coe
                 v0 erased
                 (coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1)
                    (coe (\ v2 -> v2))))))
      (coe
         (\ v1 ->
            MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
              (coe
                 v0 erased
                 (coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1)
                    (coe (\ v2 -> v2))))))
