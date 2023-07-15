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

module MAlonzo.Code.Data.Container.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Function.Inverse

-- Data.Container.Core.Container
d_Container_10 a0 a1 = ()
data T_Container_10 = C__'9655'__24
-- Data.Container.Core.Container.Shape
d_Shape_20 :: T_Container_10 -> ()
d_Shape_20 = erased
-- Data.Container.Core.Container.Position
d_Position_22 :: T_Container_10 -> AgdaAny -> ()
d_Position_22 = erased
-- Data.Container.Core.⟦_⟧
d_'10214'_'10215'_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Container_10 -> () -> ()
d_'10214'_'10215'_32 = erased
-- Data.Container.Core.map
d_map_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Container_10 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_map_56 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_map_56 v7
du_map_56 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_map_56 v0
  = coe
      MAlonzo.Code.Data.Product.Base.du_map'8322'_126
      (coe (\ v1 v2 v3 -> coe v0 (coe v2 v3)))
-- Data.Container.Core._⇒_
d__'8658'__74 a0 a1 a2 a3 a4 a5 = ()
data T__'8658'__74
  = C__'9655'__108 (AgdaAny -> AgdaAny)
                   (AgdaAny -> AgdaAny -> AgdaAny)
-- Data.Container.Core._⇒_.shape
d_shape_94 :: T__'8658'__74 -> AgdaAny -> AgdaAny
d_shape_94 v0
  = case coe v0 of
      C__'9655'__108 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Container.Core._⇒_.position
d_position_98 :: T__'8658'__74 -> AgdaAny -> AgdaAny -> AgdaAny
d_position_98 v0
  = case coe v0 of
      C__'9655'__108 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Container.Core._⇒_.⟪_⟫
d_'10218'_'10219'_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Container_10 ->
  T_Container_10 ->
  T__'8658'__74 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'10218'_'10219'_104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_'10218'_'10219'_104 v6
du_'10218'_'10219'_104 ::
  T__'8658'__74 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'10218'_'10219'_104 v0
  = coe
      MAlonzo.Code.Data.Product.Base.du_map_104 (coe d_shape_94 (coe v0))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Function.Base.du__'8728''8242'__216 (coe v2)
              (coe d_position_98 v0 v1)))
-- Data.Container.Core._⊸_
d__'8888'__122 a0 a1 a2 a3 a4 a5 = ()
data T__'8888'__122
  = C__'8888'_'46'constructor_4735 (AgdaAny -> AgdaAny)
                                   (AgdaAny -> MAlonzo.Code.Function.Inverse.T_Inverse_58)
-- Data.Container.Core._⊸_.shape⊸
d_shape'8888'_142 :: T__'8888'__122 -> AgdaAny -> AgdaAny
d_shape'8888'_142 v0
  = case coe v0 of
      C__'8888'_'46'constructor_4735 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Container.Core._⊸_.position⊸
d_position'8888'_146 ::
  T__'8888'__122 ->
  AgdaAny -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_position'8888'_146 v0
  = case coe v0 of
      C__'8888'_'46'constructor_4735 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Container.Core._⊸_.morphism
d_morphism_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Container_10 -> T_Container_10 -> T__'8888'__122 -> T__'8658'__74
d_morphism_148 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_morphism_148 v6
du_morphism_148 :: T__'8888'__122 -> T__'8658'__74
du_morphism_148 v0
  = coe
      C__'9655'__108 (coe d_shape'8888'_142 (coe v0))
      (coe
         (\ v1 ->
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
              (coe
                 MAlonzo.Code.Function.Inverse.d_to_78
                 (coe d_position'8888'_146 v0 v1))))
-- Data.Container.Core._⊸_.⟪_⟫⊸
d_'10218'_'10219''8888'_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Container_10 ->
  T_Container_10 ->
  T__'8888'__122 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'10218'_'10219''8888'_154 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_'10218'_'10219''8888'_154 v6
du_'10218'_'10219''8888'_154 ::
  T__'8888'__122 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'10218'_'10219''8888'_154 v0
  = coe du_'10218'_'10219'_104 (coe du_morphism_148 (coe v0))
