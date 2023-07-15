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

module MAlonzo.Code.Class.Functor.Instances where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Functor.Core
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.NonEmpty.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Reflection.AST.Abstraction
import qualified MAlonzo.Code.Reflection.AST.Argument

-- Class.Functor.Instances.Functor-Maybe
d_Functor'45'Maybe_12 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_Functor'45'Maybe_12
  = coe
      MAlonzo.Code.Class.Functor.Core.C_Functor'46'constructor_219
      (coe (\ v0 v1 v2 v3 -> coe MAlonzo.Code.Data.Maybe.Base.du_map_68))
-- Class.Functor.Instances.FunctorLaws-Maybe
d_FunctorLaws'45'Maybe_14 ::
  MAlonzo.Code.Class.Functor.Core.T_FunctorLaws_66
d_FunctorLaws'45'Maybe_14 = erased
-- Class.Functor.Instances.Functor-List
d_Functor'45'List_22 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_Functor'45'List_22
  = coe
      MAlonzo.Code.Class.Functor.Core.C_Functor'46'constructor_219
      (coe (\ v0 v1 v2 v3 -> coe MAlonzo.Code.Data.List.Base.du_map_22))
-- Class.Functor.Instances.FunctorLaws-List
d_FunctorLaws'45'List_24 ::
  MAlonzo.Code.Class.Functor.Core.T_FunctorLaws_66
d_FunctorLaws'45'List_24 = erased
-- Class.Functor.Instances._.p
d_p_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_p_34 = erased
-- Class.Functor.Instances._..extendedlambda3
d_'46'extendedlambda3_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'46'extendedlambda3_36 = erased
-- Class.Functor.Instances._.q
d_q_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_q_56 = erased
-- Class.Functor.Instances._..extendedlambda4
d_'46'extendedlambda4_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'46'extendedlambda4_62 = erased
-- Class.Functor.Instances.Functor-List⁺
d_Functor'45'List'8314'_70 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_Functor'45'List'8314'_70
  = coe
      MAlonzo.Code.Class.Functor.Core.C_Functor'46'constructor_219
      (coe
         (\ v0 v1 v2 v3 ->
            coe MAlonzo.Code.Data.List.NonEmpty.Base.du_map_98))
-- Class.Functor.Instances.Functor-Vec
d_Functor'45'Vec_74 ::
  Integer -> MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_Functor'45'Vec_74 ~v0 = du_Functor'45'Vec_74
du_Functor'45'Vec_74 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34
du_Functor'45'Vec_74
  = coe
      MAlonzo.Code.Class.Functor.Core.C_Functor'46'constructor_219
      (coe (\ v0 v1 v2 v3 -> coe MAlonzo.Code.Data.Vec.Base.du_map_178))
-- Class.Functor.Instances.Functor-TC
d_Functor'45'TC_76 :: MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_Functor'45'TC_76
  = coe
      MAlonzo.Code.Class.Functor.Core.C_Functor'46'constructor_219
      (coe
         (\ v0 v1 v2 v3 v4 v5 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 v0 v2 erased
              erased v5
              (\ v6 ->
                 coe
                   MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 v2 erased
                   (coe v4 v6))))
-- Class.Functor.Instances.Functor-Abs
d_Functor'45'Abs_82 :: MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_Functor'45'Abs_82
  = coe
      MAlonzo.Code.Class.Functor.Core.C_Functor'46'constructor_219
      (\ v0 v1 v2 v3 v4 v5 ->
         coe MAlonzo.Code.Reflection.AST.Abstraction.du_map_22 v4 v5)
-- Class.Functor.Instances.Functor-Arg
d_Functor'45'Arg_88 :: MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_Functor'45'Arg_88
  = coe
      MAlonzo.Code.Class.Functor.Core.C_Functor'46'constructor_219
      (\ v0 v1 v2 v3 v4 v5 ->
         coe MAlonzo.Code.Reflection.AST.Argument.du_map_54 v4 v5)
-- Class.Functor.Instances.Functor-∃Vec
d_Functor'45''8707'Vec_94 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_Functor'45''8707'Vec_94
  = coe
      MAlonzo.Code.Class.Functor.Core.C_Functor'46'constructor_219
      (coe
         (\ v0 v1 v2 v3 v4 v5 ->
            case coe v5 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                -> coe
                     MAlonzo.Code.Data.Product.Base.du_'45''44'__68 (coe v6)
                     (coe MAlonzo.Code.Data.Vec.Base.du_map_178 (coe v4) (coe v7))
              _ -> MAlonzo.RTE.mazUnreachableError))
