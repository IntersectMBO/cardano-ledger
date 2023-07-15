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

module MAlonzo.Code.Data.List.Extrema where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Extrema.Core
import qualified MAlonzo.Code.Data.List.Membership.Propositional
import qualified MAlonzo.Code.Data.List.Membership.Propositional.Properties
import qualified MAlonzo.Code.Data.List.Properties
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Data.List.Extrema._._<_
d__'60'__80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> ()
d__'60'__80 = erased
-- Data.List.Extrema.argmin
d_argmin_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
d_argmin_118 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 = du_argmin_118 v3 v6
du_argmin_118 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
du_argmin_118 v0 v1
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480'_330 v0 v1)
-- Data.List.Extrema.argmax
d_argmax_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
d_argmax_122 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 = du_argmax_122 v3 v6
du_argmax_122 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
du_argmax_122 v0 v1
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480'_332 v0 v1)
-- Data.List.Extrema.min
d_min_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> [AgdaAny] -> AgdaAny
d_min_126 ~v0 ~v1 ~v2 v3 = du_min_126 v3
du_min_126 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> [AgdaAny] -> AgdaAny
du_min_126 v0 = coe du_argmin_118 (coe v0) (coe (\ v1 -> v1))
-- Data.List.Extrema.max
d_max_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> [AgdaAny] -> AgdaAny
d_max_128 ~v0 ~v1 ~v2 v3 = du_max_128 v3
du_max_128 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> [AgdaAny] -> AgdaAny
du_max_128 v0 = coe du_argmax_122 (coe v0) (coe (\ v1 -> v1))
-- Data.List.Extrema._.f[argmin]≤v⁺
d_f'91'argmin'93''8804'v'8314'_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_f'91'argmin'93''8804'v'8314'_146 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
  = du_f'91'argmin'93''8804'v'8314'_146 v3 v6 v7
du_f'91'argmin'93''8804'v'8314'_146 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_f'91'argmin'93''8804'v'8314'_146 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'preserves'7506'_3836
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480'_330 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480''45'pres'7506''45''8804'v_348
         (coe v0) (coe v1) (coe v2))
-- Data.List.Extrema._.f[argmin]<v⁺
d_f'91'argmin'93''60'v'8314'_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_f'91'argmin'93''60'v'8314'_156 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
  = du_f'91'argmin'93''60'v'8314'_156 v3 v6 v7
du_f'91'argmin'93''60'v'8314'_156 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_f'91'argmin'93''60'v'8314'_156 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'preserves'7506'_3836
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480'_330 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480''45'pres'7506''45''60'v_360
         (coe v0) (coe v1) (coe v2))
-- Data.List.Extrema._.v≤f[argmin]⁺
d_v'8804'f'91'argmin'93''8314'_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_v'8804'f'91'argmin'93''8314'_166 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 ~v7 v8
                                   v9
  = du_v'8804'f'91'argmin'93''8314'_166 v3 v6 v8 v9
du_v'8804'f'91'argmin'93''8314'_166 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_v'8804'f'91'argmin'93''8314'_166 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'preserves'7495'_3796
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480'_330 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480''45'pres'7495''45'v'8804'_372
         (coe v0) (coe v1))
      (coe v2) (coe v3)
-- Data.List.Extrema._.v<f[argmin]⁺
d_v'60'f'91'argmin'93''8314'_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_v'60'f'91'argmin'93''8314'_176 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 ~v7 v8
                                 v9
  = du_v'60'f'91'argmin'93''8314'_176 v3 v6 v8 v9
du_v'60'f'91'argmin'93''8314'_176 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_v'60'f'91'argmin'93''8314'_176 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'preserves'7495'_3796
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480'_330 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480''45'pres'7495''45'v'60'_388
         (coe v0) (coe v1))
      (coe v2) (coe v3)
-- Data.List.Extrema._.f[argmin]≤f[⊤]
d_f'91'argmin'93''8804'f'91''8868''93'_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
d_f'91'argmin'93''8804'f'91''8868''93'_182 ~v0 ~v1 ~v2 v3 ~v4 ~v5
                                           v6 v7 v8
  = du_f'91'argmin'93''8804'f'91''8868''93'_182 v3 v6 v7 v8
du_f'91'argmin'93''8804'f'91''8868''93'_182 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
du_f'91'argmin'93''8804'f'91''8868''93'_182 v0 v1 v2 v3
  = coe
      du_f'91'argmin'93''8804'v'8314'_146 v0 v1 (coe v1 v2) v2 v3
      (coe
         MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
         (let v4
                = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                    (coe v0) in
          let v5
                = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                    (coe v4) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.du_refl_98
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
            (coe v1 v2)))
-- Data.List.Extrema._.f[argmin]≤f[xs]
d_f'91'argmin'93''8804'f'91'xs'93'_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_f'91'argmin'93''8804'f'91'xs'93'_194 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
                                       v8
  = du_f'91'argmin'93''8804'f'91'xs'93'_194 v3 v6 v7 v8
du_f'91'argmin'93''8804'f'91'xs'93'_194 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_f'91'argmin'93''8804'f'91'xs'93'_194 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'forces'7495'_3762
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480'_330 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480''45'forces'7495''45'v'8804'_404
         (coe v0) (coe v1) (coe v1 (coe du_argmin_118 v0 v1 v2 v3)))
      (coe v2) (coe v3)
      (let v4
             = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                 (coe v0) in
       let v5
             = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                 (coe v4) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.du_refl_98
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
         (coe v1 (coe du_argmin_118 v0 v1 v2 v3)))
-- Data.List.Extrema._.f[argmin]≈f[v]⁺
d_f'91'argmin'93''8776'f'91'v'93''8314'_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  AgdaAny -> AgdaAny
d_f'91'argmin'93''8776'f'91'v'93''8314'_208 ~v0 ~v1 ~v2 v3 ~v4 ~v5
                                            v6 v7 v8 v9 v10 v11 v12
  = du_f'91'argmin'93''8776'f'91'v'93''8314'_208
      v3 v6 v7 v8 v9 v10 v11 v12
du_f'91'argmin'93''8776'f'91'v'93''8314'_208 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  AgdaAny -> AgdaAny
du_f'91'argmin'93''8776'f'91'v'93''8314'_208 v0 v1 v2 v3 v4 v5 v6
                                             v7
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0)))
      (coe v1 (coe du_argmin_118 v0 v1 v3 v4)) (coe v1 v2)
      (coe
         du_f'91'argmin'93''8804'v'8314'_146 v0 v1 (coe v1 v2) v3 v4
         (coe
            MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
            (coe
               MAlonzo.Code.Data.List.Membership.Propositional.du_lose_52 v2 v4 v5
               (let v8
                      = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                          (coe v0) in
                let v9
                      = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                          (coe v8) in
                coe
                  MAlonzo.Code.Relation.Binary.Structures.du_refl_98
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v9))
                  (coe v1 v2)))))
      (coe du_v'8804'f'91'argmin'93''8314'_166 v0 v1 v3 v4 v7 v6)
-- Data.List.Extrema.argmin[xs]≤argmin[ys]⁺
d_argmin'91'xs'93''8804'argmin'91'ys'93''8314'_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_argmin'91'xs'93''8804'argmin'91'ys'93''8314'_234 ~v0 ~v1 ~v2 v3
                                                   ~v4 ~v5 v6 v7 v8 v9 v10 v11 v12 v13
  = du_argmin'91'xs'93''8804'argmin'91'ys'93''8314'_234
      v3 v6 v7 v8 v9 v10 v11 v12 v13
du_argmin'91'xs'93''8804'argmin'91'ys'93''8314'_234 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_argmin'91'xs'93''8804'argmin'91'ys'93''8314'_234 v0 v1 v2 v3 v4
                                                    v5 v6 v7 v8
  = coe
      du_v'8804'f'91'argmin'93''8314'_166 v0 v2 v4 v6
      (coe
         du_f'91'argmin'93''8804'v'8314'_146 v0 v1 (coe v2 v4) v3 v5 v7)
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.All.du_map_166
         (coe
            (\ v9 ->
               coe du_f'91'argmin'93''8804'v'8314'_146 v0 v1 (coe v2 v9) v3 v5))
         (coe v6) (coe v8))
-- Data.List.Extrema.argmin[xs]<argmin[ys]⁺
d_argmin'91'xs'93''60'argmin'91'ys'93''8314'_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_argmin'91'xs'93''60'argmin'91'ys'93''8314'_262 ~v0 ~v1 ~v2 v3 ~v4
                                                 ~v5 v6 v7 v8 v9 v10 v11 v12 v13
  = du_argmin'91'xs'93''60'argmin'91'ys'93''8314'_262
      v3 v6 v7 v8 v9 v10 v11 v12 v13
du_argmin'91'xs'93''60'argmin'91'ys'93''8314'_262 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_argmin'91'xs'93''60'argmin'91'ys'93''8314'_262 v0 v1 v2 v3 v4 v5
                                                  v6 v7 v8
  = coe
      du_v'60'f'91'argmin'93''8314'_176 v0 v2 v4 v6
      (coe du_f'91'argmin'93''60'v'8314'_156 v0 v1 (coe v2 v4) v3 v5 v7)
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.All.du_map_166
         (coe
            (\ v9 ->
               coe du_f'91'argmin'93''60'v'8314'_156 v0 v1 (coe v2 v9) v3 v5))
         (coe v6) (coe v8))
-- Data.List.Extrema.argmin-sel
d_argmin'45'sel_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_argmin'45'sel_278 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6
  = du_argmin'45'sel_278 v3 v6
du_argmin'45'sel_278 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_argmin'45'sel_278 v0 v1
  = coe
      MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_foldr'45'selective_702
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480'_330 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8851''7480''45'sel_336
         (coe v0) (coe v1))
-- Data.List.Extrema.argmin-all
d_argmin'45'all_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_argmin'45'all_290 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8 v9 ~v10 v11
                    v12
  = du_argmin'45'all_290 v3 v7 v8 v9 v11 v12
du_argmin'45'all_290 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_argmin'45'all_290 v0 v1 v2 v3 v4 v5
  = let v6 = coe du_argmin'45'sel_278 v0 v1 v2 v3 in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7 -> coe v4
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7
        -> coe
             MAlonzo.Code.Data.List.Relation.Unary.All.du_lookup_440 v3 v5 v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Extrema._.v≤f[argmax]⁺
d_v'8804'f'91'argmax'93''8314'_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_v'8804'f'91'argmax'93''8314'_352 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
  = du_v'8804'f'91'argmax'93''8314'_352 v3 v6 v7
du_v'8804'f'91'argmax'93''8314'_352 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_v'8804'f'91'argmax'93''8314'_352 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'preserves'7506'_3836
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480'_332 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480''45'pres'7506''45'v'8804'_432
         (coe v0) (coe v1) (coe v2))
-- Data.List.Extrema._.v<f[argmax]⁺
d_v'60'f'91'argmax'93''8314'_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_v'60'f'91'argmax'93''8314'_362 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
  = du_v'60'f'91'argmax'93''8314'_362 v3 v6 v7
du_v'60'f'91'argmax'93''8314'_362 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_v'60'f'91'argmax'93''8314'_362 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'preserves'7506'_3836
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480'_332 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480''45'pres'7506''45'v'60'_454
         (coe v0) (coe v1) (coe v2))
-- Data.List.Extrema._.f[argmax]≤v⁺
d_f'91'argmax'93''8804'v'8314'_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_f'91'argmax'93''8804'v'8314'_372 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 ~v7 v8
                                   v9
  = du_f'91'argmax'93''8804'v'8314'_372 v3 v6 v8 v9
du_f'91'argmax'93''8804'v'8314'_372 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_f'91'argmax'93''8804'v'8314'_372 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'preserves'7495'_3796
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480'_332 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480''45'pres'7495''45''8804'v_476
         (coe v0) (coe v1))
      (coe v2) (coe v3)
-- Data.List.Extrema._.f[argmax]<v⁺
d_f'91'argmax'93''60'v'8314'_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_f'91'argmax'93''60'v'8314'_382 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 ~v7 v8
                                 v9
  = du_f'91'argmax'93''60'v'8314'_382 v3 v6 v8 v9
du_f'91'argmax'93''60'v'8314'_382 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_f'91'argmax'93''60'v'8314'_382 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'preserves'7495'_3796
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480'_332 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480''45'pres'7495''45''60'v_492
         (coe v0) (coe v1))
      (coe v2) (coe v3)
-- Data.List.Extrema._.f[⊥]≤f[argmax]
d_f'91''8869''93''8804'f'91'argmax'93'_388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
d_f'91''8869''93''8804'f'91'argmax'93'_388 ~v0 ~v1 ~v2 v3 ~v4 ~v5
                                           v6 v7 v8
  = du_f'91''8869''93''8804'f'91'argmax'93'_388 v3 v6 v7 v8
du_f'91''8869''93''8804'f'91'argmax'93'_388 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
du_f'91''8869''93''8804'f'91'argmax'93'_388 v0 v1 v2 v3
  = coe
      du_v'8804'f'91'argmax'93''8314'_352 v0 v1 (coe v1 v2) v2 v3
      (coe
         MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
         (let v4
                = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                    (coe v0) in
          let v5
                = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                    (coe v4) in
          coe
            MAlonzo.Code.Relation.Binary.Structures.du_refl_98
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
            (coe v1 v2)))
-- Data.List.Extrema._.f[xs]≤f[argmax]
d_f'91'xs'93''8804'f'91'argmax'93'_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_f'91'xs'93''8804'f'91'argmax'93'_400 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
                                       v8
  = du_f'91'xs'93''8804'f'91'argmax'93'_400 v3 v6 v7 v8
du_f'91'xs'93''8804'f'91'argmax'93'_400 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_f'91'xs'93''8804'f'91'argmax'93'_400 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Properties.du_foldr'45'forces'7495'_3762
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480'_332 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480''45'forces'7495''45''8804'v_508
         (coe v0) (coe v1) (coe v1 (coe du_argmax_122 v0 v1 v2 v3)))
      (coe v2) (coe v3)
      (let v4
             = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                 (coe v0) in
       let v5
             = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                 (coe v4) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.du_refl_98
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
         (coe
            v1
            (coe
               MAlonzo.Code.Data.List.Base.du_foldr_242
               (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480'_332 v0 v1)
               (coe v2) (coe v3))))
-- Data.List.Extrema._.f[argmax]≈f[v]⁺
d_f'91'argmax'93''8776'f'91'v'93''8314'_414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  AgdaAny -> AgdaAny
d_f'91'argmax'93''8776'f'91'v'93''8314'_414 ~v0 ~v1 ~v2 v3 ~v4 ~v5
                                            v6 v7 v8 v9 v10 v11 v12
  = du_f'91'argmax'93''8776'f'91'v'93''8314'_414
      v3 v6 v7 v8 v9 v10 v11 v12
du_f'91'argmax'93''8776'f'91'v'93''8314'_414 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  AgdaAny -> AgdaAny
du_f'91'argmax'93''8776'f'91'v'93''8314'_414 v0 v1 v2 v3 v4 v5 v6
                                             v7
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0)))
      (coe v1 (coe du_argmax_122 v0 v1 v3 v4)) (coe v1 v2)
      (coe du_f'91'argmax'93''8804'v'8314'_372 v0 v1 v3 v4 v7 v6)
      (coe
         du_v'8804'f'91'argmax'93''8314'_352 v0 v1 (coe v1 v2) v3 v4
         (coe
            MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
            (coe
               MAlonzo.Code.Data.List.Membership.Propositional.du_lose_52 v2 v4 v5
               (let v8
                      = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                          (coe v0) in
                let v9
                      = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                          (coe v8) in
                coe
                  MAlonzo.Code.Relation.Binary.Structures.du_refl_98
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v9))
                  (coe v1 v2)))))
-- Data.List.Extrema.argmax[xs]≤argmax[ys]⁺
d_argmax'91'xs'93''8804'argmax'91'ys'93''8314'_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_argmax'91'xs'93''8804'argmax'91'ys'93''8314'_440 ~v0 ~v1 ~v2 v3
                                                   ~v4 ~v5 v6 v7 v8 v9 v10 v11 v12 v13
  = du_argmax'91'xs'93''8804'argmax'91'ys'93''8314'_440
      v3 v6 v7 v8 v9 v10 v11 v12 v13
du_argmax'91'xs'93''8804'argmax'91'ys'93''8314'_440 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_argmax'91'xs'93''8804'argmax'91'ys'93''8314'_440 v0 v1 v2 v3 v4
                                                    v5 v6 v7 v8
  = coe
      du_f'91'argmax'93''8804'v'8314'_372 v0 v1 v3 v5
      (coe
         du_v'8804'f'91'argmax'93''8314'_352 v0 v2 (coe v1 v3) v4 v6 v7)
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.All.du_map_166
         (coe
            (\ v9 ->
               coe du_v'8804'f'91'argmax'93''8314'_352 v0 v2 (coe v1 v9) v4 v6))
         (coe v5) (coe v8))
-- Data.List.Extrema.argmax[xs]<argmax[ys]⁺
d_argmax'91'xs'93''60'argmax'91'ys'93''8314'_468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_argmax'91'xs'93''60'argmax'91'ys'93''8314'_468 ~v0 ~v1 ~v2 v3 ~v4
                                                 ~v5 v6 v7 v8 v9 v10 v11 v12 v13
  = du_argmax'91'xs'93''60'argmax'91'ys'93''8314'_468
      v3 v6 v7 v8 v9 v10 v11 v12 v13
du_argmax'91'xs'93''60'argmax'91'ys'93''8314'_468 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_argmax'91'xs'93''60'argmax'91'ys'93''8314'_468 v0 v1 v2 v3 v4 v5
                                                  v6 v7 v8
  = coe
      du_f'91'argmax'93''60'v'8314'_382 v0 v1 v3 v5
      (coe du_v'60'f'91'argmax'93''8314'_362 v0 v2 (coe v1 v3) v4 v6 v7)
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.All.du_map_166
         (coe
            (\ v9 ->
               coe du_v'60'f'91'argmax'93''8314'_362 v0 v2 (coe v1 v9) v4 v6))
         (coe v5) (coe v8))
-- Data.List.Extrema.argmax-sel
d_argmax'45'sel_484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_argmax'45'sel_484 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6
  = du_argmax'45'sel_484 v3 v6
du_argmax'45'sel_484 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_argmax'45'sel_484 v0 v1
  = coe
      MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_foldr'45'selective_702
      (coe MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480'_332 v0 v1)
      (coe
         MAlonzo.Code.Data.List.Extrema.Core.du_'8852''7480''45'sel_420
         (coe v0) (coe v1))
-- Data.List.Extrema.argmax-all
d_argmax'45'all_496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_argmax'45'all_496 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 ~v8 v9 v10 v11
                    v12
  = du_argmax'45'all_496 v3 v7 v9 v10 v11 v12
du_argmax'45'all_496 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_argmax'45'all_496 v0 v1 v2 v3 v4 v5
  = let v6 = coe du_argmax'45'sel_484 v0 v1 v2 v3 in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7 -> coe v4
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7
        -> coe
             MAlonzo.Code.Data.List.Relation.Unary.All.du_lookup_440 v3 v5 v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Extrema.min≤v⁺
d_min'8804'v'8314'_550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_min'8804'v'8314'_550 ~v0 ~v1 ~v2 v3 v4
  = du_min'8804'v'8314'_550 v3 v4
du_min'8804'v'8314'_550 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_min'8804'v'8314'_550 v0 v1
  = coe
      du_f'91'argmin'93''8804'v'8314'_146 (coe v0) (coe (\ v2 -> v2))
      (coe v1)
-- Data.List.Extrema.min<v⁺
d_min'60'v'8314'_560 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_min'60'v'8314'_560 ~v0 ~v1 ~v2 v3 v4
  = du_min'60'v'8314'_560 v3 v4
du_min'60'v'8314'_560 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_min'60'v'8314'_560 v0 v1
  = coe
      du_f'91'argmin'93''60'v'8314'_156 (coe v0) (coe (\ v2 -> v2))
      (coe v1)
-- Data.List.Extrema.v≤min⁺
d_v'8804'min'8314'_570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_v'8804'min'8314'_570 ~v0 ~v1 ~v2 v3 ~v4 v5 v6
  = du_v'8804'min'8314'_570 v3 v5 v6
du_v'8804'min'8314'_570 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_v'8804'min'8314'_570 v0 v1 v2
  = coe
      du_v'8804'f'91'argmin'93''8314'_166 (coe v0) (coe (\ v3 -> v3))
      (coe v1) (coe v2)
-- Data.List.Extrema.v<min⁺
d_v'60'min'8314'_580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_v'60'min'8314'_580 ~v0 ~v1 ~v2 v3 ~v4 v5 v6
  = du_v'60'min'8314'_580 v3 v5 v6
du_v'60'min'8314'_580 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_v'60'min'8314'_580 v0 v1 v2
  = coe
      du_v'60'f'91'argmin'93''8314'_176 (coe v0) (coe (\ v3 -> v3))
      (coe v1) (coe v2)
-- Data.List.Extrema.min≤⊤
d_min'8804''8868'_586 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> [AgdaAny] -> AgdaAny
d_min'8804''8868'_586 ~v0 ~v1 ~v2 v3 = du_min'8804''8868'_586 v3
du_min'8804''8868'_586 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> [AgdaAny] -> AgdaAny
du_min'8804''8868'_586 v0
  = coe
      du_f'91'argmin'93''8804'f'91''8868''93'_182 (coe v0)
      (coe (\ v1 -> v1))
-- Data.List.Extrema.min≤xs
d_min'8804'xs_594 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_min'8804'xs_594 ~v0 ~v1 ~v2 v3 = du_min'8804'xs_594 v3
du_min'8804'xs_594 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_min'8804'xs_594 v0
  = coe
      du_f'91'argmin'93''8804'f'91'xs'93'_194 (coe v0) (coe (\ v1 -> v1))
-- Data.List.Extrema.min≈v⁺
d_min'8776'v'8314'_604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  AgdaAny -> AgdaAny
d_min'8776'v'8314'_604 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_min'8776'v'8314'_604 v3 v4 v5 v6
du_min'8776'v'8314'_604 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  AgdaAny -> AgdaAny
du_min'8776'v'8314'_604 v0 v1 v2 v3
  = coe
      du_f'91'argmin'93''8776'f'91'v'93''8314'_208 (coe v0)
      (coe (\ v4 -> v4)) (coe v1) (coe v2) (coe v3)
-- Data.List.Extrema.min[xs]≤min[ys]⁺
d_min'91'xs'93''8804'min'91'ys'93''8314'_620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_min'91'xs'93''8804'min'91'ys'93''8314'_620 ~v0 ~v1 ~v2 v3
  = du_min'91'xs'93''8804'min'91'ys'93''8314'_620 v3
du_min'91'xs'93''8804'min'91'ys'93''8314'_620 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_min'91'xs'93''8804'min'91'ys'93''8314'_620 v0
  = coe
      du_argmin'91'xs'93''8804'argmin'91'ys'93''8314'_234 (coe v0)
      (coe (\ v1 -> v1)) (coe (\ v1 -> v1))
-- Data.List.Extrema.min[xs]<min[ys]⁺
d_min'91'xs'93''60'min'91'ys'93''8314'_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_min'91'xs'93''60'min'91'ys'93''8314'_636 ~v0 ~v1 ~v2 v3
  = du_min'91'xs'93''60'min'91'ys'93''8314'_636 v3
du_min'91'xs'93''60'min'91'ys'93''8314'_636 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_min'91'xs'93''60'min'91'ys'93''8314'_636 v0
  = coe
      du_argmin'91'xs'93''60'argmin'91'ys'93''8314'_262 (coe v0)
      (coe (\ v1 -> v1)) (coe (\ v1 -> v1))
-- Data.List.Extrema.min-mono-⊆
d_min'45'mono'45''8838'_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  AgdaAny
d_min'45'mono'45''8838'_646 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8 v9
  = du_min'45'mono'45''8838'_646 v3 v4 v5 v6 v7 v8 v9
du_min'45'mono'45''8838'_646 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  AgdaAny
du_min'45'mono'45''8838'_646 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_min'91'xs'93''8804'min'91'ys'93''8314'_620 v0 v1 v2 v3 v4
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v5))
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.All.du_tabulate_272 v4
         (\ v7 v8 ->
            coe
              MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
              (coe
                 MAlonzo.Code.Data.List.Relation.Unary.Any.du_map_76
                 (coe
                    (\ v9 v10 ->
                       let v11
                             = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                                 (coe v0) in
                       let v12
                             = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                                 (coe v11) in
                       coe
                         MAlonzo.Code.Relation.Binary.Structures.du_refl_98
                         (coe
                            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v12))
                         (coe v7)))
                 (coe v3) (coe v6 v7 v8))))
-- Data.List.Extrema.max≤v⁺
d_max'8804'v'8314'_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_max'8804'v'8314'_662 ~v0 ~v1 ~v2 v3 ~v4 v5 v6
  = du_max'8804'v'8314'_662 v3 v5 v6
du_max'8804'v'8314'_662 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_max'8804'v'8314'_662 v0 v1 v2
  = coe
      du_f'91'argmax'93''8804'v'8314'_372 (coe v0) (coe (\ v3 -> v3))
      (coe v1) (coe v2)
-- Data.List.Extrema.max<v⁺
d_max'60'v'8314'_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_max'60'v'8314'_672 ~v0 ~v1 ~v2 v3 ~v4 v5 v6
  = du_max'60'v'8314'_672 v3 v5 v6
du_max'60'v'8314'_672 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_max'60'v'8314'_672 v0 v1 v2
  = coe
      du_f'91'argmax'93''60'v'8314'_382 (coe v0) (coe (\ v3 -> v3))
      (coe v1) (coe v2)
-- Data.List.Extrema.v≤max⁺
d_v'8804'max'8314'_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_v'8804'max'8314'_682 ~v0 ~v1 ~v2 v3 v4
  = du_v'8804'max'8314'_682 v3 v4
du_v'8804'max'8314'_682 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_v'8804'max'8314'_682 v0 v1
  = coe
      du_v'8804'f'91'argmax'93''8314'_352 (coe v0) (coe (\ v2 -> v2))
      (coe v1)
-- Data.List.Extrema.v<max⁺
d_v'60'max'8314'_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_v'60'max'8314'_692 ~v0 ~v1 ~v2 v3 v4
  = du_v'60'max'8314'_692 v3 v4
du_v'60'max'8314'_692 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_v'60'max'8314'_692 v0 v1
  = coe
      du_v'60'f'91'argmax'93''8314'_362 (coe v0) (coe (\ v2 -> v2))
      (coe v1)
-- Data.List.Extrema.⊥≤max
d_'8869''8804'max_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> [AgdaAny] -> AgdaAny
d_'8869''8804'max_698 ~v0 ~v1 ~v2 v3 = du_'8869''8804'max_698 v3
du_'8869''8804'max_698 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> [AgdaAny] -> AgdaAny
du_'8869''8804'max_698 v0
  = coe
      du_f'91''8869''93''8804'f'91'argmax'93'_388 (coe v0)
      (coe (\ v1 -> v1))
-- Data.List.Extrema.xs≤max
d_xs'8804'max_706 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_xs'8804'max_706 ~v0 ~v1 ~v2 v3 = du_xs'8804'max_706 v3
du_xs'8804'max_706 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_xs'8804'max_706 v0
  = coe
      du_f'91'xs'93''8804'f'91'argmax'93'_400 (coe v0) (coe (\ v1 -> v1))
-- Data.List.Extrema.max≈v⁺
d_max'8776'v'8314'_716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  AgdaAny -> AgdaAny
d_max'8776'v'8314'_716 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_max'8776'v'8314'_716 v3 v4 v5 v6
du_max'8776'v'8314'_716 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  AgdaAny -> AgdaAny
du_max'8776'v'8314'_716 v0 v1 v2 v3
  = coe
      du_f'91'argmax'93''8776'f'91'v'93''8314'_414 (coe v0)
      (coe (\ v4 -> v4)) (coe v1) (coe v2) (coe v3)
-- Data.List.Extrema.max[xs]≤max[ys]⁺
d_max'91'xs'93''8804'max'91'ys'93''8314'_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
d_max'91'xs'93''8804'max'91'ys'93''8314'_732 ~v0 ~v1 ~v2 v3 v4
  = du_max'91'xs'93''8804'max'91'ys'93''8314'_732 v3 v4
du_max'91'xs'93''8804'max'91'ys'93''8314'_732 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 -> AgdaAny
du_max'91'xs'93''8804'max'91'ys'93''8314'_732 v0 v1
  = coe
      du_argmax'91'xs'93''8804'argmax'91'ys'93''8314'_440 (coe v0)
      (coe (\ v2 -> v2)) (coe (\ v2 -> v2)) (coe v1)
-- Data.List.Extrema.max[xs]<max[ys]⁺
d_max'91'xs'93''60'max'91'ys'93''8314'_748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_max'91'xs'93''60'max'91'ys'93''8314'_748 ~v0 ~v1 ~v2 v3 v4
  = du_max'91'xs'93''60'max'91'ys'93''8314'_748 v3 v4
du_max'91'xs'93''60'max'91'ys'93''8314'_748 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_max'91'xs'93''60'max'91'ys'93''8314'_748 v0 v1
  = coe
      du_argmax'91'xs'93''60'argmax'91'ys'93''8314'_468 (coe v0)
      (coe (\ v2 -> v2)) (coe (\ v2 -> v2)) (coe v1)
-- Data.List.Extrema.max-mono-⊆
d_max'45'mono'45''8838'_758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  AgdaAny
d_max'45'mono'45''8838'_758 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8 v9
  = du_max'45'mono'45''8838'_758 v3 v4 v5 v6 v7 v8 v9
du_max'45'mono'45''8838'_758 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  AgdaAny
du_max'45'mono'45''8838'_758 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_max'91'xs'93''8804'max'91'ys'93''8314'_732 v0 v1 v2 v3 v4
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v5))
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.All.du_tabulate_272 v3
         (\ v7 v8 ->
            coe
              MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
              (coe
                 MAlonzo.Code.Data.List.Relation.Unary.Any.du_map_76
                 (coe
                    (\ v9 v10 ->
                       let v11
                             = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                                 (coe v0) in
                       let v12
                             = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                                 (coe v11) in
                       coe
                         MAlonzo.Code.Relation.Binary.Structures.du_refl_98
                         (coe
                            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v12))
                         (coe v7)))
                 (coe v4) (coe v6 v7 v8))))
