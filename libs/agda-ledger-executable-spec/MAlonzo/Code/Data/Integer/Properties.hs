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

module MAlonzo.Code.Data.Integer.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.Base
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp
import qualified MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Sign.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Construct.Converse
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Core
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.Integer.Properties._._DistributesOver_
d__DistributesOver__10 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d__DistributesOver__10 = erased
-- Data.Integer.Properties._._DistributesOverʳ_
d__DistributesOver'691'__12 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d__DistributesOver'691'__12 = erased
-- Data.Integer.Properties._._DistributesOverˡ_
d__DistributesOver'737'__14 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d__DistributesOver'737'__14 = erased
-- Data.Integer.Properties._.Associative
d_Associative_28 :: (Integer -> Integer -> Integer) -> ()
d_Associative_28 = erased
-- Data.Integer.Properties._.Commutative
d_Commutative_32 :: (Integer -> Integer -> Integer) -> ()
d_Commutative_32 = erased
-- Data.Integer.Properties._.Identity
d_Identity_48 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_Identity_48 = erased
-- Data.Integer.Properties._.Inverse
d_Inverse_52 ::
  Integer ->
  (Integer -> Integer) -> (Integer -> Integer -> Integer) -> ()
d_Inverse_52 = erased
-- Data.Integer.Properties._.LeftIdentity
d_LeftIdentity_74 ::
  Integer -> (Integer -> Integer -> Integer) -> ()
d_LeftIdentity_74 = erased
-- Data.Integer.Properties._.LeftInverse
d_LeftInverse_76 ::
  Integer ->
  (Integer -> Integer) -> (Integer -> Integer -> Integer) -> ()
d_LeftInverse_76 = erased
-- Data.Integer.Properties._.LeftZero
d_LeftZero_82 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_LeftZero_82 = erased
-- Data.Integer.Properties._.RightIdentity
d_RightIdentity_104 ::
  Integer -> (Integer -> Integer -> Integer) -> ()
d_RightIdentity_104 = erased
-- Data.Integer.Properties._.RightInverse
d_RightInverse_106 ::
  Integer ->
  (Integer -> Integer) -> (Integer -> Integer -> Integer) -> ()
d_RightInverse_106 = erased
-- Data.Integer.Properties._.RightZero
d_RightZero_112 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_RightZero_112 = erased
-- Data.Integer.Properties._.Zero
d_Zero_130 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_Zero_130 = erased
-- Data.Integer.Properties._.IsAbelianGroup
d_IsAbelianGroup_134 a0 a1 a2 = ()
-- Data.Integer.Properties._.IsAlternativeMagma
d_IsAlternativeMagma_136 a0 = ()
-- Data.Integer.Properties._.IsBand
d_IsBand_138 a0 = ()
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring
d_IsCancellativeCommutativeSemiring_140 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsCommutativeMagma
d_IsCommutativeMagma_142 a0 = ()
-- Data.Integer.Properties._.IsCommutativeMonoid
d_IsCommutativeMonoid_144 a0 a1 = ()
-- Data.Integer.Properties._.IsCommutativeRing
d_IsCommutativeRing_146 a0 a1 a2 a3 a4 = ()
-- Data.Integer.Properties._.IsCommutativeSemigroup
d_IsCommutativeSemigroup_148 a0 = ()
-- Data.Integer.Properties._.IsCommutativeSemiring
d_IsCommutativeSemiring_150 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne
d_IsCommutativeSemiringWithoutOne_152 a0 a1 a2 = ()
-- Data.Integer.Properties._.IsFlexibleMagma
d_IsFlexibleMagma_154 a0 = ()
-- Data.Integer.Properties._.IsGroup
d_IsGroup_156 a0 a1 a2 = ()
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid
d_IsIdempotentCommutativeMonoid_158 a0 a1 = ()
-- Data.Integer.Properties._.IsIdempotentMagma
d_IsIdempotentMagma_160 a0 = ()
-- Data.Integer.Properties._.IsIdempotentSemiring
d_IsIdempotentSemiring_162 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsInvertibleMagma
d_IsInvertibleMagma_164 a0 a1 a2 = ()
-- Data.Integer.Properties._.IsInvertibleUnitalMagma
d_IsInvertibleUnitalMagma_166 a0 a1 a2 = ()
-- Data.Integer.Properties._.IsKleeneAlgebra
d_IsKleeneAlgebra_168 a0 a1 a2 a3 a4 = ()
-- Data.Integer.Properties._.IsLeftBolLoop
d_IsLeftBolLoop_170 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsLoop
d_IsLoop_172 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsMagma
d_IsMagma_174 a0 = ()
-- Data.Integer.Properties._.IsMedialMagma
d_IsMedialMagma_176 a0 = ()
-- Data.Integer.Properties._.IsMiddleBolLoop
d_IsMiddleBolLoop_178 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsMonoid
d_IsMonoid_180 a0 a1 = ()
-- Data.Integer.Properties._.IsMoufangLoop
d_IsMoufangLoop_182 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsNearSemiring
d_IsNearSemiring_184 a0 a1 a2 = ()
-- Data.Integer.Properties._.IsNearring
d_IsNearring_186 a0 a1 a2 a3 a4 = ()
-- Data.Integer.Properties._.IsNonAssociativeRing
d_IsNonAssociativeRing_188 a0 a1 a2 a3 a4 = ()
-- Data.Integer.Properties._.IsQuasigroup
d_IsQuasigroup_190 a0 a1 a2 = ()
-- Data.Integer.Properties._.IsQuasiring
d_IsQuasiring_192 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsRightBolLoop
d_IsRightBolLoop_194 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsRing
d_IsRing_196 a0 a1 a2 a3 a4 = ()
-- Data.Integer.Properties._.IsRingWithoutOne
d_IsRingWithoutOne_198 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsSelectiveMagma
d_IsSelectiveMagma_200 a0 = ()
-- Data.Integer.Properties._.IsSemigroup
d_IsSemigroup_202 a0 = ()
-- Data.Integer.Properties._.IsSemimedialMagma
d_IsSemimedialMagma_204 a0 = ()
-- Data.Integer.Properties._.IsSemiring
d_IsSemiring_206 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero
d_IsSemiringWithoutAnnihilatingZero_208 a0 a1 a2 a3 = ()
-- Data.Integer.Properties._.IsSemiringWithoutOne
d_IsSemiringWithoutOne_210 a0 a1 a2 = ()
-- Data.Integer.Properties._.IsUnitalMagma
d_IsUnitalMagma_212 a0 a1 = ()
-- Data.Integer.Properties._.IsAbelianGroup.assoc
d_assoc_218 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_218 = erased
-- Data.Integer.Properties._.IsAbelianGroup.comm
d_comm_220 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_220 = erased
-- Data.Integer.Properties._.IsAbelianGroup.identity
d_identity_222 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Integer.Properties._.IsAbelianGroup.inverse
d_inverse_228 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Integer.Properties._.IsAbelianGroup.isEquivalence
d_isEquivalence_240 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))))
-- Data.Integer.Properties._.IsAbelianGroup.isGroup
d_isGroup_242 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_242 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)
-- Data.Integer.Properties._.IsAbelianGroup.isMagma
d_isMagma_248 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_248 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))))
-- Data.Integer.Properties._.IsAbelianGroup.isMonoid
d_isMonoid_250 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_250 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Integer.Properties._.IsAbelianGroup.isSemigroup
d_isSemigroup_254 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Integer.Properties._.IsAbelianGroup.⁻¹-cong
d_'8315''185''45'cong_272 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_272 = erased
-- Data.Integer.Properties._.IsAbelianGroup.∙-cong
d_'8729''45'cong_274 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_274 = erased
-- Data.Integer.Properties._.IsAlternativeMagma.alter
d_alter_282 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_alter_282 v0
  = coe MAlonzo.Code.Algebra.Structures.d_alter_258 (coe v0)
-- Data.Integer.Properties._.IsAlternativeMagma.isEquivalence
d_isEquivalence_288 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_288 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0))
-- Data.Integer.Properties._.IsAlternativeMagma.isMagma
d_isMagma_290 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_290 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0)
-- Data.Integer.Properties._.IsAlternativeMagma.∙-cong
d_'8729''45'cong_304 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_304 = erased
-- Data.Integer.Properties._.IsBand.assoc
d_assoc_312 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_312 = erased
-- Data.Integer.Properties._.IsBand.idem
d_idem_314 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_314 = erased
-- Data.Integer.Properties._.IsBand.isEquivalence
d_isEquivalence_316 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_316 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)))
-- Data.Integer.Properties._.IsBand.isMagma
d_isMagma_318 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_318 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0))
-- Data.Integer.Properties._.IsBand.isSemigroup
d_isSemigroup_322 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_322 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)
-- Data.Integer.Properties._.IsBand.∙-cong
d_'8729''45'cong_334 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_334 = erased
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.*-assoc
d_'42''45'assoc_342 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_342 = erased
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.*-cancelˡ-nonZero
d_'42''45'cancel'737''45'nonZero_344 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'737''45'nonZero_344 = erased
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.*-comm
d_'42''45'comm_346 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_346 = erased
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.*-cong
d_'42''45'cong_348 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_348 = erased
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.*-identity
d_'42''45'identity_354 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_354 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.assoc
d_assoc_372 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_372 = erased
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.comm
d_comm_374 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_374 = erased
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.∙-cong
d_'8729''45'cong_376 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_376 = erased
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.identity
d_identity_382 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_382 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                     (coe v0))))))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_390 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_390 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.isMagma
d_isMagma_394 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_394 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                        (coe v0)))))))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.isMonoid
d_isMonoid_396 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_396 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                  (coe v0)))))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.isSemigroup
d_isSemigroup_398 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_398 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                     (coe v0))))))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.distrib
d_distrib_402 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_402 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.isCommutativeSemiring
d_isCommutativeSemiring_408 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_408 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
      (coe v0)
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.isEquivalence
d_isEquivalence_412 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_412 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                           (coe v0))))))))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.isSemiring
d_isSemiring_418 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_418 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe v0))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_420 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_420 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe v0)))
-- Data.Integer.Properties._.IsCancellativeCommutativeSemiring.zero
d_zero_434 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_434 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe v0)))
-- Data.Integer.Properties._.IsCommutativeMagma.comm
d_comm_442 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_442 = erased
-- Data.Integer.Properties._.IsCommutativeMagma.isEquivalence
d_isEquivalence_444 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_444 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0))
-- Data.Integer.Properties._.IsCommutativeMagma.isMagma
d_isMagma_446 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_446 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0)
-- Data.Integer.Properties._.IsCommutativeMagma.∙-cong
d_'8729''45'cong_460 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_460 = erased
-- Data.Integer.Properties._.IsCommutativeMonoid.assoc
d_assoc_468 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_468 = erased
-- Data.Integer.Properties._.IsCommutativeMonoid.comm
d_comm_470 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_470 = erased
-- Data.Integer.Properties._.IsCommutativeMonoid.identity
d_identity_472 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_472 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Integer.Properties._.IsCommutativeMonoid.isEquivalence
d_isEquivalence_482 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_482 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))))
-- Data.Integer.Properties._.IsCommutativeMonoid.isMagma
d_isMagma_484 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_484 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)))
-- Data.Integer.Properties._.IsCommutativeMonoid.isMonoid
d_isMonoid_486 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_486 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)
-- Data.Integer.Properties._.IsCommutativeMonoid.isSemigroup
d_isSemigroup_490 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_490 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Integer.Properties._.IsCommutativeMonoid.∙-cong
d_'8729''45'cong_504 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_504 = erased
-- Data.Integer.Properties._.IsCommutativeRing.*-assoc
d_'42''45'assoc_514 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_514 = erased
-- Data.Integer.Properties._.IsCommutativeRing.*-comm
d_'42''45'comm_516 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_516 = erased
-- Data.Integer.Properties._.IsCommutativeRing.*-cong
d_'42''45'cong_518 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_518 = erased
-- Data.Integer.Properties._.IsCommutativeRing.*-identity
d_'42''45'identity_524 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_524 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Integer.Properties._.IsCommutativeRing.assoc
d_assoc_542 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_542 = erased
-- Data.Integer.Properties._.IsCommutativeRing.comm
d_comm_544 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_544 = erased
-- Data.Integer.Properties._.IsCommutativeRing.∙-cong
d_'8729''45'cong_546 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_546 = erased
-- Data.Integer.Properties._.IsCommutativeRing.identity
d_identity_552 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_552 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))
-- Data.Integer.Properties._.IsCommutativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_558 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_558 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Integer.Properties._.IsCommutativeRing.isGroup
d_isGroup_566 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_566 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))
-- Data.Integer.Properties._.IsCommutativeRing.isMagma
d_isMagma_572 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_572 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))))
-- Data.Integer.Properties._.IsCommutativeRing.isMonoid
d_isMonoid_574 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))
-- Data.Integer.Properties._.IsCommutativeRing.isSemigroup
d_isSemigroup_576 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_576 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))
-- Data.Integer.Properties._.IsCommutativeRing.⁻¹-cong
d_'8315''185''45'cong_580 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_580 = erased
-- Data.Integer.Properties._.IsCommutativeRing.inverse
d_inverse_582 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))
-- Data.Integer.Properties._.IsCommutativeRing.distrib
d_distrib_588 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_588 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2426
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Integer.Properties._.IsCommutativeRing.isEquivalence
d_isEquivalence_598 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_598 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))))
-- Data.Integer.Properties._.IsCommutativeRing.isRing
d_isRing_604 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_604 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)
-- Data.Integer.Properties._.IsCommutativeRing.zero
d_zero_626 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_626 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2428
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Integer.Properties._.IsCommutativeSemigroup.assoc
d_assoc_634 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_634 = erased
-- Data.Integer.Properties._.IsCommutativeSemigroup.comm
d_comm_636 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_636 = erased
-- Data.Integer.Properties._.IsCommutativeSemigroup.isEquivalence
d_isEquivalence_640 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_640 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)))
-- Data.Integer.Properties._.IsCommutativeSemigroup.isMagma
d_isMagma_642 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0))
-- Data.Integer.Properties._.IsCommutativeSemigroup.isSemigroup
d_isSemigroup_646 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_646 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)
-- Data.Integer.Properties._.IsCommutativeSemigroup.∙-cong
d_'8729''45'cong_658 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_658 = erased
-- Data.Integer.Properties._.IsCommutativeSemiring.*-assoc
d_'42''45'assoc_666 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_666 = erased
-- Data.Integer.Properties._.IsCommutativeSemiring.*-comm
d_'42''45'comm_668 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_668 = erased
-- Data.Integer.Properties._.IsCommutativeSemiring.*-cong
d_'42''45'cong_670 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_670 = erased
-- Data.Integer.Properties._.IsCommutativeSemiring.*-identity
d_'42''45'identity_676 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_676 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Integer.Properties._.IsCommutativeSemiring.assoc
d_assoc_694 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_694 = erased
-- Data.Integer.Properties._.IsCommutativeSemiring.comm
d_comm_696 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_696 = erased
-- Data.Integer.Properties._.IsCommutativeSemiring.∙-cong
d_'8729''45'cong_698 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_698 = erased
-- Data.Integer.Properties._.IsCommutativeSemiring.identity
d_identity_704 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_704 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))
-- Data.Integer.Properties._.IsCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_712 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_712 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Integer.Properties._.IsCommutativeSemiring.isMagma
d_isMagma_716 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_716 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))))))
-- Data.Integer.Properties._.IsCommutativeSemiring.isMonoid
d_isMonoid_718 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_718 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))))
-- Data.Integer.Properties._.IsCommutativeSemiring.isSemigroup
d_isSemigroup_720 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_720 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))
-- Data.Integer.Properties._.IsCommutativeSemiring.distrib
d_distrib_724 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_724 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Integer.Properties._.IsCommutativeSemiring.isEquivalence
d_isEquivalence_732 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_732 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))))
-- Data.Integer.Properties._.IsCommutativeSemiring.isSemiring
d_isSemiring_738 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_738 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)
-- Data.Integer.Properties._.IsCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_740 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_740 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Integer.Properties._.IsCommutativeSemiring.zero
d_zero_754 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_754 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.*-assoc
d_'42''45'assoc_762 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_762 = erased
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.*-comm
d_'42''45'comm_764 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_764 = erased
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.*-cong
d_'42''45'cong_766 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_766 = erased
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.comm
d_comm_780 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_780 = erased
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_784 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_784 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.isMonoid
d_isMonoid_788 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_788 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
            (coe v0)))
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.distrib
d_distrib_790 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_790 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1166
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.isEquivalence
d_isEquivalence_792 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_792 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
                     (coe v0))))))
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.isSemiringWithoutOne
d_isSemiringWithoutOne_796 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_796 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
      (coe v0)
-- Data.Integer.Properties._.IsCommutativeSemiringWithoutOne.zero
d_zero_798 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_798 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1168
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Integer.Properties._.IsFlexibleMagma.flex
d_flex_806 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_flex_806 = erased
-- Data.Integer.Properties._.IsFlexibleMagma.isEquivalence
d_isEquivalence_808 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_808 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0))
-- Data.Integer.Properties._.IsFlexibleMagma.isMagma
d_isMagma_810 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_810 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0)
-- Data.Integer.Properties._.IsFlexibleMagma.∙-cong
d_'8729''45'cong_824 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_824 = erased
-- Data.Integer.Properties._.IsGroup.assoc
d_assoc_834 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_834 = erased
-- Data.Integer.Properties._.IsGroup.identity
d_identity_836 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_836 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Integer.Properties._.IsGroup.inverse
d_inverse_842 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_842 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_904 (coe v0)
-- Data.Integer.Properties._.IsGroup.isEquivalence
d_isEquivalence_848 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_848 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))))
-- Data.Integer.Properties._.IsGroup.isMagma
d_isMagma_854 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_854 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)))
-- Data.Integer.Properties._.IsGroup.isMonoid
d_isMonoid_856 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_856 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)
-- Data.Integer.Properties._.IsGroup.isSemigroup
d_isSemigroup_860 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_860 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Integer.Properties._.IsGroup.⁻¹-cong
d_'8315''185''45'cong_878 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_878 = erased
-- Data.Integer.Properties._.IsGroup.∙-cong
d_'8729''45'cong_880 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_880 = erased
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.assoc
d_assoc_888 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_888 = erased
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.comm
d_comm_890 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_890 = erased
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.idem
d_idem_892 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_892 = erased
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.identity
d_identity_894 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_894 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.isCommutativeMonoid
d_isCommutativeMonoid_904 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_904 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0)
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.isEquivalence
d_isEquivalence_908 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_908 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe v0)))))
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.isMagma
d_isMagma_910 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_910 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.isMonoid
d_isMonoid_912 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_912 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.isSemigroup
d_isSemigroup_916 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_916 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Integer.Properties._.IsIdempotentCommutativeMonoid.∙-cong
d_'8729''45'cong_930 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_930 = erased
-- Data.Integer.Properties._.IsIdempotentMagma.idem
d_idem_938 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_938 = erased
-- Data.Integer.Properties._.IsIdempotentMagma.isEquivalence
d_isEquivalence_940 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_940 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0))
-- Data.Integer.Properties._.IsIdempotentMagma.isMagma
d_isMagma_942 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_942 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0)
-- Data.Integer.Properties._.IsIdempotentMagma.∙-cong
d_'8729''45'cong_956 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_956 = erased
-- Data.Integer.Properties._.IsIdempotentSemiring.*-assoc
d_'42''45'assoc_964 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_964 = erased
-- Data.Integer.Properties._.IsIdempotentSemiring.*-cong
d_'42''45'cong_966 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_966 = erased
-- Data.Integer.Properties._.IsIdempotentSemiring.*-identity
d_'42''45'identity_972 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_972 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Integer.Properties._.IsIdempotentSemiring.assoc
d_assoc_984 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_984 = erased
-- Data.Integer.Properties._.IsIdempotentSemiring.comm
d_comm_986 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_986 = erased
-- Data.Integer.Properties._.IsIdempotentSemiring.∙-cong
d_'8729''45'cong_988 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_988 = erased
-- Data.Integer.Properties._.IsIdempotentSemiring.+-idem
d_'43''45'idem_994 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_994 = erased
-- Data.Integer.Properties._.IsIdempotentSemiring.identity
d_identity_996 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_996 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))
-- Data.Integer.Properties._.IsIdempotentSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1004 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1004 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Integer.Properties._.IsIdempotentSemiring.isMagma
d_isMagma_1008 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1008 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))))))
-- Data.Integer.Properties._.IsIdempotentSemiring.isMonoid
d_isMonoid_1010 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1010 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))))
-- Data.Integer.Properties._.IsIdempotentSemiring.isSemigroup
d_isSemigroup_1012 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1012 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))
-- Data.Integer.Properties._.IsIdempotentSemiring.distrib
d_distrib_1016 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1016 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Integer.Properties._.IsIdempotentSemiring.isEquivalence
d_isEquivalence_1022 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1022 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))))
-- Data.Integer.Properties._.IsIdempotentSemiring.isSemiring
d_isSemiring_1028 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1028 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)
-- Data.Integer.Properties._.IsIdempotentSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1030 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1030 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Integer.Properties._.IsIdempotentSemiring.zero
d_zero_1044 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1044 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Integer.Properties._.IsInvertibleMagma.inverse
d_inverse_1052 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1052 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_792 (coe v0)
-- Data.Integer.Properties._.IsInvertibleMagma.isEquivalence
d_isEquivalence_1058 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0))
-- Data.Integer.Properties._.IsInvertibleMagma.isMagma
d_isMagma_1060 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1060 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0)
-- Data.Integer.Properties._.IsInvertibleMagma.⁻¹-cong
d_'8315''185''45'cong_1074 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1074 = erased
-- Data.Integer.Properties._.IsInvertibleMagma.∙-cong
d_'8729''45'cong_1076 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1076 = erased
-- Data.Integer.Properties._.IsInvertibleUnitalMagma.identity
d_identity_1084 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1084 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_842 (coe v0)
-- Data.Integer.Properties._.IsInvertibleUnitalMagma.inverse
d_inverse_1090 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1090 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_792
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Integer.Properties._.IsInvertibleUnitalMagma.isEquivalence
d_isEquivalence_1096 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1096 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe
            MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)))
-- Data.Integer.Properties._.IsInvertibleUnitalMagma.isInvertibleMagma
d_isInvertibleMagma_1098 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_1098 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)
-- Data.Integer.Properties._.IsInvertibleUnitalMagma.isMagma
d_isMagma_1100 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1100 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_790
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Integer.Properties._.IsInvertibleUnitalMagma.⁻¹-cong
d_'8315''185''45'cong_1116 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1116 = erased
-- Data.Integer.Properties._.IsInvertibleUnitalMagma.∙-cong
d_'8729''45'cong_1118 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1118 = erased
-- Data.Integer.Properties._.IsKleeneAlgebra.*-assoc
d_'42''45'assoc_1126 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1126 = erased
-- Data.Integer.Properties._.IsKleeneAlgebra.*-cong
d_'42''45'cong_1128 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1128 = erased
-- Data.Integer.Properties._.IsKleeneAlgebra.*-identity
d_'42''45'identity_1134 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1134 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Integer.Properties._.IsKleeneAlgebra.assoc
d_assoc_1146 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1146 = erased
-- Data.Integer.Properties._.IsKleeneAlgebra.comm
d_comm_1148 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1148 = erased
-- Data.Integer.Properties._.IsKleeneAlgebra.∙-cong
d_'8729''45'cong_1150 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1150 = erased
-- Data.Integer.Properties._.IsKleeneAlgebra.+-idem
d_'43''45'idem_1156 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_1156 = erased
-- Data.Integer.Properties._.IsKleeneAlgebra.identity
d_identity_1158 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1158 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                     (coe v0))))))
-- Data.Integer.Properties._.IsKleeneAlgebra.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1166 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1166 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Integer.Properties._.IsKleeneAlgebra.isMagma
d_isMagma_1170 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1170 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                        (coe v0)))))))
-- Data.Integer.Properties._.IsKleeneAlgebra.isMonoid
d_isMonoid_1172 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1172 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                  (coe v0)))))
-- Data.Integer.Properties._.IsKleeneAlgebra.isSemigroup
d_isSemigroup_1174 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1174 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                     (coe v0))))))
-- Data.Integer.Properties._.IsKleeneAlgebra.distrib
d_distrib_1178 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1178 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Integer.Properties._.IsKleeneAlgebra.isEquivalence
d_isEquivalence_1184 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1184 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                           (coe v0))))))))
-- Data.Integer.Properties._.IsKleeneAlgebra.isIdempotentSemiring
d_isIdempotentSemiring_1186 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722
d_isIdempotentSemiring_1186 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
      (coe v0)
-- Data.Integer.Properties._.IsKleeneAlgebra.isSemiring
d_isSemiring_1192 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1192 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
      (coe
         MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
         (coe v0))
-- Data.Integer.Properties._.IsKleeneAlgebra.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1194 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1194 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe v0)))
-- Data.Integer.Properties._.IsKleeneAlgebra.starDestructive
d_starDestructive_1204 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starDestructive_1204 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_starDestructive_1856 (coe v0)
-- Data.Integer.Properties._.IsKleeneAlgebra.starExpansive
d_starExpansive_1210 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starExpansive_1210 v0
  = coe MAlonzo.Code.Algebra.Structures.d_starExpansive_1854 (coe v0)
-- Data.Integer.Properties._.IsKleeneAlgebra.zero
d_zero_1220 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1220 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe v0)))
-- Data.Integer.Properties._.IsLeftBolLoop.//-cong
d_'47''47''45'cong_1228 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1228 = erased
-- Data.Integer.Properties._.IsLeftBolLoop.\\-cong
d_'92''92''45'cong_1234 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1234 = erased
-- Data.Integer.Properties._.IsLeftBolLoop.identity
d_identity_1240 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Integer.Properties._.IsLeftBolLoop.isEquivalence
d_isEquivalence_1246 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1246 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))))
-- Data.Integer.Properties._.IsLeftBolLoop.isLoop
d_isLoop_1248 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1248 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)
-- Data.Integer.Properties._.IsLeftBolLoop.isMagma
d_isMagma_1250 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1250 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Integer.Properties._.IsLeftBolLoop.isQuasigroup
d_isQuasigroup_1254 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Integer.Properties._.IsLeftBolLoop.leftBol
d_leftBol_1256 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1256 = erased
-- Data.Integer.Properties._.IsLeftBolLoop.leftDivides
d_leftDivides_1258 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1258 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Integer.Properties._.IsLeftBolLoop.rightDivides
d_rightDivides_1268 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1268 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Integer.Properties._.IsLeftBolLoop.∙-cong
d_'8729''45'cong_1280 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1280 = erased
-- Data.Integer.Properties._.IsLoop.//-cong
d_'47''47''45'cong_1288 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1288 = erased
-- Data.Integer.Properties._.IsLoop.\\-cong
d_'92''92''45'cong_1294 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1294 = erased
-- Data.Integer.Properties._.IsLoop.identity
d_identity_1300 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1300 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2784 (coe v0)
-- Data.Integer.Properties._.IsLoop.isEquivalence
d_isEquivalence_1306 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1306 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)))
-- Data.Integer.Properties._.IsLoop.isMagma
d_isMagma_1308 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1308 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Integer.Properties._.IsLoop.isQuasigroup
d_isQuasigroup_1312 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1312 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)
-- Data.Integer.Properties._.IsLoop.leftDivides
d_leftDivides_1314 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1314 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Integer.Properties._.IsLoop.rightDivides
d_rightDivides_1324 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1324 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Integer.Properties._.IsLoop.∙-cong
d_'8729''45'cong_1336 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1336 = erased
-- Data.Integer.Properties._.IsMagma.isEquivalence
d_isEquivalence_1344 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1344 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v0)
-- Data.Integer.Properties._.IsMagma.∙-cong
d_'8729''45'cong_1358 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1358 = erased
-- Data.Integer.Properties._.IsMedialMagma.isEquivalence
d_isEquivalence_1366 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1366 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0))
-- Data.Integer.Properties._.IsMedialMagma.isMagma
d_isMagma_1368 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1368 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0)
-- Data.Integer.Properties._.IsMedialMagma.medial
d_medial_1372 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_medial_1372 = erased
-- Data.Integer.Properties._.IsMedialMagma.∙-cong
d_'8729''45'cong_1384 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1384 = erased
-- Data.Integer.Properties._.IsMiddleBolLoop.//-cong
d_'47''47''45'cong_1392 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1392 = erased
-- Data.Integer.Properties._.IsMiddleBolLoop.\\-cong
d_'92''92''45'cong_1398 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1398 = erased
-- Data.Integer.Properties._.IsMiddleBolLoop.identity
d_identity_1404 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1404 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Integer.Properties._.IsMiddleBolLoop.isEquivalence
d_isEquivalence_1410 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1410 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))))
-- Data.Integer.Properties._.IsMiddleBolLoop.isLoop
d_isLoop_1412 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1412 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)
-- Data.Integer.Properties._.IsMiddleBolLoop.isMagma
d_isMagma_1414 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1414 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Integer.Properties._.IsMiddleBolLoop.isQuasigroup
d_isQuasigroup_1418 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1418 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Integer.Properties._.IsMiddleBolLoop.leftDivides
d_leftDivides_1420 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1420 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Integer.Properties._.IsMiddleBolLoop.middleBol
d_middleBol_1426 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_middleBol_1426 = erased
-- Data.Integer.Properties._.IsMiddleBolLoop.rightDivides
d_rightDivides_1432 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1432 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Integer.Properties._.IsMiddleBolLoop.∙-cong
d_'8729''45'cong_1444 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1444 = erased
-- Data.Integer.Properties._.IsMonoid.assoc
d_assoc_1452 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1452 = erased
-- Data.Integer.Properties._.IsMonoid.identity
d_identity_1454 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1454 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_612 (coe v0)
-- Data.Integer.Properties._.IsMonoid.isEquivalence
d_isEquivalence_1460 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1460 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)))
-- Data.Integer.Properties._.IsMonoid.isMagma
d_isMagma_1462 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1462 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0))
-- Data.Integer.Properties._.IsMonoid.isSemigroup
d_isSemigroup_1466 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1466 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)
-- Data.Integer.Properties._.IsMonoid.∙-cong
d_'8729''45'cong_1480 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1480 = erased
-- Data.Integer.Properties._.IsMoufangLoop.//-cong
d_'47''47''45'cong_1488 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1488 = erased
-- Data.Integer.Properties._.IsMoufangLoop.\\-cong
d_'92''92''45'cong_1494 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1494 = erased
-- Data.Integer.Properties._.IsMoufangLoop.identical
d_identical_1500 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identical_1500 = erased
-- Data.Integer.Properties._.IsMoufangLoop.identity
d_identity_1502 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1502 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))
-- Data.Integer.Properties._.IsMoufangLoop.isEquivalence
d_isEquivalence_1508 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1508 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_2860
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))))
-- Data.Integer.Properties._.IsMoufangLoop.isLeftBolLoop
d_isLeftBolLoop_1510 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846
d_isLeftBolLoop_1510 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)
-- Data.Integer.Properties._.IsMoufangLoop.isLoop
d_isLoop_1512 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1512 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_2860
      (coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))
-- Data.Integer.Properties._.IsMoufangLoop.isMagma
d_isMagma_1514 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1514 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Integer.Properties._.IsMoufangLoop.isQuasigroup
d_isQuasigroup_1518 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1518 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))
-- Data.Integer.Properties._.IsMoufangLoop.leftBol
d_leftBol_1520 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1520 = erased
-- Data.Integer.Properties._.IsMoufangLoop.leftDivides
d_leftDivides_1522 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1522 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Integer.Properties._.IsMoufangLoop.rightBol
d_rightBol_1532 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1532 = erased
-- Data.Integer.Properties._.IsMoufangLoop.rightDivides
d_rightDivides_1534 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1534 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Integer.Properties._.IsMoufangLoop.∙-cong
d_'8729''45'cong_1546 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1546 = erased
-- Data.Integer.Properties._.IsNearSemiring.*-assoc
d_'42''45'assoc_1554 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1554 = erased
-- Data.Integer.Properties._.IsNearSemiring.*-cong
d_'42''45'cong_1556 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1556 = erased
-- Data.Integer.Properties._.IsNearSemiring.assoc
d_assoc_1566 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1566 = erased
-- Data.Integer.Properties._.IsNearSemiring.∙-cong
d_'8729''45'cong_1568 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1568 = erased
-- Data.Integer.Properties._.IsNearSemiring.identity
d_identity_1574 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Integer.Properties._.IsNearSemiring.isMagma
d_isMagma_1580 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1580 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)))
-- Data.Integer.Properties._.IsNearSemiring.+-isMonoid
d_'43''45'isMonoid_1582 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)
-- Data.Integer.Properties._.IsNearSemiring.isSemigroup
d_isSemigroup_1584 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1584 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Integer.Properties._.IsNearSemiring.distribʳ
d_distrib'691'_1588 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'691'_1588 = erased
-- Data.Integer.Properties._.IsNearSemiring.isEquivalence
d_isEquivalence_1590 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1590 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))))
-- Data.Integer.Properties._.IsNearSemiring.zeroˡ
d_zero'737'_1604 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zero'737'_1604 = erased
-- Data.Integer.Properties._.IsNearring.*-assoc
d_'42''45'assoc_1608 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1608 = erased
-- Data.Integer.Properties._.IsNearring.*-cong
d_'42''45'cong_1610 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1610 = erased
-- Data.Integer.Properties._.IsNearring.*-identity
d_'42''45'identity_1616 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1616 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Integer.Properties._.IsNearring.assoc
d_assoc_1628 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1628 = erased
-- Data.Integer.Properties._.IsNearring.∙-cong
d_'8729''45'cong_1630 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1630 = erased
-- Data.Integer.Properties._.IsNearring.identity
d_identity_1636 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1636 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Integer.Properties._.IsNearring.+-inverse
d_'43''45'inverse_1642 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_1642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'inverse_2314 (coe v0)
-- Data.Integer.Properties._.IsNearring.isMagma
d_isMagma_1648 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1648 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
            (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))))
-- Data.Integer.Properties._.IsNearring.+-isMonoid
d_'43''45'isMonoid_1650 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1650 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Integer.Properties._.IsNearring.isSemigroup
d_isSemigroup_1652 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1652 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Integer.Properties._.IsNearring.distrib
d_distrib_1656 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1656 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1992
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Integer.Properties._.IsNearring.isEquivalence
d_isEquivalence_1658 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1658 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))))
-- Data.Integer.Properties._.IsNearring.isQuasiring
d_isQuasiring_1662 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962
d_isQuasiring_1662 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)
-- Data.Integer.Properties._.IsNearring.zero
d_zero_1674 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1674 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1994
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Integer.Properties._.IsNearring.⁻¹-cong
d_'8315''185''45'cong_1676 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1676 = erased
-- Data.Integer.Properties._.IsNonAssociativeRing.*-cong
d_'42''45'cong_1682 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1682 = erased
-- Data.Integer.Properties._.IsNonAssociativeRing.assoc
d_assoc_1690 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1690 = erased
-- Data.Integer.Properties._.IsNonAssociativeRing.comm
d_comm_1692 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1692 = erased
-- Data.Integer.Properties._.IsNonAssociativeRing.∙-cong
d_'8729''45'cong_1694 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1694 = erased
-- Data.Integer.Properties._.IsNonAssociativeRing.identity
d_identity_1700 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1700 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe v0))))
-- Data.Integer.Properties._.IsNonAssociativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1706 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1706 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
      (coe v0)
-- Data.Integer.Properties._.IsNonAssociativeRing.isGroup
d_isGroup_1714 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1714 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe v0))
-- Data.Integer.Properties._.IsNonAssociativeRing.isMagma
d_isMagma_1720 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1720 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                  (coe v0)))))
-- Data.Integer.Properties._.IsNonAssociativeRing.isMonoid
d_isMonoid_1722 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1722 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe v0)))
-- Data.Integer.Properties._.IsNonAssociativeRing.isSemigroup
d_isSemigroup_1724 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1724 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe v0))))
-- Data.Integer.Properties._.IsNonAssociativeRing.⁻¹-cong
d_'8315''185''45'cong_1728 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1728 = erased
-- Data.Integer.Properties._.IsNonAssociativeRing.inverse
d_inverse_1730 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1730 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe v0)))
-- Data.Integer.Properties._.IsNonAssociativeRing.distrib
d_distrib_1736 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1736 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2208 (coe v0)
-- Data.Integer.Properties._.IsNonAssociativeRing.identity
d_identity_1738 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1738 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2206 (coe v0)
-- Data.Integer.Properties._.IsNonAssociativeRing.isEquivalence
d_isEquivalence_1740 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1740 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                     (coe v0))))))
-- Data.Integer.Properties._.IsNonAssociativeRing.zero
d_zero_1758 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1758 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2210 (coe v0)
-- Data.Integer.Properties._.IsQuasigroup.//-cong
d_'47''47''45'cong_1762 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1762 = erased
-- Data.Integer.Properties._.IsQuasigroup.\\-cong
d_'92''92''45'cong_1768 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1768 = erased
-- Data.Integer.Properties._.IsQuasigroup.isEquivalence
d_isEquivalence_1774 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1774 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0))
-- Data.Integer.Properties._.IsQuasigroup.isMagma
d_isMagma_1776 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1776 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0)
-- Data.Integer.Properties._.IsQuasigroup.leftDivides
d_leftDivides_1780 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1780 v0
  = coe MAlonzo.Code.Algebra.Structures.d_leftDivides_2710 (coe v0)
-- Data.Integer.Properties._.IsQuasigroup.rightDivides
d_rightDivides_1790 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1790 v0
  = coe MAlonzo.Code.Algebra.Structures.d_rightDivides_2712 (coe v0)
-- Data.Integer.Properties._.IsQuasigroup.∙-cong
d_'8729''45'cong_1802 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1802 = erased
-- Data.Integer.Properties._.IsQuasiring.*-assoc
d_'42''45'assoc_1810 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1810 = erased
-- Data.Integer.Properties._.IsQuasiring.*-cong
d_'42''45'cong_1812 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1812 = erased
-- Data.Integer.Properties._.IsQuasiring.*-identity
d_'42''45'identity_1818 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1818 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990 (coe v0)
-- Data.Integer.Properties._.IsQuasiring.assoc
d_assoc_1830 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1830 = erased
-- Data.Integer.Properties._.IsQuasiring.∙-cong
d_'8729''45'cong_1832 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1832 = erased
-- Data.Integer.Properties._.IsQuasiring.identity
d_identity_1838 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1838 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Integer.Properties._.IsQuasiring.isMagma
d_isMagma_1844 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1844 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)))
-- Data.Integer.Properties._.IsQuasiring.+-isMonoid
d_'43''45'isMonoid_1846 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1846 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)
-- Data.Integer.Properties._.IsQuasiring.isSemigroup
d_isSemigroup_1848 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1848 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Integer.Properties._.IsQuasiring.distrib
d_distrib_1852 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1852 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1992 (coe v0)
-- Data.Integer.Properties._.IsQuasiring.isEquivalence
d_isEquivalence_1854 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1854 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))))
-- Data.Integer.Properties._.IsQuasiring.zero
d_zero_1868 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1868 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1994 (coe v0)
-- Data.Integer.Properties._.IsRightBolLoop.//-cong
d_'47''47''45'cong_1872 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1872 = erased
-- Data.Integer.Properties._.IsRightBolLoop.\\-cong
d_'92''92''45'cong_1878 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1878 = erased
-- Data.Integer.Properties._.IsRightBolLoop.identity
d_identity_1884 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1884 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Integer.Properties._.IsRightBolLoop.isEquivalence
d_isEquivalence_1890 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1890 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))))
-- Data.Integer.Properties._.IsRightBolLoop.isLoop
d_isLoop_1892 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1892 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)
-- Data.Integer.Properties._.IsRightBolLoop.isMagma
d_isMagma_1894 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1894 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Integer.Properties._.IsRightBolLoop.isQuasigroup
d_isQuasigroup_1898 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1898 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Integer.Properties._.IsRightBolLoop.leftDivides
d_leftDivides_1900 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1900 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Integer.Properties._.IsRightBolLoop.rightBol
d_rightBol_1910 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1910 = erased
-- Data.Integer.Properties._.IsRightBolLoop.rightDivides
d_rightDivides_1912 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1912 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Integer.Properties._.IsRightBolLoop.∙-cong
d_'8729''45'cong_1924 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1924 = erased
-- Data.Integer.Properties._.IsRing.*-assoc
d_'42''45'assoc_1934 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1934 = erased
-- Data.Integer.Properties._.IsRing.*-cong
d_'42''45'cong_1936 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1936 = erased
-- Data.Integer.Properties._.IsRing.*-identity
d_'42''45'identity_1942 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1942 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424 (coe v0)
-- Data.Integer.Properties._.IsRing.assoc
d_assoc_1954 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1954 = erased
-- Data.Integer.Properties._.IsRing.comm
d_comm_1956 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1956 = erased
-- Data.Integer.Properties._.IsRing.∙-cong
d_'8729''45'cong_1958 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1958 = erased
-- Data.Integer.Properties._.IsRing.identity
d_identity_1964 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1964 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v0))))
-- Data.Integer.Properties._.IsRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1970 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1970 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe v0)
-- Data.Integer.Properties._.IsRing.isGroup
d_isGroup_1978 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1978 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe v0))
-- Data.Integer.Properties._.IsRing.isMagma
d_isMagma_1984 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1984 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe v0)))))
-- Data.Integer.Properties._.IsRing.isMonoid
d_isMonoid_1986 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1986 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v0)))
-- Data.Integer.Properties._.IsRing.isSemigroup
d_isSemigroup_1988 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1988 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v0))))
-- Data.Integer.Properties._.IsRing.⁻¹-cong
d_'8315''185''45'cong_1992 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1992 = erased
-- Data.Integer.Properties._.IsRing.inverse
d_inverse_1994 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1994 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v0)))
-- Data.Integer.Properties._.IsRing.distrib
d_distrib_2000 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2000 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2426 (coe v0)
-- Data.Integer.Properties._.IsRing.isEquivalence
d_isEquivalence_2006 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2006 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe v0))))))
-- Data.Integer.Properties._.IsRing.zero
d_zero_2032 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2032 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2428 (coe v0)
-- Data.Integer.Properties._.IsRingWithoutOne.*-assoc
d_'42''45'assoc_2042 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2042 = erased
-- Data.Integer.Properties._.IsRingWithoutOne.*-cong
d_'42''45'cong_2044 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2044 = erased
-- Data.Integer.Properties._.IsRingWithoutOne.assoc
d_assoc_2054 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2054 = erased
-- Data.Integer.Properties._.IsRingWithoutOne.comm
d_comm_2056 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2056 = erased
-- Data.Integer.Properties._.IsRingWithoutOne.∙-cong
d_'8729''45'cong_2058 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2058 = erased
-- Data.Integer.Properties._.IsRingWithoutOne.identity
d_identity_2064 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2064 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe v0))))
-- Data.Integer.Properties._.IsRingWithoutOne.+-isAbelianGroup
d_'43''45'isAbelianGroup_2070 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_2070 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
      (coe v0)
-- Data.Integer.Properties._.IsRingWithoutOne.isGroup
d_isGroup_2078 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_2078 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe v0))
-- Data.Integer.Properties._.IsRingWithoutOne.isMagma
d_isMagma_2084 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2084 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                  (coe v0)))))
-- Data.Integer.Properties._.IsRingWithoutOne.isMonoid
d_isMonoid_2086 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2086 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe v0)))
-- Data.Integer.Properties._.IsRingWithoutOne.isSemigroup
d_isSemigroup_2088 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2088 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe v0))))
-- Data.Integer.Properties._.IsRingWithoutOne.⁻¹-cong
d_'8315''185''45'cong_2092 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_2092 = erased
-- Data.Integer.Properties._.IsRingWithoutOne.inverse
d_inverse_2094 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_2094 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe v0)))
-- Data.Integer.Properties._.IsRingWithoutOne.distrib
d_distrib_2100 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2100 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2082 (coe v0)
-- Data.Integer.Properties._.IsRingWithoutOne.isEquivalence
d_isEquivalence_2106 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2106 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                     (coe v0))))))
-- Data.Integer.Properties._.IsRingWithoutOne.zero
d_zero_2124 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2124 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2084 (coe v0)
-- Data.Integer.Properties._.IsSelectiveMagma.isEquivalence
d_isEquivalence_2132 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0))
-- Data.Integer.Properties._.IsSelectiveMagma.isMagma
d_isMagma_2134 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2134 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0)
-- Data.Integer.Properties._.IsSelectiveMagma.sel
d_sel_2142 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_2142 v0
  = coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0)
-- Data.Integer.Properties._.IsSelectiveMagma.∙-cong
d_'8729''45'cong_2150 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2150 = erased
-- Data.Integer.Properties._.IsSemigroup.assoc
d_assoc_2158 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2158 = erased
-- Data.Integer.Properties._.IsSemigroup.isEquivalence
d_isEquivalence_2160 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2160 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0))
-- Data.Integer.Properties._.IsSemigroup.isMagma
d_isMagma_2162 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2162 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0)
-- Data.Integer.Properties._.IsSemigroup.∙-cong
d_'8729''45'cong_2176 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2176 = erased
-- Data.Integer.Properties._.IsSemimedialMagma.isEquivalence
d_isEquivalence_2184 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2184 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0))
-- Data.Integer.Properties._.IsSemimedialMagma.isMagma
d_isMagma_2186 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2186 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0)
-- Data.Integer.Properties._.IsSemimedialMagma.semiMedial
d_semiMedial_2194 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_semiMedial_2194 v0
  = coe MAlonzo.Code.Algebra.Structures.d_semiMedial_370 (coe v0)
-- Data.Integer.Properties._.IsSemimedialMagma.∙-cong
d_'8729''45'cong_2206 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2206 = erased
-- Data.Integer.Properties._.IsSemiring.*-assoc
d_'42''45'assoc_2214 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2214 = erased
-- Data.Integer.Properties._.IsSemiring.*-cong
d_'42''45'cong_2216 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2216 = erased
-- Data.Integer.Properties._.IsSemiring.*-identity
d_'42''45'identity_2222 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Integer.Properties._.IsSemiring.assoc
d_assoc_2234 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2234 = erased
-- Data.Integer.Properties._.IsSemiring.comm
d_comm_2236 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2236 = erased
-- Data.Integer.Properties._.IsSemiring.∙-cong
d_'8729''45'cong_2238 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2238 = erased
-- Data.Integer.Properties._.IsSemiring.identity
d_identity_2244 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2244 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v0))))
-- Data.Integer.Properties._.IsSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2252 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2252 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Integer.Properties._.IsSemiring.isMagma
d_isMagma_2256 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2256 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe v0)))))
-- Data.Integer.Properties._.IsSemiring.isMonoid
d_isMonoid_2258 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2258 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe v0)))
-- Data.Integer.Properties._.IsSemiring.isSemigroup
d_isSemigroup_2260 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2260 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v0))))
-- Data.Integer.Properties._.IsSemiring.distrib
d_distrib_2264 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2264 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Integer.Properties._.IsSemiring.isEquivalence
d_isEquivalence_2270 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2270 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe v0))))))
-- Data.Integer.Properties._.IsSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2276 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2276 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe v0)
-- Data.Integer.Properties._.IsSemiring.zero
d_zero_2290 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2290 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1388 (coe v0)
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.*-assoc
d_'42''45'assoc_2298 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2298 = erased
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.*-cong
d_'42''45'cong_2300 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2300 = erased
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.*-identity
d_'42''45'identity_2306 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2306 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296 (coe v0)
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.assoc
d_assoc_2318 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2318 = erased
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.comm
d_comm_2320 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2320 = erased
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.∙-cong
d_'8729''45'cong_2322 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2322 = erased
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.identity
d_identity_2328 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2328 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe v0)))
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2336 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2336 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe v0)
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.isMagma
d_isMagma_2340 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2340 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe v0))))
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.isMonoid
d_isMonoid_2342 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2342 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v0))
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.isSemigroup
d_isSemigroup_2344 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2344 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe v0)))
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.distrib
d_distrib_2348 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2348 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1298 (coe v0)
-- Data.Integer.Properties._.IsSemiringWithoutAnnihilatingZero.isEquivalence
d_isEquivalence_2354 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2354 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe v0)))))
-- Data.Integer.Properties._.IsSemiringWithoutOne.*-assoc
d_'42''45'assoc_2370 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2370 = erased
-- Data.Integer.Properties._.IsSemiringWithoutOne.*-cong
d_'42''45'cong_2372 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2372 = erased
-- Data.Integer.Properties._.IsSemiringWithoutOne.comm
d_comm_2382 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2382 = erased
-- Data.Integer.Properties._.IsSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2386 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2386 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe v0)
-- Data.Integer.Properties._.IsSemiringWithoutOne.isMonoid
d_isMonoid_2390 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2390 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe v0))
-- Data.Integer.Properties._.IsSemiringWithoutOne.distrib
d_distrib_2392 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2392 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1166 (coe v0)
-- Data.Integer.Properties._.IsSemiringWithoutOne.isEquivalence
d_isEquivalence_2394 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2394 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
                  (coe v0)))))
-- Data.Integer.Properties._.IsSemiringWithoutOne.zero
d_zero_2398 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2398 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1168 (coe v0)
-- Data.Integer.Properties._.IsUnitalMagma.identity
d_identity_2406 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2406 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_568 (coe v0)
-- Data.Integer.Properties._.IsUnitalMagma.isEquivalence
d_isEquivalence_2412 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2412 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0))
-- Data.Integer.Properties._.IsUnitalMagma.isMagma
d_isMagma_2414 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2414 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0)
-- Data.Integer.Properties._.IsUnitalMagma.∙-cong
d_'8729''45'cong_2428 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2428 = erased
-- Data.Integer.Properties.ℤtoℕ.Homomorphic₀
d_Homomorphic'8320'_2436 ::
  (Integer -> Integer) -> Integer -> Integer -> ()
d_Homomorphic'8320'_2436 = erased
-- Data.Integer.Properties.ℤtoℕ.Homomorphic₁
d_Homomorphic'8321'_2438 ::
  (Integer -> Integer) ->
  (Integer -> Integer) -> (Integer -> Integer) -> ()
d_Homomorphic'8321'_2438 = erased
-- Data.Integer.Properties.ℤtoℕ.Homomorphic₂
d_Homomorphic'8322'_2440 ::
  (Integer -> Integer) ->
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d_Homomorphic'8322'_2440 = erased
-- Data.Integer.Properties.ℤtoℕ.Morphism
d_Morphism_2442 :: ()
d_Morphism_2442 = erased
-- Data.Integer.Properties.ℕtoℤ.Homomorphic₀
d_Homomorphic'8320'_2446 ::
  (Integer -> Integer) -> Integer -> Integer -> ()
d_Homomorphic'8320'_2446 = erased
-- Data.Integer.Properties.ℕtoℤ.Homomorphic₁
d_Homomorphic'8321'_2448 ::
  (Integer -> Integer) ->
  (Integer -> Integer) -> (Integer -> Integer) -> ()
d_Homomorphic'8321'_2448 = erased
-- Data.Integer.Properties.ℕtoℤ.Homomorphic₂
d_Homomorphic'8322'_2450 ::
  (Integer -> Integer) ->
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d_Homomorphic'8322'_2450 = erased
-- Data.Integer.Properties.ℕtoℤ.Morphism
d_Morphism_2452 :: ()
d_Morphism_2452 = erased
-- Data.Integer.Properties.+-injective
d_'43''45'injective_2470 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'injective_2470 = erased
-- Data.Integer.Properties.-[1+-injective
d_'45''91'1'43''45'injective_2472 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'45''91'1'43''45'injective_2472 = erased
-- Data.Integer.Properties.+[1+-injective
d_'43''91'1'43''45'injective_2474 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''91'1'43''45'injective_2474 = erased
-- Data.Integer.Properties._≟_
d__'8799'__2476 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__2476 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                  erased
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464 (coe v0) (coe v1))
            _ -> coe
                   MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                   (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                   (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    erased
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464 (coe v2) (coe v3))
-- Data.Integer.Properties.≡-setoid
d_'8801''45'setoid_2494 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8801''45'setoid_2494
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402
-- Data.Integer.Properties.≡-decSetoid
d_'8801''45'decSetoid_2496 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8801''45'decSetoid_2496
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_decSetoid_406
      (coe d__'8799'__2476)
-- Data.Integer.Properties.drop‿+≤+
d_drop'8255''43''8804''43'_2498 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_drop'8255''43''8804''43'_2498 ~v0 ~v1 v2
  = du_drop'8255''43''8804''43'_2498 v2
du_drop'8255''43''8804''43'_2498 ::
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_drop'8255''43''8804''43'_2498 v0
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.drop‿-≤-
d_drop'8255''45''8804''45'_2502 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_drop'8255''45''8804''45'_2502 ~v0 ~v1 v2
  = du_drop'8255''45''8804''45'_2502 v2
du_drop'8255''45''8804''45'_2502 ::
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_drop'8255''45''8804''45'_2502 v0
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.≤-reflexive
d_'8804''45'reflexive_2506 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8804''45'reflexive_2506 v0 ~v1 ~v2
  = du_'8804''45'reflexive_2506 v0
du_'8804''45'reflexive_2506 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'8804''45'reflexive_2506 v0
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          coe
            MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
            (MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570 (coe v0))
      _ -> let v1 = subInt (coe (-1 :: Integer)) (coe v0) in
           coe
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
             (MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570 (coe v1))
-- Data.Integer.Properties.≤-refl
d_'8804''45'refl_2512 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8804''45'refl_2512 v0 = coe du_'8804''45'reflexive_2506 (coe v0)
-- Data.Integer.Properties.≤-trans
d_'8804''45'trans_2514 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8804''45'trans_2514 ~v0 ~v1 ~v2 v3 v4
  = du_'8804''45'trans_2514 v3 v4
du_'8804''45'trans_2514 ::
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'8804''45'trans_2514 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v7
               -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'8804''45'trans_2578 (coe v7)
                       (coe v4))
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
               -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe
             seq (coe v1)
             (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40)
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v7
               -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'8804''45'trans_2578 (coe v4)
                       (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.≤-antisym
d_'8804''45'antisym_2528 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'antisym_2528 = erased
-- Data.Integer.Properties.≤-total
d_'8804''45'total_2538 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8804''45'total_2538 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Data.Sum.Base.du_map_84
                  (coe MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48)
                  (coe MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48)
                  (MAlonzo.Code.Data.Nat.Properties.d_'8804''45'total_2584
                     (coe v0) (coe v1))
            _ -> coe
                   MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                   (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40)
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                   (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40)
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Data.Sum.Base.du_map_84
                    (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34)
                    (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34)
                    (MAlonzo.Code.Data.Nat.Properties.d_'8804''45'total_2584
                       (coe v3) (coe v2))
-- Data.Integer.Properties._≤?_
d__'8804''63'__2556 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__2556 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                  (coe MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48)
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.d__'8804''63'__2612 (coe v0)
                     (coe v1))
            _ -> coe
                   MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                   (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                   (coe
                      MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                      (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40))
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34)
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.d__'8804''63'__2612 (coe v3)
                       (coe v2))
-- Data.Integer.Properties.≤-irrelevant
d_'8804''45'irrelevant_2574 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'irrelevant_2574 = erased
-- Data.Integer.Properties.≤-isPreorder
d_'8804''45'isPreorder_2584 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8804''45'isPreorder_2584
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'8804''45'reflexive_2506 v0)
      (\ v0 v1 v2 v3 v4 -> coe du_'8804''45'trans_2514 v3 v4)
-- Data.Integer.Properties.≤-isTotalPreorder
d_'8804''45'isTotalPreorder_2586 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_'8804''45'isTotalPreorder_2586
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalPreorder'46'constructor_7157
      (coe d_'8804''45'isPreorder_2584) (coe d_'8804''45'total_2538)
-- Data.Integer.Properties.≤-isPartialOrder
d_'8804''45'isPartialOrder_2588 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8804''45'isPartialOrder_2588
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe d_'8804''45'isPreorder_2584) erased
-- Data.Integer.Properties.≤-isTotalOrder
d_'8804''45'isTotalOrder_2590 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_'8804''45'isTotalOrder_2590
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe d_'8804''45'isPartialOrder_2588) (coe d_'8804''45'total_2538)
-- Data.Integer.Properties.≤-isDecTotalOrder
d_'8804''45'isDecTotalOrder_2592 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_'8804''45'isDecTotalOrder_2592
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe d_'8804''45'isTotalOrder_2590) (coe d__'8799'__2476)
      (coe d__'8804''63'__2556)
-- Data.Integer.Properties.≤-preorder
d_'8804''45'preorder_2594 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8804''45'preorder_2594
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      d_'8804''45'isPreorder_2584
-- Data.Integer.Properties.≤-totalPreorder
d_'8804''45'totalPreorder_2596 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
d_'8804''45'totalPreorder_2596
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalPreorder'46'constructor_3645
      d_'8804''45'isTotalPreorder_2586
-- Data.Integer.Properties.≤-poset
d_'8804''45'poset_2598 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8804''45'poset_2598
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      d_'8804''45'isPartialOrder_2588
-- Data.Integer.Properties.≤-totalOrder
d_'8804''45'totalOrder_2600 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_'8804''45'totalOrder_2600
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      d_'8804''45'isTotalOrder_2590
-- Data.Integer.Properties.≤-decTotalOrder
d_'8804''45'decTotalOrder_2602 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_'8804''45'decTotalOrder_2602
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      d_'8804''45'isDecTotalOrder_2592
-- Data.Integer.Properties.≤ᵇ⇒≤
d_'8804''7495''8658''8804'_2604 ::
  Integer ->
  Integer -> AgdaAny -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8804''7495''8658''8804'_2604 v0 v1 ~v2
  = du_'8804''7495''8658''8804'_2604 v0 v1
du_'8804''7495''8658''8804'_2604 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'8804''7495''8658''8804'_2604 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          coe
            MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
            (coe
               MAlonzo.Code.Data.Nat.Properties.du_'8804''7495''8658''8804'_2536
               (coe v0))
      _ -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
             _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'8804''7495''8658''8804'_2536
                       (coe v2))
-- Data.Integer.Properties.≤⇒≤ᵇ
d_'8804''8658''8804''7495'_2612 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> AgdaAny
d_'8804''8658''8804''7495'_2612 ~v0 ~v1 v2
  = du_'8804''8658''8804''7495'_2612 v2
du_'8804''8658''8804''7495'_2612 ::
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 -> AgdaAny
du_'8804''8658''8804''7495'_2612 v0
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v3
        -> coe
             MAlonzo.Code.Data.Nat.Properties.du_'8804''8658''8804''7495'_2552
             (coe v3)
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v3
        -> coe
             MAlonzo.Code.Data.Nat.Properties.du_'8804''8658''8804''7495'_2552
             (coe v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.drop‿+<+
d_drop'8255''43''60''43'_2618 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_drop'8255''43''60''43'_2618 ~v0 ~v1 v2
  = du_drop'8255''43''60''43'_2618 v2
du_drop'8255''43''60''43'_2618 ::
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_drop'8255''43''60''43'_2618 v0
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.drop‿-<-
d_drop'8255''45''60''45'_2622 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_drop'8255''45''60''45'_2622 ~v0 ~v1 v2
  = du_drop'8255''45''60''45'_2622 v2
du_drop'8255''45''60''45'_2622 ::
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_drop'8255''45''60''45'_2622 v0
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.+≮0
d_'43''8814'0_2626 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'43''8814'0_2626 = erased
-- Data.Integer.Properties.+≮-
d_'43''8814''45'_2628 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'43''8814''45'_2628 = erased
-- Data.Integer.Properties.<⇒≤
d_'60''8658''8804'_2630 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'60''8658''8804'_2630 ~v0 ~v1 v2 = du_'60''8658''8804'_2630 v2
du_'60''8658''8804'_2630 ::
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'60''8658''8804'_2630 v0
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v3
        -> coe
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
             (coe
                MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 (coe v3))
      MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
        -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
      MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v3
        -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe
                MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.<⇒≢
d_'60''8658''8802'_2636 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8802'_2636 = erased
-- Data.Integer.Properties.<⇒≱
d_'60''8658''8817'_2642 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8817'_2642 = erased
-- Data.Integer.Properties.≤⇒≯
d_'8804''8658''8815'_2648 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8804''8658''8815'_2648 = erased
-- Data.Integer.Properties.≰⇒>
d_'8816''8658''62'_2658 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'8816''8658''62'_2658 v0 v1 ~v2 = du_'8816''8658''62'_2658 v0 v1
du_'8816''8658''62'_2658 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'8816''8658''62'_2658 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.du_'8816''8658''62'_2718 (coe v0)
                     (coe v1))
            _ -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'8816''8658''62'_2718 (coe v3)
                       (coe v2))
-- Data.Integer.Properties.≮⇒≥
d_'8814''8658''8805'_2684 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8814''8658''8805'_2684 v0 v1 ~v2
  = du_'8814''8658''8805'_2684 v0 v1
du_'8814''8658''8805'_2684 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'8814''8658''8805'_2684 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.du_'8814''8658''8805'_2732
                     (coe v0) (coe v1))
            _ -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'8814''8658''8805'_2732
                       (coe v3) (coe v2))
-- Data.Integer.Properties.>⇒≰
d_'62''8658''8816'_2710 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'62''8658''8816'_2710 = erased
-- Data.Integer.Properties.≤∧≢⇒<
d_'8804''8743''8802''8658''60'_2712 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'8804''8743''8802''8658''60'_2712 v0 v1 v2 ~v3
  = du_'8804''8743''8802''8658''60'_2712 v0 v1 v2
du_'8804''8743''8802''8658''60'_2712 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'8804''8743''8802''8658''60'_2712 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v5
        -> let v6 = subInt (coe (-1 :: Integer)) (coe v0) in
           coe
             MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
             (coe
                MAlonzo.Code.Data.Nat.Properties.du_'8804''8743''8802''8658''60'_2748
                (coe v6) (coe v5))
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v5
        -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
             (coe
                MAlonzo.Code.Data.Nat.Properties.du_'8804''8743''8802''8658''60'_2748
                (coe v1) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.≤∧≮⇒≡
d_'8804''8743''8814''8658''8801'_2724 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  (MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''8743''8814''8658''8801'_2724 = erased
-- Data.Integer.Properties.<-irrefl
d_'60''45'irrefl_2730 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'irrefl_2730 = erased
-- Data.Integer.Properties.<-asym
d_'60''45'asym_2736 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'asym_2736 = erased
-- Data.Integer.Properties.≤-<-trans
d_'8804''45''60''45'trans_2742 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'8804''45''60''45'trans_2742 ~v0 ~v1 ~v2 v3 v4
  = du_'8804''45''60''45'trans_2742 v3 v4
du_'8804''45''60''45'trans_2742 ::
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'8804''45''60''45'trans_2742 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v7
               -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822
                       (coe v7) (coe v4))
             MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
               -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe
             seq (coe v1) (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v7
               -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816
                       (coe v4) (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.<-≤-trans
d_'60''45''8804''45'trans_2756 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'60''45''8804''45'trans_2756 ~v0 ~v1 ~v2 v3 v4
  = du_'60''45''8804''45'trans_2756 v3 v4
du_'60''45''8804''45'trans_2756 ::
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'60''45''8804''45'trans_2756 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v7
               -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816
                       (coe v7) (coe v4))
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
               -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
        -> coe
             seq (coe v1) (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
      MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v7
               -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822
                       (coe v4) (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.<-trans
d_'60''45'trans_2770 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'60''45'trans_2770 ~v0 ~v1 ~v2 v3 v4
  = du_'60''45'trans_2770 v3 v4
du_'60''45'trans_2770 ::
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'60''45'trans_2770 v0 v1
  = coe
      du_'8804''45''60''45'trans_2742
      (coe du_'60''8658''8804'_2630 (coe v0)) (coe v1)
-- Data.Integer.Properties.<-cmp
d_'60''45'cmp_2776 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_'60''45'cmp_2776 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          let v2
                = coe
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
                    (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64) in
          case coe v0 of
            0 -> case coe v1 of
                   0 -> coe
                          MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 erased
                   _ | coe geqInt (coe v1) (coe (1 :: Integer)) ->
                       coe
                         MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
                         (coe
                            MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                            (coe
                               MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                               (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
                   _ -> coe v2
            _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
                 case coe v1 of
                   0 -> coe
                          MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
                          (coe
                             MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                             (coe
                                MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                                (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
                   _ | coe geqInt (coe v1) (coe (1 :: Integer)) ->
                       let v4 = subInt (coe v1) (coe (1 :: Integer)) in
                       let v5
                             = MAlonzo.Code.Data.Nat.Properties.d_'60''45'cmp_2828
                                 (coe v3) (coe v4) in
                       case coe v5 of
                         MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v6
                           -> coe
                                MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                                   (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6))
                         MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v7
                           -> coe
                                MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 erased
                         MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v8
                           -> coe
                                MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                                   (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8))
                         _ -> MAlonzo.RTE.mazUnreachableError
                   _ -> coe
                          MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
                          (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
                   (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  let v4
                        = MAlonzo.Code.Data.Nat.Properties.d_'60''45'cmp_2828
                            (coe v2) (coe v3) in
                  case coe v4 of
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v5
                      -> coe
                           MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
                           (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v5)
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v6
                      -> coe
                           MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 erased
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v7
                      -> coe
                           MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
                           (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v7)
                    _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties._<?_
d__'60''63'__2866 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__2866 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                  (coe MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72)
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.d__'60''63'__2860 (coe v0)
                     (coe v1))
            _ -> coe
                   MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                   (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                   (coe
                      MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                      (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64))
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58)
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.d__'60''63'__2860 (coe v3)
                       (coe v2))
-- Data.Integer.Properties.<-irrelevant
d_'60''45'irrelevant_2884 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''45'irrelevant_2884 = erased
-- Data.Integer.Properties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_2894 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_2894
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45'trans_2770 v3 v4)
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe (\ v0 v1 v2 v3 v4 -> v4)) (coe (\ v0 v1 v2 v3 v4 -> v4)))
-- Data.Integer.Properties.<-isStrictTotalOrder
d_'60''45'isStrictTotalOrder_2900 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder_2900
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45'trans_2770 v3 v4)
      (coe d_'60''45'cmp_2776)
-- Data.Integer.Properties.<-strictPartialOrder
d_'60''45'strictPartialOrder_2902 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_2902
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      d_'60''45'isStrictPartialOrder_2894
-- Data.Integer.Properties.<-strictTotalOrder
d_'60''45'strictTotalOrder_2904 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder_2904
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      d_'60''45'isStrictTotalOrder_2900
-- Data.Integer.Properties.i≮i
d_i'8814'i_2906 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_i'8814'i_2906 = erased
-- Data.Integer.Properties.>-irrefl
d_'62''45'irrefl_2908 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'62''45'irrefl_2908 = erased
-- Data.Integer.Properties.≤-Reasoning._._IsRelatedTo_
d__IsRelatedTo__2914 a0 a1 = ()
-- Data.Integer.Properties.≤-Reasoning._._∎
d__'8718'_2916 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d__'8718'_2916
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
      (coe d_'8804''45'isPreorder_2584)
-- Data.Integer.Properties.≤-Reasoning._._≡⟨⟩_
d__'8801''10216''10217'__2918 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d__'8801''10216''10217'__2918 v0 = coe v0
-- Data.Integer.Properties.≤-Reasoning._.IsEquality
d_IsEquality_2920 a0 a1 a2 = ()
-- Data.Integer.Properties.≤-Reasoning._.IsEquality?
d_IsEquality'63'_2922 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsEquality'63'_2922 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_IsEquality'63'_142
      v2
-- Data.Integer.Properties.≤-Reasoning._.IsStrict
d_IsStrict_2924 a0 a1 a2 = ()
-- Data.Integer.Properties.≤-Reasoning._.IsStrict?
d_IsStrict'63'_2926 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsStrict'63'_2926 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_IsStrict'63'_108
      v2
-- Data.Integer.Properties.≤-Reasoning._.begin_
d_begin__2928 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_begin__2928
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_2584)
      (\ v0 v1 v2 -> coe du_'60''8658''8804'_2630 v2)
-- Data.Integer.Properties.≤-Reasoning._.begin-equality_
d_begin'45'equality__2930 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_begin'45'equality__2930 = erased
-- Data.Integer.Properties.≤-Reasoning._.begin-strict_
d_begin'45'strict__2932 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  AgdaAny -> MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_begin'45'strict__2932 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      v2
-- Data.Integer.Properties.≤-Reasoning._.extractEquality
d_extractEquality_2936 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T_IsEquality_126 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_extractEquality_2936 = erased
-- Data.Integer.Properties.≤-Reasoning._.extractStrict
d_extractStrict_2938 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T_IsStrict_92 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_extractStrict_2938 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_extractStrict_118
      v2 v3
-- Data.Integer.Properties.≤-Reasoning._.step-<
d_step'45''60'_2946 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''60'_2946
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45'trans_2770 v3 v4)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45''8804''45'trans_2756 v3 v4)
-- Data.Integer.Properties.≤-Reasoning._.step-≡
d_step'45''8801'_2948 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8801'_2948 ~v0 ~v1 ~v2 v3 ~v4
  = du_step'45''8801'_2948 v3
du_step'45''8801'_2948 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
du_step'45''8801'_2948 v0 = coe v0
-- Data.Integer.Properties.≤-Reasoning._.step-≡˘
d_step'45''8801''728'_2950 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8801''728'_2950 ~v0 ~v1 ~v2 v3 ~v4
  = du_step'45''8801''728'_2950 v3
du_step'45''8801''728'_2950 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
du_step'45''8801''728'_2950 v0 = coe v0
-- Data.Integer.Properties.≤-Reasoning._.step-≤
d_step'45''8804'_2952 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8804'_2952
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
      (coe d_'8804''45'isPreorder_2584)
      (\ v0 v1 v2 v3 v4 -> coe du_'8804''45''60''45'trans_2742 v3 v4)
-- Data.Integer.Properties.positive⁻¹
d_positive'8315''185'_2974 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_positive'8315''185'_2974 ~v0 ~v1 = du_positive'8315''185'_2974
du_positive'8315''185'_2974 ::
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_positive'8315''185'_2974
  = coe
      MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
      (coe
         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
         (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
-- Data.Integer.Properties.negative⁻¹
d_negative'8315''185'_2980 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_negative'8315''185'_2980 ~v0 ~v1 = du_negative'8315''185'_2980
du_negative'8315''185'_2980 ::
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_negative'8315''185'_2980
  = coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
-- Data.Integer.Properties.nonPositive⁻¹
d_nonPositive'8315''185'_2986 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_nonPositive'8315''185'_2986 v0 ~v1
  = du_nonPositive'8315''185'_2986 v0
du_nonPositive'8315''185'_2986 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_nonPositive'8315''185'_2986 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
-- Data.Integer.Properties.nonNegative⁻¹
d_nonNegative'8315''185'_2992 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_nonNegative'8315''185'_2992 ~v0 ~v1
  = du_nonNegative'8315''185'_2992
du_nonNegative'8315''185'_2992 ::
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_nonNegative'8315''185'_2992
  = coe
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
      (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
-- Data.Integer.Properties.negative<positive
d_negative'60'positive_3000 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_negative'60'positive_3000 ~v0 ~v1 ~v2 ~v3
  = du_negative'60'positive_3000
du_negative'60'positive_3000 ::
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_negative'60'positive_3000
  = coe
      du_'60''45'trans_2770 (coe du_negative'8315''185'_2980)
      (coe du_positive'8315''185'_2974)
-- Data.Integer.Properties.neg-involutive
d_neg'45'involutive_3008 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'involutive_3008 = erased
-- Data.Integer.Properties.neg-injective
d_neg'45'injective_3014 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'injective_3014 = erased
-- Data.Integer.Properties.neg-≤-pos
d_neg'45''8804''45'pos_3030 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_neg'45''8804''45'pos_3030 v0 ~v1
  = du_neg'45''8804''45'pos_3030 v0
du_neg'45''8804''45'pos_3030 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_neg'45''8804''45'pos_3030 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
-- Data.Integer.Properties.neg-mono-≤
d_neg'45'mono'45''8804'_3034 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_neg'45'mono'45''8804'_3034 ~v0 v1 v2
  = du_neg'45'mono'45''8804'_3034 v1 v2
du_neg'45'mono'45''8804'_3034 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_neg'45'mono'45''8804'_3034 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v4
        -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4)
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe du_neg'45''8804''45'pos_3030 (coe v0)
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v4
        -> case coe v4 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe du_neg'45''8804''45'pos_3030 (coe v0)
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7
               -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v7
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.neg-cancel-≤
d_neg'45'cancel'45''8804'_3040 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_neg'45'cancel'45''8804'_3040 v0 v1 v2
  = case coe v0 of
      0 -> case coe v1 of
             0 -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             _ -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
      _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
          case coe v1 of
            0 -> coe
                   seq (coe v2)
                   (coe
                      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                      (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
            _ | coe geqInt (coe v1) (coe (1 :: Integer)) ->
                case coe v2 of
                  MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v5
                    -> coe
                         MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                         (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
                  _ -> MAlonzo.RTE.mazUnreachableError
            _ -> coe
                   seq (coe v2)
                   (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40)
      _ -> case coe v2 of
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v5
               -> case coe v5 of
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
                      -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v8
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.neg-mono-<
d_neg'45'mono'45''60'_3064 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_neg'45'mono'45''60'_3064 v0 v1 v2
  = case coe v0 of
      0 -> coe
             seq (coe v2) (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
      _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
          case coe v2 of
            MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v5
              -> coe
                   MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                   (coe
                      MAlonzo.Code.Data.Nat.Properties.du_'8804''45'pred_2654 (coe v5))
            _ -> MAlonzo.RTE.mazUnreachableError
      _ -> case coe v1 of
             0 -> coe
                    seq (coe v2)
                    (coe
                       MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                       (coe
                          MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                          (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
             _ | coe geqInt (coe v1) (coe (1 :: Integer)) ->
                 coe
                   seq (coe v2) (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
             _ -> case coe v2 of
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v5
                      -> coe
                           MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
                    _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.neg-cancel-<
d_neg'45'cancel'45''60'_3078 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_neg'45'cancel'45''60'_3078 v0 v1 v2
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
      _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
          case coe v1 of
            0 -> coe
                   seq (coe v2)
                   (coe
                      MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                      (coe
                         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                         (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
            _ | coe geqInt (coe v1) (coe (1 :: Integer)) ->
                case coe v2 of
                  MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v5
                    -> coe
                         MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                         (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
                  _ -> MAlonzo.RTE.mazUnreachableError
            _ -> coe
                   seq (coe v2) (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
      _ -> case coe v2 of
             MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v5
               -> case coe v5 of
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
                      -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v8
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.∣i∣≡0⇒i≡0
d_'8739'i'8739''8801'0'8658'i'8801'0_3102 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'i'8739''8801'0'8658'i'8801'0_3102 = erased
-- Data.Integer.Properties.∣-i∣≡∣i∣
d_'8739''45'i'8739''8801''8739'i'8739'_3106 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45'i'8739''8801''8739'i'8739'_3106 = erased
-- Data.Integer.Properties.0≤i⇒+∣i∣≡i
d_0'8804'i'8658''43''8739'i'8739''8801'i_3112 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'8804'i'8658''43''8739'i'8739''8801'i_3112 = erased
-- Data.Integer.Properties.+∣i∣≡i⇒0≤i
d_'43''8739'i'8739''8801'i'8658'0'8804'i_3114 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'43''8739'i'8739''8801'i'8658'0'8804'i_3114 ~v0 ~v1
  = du_'43''8739'i'8739''8801'i'8658'0'8804'i_3114
du_'43''8739'i'8739''8801'i'8658'0'8804'i_3114 ::
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'43''8739'i'8739''8801'i'8658'0'8804'i_3114
  = coe
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
      (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
-- Data.Integer.Properties.+∣i∣≡i⊎+∣i∣≡-i
d_'43''8739'i'8739''8801'i'8846''43''8739'i'8739''8801''45'i_3120 ::
  Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'43''8739'i'8739''8801'i'8846''43''8739'i'8739''8801''45'i_3120 v0
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
      _ -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
-- Data.Integer.Properties.∣m⊝n∣≤m⊔n
d_'8739'm'8861'n'8739''8804'm'8852'n_3130 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8739'm'8861'n'8739''8804'm'8852'n_3130 v0 v1
  = let v2 = ltInt (coe v0) (coe v1) in
    if coe v2
      then coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
             (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
             (\ v3 v4 v5 ->
                coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v5)
             (coe
                MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                (coe
                   MAlonzo.Code.Data.Integer.Base.d_'45'__252
                   (coe subInt (coe v1) (coe v0))))
             (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1))
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                (\ v3 v4 v5 v6 v7 ->
                   coe
                     MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v6 v7)
                (coe subInt (coe v1) (coe v0)) (coe v1)
                (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                   (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                   (\ v3 v4 v5 v6 v7 ->
                      coe
                        MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v6 v7)
                   (coe v1)
                   (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1))
                   (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                      (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1)))
                   (let v3
                          = MAlonzo.Code.Data.Nat.Properties.d_'8804''45'totalPreorder_2632 in
                    let v4
                          = MAlonzo.Code.Data.Nat.Properties.d_'8852''45'operator_4226 in
                    coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
                      (coe
                         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
                         (coe v3))
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
                         (coe v4))
                      (coe v0) (coe v1)))
                (coe
                   MAlonzo.Code.Data.Nat.Properties.d_m'8760'n'8804'm_4860 (coe v1)
                   (coe v0)))
      else coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
             (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
             (\ v3 v4 v5 ->
                coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v5)
             (coe
                MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                (coe subInt (coe v0) (coe v1)))
             (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1))
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                (\ v3 v4 v5 v6 v7 ->
                   coe
                     MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v6 v7)
                (coe subInt (coe v0) (coe v1)) (coe v0)
                (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                   (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                   (\ v3 v4 v5 v6 v7 ->
                      coe
                        MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v6 v7)
                   (coe v0)
                   (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1))
                   (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                      (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1)))
                   (let v3
                          = MAlonzo.Code.Data.Nat.Properties.d_'8804''45'totalPreorder_2632 in
                    let v4
                          = MAlonzo.Code.Data.Nat.Properties.d_'8852''45'operator_4226 in
                    coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
                      (coe
                         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
                         (coe v3))
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
                         (coe v4))
                      (coe v0) (coe v1)))
                (coe
                   MAlonzo.Code.Data.Nat.Properties.d_m'8760'n'8804'm_4860 (coe v0)
                   (coe v1)))
-- Data.Integer.Properties.∣i+j∣≤∣i∣+∣j∣
d_'8739'i'43'j'8739''8804''8739'i'8739''43''8739'j'8739'_3160 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8739'i'43'j'8739''8804''8739'i'8739''43''8739'j'8739'_3160 v0 v1
  = case coe v0 of
      0 -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
                (coe
                   MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                   (coe
                      MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe (0 :: Integer))
                      (coe v1))))
      _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
                  (coe
                     MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                     (coe
                        MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v0) (coe v1)))
            _ -> coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                   (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                   (\ v2 v3 v4 ->
                      coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v4)
                   (coe
                      MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                      (coe
                         MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v0) (coe v1)))
                   (coe
                      addInt
                      (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
                      (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                      (\ v2 v3 v4 v5 v6 ->
                         coe
                           MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v5 v6)
                      (coe
                         MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                         (coe
                            MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0)
                            (coe subInt (coe (0 :: Integer)) (coe v1))))
                      (coe
                         MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0)
                         (coe subInt (coe (0 :: Integer)) (coe v1)))
                      (coe
                         addInt
                         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
                         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                         (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                         (\ v2 v3 v4 v5 v6 ->
                            coe
                              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v5 v6)
                         (coe
                            MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0)
                            (coe subInt (coe (0 :: Integer)) (coe v1)))
                         (coe subInt (coe v0) (coe v1))
                         (coe
                            addInt
                            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
                            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                            (coe subInt (coe v0) (coe v1)))
                         (coe
                            MAlonzo.Code.Data.Nat.Properties.d_m'8852'n'8804'm'43'n_4630
                            (coe v0) (coe subInt (coe (0 :: Integer)) (coe v1))))
                      (coe
                         d_'8739'm'8861'n'8739''8804'm'8852'n_3130 (coe v0)
                         (coe subInt (coe (0 :: Integer)) (coe v1))))
      _ -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                   (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                   (\ v2 v3 v4 ->
                      coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v4)
                   (coe
                      MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                      (coe
                         MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v0) (coe v1)))
                   (coe
                      addInt
                      (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
                      (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                      (\ v2 v3 v4 v5 v6 ->
                         coe
                           MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v5 v6)
                      (coe
                         MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                         (coe
                            MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v1)
                            (coe subInt (coe (0 :: Integer)) (coe v0))))
                      (coe
                         MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v1)
                         (coe subInt (coe (0 :: Integer)) (coe v0)))
                      (coe
                         addInt
                         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
                         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                         (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                         (\ v2 v3 v4 v5 v6 ->
                            coe
                              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v5 v6)
                         (coe
                            MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v1)
                            (coe subInt (coe (0 :: Integer)) (coe v0)))
                         (coe subInt (coe v1) (coe v0))
                         (coe
                            addInt
                            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
                            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                            (coe subInt (coe v1) (coe v0)))
                         (coe
                            MAlonzo.Code.Data.Nat.Properties.d_m'8852'n'8804'm'43'n_4630
                            (coe v1) (coe subInt (coe (0 :: Integer)) (coe v0))))
                      (coe
                         d_'8739'm'8861'n'8739''8804'm'8852'n_3130 (coe v1)
                         (coe subInt (coe (0 :: Integer)) (coe v0))))
             _ -> coe
                    MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
                    (coe subInt (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v1))
-- Data.Integer.Properties.∣i-j∣≤∣i∣+∣j∣
d_'8739'i'45'j'8739''8804''8739'i'8739''43''8739'j'8739'_3198 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8739'i'45'j'8739''8804''8739'i'8739''43''8739'j'8739'_3198 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
      (\ v2 v3 v4 ->
         coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v4)
      (coe
         MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v1)))
      (coe
         addInt
         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
         (\ v2 v3 v4 v5 v6 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v5 v6)
         (coe
            MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v1)))
         (coe
            addInt
            (coe
               MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
               (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1)))
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0)))
         (coe
            addInt
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (coe
               addInt
               (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
               (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1))))
         (coe
            d_'8739'i'43'j'8739''8804''8739'i'8739''43''8739'j'8739'_3160
            (coe v0)
            (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1))))
-- Data.Integer.Properties.◃-inverse
d_'9667''45'inverse_3212 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'9667''45'inverse_3212 = erased
-- Data.Integer.Properties.◃-cong
d_'9667''45'cong_3218 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'9667''45'cong_3218 = erased
-- Data.Integer.Properties.+◃n≡+n
d_'43''9667'n'8801''43'n_3234 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''9667'n'8801''43'n_3234 = erased
-- Data.Integer.Properties.-◃n≡-n
d_'45''9667'n'8801''45'n_3238 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'45''9667'n'8801''45'n_3238 = erased
-- Data.Integer.Properties.sign-◃
d_sign'45''9667'_3246 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sign'45''9667'_3246 = erased
-- Data.Integer.Properties.abs-◃
d_abs'45''9667'_3252 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_abs'45''9667'_3252 = erased
-- Data.Integer.Properties.signᵢ◃∣i∣≡i
d_sign'7522''9667''8739'i'8739''8801'i_3260 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sign'7522''9667''8739'i'8739''8801'i_3260 = erased
-- Data.Integer.Properties.sign-cong
d_sign'45'cong_3270 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sign'45'cong_3270 = erased
-- Data.Integer.Properties.sign-cong′
d_sign'45'cong'8242'_3286 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sign'45'cong'8242'_3286 v0 v1 ~v2 ~v3 ~v4
  = du_sign'45'cong'8242'_3286 v0 v1
du_sign'45'cong'8242'_3286 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sign'45'cong'8242'_3286 v0 v1
  = case coe v1 of
      0 -> coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
             (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
      _ -> let v2
                 = coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased in
           coe seq (coe v0) (coe v2)
-- Data.Integer.Properties.abs-cong
d_abs'45'cong_3320 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_abs'45'cong_3320 = erased
-- Data.Integer.Properties.∣s◃m∣*∣t◃n∣≡m*n
d_'8739's'9667'm'8739''42''8739't'9667'n'8739''8801'm'42'n_3344 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739's'9667'm'8739''42''8739't'9667'n'8739''8801'm'42'n_3344
  = erased
-- Data.Integer.Properties.+◃-mono-<
d_'43''9667''45'mono'45''60'_3354 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'43''9667''45'mono'45''60'_3354 v0 ~v1 v2
  = du_'43''9667''45'mono'45''60'_3354 v0 v2
du_'43''9667''45'mono'45''60'_3354 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'43''9667''45'mono'45''60'_3354 v0 v1
  = coe
      seq (coe v0)
      (coe MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v1)
-- Data.Integer.Properties.+◃-cancel-<
d_'43''9667''45'cancel'45''60'_3366 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''9667''45'cancel'45''60'_3366 v0 ~v1 v2
  = du_'43''9667''45'cancel'45''60'_3366 v0 v2
du_'43''9667''45'cancel'45''60'_3366 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''9667''45'cancel'45''60'_3366 v0 v1
  = coe
      seq (coe v0)
      (case coe v1 of
         MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v4 -> coe v4
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Data.Integer.Properties.neg◃-cancel-<
d_neg'9667''45'cancel'45''60'_3380 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_neg'9667''45'cancel'45''60'_3380 ~v0 v1 v2
  = du_neg'9667''45'cancel'45''60'_3380 v1 v2
du_neg'9667''45'cancel'45''60'_3380 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_neg'9667''45'cancel'45''60'_3380 v0 v1
  = case coe v0 of
      0 -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
      _ -> case coe v1 of
             MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v4
               -> coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.-◃<+◃
d_'45''9667''60''43''9667'_3396 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'45''9667''60''43''9667'_3396 ~v0 v1 ~v2
  = du_'45''9667''60''43''9667'_3396 v1
du_'45''9667''60''43''9667'_3396 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'45''9667''60''43''9667'_3396 v0
  = coe
      seq (coe v0) (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
-- Data.Integer.Properties.+◃≮-◃
d_'43''9667''8814''45''9667'_3398 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'43''9667''8814''45''9667'_3398 = erased
-- Data.Integer.Properties.n⊖n≡0
d_n'8854'n'8801'0_3404 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'8854'n'8801'0_3404 = erased
-- Data.Integer.Properties.[1+m]⊖[1+n]≡m⊖n
d_'91'1'43'm'93''8854''91'1'43'n'93''8801'm'8854'n_3422 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'1'43'm'93''8854''91'1'43'n'93''8801'm'8854'n_3422 = erased
-- Data.Integer.Properties.⊖-swap
d_'8854''45'swap_3444 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8854''45'swap_3444 = erased
-- Data.Integer.Properties.⊖-≥
d_'8854''45''8805'_3458 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8854''45''8805'_3458 = erased
-- Data.Integer.Properties.≤-⊖
d_'8804''45''8854'_3486 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45''8854'_3486 = erased
-- Data.Integer.Properties.⊖-≤
d_'8854''45''8804'_3500 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8854''45''8804'_3500 = erased
-- Data.Integer.Properties.⊖-<
d_'8854''45''60'_3536 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8854''45''60'_3536 = erased
-- Data.Integer.Properties.⊖-≰
d_'8854''45''8816'_3538 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8854''45''8816'_3538 = erased
-- Data.Integer.Properties.∣⊖∣-≤
d_'8739''8854''8739''45''8804'_3540 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''8854''8739''45''8804'_3540 = erased
-- Data.Integer.Properties.∣⊖∣-<
d_'8739''8854''8739''45''60'_3552 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''8854''8739''45''60'_3552 = erased
-- Data.Integer.Properties.∣⊖∣-≰
d_'8739''8854''8739''45''8816'_3564 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''8854''8739''45''8816'_3564 = erased
-- Data.Integer.Properties.-m+n≡n⊖m
d_'45'm'43'n'8801'n'8854'm_3570 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'45'm'43'n'8801'n'8854'm_3570 = erased
-- Data.Integer.Properties.m-n≡m⊖n
d_m'45'n'8801'm'8854'n_3582 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'45'n'8801'm'8854'n_3582 = erased
-- Data.Integer.Properties.-[n⊖m]≡-m+n
d_'45''91'n'8854'm'93''8801''45'm'43'n_3596 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'45''91'n'8854'm'93''8801''45'm'43'n_3596 = erased
-- Data.Integer.Properties.∣m⊖n∣≡∣n⊖m∣
d_'8739'm'8854'n'8739''8801''8739'n'8854'm'8739'_3630 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'm'8854'n'8739''8801''8739'n'8854'm'8739'_3630 = erased
-- Data.Integer.Properties.+-cancelˡ-⊖
d_'43''45'cancel'737''45''8854'_3646 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'cancel'737''45''8854'_3646 = erased
-- Data.Integer.Properties.m⊖n≤m
d_m'8854'n'8804'm_3666 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8854'n'8804'm_3666 v0 v1
  = case coe v1 of
      0 -> coe
             d_'8804''45'refl_2512
             (coe
                MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0)
                (coe (0 :: Integer)))
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           case coe v0 of
             0 -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
             _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                    (coe d_'8804''45'isPreorder_2584)
                    (\ v4 v5 v6 -> coe du_'60''8658''8804'_2630 v6)
                    (coe
                       MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0) (coe v1))
                    (coe v0)
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                       (coe d_'8804''45'isPreorder_2584)
                       (\ v4 v5 v6 v7 v8 -> coe du_'8804''45''60''45'trans_2742 v7 v8)
                       (coe
                          MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v3) (coe v2))
                       (coe v3) (coe v0)
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                          (coe d_'8804''45'isPreorder_2584)
                          (\ v4 v5 v6 v7 v8 -> coe du_'8804''45''60''45'trans_2742 v7 v8)
                          (coe v3) (coe v0) (coe v0)
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                             (coe d_'8804''45'isPreorder_2584) (coe v0))
                          (coe
                             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                             (MAlonzo.Code.Data.Nat.Properties.d_n'8804'1'43'n_2668 (coe v3))))
                       (coe d_m'8854'n'8804'm_3666 (coe v3) (coe v2)))
-- Data.Integer.Properties.m⊖n<1+m
d_m'8854'n'60'1'43'm_3684 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_m'8854'n'60'1'43'm_3684 v0 v1
  = coe
      du_'8804''45''60''45'trans_2742
      (coe d_m'8854'n'8804'm_3666 (coe v0) (coe v1))
      (coe
         MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
         (coe
            MAlonzo.Code.Data.Nat.Properties.du_m'60'n'43'm_3510 (coe v0)
            (coe
               MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
               (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))))
-- Data.Integer.Properties.m⊖1+n<m
d_m'8854'1'43'n'60'm_3696 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_m'8854'1'43'n'60'm_3696 v0 v1 ~v2
  = du_m'8854'1'43'n'60'm_3696 v0 v1
du_m'8854'1'43'n'60'm_3696 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_m'8854'1'43'n'60'm_3696 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans_2770 v7 v8)
                (coe
                   MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                (\ v4 v5 v6 v7 v8 -> coe du_'60''45''8804''45'trans_2756 v7 v8)
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v2) (coe v3))
                (coe v0) (coe v0)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                   (coe d_'8804''45'isPreorder_2584) (coe v0))
                (coe d_m'8854'n'60'1'43'm_3684 (coe v2) (coe v3)))
-- Data.Integer.Properties.-1+m<n⊖m
d_'45'1'43'm'60'n'8854'm_3712 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'45'1'43'm'60'n'8854'm_3712 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                    (MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570 (coe v0))
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                       (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans_2770 v7 v8)
                       (coe
                          MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                       (\ v4 v5 v6 v7 v8 -> coe du_'60''45''8804''45'trans_2756 v7 v8)
                       (coe subInt (coe (-1 :: Integer)) (coe v0))
                       (coe subInt (coe (0 :: Integer)) (coe v0))
                       (coe
                          MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v1) (coe v0))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                          (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans_2770 v7 v8)
                          (coe
                             MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                          (\ v4 v5 v6 v7 v8 -> coe du_'60''45''8804''45'trans_2756 v7 v8)
                          (coe subInt (coe (0 :: Integer)) (coe v0))
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v3) (coe v2))
                          (coe
                             MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v1) (coe v0))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                             (coe d_'8804''45'isPreorder_2584)
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v1) (coe v0)))
                          (coe d_'45'1'43'm'60'n'8854'm_3712 (coe v2) (coe v3)))
                       (coe
                          MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                          (MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570 (coe v0))))
-- Data.Integer.Properties.-[1+m]≤n⊖m+1
d_'45''91'1'43'm'93''8804'n'8854'm'43'1_3730 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'45''91'1'43'm'93''8804'n'8854'm'43'1_3730 v0 v1
  = case coe v1 of
      0 -> coe
             d_'8804''45'refl_2512 (coe subInt (coe (-1 :: Integer)) (coe v0))
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
             (coe d_'8804''45'isPreorder_2584)
             (\ v3 v4 v5 -> coe du_'60''8658''8804'_2630 v5)
             (coe subInt (coe (-1 :: Integer)) (coe v0))
             (coe
                MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v1)
                (coe addInt (coe (1 :: Integer)) (coe v0)))
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                (coe d_'8804''45'isPreorder_2584)
                (\ v3 v4 v5 v6 v7 -> coe du_'8804''45''60''45'trans_2742 v6 v7)
                (coe subInt (coe (-1 :: Integer)) (coe v0))
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v2) (coe v0))
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v1)
                   (coe addInt (coe (1 :: Integer)) (coe v0)))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                   (coe d_'8804''45'isPreorder_2584)
                   (coe
                      MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v1)
                      (coe addInt (coe (1 :: Integer)) (coe v0))))
                (coe
                   du_'60''8658''8804'_2630
                   (coe d_'45'1'43'm'60'n'8854'm_3712 (coe v0) (coe v2))))
-- Data.Integer.Properties.-1+m≤n⊖m
d_'45'1'43'm'8804'n'8854'm_3746 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'45'1'43'm'8804'n'8854'm_3746 v0 v1
  = coe
      du_'60''8658''8804'_2630
      (coe d_'45'1'43'm'60'n'8854'm_3712 (coe v0) (coe v1))
-- Data.Integer.Properties.0⊖m≤+
d_0'8854'm'8804''43'_3756 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_0'8854'm'8804''43'_3756 v0 ~v1 = du_0'8854'm'8804''43'_3756 v0
du_0'8854'm'8804''43'_3756 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_0'8854'm'8804''43'_3756 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
-- Data.Integer.Properties.sign-⊖-<
d_sign'45''8854''45''60'_3760 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sign'45''8854''45''60'_3760 = erased
-- Data.Integer.Properties.sign-⊖-≰
d_sign'45''8854''45''8816'_3772 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sign'45''8854''45''8816'_3772 = erased
-- Data.Integer.Properties.⊖-monoʳ-≥-≤
d_'8854''45'mono'691''45''8805''45''8804'_3778 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8854''45'mono'691''45''8805''45''8804'_3778 v0 v1 v2 v3
  = case coe v0 of
      0 -> case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe du_0'8854'm'8804''43'_3756 (coe v1)
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v6
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> let v4 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    seq (coe v3)
                    (coe
                       d_'8804''45'refl_2512
                       (coe
                          MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                          (MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0))
                          (\ v5 v6 -> v5) (0 :: Integer) (0 :: Integer)))
             _ -> let v5 = subInt (coe v1) (coe (1 :: Integer)) in
                  case coe v3 of
                    MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
                      -> coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                           (coe d_'8804''45'isPreorder_2584)
                           (\ v7 v8 v9 -> coe du_'60''8658''8804'_2630 v9)
                           (coe
                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                              (MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0))
                              (\ v7 v8 -> v7) v1 (0 :: Integer))
                           (coe
                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                              (\ v7 v8 -> v8)
                              (MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0)) v1
                              (0 :: Integer))
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                              (\ v7 v8 v9 v10 v11 -> coe du_'60''45'trans_2770 v10 v11)
                              (coe
                                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                              (\ v7 v8 v9 v10 v11 -> coe du_'60''45''8804''45'trans_2756 v10 v11)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v4) (coe v5))
                              (coe v0)
                              (coe
                                 MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                 (\ v7 v8 -> v8)
                                 (MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0)) v1
                                 (0 :: Integer))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2584) (coe v0))
                              (coe d_m'8854'n'60'1'43'm_3684 (coe v4) (coe v5)))
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
                      -> let v9 = subInt (coe v2) (coe (1 :: Integer)) in
                         coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                           (coe d_'8804''45'isPreorder_2584)
                           (\ v10 v11 v12 -> coe du_'60''8658''8804'_2630 v12)
                           (coe
                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                              (MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0))
                              (\ v10 v11 -> v10) v1 v2)
                           (coe
                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                              (\ v10 v11 -> v11)
                              (MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0)) v1 v2)
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                              (coe d_'8804''45'isPreorder_2584)
                              (\ v10 v11 v12 v13 v14 ->
                                 coe du_'8804''45''60''45'trans_2742 v13 v14)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v4) (coe v5))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v4) (coe v9))
                              (coe
                                 MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                 (\ v10 v11 -> v11)
                                 (MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0)) v1 v2)
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2584)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0)
                                    (coe v2)))
                              (coe
                                 d_'8854''45'mono'691''45''8805''45''8804'_3778 (coe v4) (coe v5)
                                 (coe v9) (coe v8)))
                    _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.⊖-monoˡ-≤
d_'8854''45'mono'737''45''8804'_3810 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8854''45'mono'737''45''8804'_3810 v0 v1 v2 v3
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v3
      _ -> let v4 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v2 of
             0 -> coe
                    seq (coe v3)
                    (coe
                       d_'8804''45'refl_2512
                       (coe
                          MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                          (\ v5 ->
                             MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v5) (coe v0))
                          (\ v5 v6 -> v5) (0 :: Integer) (0 :: Integer)))
             _ -> let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                  case coe v3 of
                    MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
                      -> coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                           (coe d_'8804''45'isPreorder_2584)
                           (\ v7 v8 v9 -> coe du_'60''8658''8804'_2630 v9)
                           (coe
                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                              (\ v7 ->
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v7) (coe v0))
                              (\ v7 v8 -> v7) (0 :: Integer) v2)
                           (coe
                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                              (\ v7 v8 -> v8)
                              (\ v7 ->
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v7) (coe v0))
                              (0 :: Integer) v2)
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                              (coe d_'8804''45'isPreorder_2584)
                              (\ v7 v8 v9 v10 v11 -> coe du_'8804''45''60''45'trans_2742 v10 v11)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe (0 :: Integer))
                                 (coe v0))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe (0 :: Integer))
                                 (coe v4))
                              (coe
                                 MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                 (\ v7 v8 -> v8)
                                 (\ v7 ->
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v7) (coe v0))
                                 (0 :: Integer) v2)
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                 (coe d_'8804''45'isPreorder_2584)
                                 (\ v7 v8 v9 v10 v11 -> coe du_'8804''45''60''45'trans_2742 v10 v11)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258
                                    (coe (0 :: Integer)) (coe v4))
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v5) (coe v4))
                                 (coe
                                    MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                    (\ v7 v8 -> v8)
                                    (\ v7 ->
                                       MAlonzo.Code.Data.Integer.Base.d__'8854'__258
                                         (coe v7) (coe v0))
                                    (0 :: Integer) v2)
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                    (coe d_'8804''45'isPreorder_2584)
                                    (coe
                                       MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v2)
                                       (coe v0)))
                                 (coe
                                    d_'8854''45'mono'737''45''8804'_3810 (coe v4)
                                    (coe (0 :: Integer)) (coe v5)
                                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
                              (coe
                                 d_'8854''45'mono'691''45''8805''45''8804'_3778 (coe (0 :: Integer))
                                 (coe v0) (coe v4)
                                 (coe
                                    MAlonzo.Code.Data.Nat.Properties.d_n'8804'1'43'n_2668
                                    (coe v4))))
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
                      -> let v9 = subInt (coe v1) (coe (1 :: Integer)) in
                         coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                           (coe d_'8804''45'isPreorder_2584)
                           (\ v10 v11 v12 -> coe du_'60''8658''8804'_2630 v12)
                           (coe
                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                              (\ v10 ->
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v10) (coe v0))
                              (\ v10 v11 -> v10) v1 v2)
                           (coe
                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                              (\ v10 v11 -> v11)
                              (\ v10 ->
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v10) (coe v0))
                              v1 v2)
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                              (coe d_'8804''45'isPreorder_2584)
                              (\ v10 v11 v12 v13 v14 ->
                                 coe du_'8804''45''60''45'trans_2742 v13 v14)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v9) (coe v4))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v5) (coe v4))
                              (coe
                                 MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                 (\ v10 v11 -> v11)
                                 (\ v10 ->
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258
                                      (coe v10) (coe v0))
                                 v1 v2)
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2584)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v2)
                                    (coe v0)))
                              (coe
                                 d_'8854''45'mono'737''45''8804'_3810 (coe v4) (coe v9) (coe v5)
                                 (coe v8)))
                    _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.⊖-monoʳ->-<
d_'8854''45'mono'691''45''62''45''60'_3840 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'8854''45'mono'691''45''62''45''60'_3840 v0 v1 v2 v3
  = case coe v0 of
      0 -> case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> case coe v6 of
                    MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
                      -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
                      -> coe
                           MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> let v4 = subInt (coe v0) (coe (1 :: Integer)) in
           let v5 = subInt (coe v1) (coe (1 :: Integer)) in
           case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
               -> case coe v8 of
                    MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
                      -> coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                              (\ v10 v11 v12 v13 v14 -> coe du_'60''45'trans_2770 v13 v14)
                              (coe
                                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                              (\ v10 v11 v12 v13 v14 ->
                                 coe du_'60''45''8804''45'trans_2756 v13 v14)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v4) (coe v5))
                              (coe v0) (coe v0)
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2584) (coe v0))
                              (coe d_m'8854'n'60'1'43'm_3684 (coe v4) (coe v5)))
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v11
                      -> let v12 = subInt (coe v2) (coe (1 :: Integer)) in
                         coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                              (\ v13 v14 v15 v16 v17 -> coe du_'60''45'trans_2770 v16 v17)
                              (coe
                                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                              (\ v13 v14 v15 v16 v17 ->
                                 coe du_'60''45''8804''45'trans_2756 v16 v17)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v4)
                                 (coe subInt (coe v1) (coe (1 :: Integer))))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v4) (coe v12))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0) (coe v2))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2584)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v0)
                                    (coe v2)))
                              (coe
                                 d_'8854''45'mono'691''45''62''45''60'_3840 (coe v4)
                                 (coe subInt (coe v1) (coe (1 :: Integer))) (coe v12)
                                 (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v11)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.⊖-monoˡ-<
d_'8854''45'mono'737''45''60'_3868 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'8854''45'mono'737''45''60'_3868 v0 v1 v2 v3
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v3
      _ -> let v4 = subInt (coe v0) (coe (1 :: Integer)) in
           let v5 = subInt (coe v2) (coe (1 :: Integer)) in
           case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
               -> case coe v8 of
                    MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
                      -> coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                              (\ v10 v11 v12 v13 v14 -> coe du_'60''45'trans_2770 v13 v14)
                              (coe
                                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                              (\ v10 v11 v12 v13 v14 ->
                                 coe du_'60''45''8804''45'trans_2756 v13 v14)
                              (coe subInt (coe (0 :: Integer)) (coe v0))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v5) (coe v4))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v2) (coe v0))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2584)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v2)
                                    (coe v0)))
                              (coe d_'45'1'43'm'60'n'8854'm_3712 (coe v4) (coe v5)))
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v11
                      -> let v12 = subInt (coe v1) (coe (1 :: Integer)) in
                         coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                              (\ v13 v14 v15 v16 v17 -> coe du_'60''45'trans_2770 v16 v17)
                              (coe
                                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                              (\ v13 v14 v15 v16 v17 ->
                                 coe du_'60''45''8804''45'trans_2756 v16 v17)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v12) (coe v4))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258
                                 (coe subInt (coe v2) (coe (1 :: Integer))) (coe v4))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v2) (coe v0))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2584)
                                 (coe
                                    MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v2)
                                    (coe v0)))
                              (coe
                                 d_'8854''45'mono'737''45''60'_3868 (coe v4) (coe v12)
                                 (coe subInt (coe v2) (coe (1 :: Integer)))
                                 (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v11)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.+-comm
d_'43''45'comm_3892 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'comm_3892 = erased
-- Data.Integer.Properties.+-identityˡ
d_'43''45'identity'737'_3902 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'737'_3902 = erased
-- Data.Integer.Properties.+-identityʳ
d_'43''45'identity'691'_3904 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'691'_3904 = erased
-- Data.Integer.Properties.+-identity
d_'43''45'identity_3906 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'identity_3906
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Integer.Properties.distribˡ-⊖-+-pos
d_distrib'737''45''8854''45''43''45'pos_3914 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'737''45''8854''45''43''45'pos_3914 = erased
-- Data.Integer.Properties.distribˡ-⊖-+-neg
d_distrib'737''45''8854''45''43''45'neg_3934 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'737''45''8854''45''43''45'neg_3934 = erased
-- Data.Integer.Properties.distribʳ-⊖-+-pos
d_distrib'691''45''8854''45''43''45'pos_3954 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'691''45''8854''45''43''45'pos_3954 = erased
-- Data.Integer.Properties.distribʳ-⊖-+-neg
d_distrib'691''45''8854''45''43''45'neg_3974 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'691''45''8854''45''43''45'neg_3974 = erased
-- Data.Integer.Properties.+-assoc
d_'43''45'assoc_3988 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'assoc_3988 = erased
-- Data.Integer.Properties.+-inverseˡ
d_'43''45'inverse'737'_4168 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'inverse'737'_4168 = erased
-- Data.Integer.Properties.+-inverseʳ
d_'43''45'inverse'691'_4174 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'inverse'691'_4174 = erased
-- Data.Integer.Properties.+-inverse
d_'43''45'inverse_4176 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_4176
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Integer.Properties.+-isMagma
d_'43''45'isMagma_4178 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'43''45'isMagma_4178
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.Integer.Properties.+-isSemigroup
d_'43''45'isSemigroup_4180 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'43''45'isSemigroup_4180
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'43''45'isMagma_4178) erased
-- Data.Integer.Properties.+-isCommutativeSemigroup
d_'43''45'isCommutativeSemigroup_4182 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'43''45'isCommutativeSemigroup_4182
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemigroup'46'constructor_10975
      (coe d_'43''45'isSemigroup_4180) erased
-- Data.Integer.Properties.+-0-isMonoid
d_'43''45'0'45'isMonoid_4184 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'0'45'isMonoid_4184
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'43''45'isSemigroup_4180) (coe d_'43''45'identity_3906)
-- Data.Integer.Properties.+-0-isCommutativeMonoid
d_'43''45'0'45'isCommutativeMonoid_4186 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'0'45'isCommutativeMonoid_4186
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'43''45'0'45'isMonoid_4184) erased
-- Data.Integer.Properties.+-0-isGroup
d_'43''45'0'45'isGroup_4188 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_'43''45'0'45'isGroup_4188
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsGroup'46'constructor_22905
      (coe d_'43''45'0'45'isMonoid_4184) (coe d_'43''45'inverse_4176)
      erased
-- Data.Integer.Properties.+-0-isAbelianGroup
d_'43''45'0'45'isAbelianGroup_4190 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'0'45'isAbelianGroup_4190
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsAbelianGroup'46'constructor_27897
      (coe d_'43''45'0'45'isGroup_4188) erased
-- Data.Integer.Properties.+-magma
d_'43''45'magma_4192 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'43''45'magma_4192
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Integer.Base.d__'43'__276 d_'43''45'isMagma_4178
-- Data.Integer.Properties.+-semigroup
d_'43''45'semigroup_4194 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'43''45'semigroup_4194
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Integer.Base.d__'43'__276
      d_'43''45'isSemigroup_4180
-- Data.Integer.Properties.+-commutativeSemigroup
d_'43''45'commutativeSemigroup_4196 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'43''45'commutativeSemigroup_4196
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemigroup'46'constructor_10763
      MAlonzo.Code.Data.Integer.Base.d__'43'__276
      d_'43''45'isCommutativeSemigroup_4182
-- Data.Integer.Properties.+-0-monoid
d_'43''45'0'45'monoid_4198 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'43''45'0'45'monoid_4198
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      MAlonzo.Code.Data.Integer.Base.d__'43'__276 (0 :: Integer)
      d_'43''45'0'45'isMonoid_4184
-- Data.Integer.Properties.+-0-commutativeMonoid
d_'43''45'0'45'commutativeMonoid_4200 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'0'45'commutativeMonoid_4200
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Integer.Base.d__'43'__276 (0 :: Integer)
      d_'43''45'0'45'isCommutativeMonoid_4186
-- Data.Integer.Properties.+-0-abelianGroup
d_'43''45'0'45'abelianGroup_4202 ::
  MAlonzo.Code.Algebra.Bundles.T_AbelianGroup_1378
d_'43''45'0'45'abelianGroup_4202
  = coe
      MAlonzo.Code.Algebra.Bundles.C_AbelianGroup'46'constructor_24425
      MAlonzo.Code.Data.Integer.Base.d__'43'__276 (0 :: Integer)
      MAlonzo.Code.Data.Integer.Base.d_'45'__252
      d_'43''45'0'45'isAbelianGroup_4190
-- Data.Integer.Properties.pos-+
d_pos'45''43'_4204 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pos'45''43'_4204 = erased
-- Data.Integer.Properties.neg-distrib-+
d_neg'45'distrib'45''43'_4216 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'45''43'_4216 = erased
-- Data.Integer.Properties.◃-distrib-+
d_'9667''45'distrib'45''43'_4244 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'9667''45'distrib'45''43'_4244 = erased
-- Data.Integer.Properties.+-monoʳ-≤
d_'43''45'mono'691''45''8804'_4264 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'43''45'mono'691''45''8804'_4264 v0 v1 v2 v3
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v3 of
            MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v6
              -> coe
                   d_'8854''45'mono'691''45''8805''45''8804'_3778 (coe v0)
                   (coe subInt (coe (0 :: Integer)) (coe v1))
                   (coe subInt (coe (0 :: Integer)) (coe v2))
                   (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6)
            MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
              -> coe
                   du_'8804''45'trans_2514
                   (coe
                      d_m'8854'n'8804'm_3666 (coe v0)
                      (coe subInt (coe (0 :: Integer)) (coe v1)))
                   (coe
                      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                      (coe
                         MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362 (coe v0)))
            MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v6
              -> coe
                   MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                   (coe
                      MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'691''45''8804'_3434
                      (coe v0) (coe v2) (coe v6))
            _ -> MAlonzo.RTE.mazUnreachableError
      _ -> case coe v3 of
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v6
               -> let v7 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'691''45''8804'_3434
                       (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v7) (coe v6))
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
               -> coe
                    du_'8804''45'trans_2514
                    (coe
                       MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362
                          (coe subInt (coe (0 :: Integer)) (coe v0))))
                    (coe
                       d_'45'1'43'm'8804'n'8854'm_3746
                       (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v2))
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v6
               -> coe
                    d_'8854''45'mono'737''45''8804'_3810
                    (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v1) (coe v2)
                    (coe v6)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.+-monoˡ-≤
d_'43''45'mono'737''45''8804'_4294 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'43''45'mono'737''45''8804'_4294 v0 v1 v2
  = coe d_'43''45'mono'691''45''8804'_4264 (coe v0) (coe v1) (coe v2)
-- Data.Integer.Properties.+-mono-≤
d_'43''45'mono'45''8804'_4310 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'43''45'mono'45''8804'_4310 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_2584)
      (\ v6 v7 v8 -> coe du_'60''8658''8804'_2630 v8)
      (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v0) (coe v2))
      (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_2584)
         (\ v6 v7 v8 v9 v10 -> coe du_'8804''45''60''45'trans_2742 v9 v10)
         (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v0) (coe v2))
         (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v2))
         (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
            (coe d_'8804''45'isPreorder_2584)
            (\ v6 v7 v8 v9 v10 -> coe du_'8804''45''60''45'trans_2742 v9 v10)
            (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v2))
            (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3))
            (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_2584)
               (coe
                  MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3)))
            (coe
               d_'43''45'mono'691''45''8804'_4264 (coe v1) (coe v2) (coe v3)
               (coe v5)))
         (coe d_'43''45'mono'737''45''8804'_4294 v2 v0 v1 v4))
-- Data.Integer.Properties.i≤j⇒i≤k+j
d_i'8804'j'8658'i'8804'k'43'j_4332 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'j'8658'i'8804'k'43'j_4332 v0 v1 v2 ~v3 v4
  = du_i'8804'j'8658'i'8804'k'43'j_4332 v0 v1 v2 v4
du_i'8804'j'8658'i'8804'k'43'j_4332 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_i'8804'j'8658'i'8804'k'43'j_4332 v0 v1 v2 v3
  = coe
      d_'43''45'mono'45''8804'_4310 (coe (0 :: Integer)) (coe v2)
      (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
         (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
      (coe v3)
-- Data.Integer.Properties.i≤j+i
d_i'8804'j'43'i_4346 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'j'43'i_4346 v0 v1 ~v2 = du_i'8804'j'43'i_4346 v0 v1
du_i'8804'j'43'i_4346 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_i'8804'j'43'i_4346 v0 v1
  = coe
      du_i'8804'j'8658'i'8804'k'43'j_4332 (coe v0) (coe v0) (coe v1)
      (coe d_'8804''45'refl_2512 (coe v0))
-- Data.Integer.Properties.i≤i+j
d_i'8804'i'43'j_4358 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'i'43'j_4358 v0 v1 ~v2 = du_i'8804'i'43'j_4358 v0 v1
du_i'8804'i'43'j_4358 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_i'8804'i'43'j_4358 v0 v1
  = coe du_i'8804'j'43'i_4346 (coe v0) (coe v1)
-- Data.Integer.Properties.+-monoʳ-<
d_'43''45'mono'691''45''60'_4370 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'43''45'mono'691''45''60'_4370 v0 v1 v2 v3
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v3 of
            MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v6
              -> coe
                   d_'8854''45'mono'691''45''62''45''60'_3840 (coe v0)
                   (coe subInt (coe (0 :: Integer)) (coe v1))
                   (coe subInt (coe (0 :: Integer)) (coe v2))
                   (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6)
            MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
              -> coe
                   du_'60''45''8804''45'trans_2756
                   (coe
                      du_m'8854'1'43'n'60'm_3696 (coe v0)
                      (coe subInt (coe (0 :: Integer)) (coe v1)))
                   (coe
                      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                      (coe
                         MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362 (coe v0)))
            MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v6
              -> coe
                   MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                   (coe
                      MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'691''45''60'_3476
                      (coe v0) (coe v6))
            _ -> MAlonzo.RTE.mazUnreachableError
      _ -> let v4 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v3 of
             MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v7
               -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'691''45''60'_3476
                       (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v7))
             MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
               -> coe
                    du_'60''45''8804''45'trans_2756
                    (coe
                       MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362
                          (coe subInt (coe (0 :: Integer)) (coe v0))))
                    (coe
                       d_'45''91'1'43'm'93''8804'n'8854'm'43'1_3730 (coe v4) (coe v2))
             MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v7
               -> coe
                    d_'8854''45'mono'737''45''60'_3868
                    (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v1) (coe v2)
                    (coe v7)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.+-monoˡ-<
d_'43''45'mono'737''45''60'_4398 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'43''45'mono'737''45''60'_4398 v0 v1 v2
  = coe d_'43''45'mono'691''45''60'_4370 (coe v0) (coe v1) (coe v2)
-- Data.Integer.Properties.+-mono-<
d_'43''45'mono'45''60'_4414 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'43''45'mono'45''60'_4414 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
         (\ v6 v7 v8 v9 v10 -> coe du_'60''45'trans_2770 v9 v10)
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
         (\ v6 v7 v8 v9 v10 -> coe du_'60''45''8804''45'trans_2756 v9 v10)
         (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v0) (coe v2))
         (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v2))
         (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
            (\ v6 v7 v8 v9 v10 -> coe du_'60''45'trans_2770 v9 v10)
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
            (\ v6 v7 v8 v9 v10 -> coe du_'60''45''8804''45'trans_2756 v9 v10)
            (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v2))
            (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3))
            (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
               (coe d_'8804''45'isPreorder_2584)
               (coe
                  MAlonzo.Code.Data.Integer.Base.d__'43'__276 (coe v1) (coe v3)))
            (coe
               d_'43''45'mono'691''45''60'_4370 (coe v1) (coe v2) (coe v3)
               (coe v5)))
         (coe d_'43''45'mono'737''45''60'_4398 v2 v0 v1 v4))
-- Data.Integer.Properties.+-mono-≤-<
d_'43''45'mono'45''8804''45''60'_4432 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'43''45'mono'45''8804''45''60'_4432 v0 v1 v2 v3 v4 v5
  = coe
      du_'8804''45''60''45'trans_2742
      (coe d_'43''45'mono'737''45''8804'_4294 v2 v0 v1 v4)
      (coe
         d_'43''45'mono'691''45''60'_4370 (coe v1) (coe v2) (coe v3)
         (coe v5))
-- Data.Integer.Properties.+-mono-<-≤
d_'43''45'mono'45''60''45''8804'_4444 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'43''45'mono'45''60''45''8804'_4444 v0 v1 v2 v3 v4 v5
  = coe
      du_'60''45''8804''45'trans_2756
      (coe d_'43''45'mono'737''45''60'_4398 v2 v0 v1 v4)
      (coe
         d_'43''45'mono'691''45''8804'_4264 (coe v1) (coe v2) (coe v3)
         (coe v5))
-- Data.Integer.Properties.neg-minus-pos
d_neg'45'minus'45'pos_4460 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'minus'45'pos_4460 = erased
-- Data.Integer.Properties.+-minus-telescope
d_'43''45'minus'45'telescope_4476 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'minus'45'telescope_4476 = erased
-- Data.Integer.Properties.[+m]-[+n]≡m⊖n
d_'91''43'm'93''45''91''43'n'93''8801'm'8854'n_4498 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91''43'm'93''45''91''43'n'93''8801'm'8854'n_4498 = erased
-- Data.Integer.Properties.∣i-j∣≡∣j-i∣
d_'8739'i'45'j'8739''8801''8739'j'45'i'8739'_4512 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'i'45'j'8739''8801''8739'j'45'i'8739'_4512 = erased
-- Data.Integer.Properties.∣-∣-≤
d_'8739''45''8739''45''8804'_4542 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45''8739''45''8804'_4542 = erased
-- Data.Integer.Properties.i≡j⇒i-j≡0
d_i'8801'j'8658'i'45'j'8801'0_4580 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_i'8801'j'8658'i'45'j'8801'0_4580 = erased
-- Data.Integer.Properties.i-j≡0⇒i≡j
d_i'45'j'8801'0'8658'i'8801'j_4588 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_i'45'j'8801'0'8658'i'8801'j_4588 = erased
-- Data.Integer.Properties.i≤j⇒i-k≤j
d_i'8804'j'8658'i'45'k'8804'j_4606 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'j'8658'i'45'k'8804'j_4606 v0 ~v1 v2 ~v3 v4
  = du_i'8804'j'8658'i'45'k'8804'j_4606 v0 v2 v4
du_i'8804'j'8658'i'45'k'8804'j_4606 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_i'8804'j'8658'i'45'k'8804'j_4606 v0 v1 v2
  = case coe v1 of
      0 -> coe v2
      _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           case coe v0 of
             _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
                 coe
                   du_'8804''45'trans_2514
                   (coe d_m'8854'n'8804'm_3666 (coe v0) (coe v1)) (coe v2)
             _ -> let v4 = subInt (coe (-1 :: Integer)) (coe v0) in
                  coe
                    du_'8804''45'trans_2514
                    (coe
                       MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.du_'8804''45'trans_2578
                          (coe
                             MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362 (coe v4))
                          (coe
                             MAlonzo.Code.Data.Nat.Properties.d_n'8804'1'43'n_2668
                             (coe addInt (coe v4) (coe v3)))))
                    (coe v2)
-- Data.Integer.Properties.i-j≤i
d_i'45'j'8804'i_4634 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'45'j'8804'i_4634 v0 v1 ~v2 = du_i'45'j'8804'i_4634 v0 v1
du_i'45'j'8804'i_4634 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_i'45'j'8804'i_4634 v0 v1
  = coe
      du_i'8804'j'8658'i'45'k'8804'j_4606 (coe v0) (coe v1)
      (coe d_'8804''45'refl_2512 (coe v0))
-- Data.Integer.Properties.i≤j⇒i-j≤0
d_i'8804'j'8658'i'45'j'8804'0_4640 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'j'8658'i'45'j'8804'0_4640 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v5
        -> let v6 = subInt (coe (-1 :: Integer)) (coe v0) in
           let v7 = subInt (coe (-1 :: Integer)) (coe v1) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
             (coe d_'8804''45'isPreorder_2584)
             (\ v8 v9 v10 -> coe du_'60''8658''8804'_2630 v10)
             (coe MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v1))
             (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12)
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                (coe d_'8804''45'isPreorder_2584)
                (\ v8 v9 v10 v11 v12 ->
                   coe du_'8804''45''60''45'trans_2742 v11 v12)
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v7) (coe v6))
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v7) (coe v7))
                (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                   (coe d_'8804''45'isPreorder_2584)
                   (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12))
                (coe
                   d_'8854''45'mono'691''45''8805''45''8804'_3778 (coe v7) (coe v6)
                   (coe v7) (coe v5)))
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe
             du_i'8804'j'8658'i'45'k'8804'j_4606 (coe v0) (coe v1)
             (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40)
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v5
        -> case coe v1 of
             0 -> coe
                    seq (coe v5)
                    (coe
                       MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                       (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
             _ -> let v6 = subInt (coe v1) (coe (1 :: Integer)) in
                  case coe v5 of
                    MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
                      -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
                      -> let v10 = subInt (coe v0) (coe (1 :: Integer)) in
                         coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                           (coe d_'8804''45'isPreorder_2584)
                           (\ v11 v12 v13 -> coe du_'60''8658''8804'_2630 v13)
                           (coe MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v1))
                           (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12)
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                              (coe d_'8804''45'isPreorder_2584)
                              (\ v11 v12 v13 v14 v15 ->
                                 coe du_'8804''45''60''45'trans_2742 v14 v15)
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v10) (coe v6))
                              (coe
                                 MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe v10) (coe v10))
                              (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12)
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2584)
                                 (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12))
                              (coe
                                 d_'8854''45'mono'691''45''8805''45''8804'_3778 (coe v10) (coe v6)
                                 (coe v10) (coe v9)))
                    _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.i-j≤0⇒i≤j
d_i'45'j'8804'0'8658'i'8804'j_4666 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'45'j'8804'0'8658'i'8804'j_4666 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_2584)
      (\ v3 v4 v5 -> coe du_'60''8658''8804'_2630 v5) (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_2584)
         (\ v3 v4 v5 v6 v7 -> coe du_'8804''45''60''45'trans_2742 v6 v7)
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'43'__276
            (coe MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v1))
            (coe v1))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'43'__276
            (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12) (coe v1))
         (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe d_'8804''45'isPreorder_2584) (coe v1))
         (coe
            d_'43''45'mono'737''45''8804'_4294 v1
            (MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v1))
            MAlonzo.Code.Data.Integer.Base.d_0ℤ_12 v2))
-- Data.Integer.Properties.i≤j⇒0≤j-i
d_i'8804'j'8658'0'8804'j'45'i_4678 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'j'8658'0'8804'j'45'i_4678 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_2584)
      (\ v3 v4 v5 -> coe du_'60''8658''8804'_2630 v5)
      (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12)
      (coe MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v1) (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_2584)
         (\ v3 v4 v5 v6 v7 -> coe du_'8804''45''60''45'trans_2742 v6 v7)
         (coe MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v0))
         (coe MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v1) (coe v0))
         (coe MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v1) (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe d_'8804''45'isPreorder_2584)
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v1) (coe v0)))
         (coe
            d_'43''45'mono'737''45''8804'_4294
            (MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0)) v0 v1 v2))
-- Data.Integer.Properties.0≤i-j⇒j≤i
d_0'8804'i'45'j'8658'j'8804'i_4690 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_0'8804'i'45'j'8658'j'8804'i_4690 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_2584)
      (\ v3 v4 v5 -> coe du_'60''8658''8804'_2630 v5) (coe v1) (coe v0)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_2584)
         (\ v3 v4 v5 v6 v7 -> coe du_'8804''45''60''45'trans_2742 v6 v7)
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'43'__276
            (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12) (coe v1))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'43'__276
            (coe MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v1))
            (coe v1))
         (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe d_'8804''45'isPreorder_2584) (coe v0))
         (coe
            d_'43''45'mono'737''45''8804'_4294 v1
            MAlonzo.Code.Data.Integer.Base.d_0ℤ_12
            (MAlonzo.Code.Data.Integer.Base.d__'45'__294 (coe v0) (coe v1))
            v2))
-- Data.Integer.Properties.i≤j⇒i≤1+j
d_i'8804'j'8658'i'8804'1'43'j_4702 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'j'8658'i'8804'1'43'j_4702 v0 v1
  = coe
      du_i'8804'j'8658'i'8804'k'43'j_4332 (coe v0) (coe v1)
      (coe (1 :: Integer))
-- Data.Integer.Properties.i≤suc[i]
d_i'8804'suc'91'i'93'_4706 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'suc'91'i'93'_4706 v0
  = coe du_i'8804'j'43'i_4346 (coe v0) (coe (1 :: Integer))
-- Data.Integer.Properties.suc-+
d_suc'45''43'_4714 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_suc'45''43'_4714 = erased
-- Data.Integer.Properties.i≢suc[i]
d_i'8802'suc'91'i'93'_4724 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_i'8802'suc'91'i'93'_4724 = erased
-- Data.Integer.Properties.1-[1+n]≡-n
d_1'45''91'1'43'n'93''8801''45'n_4730 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_1'45''91'1'43'n'93''8801''45'n_4730 = erased
-- Data.Integer.Properties.suc-mono
d_suc'45'mono_4734 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_suc'45'mono_4734 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v5
        -> coe
             d_'8854''45'mono'691''45''8805''45''8804'_3778 (coe (1 :: Integer))
             (coe subInt (coe (0 :: Integer)) (coe v0))
             (coe subInt (coe (0 :: Integer)) (coe v1))
             (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> let v5 = subInt (coe (-1 :: Integer)) (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
             (coe d_'8804''45'isPreorder_2584)
             (\ v6 v7 v8 -> coe du_'60''8658''8804'_2630 v8)
             (coe
                MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                MAlonzo.Code.Data.Integer.Base.d_suc_300 (\ v6 v7 -> v6) v0 v1)
             (coe
                MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                (\ v6 v7 -> v7) MAlonzo.Code.Data.Integer.Base.d_suc_300 v0 v1)
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                (coe d_'8804''45'isPreorder_2584)
                (\ v6 v7 v8 v9 v10 -> coe du_'8804''45''60''45'trans_2742 v9 v10)
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'8854'__258 (coe (0 :: Integer))
                   (coe v5))
                (coe MAlonzo.Code.Data.Integer.Base.d_suc_300 (coe v1))
                (coe
                   MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                   (\ v6 v7 -> v7) MAlonzo.Code.Data.Integer.Base.d_suc_300 v0 v1)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                   (coe d_'8804''45'isPreorder_2584)
                   (coe MAlonzo.Code.Data.Integer.Base.d_suc_300 (coe v1)))
                (coe du_0'8854'm'8804''43'_3756 (coe v5)))
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v5
        -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.suc[i]≤j⇒i<j
d_suc'91'i'93''8804'j'8658'i'60'j_4748 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_suc'91'i'93''8804'j'8658'i'60'j_4748 v0 v1 v2
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v2 of
            MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v5
              -> coe MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v5
            _ -> MAlonzo.RTE.mazUnreachableError
      -1 -> coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
      _ -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   seq (coe v2) (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64)
             _ -> case coe v2 of
                    MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v5
                      -> coe
                           MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
                    _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.i<j⇒suc[i]≤j
d_i'60'j'8658'suc'91'i'93''8804'j_4768 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'60'j'8658'suc'91'i'93''8804'j_4768 v0 v1 v2
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v2 of
            MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v5
              -> coe MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v5
            _ -> MAlonzo.RTE.mazUnreachableError
      -1
        -> coe
             seq (coe v2)
             (coe
                MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
      _ -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   seq (coe v2)
                   (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40)
             _ -> case coe v2 of
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v5
                      -> coe
                           MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.du_'8804''45'pred_2654 (coe v5))
                    _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.suc-pred
d_suc'45'pred_4780 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_suc'45'pred_4780 = erased
-- Data.Integer.Properties.pred-suc
d_pred'45'suc_4790 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pred'45'suc_4790 = erased
-- Data.Integer.Properties.+-pred
d_'43''45'pred_4802 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'pred_4802 = erased
-- Data.Integer.Properties.pred-+
d_pred'45''43'_4818 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pred'45''43'_4818 = erased
-- Data.Integer.Properties.neg-suc
d_neg'45'suc_4830 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'suc_4830 = erased
-- Data.Integer.Properties.minus-suc
d_minus'45'suc_4838 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_minus'45'suc_4838 = erased
-- Data.Integer.Properties.i≤pred[j]⇒i<j
d_i'8804'pred'91'j'93''8658'i'60'j_4848 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_i'8804'pred'91'j'93''8658'i'60'j_4848 ~v0 v1 v2
  = du_i'8804'pred'91'j'93''8658'i'60'j_4848 v1 v2
du_i'8804'pred'91'j'93''8658'i'60'j_4848 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_i'8804'pred'91'j'93''8658'i'60'j_4848 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          coe
            du_'8804''45''60''45'trans_2742 (coe v1)
            (coe du_m'8854'1'43'n'60'm_3696 (coe v0) (coe (1 :: Integer)))
      _ -> coe
             du_'8804''45''60''45'trans_2742 (coe v1)
             (coe
                MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                (MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570
                   (coe subInt (coe (0 :: Integer)) (coe v0))))
-- Data.Integer.Properties.i<j⇒i≤pred[j]
d_i'60'j'8658'i'8804'pred'91'j'93'_4858 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'60'j'8658'i'8804'pred'91'j'93'_4858 ~v0 v1 v2
  = du_i'60'j'8658'i'8804'pred'91'j'93'_4858 v1 v2
du_i'60'j'8658'i'8804'pred'91'j'93'_4858 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_i'60'j'8658'i'8804'pred'91'j'93'_4858 v0 v1
  = case coe v0 of
      0 -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
      _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
          case coe v1 of
            MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
              -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
            MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v4
              -> coe
                   MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                   (coe
                      MAlonzo.Code.Data.Nat.Properties.du_'8804''45'pred_2654 (coe v4))
            _ -> MAlonzo.RTE.mazUnreachableError
      _ -> case coe v1 of
             MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v4
               -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v4
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.i≤j⇒pred[i]≤j
d_i'8804'j'8658'pred'91'i'93''8804'j_4870 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_i'8804'j'8658'pred'91'i'93''8804'j_4870 ~v0 ~v1 v2
  = du_i'8804'j'8658'pred'91'i'93''8804'j_4870 v2
du_i'8804'j'8658'pred'91'i'93''8804'j_4870 ::
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_i'8804'j'8658'pred'91'i'93''8804'j_4870 v0
  = case coe v0 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v3
        -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v3
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v3
        -> case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> coe MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v6
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.pred-mono
d_pred'45'mono_4876 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_pred'45'mono_4876 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v5
        -> coe
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
             (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> case coe v1 of
             0 -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             _ -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v5
        -> coe
             d_'8854''45'mono'737''45''8804'_3810 (coe (1 :: Integer)) (coe v0)
             (coe v1) (coe v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.*-comm
d_'42''45'comm_4884 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_4884 = erased
-- Data.Integer.Properties.*-identityˡ
d_'42''45'identity'737'_4918 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'737'_4918 = erased
-- Data.Integer.Properties.*-identityʳ
d_'42''45'identity'691'_4932 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'691'_4932 = erased
-- Data.Integer.Properties.*-identity
d_'42''45'identity_4934 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_4934
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Integer.Properties.*-zeroˡ
d_'42''45'zero'737'_4936 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'zero'737'_4936 = erased
-- Data.Integer.Properties.*-zeroʳ
d_'42''45'zero'691'_4938 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'zero'691'_4938 = erased
-- Data.Integer.Properties.*-zero
d_'42''45'zero_4940 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'zero_4940
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Integer.Properties.lemma
d_lemma_4948 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lemma_4948 = erased
-- Data.Integer.Properties.*-assoc
d_'42''45'assoc_4956 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_4956 = erased
-- Data.Integer.Properties.distrib-lemma
d_distrib'45'lemma_5034 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'45'lemma_5034 = erased
-- Data.Integer.Properties.*-distribʳ-+
d_'42''45'distrib'691''45''43'_5110 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''43'_5110 = erased
-- Data.Integer.Properties.*-distribˡ-+
d_'42''45'distrib'737''45''43'_5400 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''43'_5400 = erased
-- Data.Integer.Properties.*-distrib-+
d_'42''45'distrib'45''43'_5402 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'distrib'45''43'_5402
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Integer.Properties.*-isMagma
d_'42''45'isMagma_5404 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_5404
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.Integer.Properties.*-isSemigroup
d_'42''45'isSemigroup_5406 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_5406
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'42''45'isMagma_5404) erased
-- Data.Integer.Properties.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_5408 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_5408
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemigroup'46'constructor_10975
      (coe d_'42''45'isSemigroup_5406) erased
-- Data.Integer.Properties.*-1-isMonoid
d_'42''45'1'45'isMonoid_5410 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'1'45'isMonoid_5410
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'42''45'isSemigroup_5406) (coe d_'42''45'identity_4934)
-- Data.Integer.Properties.*-1-isCommutativeMonoid
d_'42''45'1'45'isCommutativeMonoid_5412 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'1'45'isCommutativeMonoid_5412
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'42''45'1'45'isMonoid_5410) erased
-- Data.Integer.Properties.+-*-isSemiring
d_'43''45''42''45'isSemiring_5414 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_'43''45''42''45'isSemiring_5414
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
         (coe d_'43''45'0'45'isCommutativeMonoid_4186) erased erased
         (coe d_'42''45'identity_4934) (coe d_'42''45'distrib'45''43'_5402))
      (coe d_'42''45'zero_4940)
-- Data.Integer.Properties.+-*-isCommutativeSemiring
d_'43''45''42''45'isCommutativeSemiring_5416 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'43''45''42''45'isCommutativeSemiring_5416
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe d_'43''45''42''45'isSemiring_5414) erased
-- Data.Integer.Properties.+-*-isRing
d_'43''45''42''45'isRing_5418 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_'43''45''42''45'isRing_5418
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsRing'46'constructor_80853
      (coe d_'43''45'0'45'isAbelianGroup_4190) erased erased
      (coe d_'42''45'identity_4934) (coe d_'42''45'distrib'45''43'_5402)
      (coe d_'42''45'zero_4940)
-- Data.Integer.Properties.+-*-isCommutativeRing
d_'43''45''42''45'isCommutativeRing_5420 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
d_'43''45''42''45'isCommutativeRing_5420
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeRing'46'constructor_87819
      (coe d_'43''45''42''45'isRing_5418) erased
-- Data.Integer.Properties.*-magma
d_'42''45'magma_5422 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'42''45'magma_5422
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Integer.Base.d__'42'__308 d_'42''45'isMagma_5404
-- Data.Integer.Properties.*-semigroup
d_'42''45'semigroup_5424 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'42''45'semigroup_5424
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      d_'42''45'isSemigroup_5406
-- Data.Integer.Properties.*-commutativeSemigroup
d_'42''45'commutativeSemigroup_5426 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'42''45'commutativeSemigroup_5426
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemigroup'46'constructor_10763
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      d_'42''45'isCommutativeSemigroup_5408
-- Data.Integer.Properties.*-1-monoid
d_'42''45'1'45'monoid_5428 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'1'45'monoid_5428
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      MAlonzo.Code.Data.Integer.Base.d_1ℤ_16 d_'42''45'1'45'isMonoid_5410
-- Data.Integer.Properties.*-1-commutativeMonoid
d_'42''45'1'45'commutativeMonoid_5430 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'1'45'commutativeMonoid_5430
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      MAlonzo.Code.Data.Integer.Base.d_1ℤ_16
      d_'42''45'1'45'isCommutativeMonoid_5412
-- Data.Integer.Properties.+-*-semiring
d_'43''45''42''45'semiring_5432 ::
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_'43''45''42''45'semiring_5432
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semiring'46'constructor_35691
      MAlonzo.Code.Data.Integer.Base.d__'43'__276
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      MAlonzo.Code.Data.Integer.Base.d_0ℤ_12
      MAlonzo.Code.Data.Integer.Base.d_1ℤ_16
      d_'43''45''42''45'isSemiring_5414
-- Data.Integer.Properties.+-*-commutativeSemiring
d_'43''45''42''45'commutativeSemiring_5434 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'43''45''42''45'commutativeSemiring_5434
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      MAlonzo.Code.Data.Integer.Base.d__'43'__276
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      MAlonzo.Code.Data.Integer.Base.d_0ℤ_12
      MAlonzo.Code.Data.Integer.Base.d_1ℤ_16
      d_'43''45''42''45'isCommutativeSemiring_5416
-- Data.Integer.Properties.+-*-ring
d_'43''45''42''45'ring_5436 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432
d_'43''45''42''45'ring_5436
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Ring'46'constructor_60565
      MAlonzo.Code.Data.Integer.Base.d__'43'__276
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      MAlonzo.Code.Data.Integer.Base.d_'45'__252
      MAlonzo.Code.Data.Integer.Base.d_0ℤ_12
      MAlonzo.Code.Data.Integer.Base.d_1ℤ_16
      d_'43''45''42''45'isRing_5418
-- Data.Integer.Properties.+-*-commutativeRing
d_'43''45''42''45'commutativeRing_5438 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634
d_'43''45''42''45'commutativeRing_5438
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeRing'46'constructor_64147
      MAlonzo.Code.Data.Integer.Base.d__'43'__276
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      MAlonzo.Code.Data.Integer.Base.d_'45'__252
      MAlonzo.Code.Data.Integer.Base.d_0ℤ_12
      MAlonzo.Code.Data.Integer.Base.d_1ℤ_16
      d_'43''45''42''45'isCommutativeRing_5420
-- Data.Integer.Properties.abs-*
d_abs'45''42'_5440 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_abs'45''42'_5440 = erased
-- Data.Integer.Properties.*-cancelʳ-≡
d_'42''45'cancel'691''45''8801'_5454 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'691''45''8801'_5454 = erased
-- Data.Integer.Properties.*-cancelˡ-≡
d_'42''45'cancel'737''45''8801'_5498 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'737''45''8801'_5498 = erased
-- Data.Integer.Properties.suc-*
d_suc'45''42'_5518 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_suc'45''42'_5518 = erased
-- Data.Integer.Properties.*-suc
d_'42''45'suc_5534 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'suc_5534 = erased
-- Data.Integer.Properties.-1*i≡-i
d_'45'1'42'i'8801''45'i_5548 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'45'1'42'i'8801''45'i_5548 = erased
-- Data.Integer.Properties.i*j≡0⇒i≡0∨j≡0
d_i'42'j'8801'0'8658'i'8801'0'8744'j'8801'0_5558 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_i'42'j'8801'0'8658'i'8801'0'8744'j'8801'0_5558 v0 ~v1 ~v2
  = du_i'42'j'8801'0'8658'i'8801'0'8744'j'8801'0_5558 v0
du_i'42'j'8801'0'8658'i'8801'0'8744'j'8801'0_5558 ::
  Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_i'42'j'8801'0'8658'i'8801'0'8744'j'8801'0_5558 v0
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_m'42'n'8801'0'8658'm'8801'0'8744'n'8801'0_3702
      (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
-- Data.Integer.Properties.^-identityʳ
d_'94''45'identity'691'_5582 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'94''45'identity'691'_5582 = erased
-- Data.Integer.Properties.^-zeroˡ
d_'94''45'zero'737'_5586 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'94''45'zero'737'_5586 = erased
-- Data.Integer.Properties.^-distribˡ-+-*
d_'94''45'distrib'737''45''43''45''42'_5600 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'94''45'distrib'737''45''43''45''42'_5600 = erased
-- Data.Integer.Properties.^-isMagmaHomomorphism
d_'94''45'isMagmaHomomorphism_5622 ::
  Integer ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_'94''45'isMagmaHomomorphism_5622 ~v0
  = du_'94''45'isMagmaHomomorphism_5622
du_'94''45'isMagmaHomomorphism_5622 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
du_'94''45'isMagmaHomomorphism_5622
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaHomomorphism'46'constructor_1049
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsRelHomomorphism'46'constructor_587
         erased)
      erased
-- Data.Integer.Properties.^-isMonoidHomomorphism
d_'94''45'isMonoidHomomorphism_5632 ::
  Integer ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidHomomorphism_266
d_'94''45'isMonoidHomomorphism_5632 ~v0
  = du_'94''45'isMonoidHomomorphism_5632
du_'94''45'isMonoidHomomorphism_5632 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidHomomorphism_266
du_'94''45'isMonoidHomomorphism_5632
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMonoidHomomorphism'46'constructor_5533
      (coe du_'94''45'isMagmaHomomorphism_5622) erased
-- Data.Integer.Properties.^-*-assoc
d_'94''45''42''45'assoc_5642 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'94''45''42''45'assoc_5642 = erased
-- Data.Integer.Properties.i^n≡0⇒i≡0
d_i'94'n'8801'0'8658'i'8801'0_5668 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_i'94'n'8801'0'8658'i'8801'0_5668 = erased
-- Data.Integer.Properties.pos-*
d_pos'45''42'_5676 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pos'45''42'_5676 = erased
-- Data.Integer.Properties.neg-distribˡ-*
d_neg'45'distrib'737''45''42'_5690 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'737''45''42'_5690 = erased
-- Data.Integer.Properties.neg-distribʳ-*
d_neg'45'distrib'691''45''42'_5706 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'691''45''42'_5706 = erased
-- Data.Integer.Properties.◃-distrib-*
d_'9667''45'distrib'45''42'_5724 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'9667''45'distrib'45''42'_5724 = erased
-- Data.Integer.Properties.*-cancelʳ-≤-pos
d_'42''45'cancel'691''45''8804''45'pos_5758 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'cancel'691''45''8804''45'pos_5758 v0 v1 ~v2 ~v3 v4
  = du_'42''45'cancel'691''45''8804''45'pos_5758 v0 v1 v4
du_'42''45'cancel'691''45''8804''45'pos_5758 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'42''45'cancel'691''45''8804''45'pos_5758 v0 v1 v2
  = case coe v0 of
      0 -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
      _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
          coe
            seq (coe v2)
            (coe
               MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
               (coe
                  MAlonzo.Code.Data.Nat.Properties.du_'42''45'cancel'691''45''8804'_3858
                  (coe v0)))
      _ -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
             _ -> coe
                    seq (coe v2)
                    (coe
                       MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.du_'8804''45'pred_2654
                          (coe
                             MAlonzo.Code.Data.Nat.Properties.du_'42''45'cancel'691''45''8804'_3858
                             (coe subInt (coe (0 :: Integer)) (coe v1)))))
-- Data.Integer.Properties.*-cancelˡ-≤-pos
d_'42''45'cancel'737''45''8804''45'pos_5792 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'cancel'737''45''8804''45'pos_5792 v0 v1 ~v2 ~v3
  = du_'42''45'cancel'737''45''8804''45'pos_5792 v0 v1
du_'42''45'cancel'737''45''8804''45'pos_5792 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'42''45'cancel'737''45''8804''45'pos_5792 v0 v1
  = coe
      du_'42''45'cancel'691''45''8804''45'pos_5758 (coe v0) (coe v1)
-- Data.Integer.Properties.*-monoʳ-≤-nonNeg
d_'42''45'mono'691''45''8804''45'nonNeg_5814 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'691''45''8804''45'nonNeg_5814 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'691''45''8804''45'nonNeg_5814 v0 v2 v3 v4
du_'42''45'mono'691''45''8804''45'nonNeg_5814 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'42''45'mono'691''45''8804''45'nonNeg_5814 v0 v1 v2 v3
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> case coe v3 of
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v6
               -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'8804''45'pred_2654
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.du_'42''45'mono'45''8804'_3894
                          (coe subInt (coe (0 :: Integer)) (coe v1)) (coe v0)
                          (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6)
                          (coe
                             MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570 (coe v0))))
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
               -> coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v6
               -> case coe v1 of
                    0 -> case coe v2 of
                           0 -> coe MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v6
                           _ -> coe
                                  MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                                  (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
                    _ -> coe
                           MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.du_'42''45'mono'737''45''8804'_3904
                              (coe v0) (coe v2) (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.*-monoˡ-≤-nonNeg
d_'42''45'mono'737''45''8804''45'nonNeg_5856 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'737''45''8804''45'nonNeg_5856 v0 ~v1 v2 v3
  = du_'42''45'mono'737''45''8804''45'nonNeg_5856 v0 v2 v3
du_'42''45'mono'737''45''8804''45'nonNeg_5856 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'42''45'mono'737''45''8804''45'nonNeg_5856 v0 v1 v2
  = coe
      du_'42''45'mono'691''45''8804''45'nonNeg_5814 (coe v0) (coe v1)
      (coe v2)
-- Data.Integer.Properties.*-cancelˡ-≤-neg
d_'42''45'cancel'737''45''8804''45'neg_5880 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'cancel'737''45''8804''45'neg_5880 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'737''45''8804''45'neg_5880 v0 v1 v2 v4
du_'42''45'cancel'737''45''8804''45'neg_5880 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'42''45'cancel'737''45''8804''45'neg_5880 v0 v1 v2 v3
  = coe
      d_neg'45'cancel'45''8804'_3040 (coe v1) (coe v2)
      (coe
         du_'42''45'cancel'737''45''8804''45'pos_5792
         (MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1))
         (MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
            (coe d_'8804''45'isPreorder_2584)
            (\ v4 v5 v6 -> coe du_'60''8658''8804'_2630 v6)
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308
               (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
               (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1)))
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308
               (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
               (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
               (coe d_'8804''45'isPreorder_2584)
               (\ v4 v5 v6 v7 v8 -> coe du_'8804''45''60''45'trans_2742 v7 v8)
               (coe MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v1))
               (coe MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v2))
               (coe
                  MAlonzo.Code.Data.Integer.Base.d__'42'__308
                  (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
                  (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                  (coe d_'8804''45'isPreorder_2584)
                  (coe
                     MAlonzo.Code.Data.Integer.Base.d__'42'__308
                     (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
                     (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2))))
               (coe v3))))
-- Data.Integer.Properties.*-cancelʳ-≤-neg
d_'42''45'cancel'691''45''8804''45'neg_5902 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'cancel'691''45''8804''45'neg_5902 v0 v1 v2 ~v3
  = du_'42''45'cancel'691''45''8804''45'neg_5902 v0 v1 v2
du_'42''45'cancel'691''45''8804''45'neg_5902 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'42''45'cancel'691''45''8804''45'neg_5902 v0 v1 v2
  = coe
      du_'42''45'cancel'737''45''8804''45'neg_5880 (coe v2) (coe v0)
      (coe v1)
-- Data.Integer.Properties.*-monoˡ-≤-nonPos
d_'42''45'mono'737''45''8804''45'nonPos_5924 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'737''45''8804''45'nonPos_5924 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''8804''45'nonPos_5924 v0 v2 v3 v4
du_'42''45'mono'737''45''8804''45'nonPos_5924 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'42''45'mono'737''45''8804''45'nonPos_5924 v0 v1 v2 v3
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
             (coe d_'8804''45'isPreorder_2584)
             (\ v4 v5 v6 -> coe du_'60''8658''8804'_2630 v6)
             (coe
                MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                (\ v4 v5 -> v5)
                (MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0)) v1 v2)
             (coe
                MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                (MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0))
                (\ v4 v5 -> v4) v1 v2)
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                (coe d_'8804''45'isPreorder_2584)
                (\ v4 v5 v6 v7 v8 -> coe du_'8804''45''60''45'trans_2742 v7 v8)
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                   (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
                   (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2)))
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                   (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
                   (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1)))
                (coe
                   MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0))
                   (\ v4 v5 -> v4) v1 v2)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                   (coe d_'8804''45'isPreorder_2584)
                   (coe
                      MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v1)))
                (coe
                   du_'42''45'mono'737''45''8804''45'nonNeg_5856
                   (MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
                   (coe
                      MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                      (\ v4 v5 -> v5) MAlonzo.Code.Data.Integer.Base.d_'45'__252 v1 v2)
                   (coe
                      MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                      MAlonzo.Code.Data.Integer.Base.d_'45'__252 (\ v4 v5 -> v4) v1 v2)
                   (coe du_neg'45'mono'45''8804'_3034 (coe v2) (coe v3))))
-- Data.Integer.Properties.*-monoʳ-≤-nonPos
d_'42''45'mono'691''45''8804''45'nonPos_5952 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'691''45''8804''45'nonPos_5952 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''8804''45'nonPos_5952 v0 v2 v3
du_'42''45'mono'691''45''8804''45'nonPos_5952 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_'42''45'mono'691''45''8804''45'nonPos_5952 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''8804''45'nonPos_5924 (coe v0) (coe v1)
      (coe v2)
-- Data.Integer.Properties.*-monoˡ-<-pos
d_'42''45'mono'737''45''60''45'pos_5974 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'mono'737''45''60''45'pos_5974 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''60''45'pos_5974 v0 v2 v3 v4
du_'42''45'mono'737''45''60''45'pos_5974 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'42''45'mono'737''45''60''45'pos_5974 v0 v1 v2 v3
  = let v4 = subInt (coe v0) (coe (1 :: Integer)) in
    case coe v1 of
      _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
          case coe v3 of
            MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v7
              -> coe
                   du_'43''9667''45'mono'45''60'_3354
                   (coe addInt (coe v1) (coe mulInt (coe v4) (coe v1)))
                   (coe
                      MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'45''60''45''8804'_3440
                      (coe v7)
                      (coe
                         MAlonzo.Code.Data.Nat.Properties.du_'42''45'mono'691''45''8804'_3914
                         (coe v4) (coe v2)
                         (coe
                            MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684
                            (coe v7))))
            _ -> MAlonzo.RTE.mazUnreachableError
      _ -> case coe v2 of
             _ | coe geqInt (coe v2) (coe (0 :: Integer)) ->
                 coe du_'45''9667''60''43''9667'_3396 (coe mulInt (coe v0) (coe v2))
             _ -> case coe v3 of
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v7
                      -> coe
                           MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'45''60''45''8804'_3440
                              (coe v7)
                              (coe
                                 MAlonzo.Code.Data.Nat.Properties.du_'42''45'mono'691''45''8804'_3914
                                 (coe v4) (coe subInt (coe (0 :: Integer)) (coe v1))
                                 (coe
                                    MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684
                                    (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7))))
                    _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.*-monoʳ-<-pos
d_'42''45'mono'691''45''60''45'pos_6006 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'mono'691''45''60''45'pos_6006 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''60''45'pos_6006 v0 v2 v3
du_'42''45'mono'691''45''60''45'pos_6006 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'42''45'mono'691''45''60''45'pos_6006 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''60''45'pos_5974 (coe v0) (coe v1) (coe v2)
-- Data.Integer.Properties.*-cancelˡ-<-nonNeg
d_'42''45'cancel'737''45''60''45'nonNeg_6026 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'cancel'737''45''60''45'nonNeg_6026 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'737''45''60''45'nonNeg_6026 v0 v1 v2 v4
du_'42''45'cancel'737''45''60''45'nonNeg_6026 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'42''45'cancel'737''45''60''45'nonNeg_6026 v0 v1 v2 v3
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.d_'42''45'cancel'737''45''60'_4048
                     v2 (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
                     (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1))
                     (coe
                        du_'43''9667''45'cancel'45''60'_3366
                        (coe
                           mulInt (coe v2)
                           (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0)))
                        (coe v3)))
            _ -> coe
                   MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      _ -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64
             _ -> coe
                    MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'8804''45'pred_2654
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.d_'42''45'cancel'737''45''60'_4048
                          v2 (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1))
                          (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
                          (coe
                             du_neg'9667''45'cancel'45''60'_3380
                             (coe
                                mulInt (coe v2)
                                (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
                             (coe v3))))
-- Data.Integer.Properties.*-cancelʳ-<-nonNeg
d_'42''45'cancel'691''45''60''45'nonNeg_6064 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'cancel'691''45''60''45'nonNeg_6064 v0 v1 v2 ~v3
  = du_'42''45'cancel'691''45''60''45'nonNeg_6064 v0 v1 v2
du_'42''45'cancel'691''45''60''45'nonNeg_6064 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'42''45'cancel'691''45''60''45'nonNeg_6064 v0 v1 v2
  = coe
      du_'42''45'cancel'737''45''60''45'nonNeg_6026 (coe v0) (coe v1)
      (coe v2)
-- Data.Integer.Properties.*-monoˡ-<-neg
d_'42''45'mono'737''45''60''45'neg_6086 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'mono'737''45''60''45'neg_6086 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''60''45'neg_6086 v0 v2 v3 v4
du_'42''45'mono'737''45''60''45'neg_6086 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'42''45'mono'737''45''60''45'neg_6086 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
         (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans_2770 v7 v8)
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
         (\ v4 v5 v6 v7 v8 -> coe du_'60''45''8804''45'trans_2756 v7 v8)
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
            (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
            (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1)))
         (coe MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe d_'8804''45'isPreorder_2584)
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v1)))
         (coe
            du_'42''45'mono'737''45''60''45'pos_5974
            (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
            (coe
               MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
               (\ v4 v5 -> v5) MAlonzo.Code.Data.Integer.Base.d_'45'__252 v1 v2)
            (coe
               MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
               MAlonzo.Code.Data.Integer.Base.d_'45'__252 (\ v4 v5 -> v4) v1 v2)
            (coe d_neg'45'mono'45''60'_3064 (coe v1) (coe v2) (coe v3))))
-- Data.Integer.Properties.*-monoʳ-<-neg
d_'42''45'mono'691''45''60''45'neg_6106 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'mono'691''45''60''45'neg_6106 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''60''45'neg_6106 v0 v2 v3
du_'42''45'mono'691''45''60''45'neg_6106 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'42''45'mono'691''45''60''45'neg_6106 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''60''45'neg_6086 (coe v0) (coe v1) (coe v2)
-- Data.Integer.Properties.*-cancelˡ-<-nonPos
d_'42''45'cancel'737''45''60''45'nonPos_6126 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'cancel'737''45''60''45'nonPos_6126 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'737''45''60''45'nonPos_6126 v0 v1 v2 v4
du_'42''45'cancel'737''45''60''45'nonPos_6126 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'42''45'cancel'737''45''60''45'nonPos_6126 v0 v1 v2 v3
  = coe
      d_neg'45'cancel'45''60'_3078 (coe v0) (coe v1)
      (coe
         du_'42''45'cancel'737''45''60''45'nonNeg_6026
         (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v0))
         (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1))
         (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
               (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans_2770 v7 v8)
               (coe
                  MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
               (\ v4 v5 v6 v7 v8 -> coe du_'60''45''8804''45'trans_2756 v7 v8)
               (coe MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v2) (coe v0))
               (coe MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v2) (coe v1))
               (coe
                  MAlonzo.Code.Data.Integer.Base.d__'42'__308
                  (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2))
                  (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                  (coe d_'8804''45'isPreorder_2584)
                  (coe
                     MAlonzo.Code.Data.Integer.Base.d__'42'__308
                     (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v2))
                     (coe MAlonzo.Code.Data.Integer.Base.d_'45'__252 (coe v1))))
               (coe v3))))
-- Data.Integer.Properties.*-cancelʳ-<-nonPos
d_'42''45'cancel'691''45''60''45'nonPos_6148 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'cancel'691''45''60''45'nonPos_6148 v0 v1 v2 ~v3
  = du_'42''45'cancel'691''45''60''45'nonPos_6148 v0 v1 v2
du_'42''45'cancel'691''45''60''45'nonPos_6148 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_'42''45'cancel'691''45''60''45'nonPos_6148 v0 v1 v2
  = coe
      du_'42''45'cancel'737''45''60''45'nonPos_6126 (coe v0) (coe v1)
      (coe v2)
-- Data.Integer.Properties.*-cancelˡ-<-neg
d_'42''45'cancel'737''45''60''45'neg_6166 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'cancel'737''45''60''45'neg_6166 v0 v1 v2
  = coe
      du_'42''45'cancel'737''45''60''45'nonPos_6126 (coe v0) (coe v1)
      (coe subInt (coe (-1 :: Integer)) (coe v2))
-- Data.Integer.Properties.*-cancelʳ-<-neg
d_'42''45'cancel'691''45''60''45'neg_6176 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'cancel'691''45''60''45'neg_6176 v0 v1 v2
  = coe
      du_'42''45'cancel'691''45''60''45'nonPos_6148 (coe v0) (coe v1)
      (coe subInt (coe (-1 :: Integer)) (coe v2))
-- Data.Integer.Properties.∣i*j∣≡∣i∣*∣j∣
d_'8739'i'42'j'8739''8801''8739'i'8739''42''8739'j'8739'_6188 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'i'42'j'8739''8801''8739'i'8739''42''8739'j'8739'_6188
  = erased
-- Data.Integer.Properties.i≤j⇒i⊓j≡i
d_i'8804'j'8658'i'8851'j'8801'i_6194 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_i'8804'j'8658'i'8851'j'8801'i_6194 = erased
-- Data.Integer.Properties.i≥j⇒i⊓j≡j
d_i'8805'j'8658'i'8851'j'8801'j_6200 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_i'8805'j'8658'i'8851'j'8801'j_6200 = erased
-- Data.Integer.Properties.i≤j⇒i⊔j≡j
d_i'8804'j'8658'i'8852'j'8801'j_6206 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_i'8804'j'8658'i'8852'j'8801'j_6206 = erased
-- Data.Integer.Properties.i≥j⇒i⊔j≡i
d_i'8805'j'8658'i'8852'j'8801'i_6212 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_i'8805'j'8658'i'8852'j'8801'i_6212 = erased
-- Data.Integer.Properties.⊓-operator
d_'8851''45'operator_6218 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84
d_'8851''45'operator_6218
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MinOperator'46'constructor_973
      (coe MAlonzo.Code.Data.Integer.Base.d__'8851'__340) erased erased
-- Data.Integer.Properties.⊔-operator
d_'8852''45'operator_6220 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MaxOperator_114
d_'8852''45'operator_6220
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MaxOperator'46'constructor_1501
      (coe MAlonzo.Code.Data.Integer.Base.d__'8852'__322) erased erased
-- Data.Integer.Properties.⊓-⊔-properties.antimono-≤-distrib-⊓
d_antimono'45''8804''45'distrib'45''8851'_6224 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8851'_6224 = erased
-- Data.Integer.Properties.⊓-⊔-properties.antimono-≤-distrib-⊔
d_antimono'45''8804''45'distrib'45''8852'_6226 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8852'_6226 = erased
-- Data.Integer.Properties.⊓-⊔-properties.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_6228 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8851'_6228 = erased
-- Data.Integer.Properties.⊓-⊔-properties.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_6230 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8852'_6230 = erased
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≤x
d_x'8851'y'8804'x_6232 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8804'x_6232
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_6234 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8804'y'8658'x'8851'z'8804'y_6234
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_6236 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8804'y'8658'z'8851'x'8804'y_6236
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_6238 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8804'y'8658'x'8851'z'8804'y_6238
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_6240 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8804'y'8658'z'8851'x'8804'y_6240
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_6242 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8804'y'8851'z'8658'x'8804'y_6242
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_6244 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8804'y'8851'z'8658'x'8804'z_6244
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≤y
d_x'8851'y'8804'y_6246 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8804'y_6246
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_6248 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8776'x'8658'x'8804'y_6248
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_6250 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8776'y'8658'y'8804'x_6250
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≤x
d_x'8851'y'8804'x_6252 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8804'x_6252
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≤x⊔y
d_x'8851'y'8804'x'8852'y_6254 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8804'x'8852'y_6254
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_x'8851'y'8804'x'8852'y_2996
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≤y
d_x'8851'y'8804'y_6256 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8804'y_6256
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_6258 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8776'x'8658'x'8804'y_6258
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_6260 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8851'y'8776'y'8658'y'8804'x_6260
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_6262 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8804'y'8851'z'8658'x'8804'y_6262
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_6264 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_x'8804'y'8851'z'8658'x'8804'z_6264
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-absorbs-⊔
d_'8851''45'absorbs'45''8852'_6266 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'absorbs'45''8852'_6266 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-assoc
d_'8851''45'assoc_6268 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'assoc_6268 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-band
d_'8851''45'band_6270 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_6270
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-comm
d_'8851''45'comm_6272 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'comm_6272 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_6274 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_6274
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-distrib-⊔
d_'8851''45'distrib'45''8852'_6282 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'distrib'45''8852'_6282
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45'distrib'45''8852'_2816
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-distribʳ-⊔
d_'8851''45'distrib'691''45''8852'_6284 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'distrib'691''45''8852'_6284 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-distribˡ-⊔
d_'8851''45'distrib'737''45''8852'_6286 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'distrib'737''45''8852'_6286 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-glb
d_'8851''45'glb_6288 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8851''45'glb_6288
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-idem
d_'8851''45'idem_6290 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'idem_6290 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isBand
d_'8851''45'isBand_6298 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_6298
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_6300 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_6300
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isMagma
d_'8851''45'isMagma_6302 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_6302
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_6306 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_6306
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isSemigroup
d_'8851''45'isSemigroup_6308 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_6308
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-magma
d_'8851''45'magma_6310 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_6310
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-mono-≤
d_'8851''45'mono'45''8804'_6312 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8851''45'mono'45''8804'_6312
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_6316 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8851''45'mono'691''45''8804'_6316
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_6318 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8851''45'mono'737''45''8804'_6318
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-sel
d_'8851''45'sel_6322 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_6322
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-selectiveMagma
d_'8851''45'selectiveMagma_6324 ::
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_6324
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-semigroup
d_'8851''45'semigroup_6326 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_6326
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-properties.⊓-triangulate
d_'8851''45'triangulate_6328 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'triangulate_6328 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-⊔-absorptive
d_'8851''45''8852''45'absorptive_6336 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45''8852''45'absorptive_6336
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'absorptive_2896
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-properties.⊔-absorbs-⊓
d_'8852''45'absorbs'45''8851'_6338 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'absorbs'45''8851'_6338 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-assoc
d_'8851''45'assoc_6340 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'assoc_6340 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-band
d_'8851''45'band_6342 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_6342
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-comm
d_'8851''45'comm_6344 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'comm_6344 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_6346 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_6346
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊔-distrib-⊓
d_'8852''45'distrib'45''8851'_6354 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45'distrib'45''8851'_6354
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45'distrib'45''8851'_2848
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-properties.⊔-distribʳ-⊓
d_'8852''45'distrib'691''45''8851'_6356 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'distrib'691''45''8851'_6356 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊔-distribˡ-⊓
d_'8852''45'distrib'737''45''8851'_6358 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'distrib'737''45''8851'_6358 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-idem
d_'8851''45'idem_6360 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'idem_6360 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isBand
d_'8851''45'isBand_6368 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_6368
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_6370 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_6370
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isMagma
d_'8851''45'isMagma_6372 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_6372
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_6376 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_6376
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-isSemigroup
d_'8851''45'isSemigroup_6378 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_6378
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-glb
d_'8851''45'glb_6380 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8851''45'glb_6380
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-magma
d_'8851''45'magma_6382 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_6382
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-mono-≤
d_'8851''45'mono'45''8804'_6384 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8851''45'mono'45''8804'_6384
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_6388 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8851''45'mono'691''45''8804'_6388
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_6390 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8851''45'mono'737''45''8804'_6390
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-sel
d_'8851''45'sel_6392 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_6392
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-selectiveMagma
d_'8851''45'selectiveMagma_6394 ::
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_6394
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-semigroup
d_'8851''45'semigroup_6396 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_6396
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-properties.⊓-triangulate
d_'8851''45'triangulate_6398 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'triangulate_6398 = erased
-- Data.Integer.Properties.⊓-⊔-properties.⊔-⊓-absorptive
d_'8852''45''8851''45'absorptive_6406 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45''8851''45'absorptive_6406
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'absorptive_2894
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊓-isSemilattice
d_'8851''45'isSemilattice_6410 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8851''45'isSemilattice_6410
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'isSemilattice_586
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊓-semilattice
d_'8851''45'semilattice_6412 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8851''45'semilattice_6412
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'semilattice_588
      (coe v0) (coe v1)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊓-⊔-distributiveLattice
d_'8851''45''8852''45'distributiveLattice_6414 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8851''45''8852''45'distributiveLattice_6414
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'distributiveLattice_770
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊓-⊔-isDistributiveLattice
d_'8851''45''8852''45'isDistributiveLattice_6416 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8851''45''8852''45'isDistributiveLattice_6416
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'isDistributiveLattice_760
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊓-⊔-isLattice
d_'8851''45''8852''45'isLattice_6418 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8851''45''8852''45'isLattice_6418
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'isLattice_758
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊓-⊔-lattice
d_'8851''45''8852''45'lattice_6420 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8851''45''8852''45'lattice_6420
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'lattice_766
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊓-isSemilattice
d_'8851''45'isSemilattice_6422 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8851''45'isSemilattice_6422
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'isSemilattice_586
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊓-semilattice
d_'8851''45'semilattice_6424 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8851''45'semilattice_6424
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'semilattice_588
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊔-⊓-distributiveLattice
d_'8852''45''8851''45'distributiveLattice_6426 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8852''45''8851''45'distributiveLattice_6426
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'distributiveLattice_768
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊔-⊓-isDistributiveLattice
d_'8852''45''8851''45'isDistributiveLattice_6428 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8852''45''8851''45'isDistributiveLattice_6428
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'isDistributiveLattice_762
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊔-⊓-isLattice
d_'8852''45''8851''45'isLattice_6430 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8852''45''8851''45'isLattice_6430
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'isLattice_756
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.⊓-⊔-latticeProperties.⊔-⊓-lattice
d_'8852''45''8851''45'lattice_6432 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8852''45''8851''45'lattice_6432
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'lattice_764
      (coe d_'8804''45'totalPreorder_2596)
      (coe d_'8851''45'operator_6218) (coe d_'8852''45'operator_6220)
-- Data.Integer.Properties.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_6440 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8852'_6440 = erased
-- Data.Integer.Properties.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_6450 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8851'_6450 = erased
-- Data.Integer.Properties.antimono-≤-distrib-⊓
d_antimono'45''8804''45'distrib'45''8851'_6460 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8851'_6460 = erased
-- Data.Integer.Properties.antimono-≤-distrib-⊔
d_antimono'45''8804''45'distrib'45''8852'_6470 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
   MAlonzo.Code.Data.Integer.Base.T__'8804'__26) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8852'_6470 = erased
-- Data.Integer.Properties.mono-<-distrib-⊓
d_mono'45''60''45'distrib'45''8851'_6480 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
   MAlonzo.Code.Data.Integer.Base.T__'60'__50) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''60''45'distrib'45''8851'_6480 = erased
-- Data.Integer.Properties.mono-<-distrib-⊔
d_mono'45''60''45'distrib'45''8852'_6528 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
   MAlonzo.Code.Data.Integer.Base.T__'60'__50) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''60''45'distrib'45''8852'_6528 = erased
-- Data.Integer.Properties.antimono-<-distrib-⊔
d_antimono'45''60''45'distrib'45''8852'_6576 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
   MAlonzo.Code.Data.Integer.Base.T__'60'__50) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''60''45'distrib'45''8852'_6576 = erased
-- Data.Integer.Properties.antimono-<-distrib-⊓
d_antimono'45''60''45'distrib'45''8851'_6624 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
   MAlonzo.Code.Data.Integer.Base.T__'60'__50) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''60''45'distrib'45''8851'_6624 = erased
-- Data.Integer.Properties.neg-distrib-⊔-⊓
d_neg'45'distrib'45''8852''45''8851'_6670 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'45''8852''45''8851'_6670 = erased
-- Data.Integer.Properties.neg-distrib-⊓-⊔
d_neg'45'distrib'45''8851''45''8852'_6676 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'45''8851''45''8852'_6676 = erased
-- Data.Integer.Properties.*-distribˡ-⊓-nonNeg
d_'42''45'distrib'737''45''8851''45'nonNeg_6686 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8851''45'nonNeg_6686 = erased
-- Data.Integer.Properties.*-distribʳ-⊓-nonNeg
d_'42''45'distrib'691''45''8851''45'nonNeg_6702 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8851''45'nonNeg_6702 = erased
-- Data.Integer.Properties.*-distribˡ-⊓-nonPos
d_'42''45'distrib'737''45''8851''45'nonPos_6718 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8851''45'nonPos_6718 = erased
-- Data.Integer.Properties.*-distribʳ-⊓-nonPos
d_'42''45'distrib'691''45''8851''45'nonPos_6734 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8851''45'nonPos_6734 = erased
-- Data.Integer.Properties.*-distribˡ-⊔-nonNeg
d_'42''45'distrib'737''45''8852''45'nonNeg_6750 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8852''45'nonNeg_6750 = erased
-- Data.Integer.Properties.*-distribʳ-⊔-nonNeg
d_'42''45'distrib'691''45''8852''45'nonNeg_6766 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8852''45'nonNeg_6766 = erased
-- Data.Integer.Properties.*-distribˡ-⊔-nonPos
d_'42''45'distrib'737''45''8852''45'nonPos_6782 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8852''45'nonPos_6782 = erased
-- Data.Integer.Properties.*-distribʳ-⊔-nonPos
d_'42''45'distrib'691''45''8852''45'nonPos_6798 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8852''45'nonPos_6798 = erased
-- Data.Integer.Properties.neg-mono-<->
d_neg'45'mono'45''60''45''62'_6806 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_neg'45'mono'45''60''45''62'_6806 = coe d_neg'45'mono'45''60'_3064
-- Data.Integer.Properties.neg-mono-≤-≥
d_neg'45'mono'45''8804''45''8805'_6808 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_neg'45'mono'45''8804''45''8805'_6808 v0 v1 v2
  = coe du_neg'45'mono'45''8804'_3034 v1 v2
-- Data.Integer.Properties.*-monoʳ-≤-non-neg
d_'42''45'mono'691''45''8804''45'non'45'neg_6810 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'691''45''8804''45'non'45'neg_6810 v0 v1 v2 v3 v4
  = coe du_'42''45'mono'691''45''8804''45'nonNeg_5814 v0 v2 v3 v4
-- Data.Integer.Properties.*-monoˡ-≤-non-neg
d_'42''45'mono'737''45''8804''45'non'45'neg_6812 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'737''45''8804''45'non'45'neg_6812 v0 v1 v2 v3
  = coe du_'42''45'mono'737''45''8804''45'nonNeg_5856 v0 v2 v3
-- Data.Integer.Properties.*-cancelˡ-<-non-neg
d_'42''45'cancel'737''45''60''45'non'45'neg_6814 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'cancel'737''45''60''45'non'45'neg_6814 v0 v1 v2 v3 v4
  = coe du_'42''45'cancel'737''45''60''45'nonNeg_6026 v0 v1 v2 v4
-- Data.Integer.Properties.*-cancelʳ-<-non-neg
d_'42''45'cancel'691''45''60''45'non'45'neg_6816 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_'42''45'cancel'691''45''60''45'non'45'neg_6816 v0 v1 v2 v3
  = coe du_'42''45'cancel'691''45''60''45'nonNeg_6064 v0 v1 v2
-- Data.Integer.Properties.m≤n⇒m⊓n≡m
d_m'8804'n'8658'm'8851'n'8801'm_6818 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658'm'8851'n'8801'm_6818 = erased
-- Data.Integer.Properties.m⊓n≡m⇒m≤n
d_m'8851'n'8801'm'8658'm'8804'n_6820 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8851'n'8801'm'8658'm'8804'n_6820
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe v0) (coe v1)
-- Data.Integer.Properties.m≥n⇒m⊓n≡n
d_m'8805'n'8658'm'8851'n'8801'n_6822 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8805'n'8658'm'8851'n'8801'n_6822 = erased
-- Data.Integer.Properties.m⊓n≡n⇒m≥n
d_m'8851'n'8801'n'8658'm'8805'n_6824 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8851'n'8801'n'8658'm'8805'n_6824
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe v0) (coe v1)
-- Data.Integer.Properties.m⊓n≤n
d_m'8851'n'8804'n_6826 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8851'n'8804'n_6826
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe v0) (coe v1)
-- Data.Integer.Properties.m⊓n≤m
d_m'8851'n'8804'm_6828 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8851'n'8804'm_6828
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8851''45'operator_6218 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe v0) (coe v1)
-- Data.Integer.Properties.m≤n⇒m⊔n≡n
d_m'8804'n'8658'm'8852'n'8801'n_6830 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658'm'8852'n'8801'n_6830 = erased
-- Data.Integer.Properties.m⊔n≡n⇒m≤n
d_m'8852'n'8801'n'8658'm'8804'n_6832 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8852'n'8801'n'8658'm'8804'n_6832
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.m≥n⇒m⊔n≡m
d_m'8805'n'8658'm'8852'n'8801'm_6834 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8805'n'8658'm'8852'n'8801'm_6834 = erased
-- Data.Integer.Properties.m⊔n≡m⇒m≥n
d_m'8852'n'8801'm'8658'm'8805'n_6836 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8852'n'8801'm'8658'm'8805'n_6836
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.m≤m⊔n
d_m'8804'm'8852'n_6838 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8804'm'8852'n_6838
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.n≤m⊔n
d_n'8804'm'8852'n_6840 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_n'8804'm'8852'n_6840
  = let v0 = d_'8804''45'totalPreorder_2596 in
    let v1 = d_'8852''45'operator_6220 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Integer.Properties.+-pos-monoʳ-≤
d_'43''45'pos'45'mono'691''45''8804'_6844 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'43''45'pos'45'mono'691''45''8804'_6844 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v6
        -> coe
             d_'8854''45'mono'691''45''8805''45''8804'_3778 (coe v0)
             (coe subInt (coe (0 :: Integer)) (coe v1))
             (coe subInt (coe (0 :: Integer)) (coe v2))
             (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6)
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe
             du_'8804''45'trans_2514
             (coe
                d_m'8854'n'8804'm_3666 (coe v0)
                (coe subInt (coe (0 :: Integer)) (coe v1)))
             (coe
                MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                (coe
                   MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362 (coe v0)))
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v6
        -> coe
             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
             (coe
                MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'691''45''8804'_3434
                (coe v0) (coe v2) (coe v6))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.+-neg-monoʳ-≤
d_'43''45'neg'45'mono'691''45''8804'_6860 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'43''45'neg'45'mono'691''45''8804'_6860 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v6
        -> let v7 = subInt (coe (-1 :: Integer)) (coe v1) in
           coe
             MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
             (coe
                MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'691''45''8804'_3434
                (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v7) (coe v6))
      MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40
        -> coe
             du_'8804''45'trans_2514
             (coe
                MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                (coe
                   MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362
                   (coe addInt (coe (1 :: Integer)) (coe v0))))
             (coe
                d_'45'1'43'm'8804'n'8854'm_3746
                (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v2))
      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v6
        -> coe
             d_'8854''45'mono'737''45''8804'_3810
             (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1) (coe v2)
             (coe v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Properties.n≮n
d_n'8814'n_6874 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_n'8814'n_6874 = erased
-- Data.Integer.Properties.∣n∣≡0⇒n≡0
d_'8739'n'8739''8801'0'8658'n'8801'0_6876 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'n'8739''8801'0'8658'n'8801'0_6876 = erased
-- Data.Integer.Properties.∣-n∣≡∣n∣
d_'8739''45'n'8739''8801''8739'n'8739'_6878 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45'n'8739''8801''8739'n'8739'_6878 = erased
-- Data.Integer.Properties.0≤n⇒+∣n∣≡n
d_0'8804'n'8658''43''8739'n'8739''8801'n_6880 ::
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'8804'n'8658''43''8739'n'8739''8801'n_6880 = erased
-- Data.Integer.Properties.+∣n∣≡n⇒0≤n
d_'43''8739'n'8739''8801'n'8658'0'8804'n_6882 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'43''8739'n'8739''8801'n'8658'0'8804'n_6882 v0 v1
  = coe du_'43''8739'i'8739''8801'i'8658'0'8804'i_3114
-- Data.Integer.Properties.+∣n∣≡n⊎+∣n∣≡-n
d_'43''8739'n'8739''8801'n'8846''43''8739'n'8739''8801''45'n_6884 ::
  Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'43''8739'n'8739''8801'n'8846''43''8739'n'8739''8801''45'n_6884
  = coe
      d_'43''8739'i'8739''8801'i'8846''43''8739'i'8739''8801''45'i_3120
-- Data.Integer.Properties.∣m+n∣≤∣m∣+∣n∣
d_'8739'm'43'n'8739''8804''8739'm'8739''43''8739'n'8739'_6886 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8739'm'43'n'8739''8804''8739'm'8739''43''8739'n'8739'_6886
  = coe d_'8739'i'43'j'8739''8804''8739'i'8739''43''8739'j'8739'_3160
-- Data.Integer.Properties.∣m-n∣≤∣m∣+∣n∣
d_'8739'm'45'n'8739''8804''8739'm'8739''43''8739'n'8739'_6888 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8739'm'45'n'8739''8804''8739'm'8739''43''8739'n'8739'_6888
  = coe d_'8739'i'45'j'8739''8804''8739'i'8739''43''8739'j'8739'_3198
-- Data.Integer.Properties.signₙ◃∣n∣≡n
d_sign'8345''9667''8739'n'8739''8801'n_6890 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sign'8345''9667''8739'n'8739''8801'n_6890 = erased
-- Data.Integer.Properties.◃-≡
d_'9667''45''8801'_6892 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'9667''45''8801'_6892 = erased
-- Data.Integer.Properties.∣m-n∣≡∣n-m∣
d_'8739'm'45'n'8739''8801''8739'n'45'm'8739'_6894 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'm'45'n'8739''8801''8739'n'45'm'8739'_6894 = erased
-- Data.Integer.Properties.m≡n⇒m-n≡0
d_m'8801'n'8658'm'45'n'8801'0_6896 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8801'n'8658'm'45'n'8801'0_6896 = erased
-- Data.Integer.Properties.m-n≡0⇒m≡n
d_m'45'n'8801'0'8658'm'8801'n_6898 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'45'n'8801'0'8658'm'8801'n_6898 = erased
-- Data.Integer.Properties.≤-steps
d_'8804''45'steps_6900 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8804''45'steps_6900 v0 v1 v2 v3 v4
  = coe du_i'8804'j'8658'i'8804'k'43'j_4332 v0 v1 v2 v4
-- Data.Integer.Properties.≤-steps-neg
d_'8804''45'steps'45'neg_6902 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8804''45'steps'45'neg_6902 v0 v1 v2 v3 v4
  = coe du_i'8804'j'8658'i'45'k'8804'j_4606 v0 v2 v4
-- Data.Integer.Properties.≤-step
d_'8804''45'step_6904 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8804''45'step_6904 = coe d_i'8804'j'8658'i'8804'1'43'j_4702
-- Data.Integer.Properties.≤-step-neg
d_'8804''45'step'45'neg_6906 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'8804''45'step'45'neg_6906 v0 v1 v2
  = coe du_i'8804'j'8658'pred'91'i'93''8804'j_4870 v2
-- Data.Integer.Properties.m≤n⇒m-n≤0
d_m'8804'n'8658'm'45'n'8804'0_6908 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8804'n'8658'm'45'n'8804'0_6908
  = coe d_i'8804'j'8658'i'45'j'8804'0_4640
-- Data.Integer.Properties.m-n≤0⇒m≤n
d_m'45'n'8804'0'8658'm'8804'n_6910 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'45'n'8804'0'8658'm'8804'n_6910
  = coe d_i'45'j'8804'0'8658'i'8804'j_4666
-- Data.Integer.Properties.m≤n⇒0≤n-m
d_m'8804'n'8658'0'8804'n'45'm_6912 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8804'n'8658'0'8804'n'45'm_6912
  = coe d_i'8804'j'8658'0'8804'j'45'i_4678
-- Data.Integer.Properties.0≤n-m⇒m≤n
d_0'8804'n'45'm'8658'm'8804'n_6914 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_0'8804'n'45'm'8658'm'8804'n_6914
  = coe d_0'8804'i'45'j'8658'j'8804'i_4690
-- Data.Integer.Properties.n≤1+n
d_n'8804'1'43'n_6916 ::
  Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_n'8804'1'43'n_6916 = coe d_i'8804'suc'91'i'93'_4706
-- Data.Integer.Properties.n≢1+n
d_n'8802'1'43'n_6918 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_n'8802'1'43'n_6918 = erased
-- Data.Integer.Properties.m≤pred[n]⇒m<n
d_m'8804'pred'91'n'93''8658'm'60'n_6920 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_m'8804'pred'91'n'93''8658'm'60'n_6920 v0 v1 v2
  = coe du_i'8804'pred'91'j'93''8658'i'60'j_4848 v1 v2
-- Data.Integer.Properties.m<n⇒m≤pred[n]
d_m'60'n'8658'm'8804'pred'91'n'93'_6922 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'60'n'8658'm'8804'pred'91'n'93'_6922 v0 v1 v2
  = coe du_i'60'j'8658'i'8804'pred'91'j'93'_4858 v1 v2
-- Data.Integer.Properties.-1*n≡-n
d_'45'1'42'n'8801''45'n_6924 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'45'1'42'n'8801''45'n_6924 = erased
-- Data.Integer.Properties.m*n≡0⇒m≡0∨n≡0
d_m'42'n'8801'0'8658'm'8801'0'8744'n'8801'0_6926 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_m'42'n'8801'0'8658'm'8801'0'8744'n'8801'0_6926 v0 v1 v2
  = coe du_i'42'j'8801'0'8658'i'8801'0'8744'j'8801'0_5558 v0
-- Data.Integer.Properties.∣m*n∣≡∣m∣*∣n∣
d_'8739'm'42'n'8739''8801''8739'm'8739''42''8739'n'8739'_6928 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'm'42'n'8739''8801''8739'm'8739''42''8739'n'8739'_6928
  = erased
-- Data.Integer.Properties.n≤m+n
d_n'8804'm'43'n_6932 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_n'8804'm'43'n_6932 v0 v1
  = coe du_i'8804'j'43'i_4346 (coe v0) (coe v1)
-- Data.Integer.Properties.m≤m+n
d_m'8804'm'43'n_6940 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'8804'm'43'n_6940 v0 v1
  = coe du_i'8804'i'43'j_4358 (coe v0) (coe v1)
-- Data.Integer.Properties.m-n≤m
d_m'45'n'8804'm_6950 ::
  Integer -> Integer -> MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_m'45'n'8804'm_6950 v0 v1
  = coe du_i'45'j'8804'i_4634 (coe v0) (coe v1)
-- Data.Integer.Properties.*-monoʳ-≤-pos
d_'42''45'mono'691''45''8804''45'pos_6960 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'691''45''8804''45'pos_6960 v0 v1 v2
  = coe
      du_'42''45'mono'691''45''8804''45'nonNeg_5814
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1) (coe v2)
-- Data.Integer.Properties.*-monoˡ-≤-pos
d_'42''45'mono'737''45''8804''45'pos_6968 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'737''45''8804''45'pos_6968 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''8804''45'nonNeg_5856
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1) (coe v2)
-- Data.Integer.Properties.*-monoˡ-≤-neg
d_'42''45'mono'737''45''8804''45'neg_6976 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'737''45''8804''45'neg_6976 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''8804''45'nonPos_5924
      (coe subInt (coe (-1 :: Integer)) (coe v0)) (coe v1) (coe v2)
-- Data.Integer.Properties.*-monoʳ-≤-neg
d_'42''45'mono'691''45''8804''45'neg_6984 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_'42''45'mono'691''45''8804''45'neg_6984 v0 v1 v2
  = coe
      du_'42''45'mono'691''45''8804''45'nonPos_5952
      (coe subInt (coe (-1 :: Integer)) (coe v0)) (coe v1) (coe v2)
-- Data.Integer.Properties.pos-+-commute
d_pos'45''43''45'commute_6988 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pos'45''43''45'commute_6988 = erased
-- Data.Integer.Properties.abs-*-commute
d_abs'45''42''45'commute_6990 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_abs'45''42''45'commute_6990 = erased
-- Data.Integer.Properties.pos-distrib-*
d_pos'45'distrib'45''42'_6996 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pos'45'distrib'45''42'_6996 = erased
-- Data.Integer.Properties.+-isAbelianGroup
d_'43''45'isAbelianGroup_7002 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_7002
  = coe d_'43''45'0'45'isAbelianGroup_4190
