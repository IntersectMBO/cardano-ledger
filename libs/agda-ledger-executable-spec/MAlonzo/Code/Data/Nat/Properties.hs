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

module MAlonzo.Code.Data.Nat.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Nat
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
import qualified MAlonzo.Code.Algebra.Morphism
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Bool.Properties
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Metric.Nat.Bundles
import qualified MAlonzo.Code.Function.Metric.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Construct.Converse
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Algebra
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Core
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.Nat.Properties._._DistributesOver_
d__DistributesOver__10 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d__DistributesOver__10 = erased
-- Data.Nat.Properties._._DistributesOverʳ_
d__DistributesOver'691'__12 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d__DistributesOver'691'__12 = erased
-- Data.Nat.Properties._._DistributesOverˡ_
d__DistributesOver'737'__14 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d__DistributesOver'737'__14 = erased
-- Data.Nat.Properties._.Associative
d_Associative_28 :: (Integer -> Integer -> Integer) -> ()
d_Associative_28 = erased
-- Data.Nat.Properties._.Commutative
d_Commutative_30 :: (Integer -> Integer -> Integer) -> ()
d_Commutative_30 = erased
-- Data.Nat.Properties._.Identity
d_Identity_46 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_Identity_46 = erased
-- Data.Nat.Properties._.LeftIdentity
d_LeftIdentity_70 ::
  Integer -> (Integer -> Integer -> Integer) -> ()
d_LeftIdentity_70 = erased
-- Data.Nat.Properties._.LeftZero
d_LeftZero_78 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_LeftZero_78 = erased
-- Data.Nat.Properties._.RightIdentity
d_RightIdentity_98 ::
  Integer -> (Integer -> Integer -> Integer) -> ()
d_RightIdentity_98 = erased
-- Data.Nat.Properties._.RightZero
d_RightZero_106 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_RightZero_106 = erased
-- Data.Nat.Properties._.Zero
d_Zero_124 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_Zero_124 = erased
-- Data.Nat.Properties._.IsAbelianGroup
d_IsAbelianGroup_128 a0 a1 a2 = ()
-- Data.Nat.Properties._.IsAlternativeMagma
d_IsAlternativeMagma_130 a0 = ()
-- Data.Nat.Properties._.IsBand
d_IsBand_132 a0 = ()
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring
d_IsCancellativeCommutativeSemiring_134 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsCommutativeMagma
d_IsCommutativeMagma_136 a0 = ()
-- Data.Nat.Properties._.IsCommutativeMonoid
d_IsCommutativeMonoid_138 a0 a1 = ()
-- Data.Nat.Properties._.IsCommutativeRing
d_IsCommutativeRing_140 a0 a1 a2 a3 a4 = ()
-- Data.Nat.Properties._.IsCommutativeSemigroup
d_IsCommutativeSemigroup_142 a0 = ()
-- Data.Nat.Properties._.IsCommutativeSemiring
d_IsCommutativeSemiring_144 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne
d_IsCommutativeSemiringWithoutOne_146 a0 a1 a2 = ()
-- Data.Nat.Properties._.IsFlexibleMagma
d_IsFlexibleMagma_148 a0 = ()
-- Data.Nat.Properties._.IsGroup
d_IsGroup_150 a0 a1 a2 = ()
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid
d_IsIdempotentCommutativeMonoid_152 a0 a1 = ()
-- Data.Nat.Properties._.IsIdempotentMagma
d_IsIdempotentMagma_154 a0 = ()
-- Data.Nat.Properties._.IsIdempotentSemiring
d_IsIdempotentSemiring_156 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsInvertibleMagma
d_IsInvertibleMagma_158 a0 a1 a2 = ()
-- Data.Nat.Properties._.IsInvertibleUnitalMagma
d_IsInvertibleUnitalMagma_160 a0 a1 a2 = ()
-- Data.Nat.Properties._.IsKleeneAlgebra
d_IsKleeneAlgebra_162 a0 a1 a2 a3 a4 = ()
-- Data.Nat.Properties._.IsLeftBolLoop
d_IsLeftBolLoop_164 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsLoop
d_IsLoop_166 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsMagma
d_IsMagma_168 a0 = ()
-- Data.Nat.Properties._.IsMedialMagma
d_IsMedialMagma_170 a0 = ()
-- Data.Nat.Properties._.IsMiddleBolLoop
d_IsMiddleBolLoop_172 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsMonoid
d_IsMonoid_174 a0 a1 = ()
-- Data.Nat.Properties._.IsMoufangLoop
d_IsMoufangLoop_176 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsNearSemiring
d_IsNearSemiring_178 a0 a1 a2 = ()
-- Data.Nat.Properties._.IsNearring
d_IsNearring_180 a0 a1 a2 a3 a4 = ()
-- Data.Nat.Properties._.IsNonAssociativeRing
d_IsNonAssociativeRing_182 a0 a1 a2 a3 a4 = ()
-- Data.Nat.Properties._.IsQuasigroup
d_IsQuasigroup_184 a0 a1 a2 = ()
-- Data.Nat.Properties._.IsQuasiring
d_IsQuasiring_186 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsRightBolLoop
d_IsRightBolLoop_188 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsRing
d_IsRing_190 a0 a1 a2 a3 a4 = ()
-- Data.Nat.Properties._.IsRingWithoutOne
d_IsRingWithoutOne_192 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsSelectiveMagma
d_IsSelectiveMagma_194 a0 = ()
-- Data.Nat.Properties._.IsSemigroup
d_IsSemigroup_196 a0 = ()
-- Data.Nat.Properties._.IsSemimedialMagma
d_IsSemimedialMagma_198 a0 = ()
-- Data.Nat.Properties._.IsSemiring
d_IsSemiring_200 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero
d_IsSemiringWithoutAnnihilatingZero_202 a0 a1 a2 a3 = ()
-- Data.Nat.Properties._.IsSemiringWithoutOne
d_IsSemiringWithoutOne_204 a0 a1 a2 = ()
-- Data.Nat.Properties._.IsUnitalMagma
d_IsUnitalMagma_206 a0 a1 = ()
-- Data.Nat.Properties._.IsAbelianGroup.assoc
d_assoc_212 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_212 = erased
-- Data.Nat.Properties._.IsAbelianGroup.comm
d_comm_214 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_214 = erased
-- Data.Nat.Properties._.IsAbelianGroup.identity
d_identity_216 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_216 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Nat.Properties._.IsAbelianGroup.inverse
d_inverse_222 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Nat.Properties._.IsAbelianGroup.isEquivalence
d_isEquivalence_234 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_234 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))))
-- Data.Nat.Properties._.IsAbelianGroup.isGroup
d_isGroup_236 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_236 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)
-- Data.Nat.Properties._.IsAbelianGroup.isMagma
d_isMagma_242 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_242 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))))
-- Data.Nat.Properties._.IsAbelianGroup.isMonoid
d_isMonoid_244 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_244 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Nat.Properties._.IsAbelianGroup.isSemigroup
d_isSemigroup_248 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_248 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Nat.Properties._.IsAbelianGroup.⁻¹-cong
d_'8315''185''45'cong_266 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_266 = erased
-- Data.Nat.Properties._.IsAbelianGroup.∙-cong
d_'8729''45'cong_268 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_268 = erased
-- Data.Nat.Properties._.IsAlternativeMagma.alter
d_alter_276 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_alter_276 v0
  = coe MAlonzo.Code.Algebra.Structures.d_alter_258 (coe v0)
-- Data.Nat.Properties._.IsAlternativeMagma.isEquivalence
d_isEquivalence_282 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_282 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0))
-- Data.Nat.Properties._.IsAlternativeMagma.isMagma
d_isMagma_284 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_284 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0)
-- Data.Nat.Properties._.IsAlternativeMagma.∙-cong
d_'8729''45'cong_298 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_298 = erased
-- Data.Nat.Properties._.IsBand.assoc
d_assoc_306 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_306 = erased
-- Data.Nat.Properties._.IsBand.idem
d_idem_308 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_308 = erased
-- Data.Nat.Properties._.IsBand.isEquivalence
d_isEquivalence_310 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_310 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)))
-- Data.Nat.Properties._.IsBand.isMagma
d_isMagma_312 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_312 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0))
-- Data.Nat.Properties._.IsBand.isSemigroup
d_isSemigroup_316 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_316 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)
-- Data.Nat.Properties._.IsBand.∙-cong
d_'8729''45'cong_328 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_328 = erased
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.*-assoc
d_'42''45'assoc_336 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_336 = erased
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.*-cancelˡ-nonZero
d_'42''45'cancel'737''45'nonZero_338 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'737''45'nonZero_338 = erased
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.*-comm
d_'42''45'comm_340 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_340 = erased
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.*-cong
d_'42''45'cong_342 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_342 = erased
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.*-identity
d_'42''45'identity_348 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_348 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.assoc
d_assoc_366 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_366 = erased
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.comm
d_comm_368 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_368 = erased
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.∙-cong
d_'8729''45'cong_370 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_370 = erased
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.identity
d_identity_376 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_376 v0
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
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_384 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_384 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.isMagma
d_isMagma_388 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_388 v0
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
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.isMonoid
d_isMonoid_390 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_390 v0
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
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.isSemigroup
d_isSemigroup_392 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_392 v0
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
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.distrib
d_distrib_396 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_396 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.isCommutativeSemiring
d_isCommutativeSemiring_402 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_402 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
      (coe v0)
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.isEquivalence
d_isEquivalence_406 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_406 v0
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
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.isSemiring
d_isSemiring_412 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_412 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe v0))
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_414 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_414 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe v0)))
-- Data.Nat.Properties._.IsCancellativeCommutativeSemiring.zero
d_zero_428 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_428 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe v0)))
-- Data.Nat.Properties._.IsCommutativeMagma.comm
d_comm_436 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_436 = erased
-- Data.Nat.Properties._.IsCommutativeMagma.isEquivalence
d_isEquivalence_438 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_438 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0))
-- Data.Nat.Properties._.IsCommutativeMagma.isMagma
d_isMagma_440 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_440 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0)
-- Data.Nat.Properties._.IsCommutativeMagma.∙-cong
d_'8729''45'cong_454 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_454 = erased
-- Data.Nat.Properties._.IsCommutativeMonoid.assoc
d_assoc_462 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_462 = erased
-- Data.Nat.Properties._.IsCommutativeMonoid.comm
d_comm_464 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_464 = erased
-- Data.Nat.Properties._.IsCommutativeMonoid.identity
d_identity_466 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_466 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Nat.Properties._.IsCommutativeMonoid.isEquivalence
d_isEquivalence_476 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_476 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))))
-- Data.Nat.Properties._.IsCommutativeMonoid.isMagma
d_isMagma_478 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_478 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)))
-- Data.Nat.Properties._.IsCommutativeMonoid.isMonoid
d_isMonoid_480 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_480 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)
-- Data.Nat.Properties._.IsCommutativeMonoid.isSemigroup
d_isSemigroup_484 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_484 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Nat.Properties._.IsCommutativeMonoid.∙-cong
d_'8729''45'cong_498 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_498 = erased
-- Data.Nat.Properties._.IsCommutativeRing.*-assoc
d_'42''45'assoc_508 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_508 = erased
-- Data.Nat.Properties._.IsCommutativeRing.*-comm
d_'42''45'comm_510 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_510 = erased
-- Data.Nat.Properties._.IsCommutativeRing.*-cong
d_'42''45'cong_512 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_512 = erased
-- Data.Nat.Properties._.IsCommutativeRing.*-identity
d_'42''45'identity_518 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_518 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Nat.Properties._.IsCommutativeRing.assoc
d_assoc_536 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_536 = erased
-- Data.Nat.Properties._.IsCommutativeRing.comm
d_comm_538 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_538 = erased
-- Data.Nat.Properties._.IsCommutativeRing.∙-cong
d_'8729''45'cong_540 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_540 = erased
-- Data.Nat.Properties._.IsCommutativeRing.identity
d_identity_546 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_546 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))
-- Data.Nat.Properties._.IsCommutativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_552 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_552 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Nat.Properties._.IsCommutativeRing.isGroup
d_isGroup_560 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_560 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))
-- Data.Nat.Properties._.IsCommutativeRing.isMagma
d_isMagma_566 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_566 v0
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
-- Data.Nat.Properties._.IsCommutativeRing.isMonoid
d_isMonoid_568 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_568 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))
-- Data.Nat.Properties._.IsCommutativeRing.isSemigroup
d_isSemigroup_570 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_570 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))
-- Data.Nat.Properties._.IsCommutativeRing.⁻¹-cong
d_'8315''185''45'cong_574 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_574 = erased
-- Data.Nat.Properties._.IsCommutativeRing.inverse
d_inverse_576 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_576 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))
-- Data.Nat.Properties._.IsCommutativeRing.distrib
d_distrib_582 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2426
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Nat.Properties._.IsCommutativeRing.isEquivalence
d_isEquivalence_592 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_592 v0
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
-- Data.Nat.Properties._.IsCommutativeRing.isRing
d_isRing_598 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_598 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)
-- Data.Nat.Properties._.IsCommutativeRing.zero
d_zero_620 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_620 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2428
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Nat.Properties._.IsCommutativeSemigroup.assoc
d_assoc_628 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_628 = erased
-- Data.Nat.Properties._.IsCommutativeSemigroup.comm
d_comm_630 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_630 = erased
-- Data.Nat.Properties._.IsCommutativeSemigroup.isEquivalence
d_isEquivalence_634 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_634 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)))
-- Data.Nat.Properties._.IsCommutativeSemigroup.isMagma
d_isMagma_636 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_636 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0))
-- Data.Nat.Properties._.IsCommutativeSemigroup.isSemigroup
d_isSemigroup_640 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_640 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)
-- Data.Nat.Properties._.IsCommutativeSemigroup.∙-cong
d_'8729''45'cong_652 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_652 = erased
-- Data.Nat.Properties._.IsCommutativeSemiring.*-assoc
d_'42''45'assoc_660 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_660 = erased
-- Data.Nat.Properties._.IsCommutativeSemiring.*-comm
d_'42''45'comm_662 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_662 = erased
-- Data.Nat.Properties._.IsCommutativeSemiring.*-cong
d_'42''45'cong_664 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_664 = erased
-- Data.Nat.Properties._.IsCommutativeSemiring.*-identity
d_'42''45'identity_670 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_670 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Nat.Properties._.IsCommutativeSemiring.assoc
d_assoc_688 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_688 = erased
-- Data.Nat.Properties._.IsCommutativeSemiring.comm
d_comm_690 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_690 = erased
-- Data.Nat.Properties._.IsCommutativeSemiring.∙-cong
d_'8729''45'cong_692 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_692 = erased
-- Data.Nat.Properties._.IsCommutativeSemiring.identity
d_identity_698 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_698 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))
-- Data.Nat.Properties._.IsCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_706 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_706 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Nat.Properties._.IsCommutativeSemiring.isMagma
d_isMagma_710 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_710 v0
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
-- Data.Nat.Properties._.IsCommutativeSemiring.isMonoid
d_isMonoid_712 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_712 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))))
-- Data.Nat.Properties._.IsCommutativeSemiring.isSemigroup
d_isSemigroup_714 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_714 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))
-- Data.Nat.Properties._.IsCommutativeSemiring.distrib
d_distrib_718 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_718 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Nat.Properties._.IsCommutativeSemiring.isEquivalence
d_isEquivalence_726 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_726 v0
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
-- Data.Nat.Properties._.IsCommutativeSemiring.isSemiring
d_isSemiring_732 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_732 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)
-- Data.Nat.Properties._.IsCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_734 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_734 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Nat.Properties._.IsCommutativeSemiring.zero
d_zero_748 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_748 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.*-assoc
d_'42''45'assoc_756 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_756 = erased
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.*-comm
d_'42''45'comm_758 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_758 = erased
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.*-cong
d_'42''45'cong_760 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_760 = erased
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.comm
d_comm_774 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_774 = erased
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_778 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_778 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.isMonoid
d_isMonoid_782 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_782 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
            (coe v0)))
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.distrib
d_distrib_784 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_784 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1166
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.isEquivalence
d_isEquivalence_786 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_786 v0
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
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.isSemiringWithoutOne
d_isSemiringWithoutOne_790 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_790 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
      (coe v0)
-- Data.Nat.Properties._.IsCommutativeSemiringWithoutOne.zero
d_zero_792 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_792 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1168
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Nat.Properties._.IsFlexibleMagma.flex
d_flex_800 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_flex_800 = erased
-- Data.Nat.Properties._.IsFlexibleMagma.isEquivalence
d_isEquivalence_802 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_802 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0))
-- Data.Nat.Properties._.IsFlexibleMagma.isMagma
d_isMagma_804 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_804 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0)
-- Data.Nat.Properties._.IsFlexibleMagma.∙-cong
d_'8729''45'cong_818 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_818 = erased
-- Data.Nat.Properties._.IsGroup.assoc
d_assoc_828 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_828 = erased
-- Data.Nat.Properties._.IsGroup.identity
d_identity_830 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_830 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Nat.Properties._.IsGroup.inverse
d_inverse_836 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_836 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_904 (coe v0)
-- Data.Nat.Properties._.IsGroup.isEquivalence
d_isEquivalence_842 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_842 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))))
-- Data.Nat.Properties._.IsGroup.isMagma
d_isMagma_848 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_848 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)))
-- Data.Nat.Properties._.IsGroup.isMonoid
d_isMonoid_850 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_850 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)
-- Data.Nat.Properties._.IsGroup.isSemigroup
d_isSemigroup_854 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_854 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Nat.Properties._.IsGroup.⁻¹-cong
d_'8315''185''45'cong_872 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_872 = erased
-- Data.Nat.Properties._.IsGroup.∙-cong
d_'8729''45'cong_874 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_874 = erased
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.assoc
d_assoc_882 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_882 = erased
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.comm
d_comm_884 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_884 = erased
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.idem
d_idem_886 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_886 = erased
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.identity
d_identity_888 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_888 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.isCommutativeMonoid
d_isCommutativeMonoid_898 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_898 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0)
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.isEquivalence
d_isEquivalence_902 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_902 v0
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
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.isMagma
d_isMagma_904 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_904 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.isMonoid
d_isMonoid_906 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_906 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.isSemigroup
d_isSemigroup_910 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_910 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Nat.Properties._.IsIdempotentCommutativeMonoid.∙-cong
d_'8729''45'cong_924 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_924 = erased
-- Data.Nat.Properties._.IsIdempotentMagma.idem
d_idem_932 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_932 = erased
-- Data.Nat.Properties._.IsIdempotentMagma.isEquivalence
d_isEquivalence_934 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_934 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0))
-- Data.Nat.Properties._.IsIdempotentMagma.isMagma
d_isMagma_936 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_936 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0)
-- Data.Nat.Properties._.IsIdempotentMagma.∙-cong
d_'8729''45'cong_950 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_950 = erased
-- Data.Nat.Properties._.IsIdempotentSemiring.*-assoc
d_'42''45'assoc_958 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_958 = erased
-- Data.Nat.Properties._.IsIdempotentSemiring.*-cong
d_'42''45'cong_960 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_960 = erased
-- Data.Nat.Properties._.IsIdempotentSemiring.*-identity
d_'42''45'identity_966 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_966 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Nat.Properties._.IsIdempotentSemiring.assoc
d_assoc_978 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_978 = erased
-- Data.Nat.Properties._.IsIdempotentSemiring.comm
d_comm_980 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_980 = erased
-- Data.Nat.Properties._.IsIdempotentSemiring.∙-cong
d_'8729''45'cong_982 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_982 = erased
-- Data.Nat.Properties._.IsIdempotentSemiring.+-idem
d_'43''45'idem_988 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_988 = erased
-- Data.Nat.Properties._.IsIdempotentSemiring.identity
d_identity_990 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_990 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))
-- Data.Nat.Properties._.IsIdempotentSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_998 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_998 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Nat.Properties._.IsIdempotentSemiring.isMagma
d_isMagma_1002 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1002 v0
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
-- Data.Nat.Properties._.IsIdempotentSemiring.isMonoid
d_isMonoid_1004 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1004 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))))
-- Data.Nat.Properties._.IsIdempotentSemiring.isSemigroup
d_isSemigroup_1006 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1006 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))
-- Data.Nat.Properties._.IsIdempotentSemiring.distrib
d_distrib_1010 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1010 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Nat.Properties._.IsIdempotentSemiring.isEquivalence
d_isEquivalence_1016 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1016 v0
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
-- Data.Nat.Properties._.IsIdempotentSemiring.isSemiring
d_isSemiring_1022 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1022 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)
-- Data.Nat.Properties._.IsIdempotentSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1024 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1024 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Nat.Properties._.IsIdempotentSemiring.zero
d_zero_1038 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1038 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Nat.Properties._.IsInvertibleMagma.inverse
d_inverse_1046 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1046 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_792 (coe v0)
-- Data.Nat.Properties._.IsInvertibleMagma.isEquivalence
d_isEquivalence_1052 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1052 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0))
-- Data.Nat.Properties._.IsInvertibleMagma.isMagma
d_isMagma_1054 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1054 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0)
-- Data.Nat.Properties._.IsInvertibleMagma.⁻¹-cong
d_'8315''185''45'cong_1068 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1068 = erased
-- Data.Nat.Properties._.IsInvertibleMagma.∙-cong
d_'8729''45'cong_1070 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1070 = erased
-- Data.Nat.Properties._.IsInvertibleUnitalMagma.identity
d_identity_1078 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1078 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_842 (coe v0)
-- Data.Nat.Properties._.IsInvertibleUnitalMagma.inverse
d_inverse_1084 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1084 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_792
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Nat.Properties._.IsInvertibleUnitalMagma.isEquivalence
d_isEquivalence_1090 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1090 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe
            MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)))
-- Data.Nat.Properties._.IsInvertibleUnitalMagma.isInvertibleMagma
d_isInvertibleMagma_1092 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_1092 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)
-- Data.Nat.Properties._.IsInvertibleUnitalMagma.isMagma
d_isMagma_1094 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1094 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_790
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Nat.Properties._.IsInvertibleUnitalMagma.⁻¹-cong
d_'8315''185''45'cong_1110 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1110 = erased
-- Data.Nat.Properties._.IsInvertibleUnitalMagma.∙-cong
d_'8729''45'cong_1112 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1112 = erased
-- Data.Nat.Properties._.IsKleeneAlgebra.*-assoc
d_'42''45'assoc_1120 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1120 = erased
-- Data.Nat.Properties._.IsKleeneAlgebra.*-cong
d_'42''45'cong_1122 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1122 = erased
-- Data.Nat.Properties._.IsKleeneAlgebra.*-identity
d_'42''45'identity_1128 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1128 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Nat.Properties._.IsKleeneAlgebra.assoc
d_assoc_1140 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1140 = erased
-- Data.Nat.Properties._.IsKleeneAlgebra.comm
d_comm_1142 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1142 = erased
-- Data.Nat.Properties._.IsKleeneAlgebra.∙-cong
d_'8729''45'cong_1144 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1144 = erased
-- Data.Nat.Properties._.IsKleeneAlgebra.+-idem
d_'43''45'idem_1150 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_1150 = erased
-- Data.Nat.Properties._.IsKleeneAlgebra.identity
d_identity_1152 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1152 v0
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
-- Data.Nat.Properties._.IsKleeneAlgebra.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1160 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1160 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Nat.Properties._.IsKleeneAlgebra.isMagma
d_isMagma_1164 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1164 v0
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
-- Data.Nat.Properties._.IsKleeneAlgebra.isMonoid
d_isMonoid_1166 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1166 v0
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
-- Data.Nat.Properties._.IsKleeneAlgebra.isSemigroup
d_isSemigroup_1168 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1168 v0
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
-- Data.Nat.Properties._.IsKleeneAlgebra.distrib
d_distrib_1172 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1172 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Nat.Properties._.IsKleeneAlgebra.isEquivalence
d_isEquivalence_1178 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1178 v0
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
-- Data.Nat.Properties._.IsKleeneAlgebra.isIdempotentSemiring
d_isIdempotentSemiring_1180 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722
d_isIdempotentSemiring_1180 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
      (coe v0)
-- Data.Nat.Properties._.IsKleeneAlgebra.isSemiring
d_isSemiring_1186 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1186 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
      (coe
         MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
         (coe v0))
-- Data.Nat.Properties._.IsKleeneAlgebra.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1188 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1188 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe v0)))
-- Data.Nat.Properties._.IsKleeneAlgebra.starDestructive
d_starDestructive_1198 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starDestructive_1198 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_starDestructive_1856 (coe v0)
-- Data.Nat.Properties._.IsKleeneAlgebra.starExpansive
d_starExpansive_1204 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starExpansive_1204 v0
  = coe MAlonzo.Code.Algebra.Structures.d_starExpansive_1854 (coe v0)
-- Data.Nat.Properties._.IsKleeneAlgebra.zero
d_zero_1214 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1214 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe v0)))
-- Data.Nat.Properties._.IsLeftBolLoop.//-cong
d_'47''47''45'cong_1222 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1222 = erased
-- Data.Nat.Properties._.IsLeftBolLoop.\\-cong
d_'92''92''45'cong_1228 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1228 = erased
-- Data.Nat.Properties._.IsLeftBolLoop.identity
d_identity_1234 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1234 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Nat.Properties._.IsLeftBolLoop.isEquivalence
d_isEquivalence_1240 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))))
-- Data.Nat.Properties._.IsLeftBolLoop.isLoop
d_isLoop_1242 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1242 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)
-- Data.Nat.Properties._.IsLeftBolLoop.isMagma
d_isMagma_1244 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1244 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Nat.Properties._.IsLeftBolLoop.isQuasigroup
d_isQuasigroup_1248 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1248 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Nat.Properties._.IsLeftBolLoop.leftBol
d_leftBol_1250 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1250 = erased
-- Data.Nat.Properties._.IsLeftBolLoop.leftDivides
d_leftDivides_1252 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1252 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Nat.Properties._.IsLeftBolLoop.rightDivides
d_rightDivides_1262 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1262 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Nat.Properties._.IsLeftBolLoop.∙-cong
d_'8729''45'cong_1274 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1274 = erased
-- Data.Nat.Properties._.IsLoop.//-cong
d_'47''47''45'cong_1282 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1282 = erased
-- Data.Nat.Properties._.IsLoop.\\-cong
d_'92''92''45'cong_1288 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1288 = erased
-- Data.Nat.Properties._.IsLoop.identity
d_identity_1294 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1294 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2784 (coe v0)
-- Data.Nat.Properties._.IsLoop.isEquivalence
d_isEquivalence_1300 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1300 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)))
-- Data.Nat.Properties._.IsLoop.isMagma
d_isMagma_1302 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1302 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Nat.Properties._.IsLoop.isQuasigroup
d_isQuasigroup_1306 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1306 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)
-- Data.Nat.Properties._.IsLoop.leftDivides
d_leftDivides_1308 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1308 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Nat.Properties._.IsLoop.rightDivides
d_rightDivides_1318 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1318 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Nat.Properties._.IsLoop.∙-cong
d_'8729''45'cong_1330 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1330 = erased
-- Data.Nat.Properties._.IsMagma.isEquivalence
d_isEquivalence_1338 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1338 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v0)
-- Data.Nat.Properties._.IsMagma.∙-cong
d_'8729''45'cong_1352 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1352 = erased
-- Data.Nat.Properties._.IsMedialMagma.isEquivalence
d_isEquivalence_1360 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1360 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0))
-- Data.Nat.Properties._.IsMedialMagma.isMagma
d_isMagma_1362 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1362 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0)
-- Data.Nat.Properties._.IsMedialMagma.medial
d_medial_1366 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_medial_1366 = erased
-- Data.Nat.Properties._.IsMedialMagma.∙-cong
d_'8729''45'cong_1378 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1378 = erased
-- Data.Nat.Properties._.IsMiddleBolLoop.//-cong
d_'47''47''45'cong_1386 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1386 = erased
-- Data.Nat.Properties._.IsMiddleBolLoop.\\-cong
d_'92''92''45'cong_1392 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1392 = erased
-- Data.Nat.Properties._.IsMiddleBolLoop.identity
d_identity_1398 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1398 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Nat.Properties._.IsMiddleBolLoop.isEquivalence
d_isEquivalence_1404 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1404 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))))
-- Data.Nat.Properties._.IsMiddleBolLoop.isLoop
d_isLoop_1406 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1406 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)
-- Data.Nat.Properties._.IsMiddleBolLoop.isMagma
d_isMagma_1408 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1408 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Nat.Properties._.IsMiddleBolLoop.isQuasigroup
d_isQuasigroup_1412 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1412 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Nat.Properties._.IsMiddleBolLoop.leftDivides
d_leftDivides_1414 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1414 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Nat.Properties._.IsMiddleBolLoop.middleBol
d_middleBol_1420 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_middleBol_1420 = erased
-- Data.Nat.Properties._.IsMiddleBolLoop.rightDivides
d_rightDivides_1426 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1426 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Nat.Properties._.IsMiddleBolLoop.∙-cong
d_'8729''45'cong_1438 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1438 = erased
-- Data.Nat.Properties._.IsMonoid.assoc
d_assoc_1446 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1446 = erased
-- Data.Nat.Properties._.IsMonoid.identity
d_identity_1448 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1448 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_612 (coe v0)
-- Data.Nat.Properties._.IsMonoid.isEquivalence
d_isEquivalence_1454 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1454 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)))
-- Data.Nat.Properties._.IsMonoid.isMagma
d_isMagma_1456 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1456 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0))
-- Data.Nat.Properties._.IsMonoid.isSemigroup
d_isSemigroup_1460 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1460 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)
-- Data.Nat.Properties._.IsMonoid.∙-cong
d_'8729''45'cong_1474 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1474 = erased
-- Data.Nat.Properties._.IsMoufangLoop.//-cong
d_'47''47''45'cong_1482 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1482 = erased
-- Data.Nat.Properties._.IsMoufangLoop.\\-cong
d_'92''92''45'cong_1488 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1488 = erased
-- Data.Nat.Properties._.IsMoufangLoop.identical
d_identical_1494 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identical_1494 = erased
-- Data.Nat.Properties._.IsMoufangLoop.identity
d_identity_1496 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1496 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))
-- Data.Nat.Properties._.IsMoufangLoop.isEquivalence
d_isEquivalence_1502 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1502 v0
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
-- Data.Nat.Properties._.IsMoufangLoop.isLeftBolLoop
d_isLeftBolLoop_1504 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846
d_isLeftBolLoop_1504 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)
-- Data.Nat.Properties._.IsMoufangLoop.isLoop
d_isLoop_1506 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1506 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_2860
      (coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))
-- Data.Nat.Properties._.IsMoufangLoop.isMagma
d_isMagma_1508 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1508 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Nat.Properties._.IsMoufangLoop.isQuasigroup
d_isQuasigroup_1512 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1512 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))
-- Data.Nat.Properties._.IsMoufangLoop.leftBol
d_leftBol_1514 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1514 = erased
-- Data.Nat.Properties._.IsMoufangLoop.leftDivides
d_leftDivides_1516 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1516 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Nat.Properties._.IsMoufangLoop.rightBol
d_rightBol_1526 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1526 = erased
-- Data.Nat.Properties._.IsMoufangLoop.rightDivides
d_rightDivides_1528 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1528 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Nat.Properties._.IsMoufangLoop.∙-cong
d_'8729''45'cong_1540 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1540 = erased
-- Data.Nat.Properties._.IsNearSemiring.*-assoc
d_'42''45'assoc_1548 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1548 = erased
-- Data.Nat.Properties._.IsNearSemiring.*-cong
d_'42''45'cong_1550 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1550 = erased
-- Data.Nat.Properties._.IsNearSemiring.assoc
d_assoc_1560 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1560 = erased
-- Data.Nat.Properties._.IsNearSemiring.∙-cong
d_'8729''45'cong_1562 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1562 = erased
-- Data.Nat.Properties._.IsNearSemiring.identity
d_identity_1568 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1568 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Nat.Properties._.IsNearSemiring.isMagma
d_isMagma_1574 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)))
-- Data.Nat.Properties._.IsNearSemiring.+-isMonoid
d_'43''45'isMonoid_1576 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1576 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)
-- Data.Nat.Properties._.IsNearSemiring.isSemigroup
d_isSemigroup_1578 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1578 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Nat.Properties._.IsNearSemiring.distribʳ
d_distrib'691'_1582 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'691'_1582 = erased
-- Data.Nat.Properties._.IsNearSemiring.isEquivalence
d_isEquivalence_1584 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1584 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))))
-- Data.Nat.Properties._.IsNearSemiring.zeroˡ
d_zero'737'_1598 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zero'737'_1598 = erased
-- Data.Nat.Properties._.IsNearring.*-assoc
d_'42''45'assoc_1602 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1602 = erased
-- Data.Nat.Properties._.IsNearring.*-cong
d_'42''45'cong_1604 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1604 = erased
-- Data.Nat.Properties._.IsNearring.*-identity
d_'42''45'identity_1610 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1610 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Nat.Properties._.IsNearring.assoc
d_assoc_1622 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1622 = erased
-- Data.Nat.Properties._.IsNearring.∙-cong
d_'8729''45'cong_1624 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1624 = erased
-- Data.Nat.Properties._.IsNearring.identity
d_identity_1630 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1630 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Nat.Properties._.IsNearring.+-inverse
d_'43''45'inverse_1636 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_1636 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'inverse_2314 (coe v0)
-- Data.Nat.Properties._.IsNearring.isMagma
d_isMagma_1642 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
            (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))))
-- Data.Nat.Properties._.IsNearring.+-isMonoid
d_'43''45'isMonoid_1644 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1644 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Nat.Properties._.IsNearring.isSemigroup
d_isSemigroup_1646 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1646 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Nat.Properties._.IsNearring.distrib
d_distrib_1650 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1650 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1992
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Nat.Properties._.IsNearring.isEquivalence
d_isEquivalence_1652 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1652 v0
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
-- Data.Nat.Properties._.IsNearring.isQuasiring
d_isQuasiring_1656 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962
d_isQuasiring_1656 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)
-- Data.Nat.Properties._.IsNearring.zero
d_zero_1668 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1668 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1994
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Nat.Properties._.IsNearring.⁻¹-cong
d_'8315''185''45'cong_1670 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1670 = erased
-- Data.Nat.Properties._.IsNonAssociativeRing.*-cong
d_'42''45'cong_1676 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1676 = erased
-- Data.Nat.Properties._.IsNonAssociativeRing.assoc
d_assoc_1684 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1684 = erased
-- Data.Nat.Properties._.IsNonAssociativeRing.comm
d_comm_1686 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1686 = erased
-- Data.Nat.Properties._.IsNonAssociativeRing.∙-cong
d_'8729''45'cong_1688 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1688 = erased
-- Data.Nat.Properties._.IsNonAssociativeRing.identity
d_identity_1694 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1694 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe v0))))
-- Data.Nat.Properties._.IsNonAssociativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1700 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1700 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
      (coe v0)
-- Data.Nat.Properties._.IsNonAssociativeRing.isGroup
d_isGroup_1708 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1708 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe v0))
-- Data.Nat.Properties._.IsNonAssociativeRing.isMagma
d_isMagma_1714 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1714 v0
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
-- Data.Nat.Properties._.IsNonAssociativeRing.isMonoid
d_isMonoid_1716 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1716 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe v0)))
-- Data.Nat.Properties._.IsNonAssociativeRing.isSemigroup
d_isSemigroup_1718 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1718 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe v0))))
-- Data.Nat.Properties._.IsNonAssociativeRing.⁻¹-cong
d_'8315''185''45'cong_1722 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1722 = erased
-- Data.Nat.Properties._.IsNonAssociativeRing.inverse
d_inverse_1724 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1724 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe v0)))
-- Data.Nat.Properties._.IsNonAssociativeRing.distrib
d_distrib_1730 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1730 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2208 (coe v0)
-- Data.Nat.Properties._.IsNonAssociativeRing.identity
d_identity_1732 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1732 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2206 (coe v0)
-- Data.Nat.Properties._.IsNonAssociativeRing.isEquivalence
d_isEquivalence_1734 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1734 v0
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
-- Data.Nat.Properties._.IsNonAssociativeRing.zero
d_zero_1752 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1752 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2210 (coe v0)
-- Data.Nat.Properties._.IsQuasigroup.//-cong
d_'47''47''45'cong_1756 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1756 = erased
-- Data.Nat.Properties._.IsQuasigroup.\\-cong
d_'92''92''45'cong_1762 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1762 = erased
-- Data.Nat.Properties._.IsQuasigroup.isEquivalence
d_isEquivalence_1768 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1768 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0))
-- Data.Nat.Properties._.IsQuasigroup.isMagma
d_isMagma_1770 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1770 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0)
-- Data.Nat.Properties._.IsQuasigroup.leftDivides
d_leftDivides_1774 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1774 v0
  = coe MAlonzo.Code.Algebra.Structures.d_leftDivides_2710 (coe v0)
-- Data.Nat.Properties._.IsQuasigroup.rightDivides
d_rightDivides_1784 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1784 v0
  = coe MAlonzo.Code.Algebra.Structures.d_rightDivides_2712 (coe v0)
-- Data.Nat.Properties._.IsQuasigroup.∙-cong
d_'8729''45'cong_1796 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1796 = erased
-- Data.Nat.Properties._.IsQuasiring.*-assoc
d_'42''45'assoc_1804 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1804 = erased
-- Data.Nat.Properties._.IsQuasiring.*-cong
d_'42''45'cong_1806 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1806 = erased
-- Data.Nat.Properties._.IsQuasiring.*-identity
d_'42''45'identity_1812 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1812 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990 (coe v0)
-- Data.Nat.Properties._.IsQuasiring.assoc
d_assoc_1824 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1824 = erased
-- Data.Nat.Properties._.IsQuasiring.∙-cong
d_'8729''45'cong_1826 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1826 = erased
-- Data.Nat.Properties._.IsQuasiring.identity
d_identity_1832 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1832 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Nat.Properties._.IsQuasiring.isMagma
d_isMagma_1838 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1838 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)))
-- Data.Nat.Properties._.IsQuasiring.+-isMonoid
d_'43''45'isMonoid_1840 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1840 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)
-- Data.Nat.Properties._.IsQuasiring.isSemigroup
d_isSemigroup_1842 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1842 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Nat.Properties._.IsQuasiring.distrib
d_distrib_1846 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1846 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1992 (coe v0)
-- Data.Nat.Properties._.IsQuasiring.isEquivalence
d_isEquivalence_1848 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1848 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))))
-- Data.Nat.Properties._.IsQuasiring.zero
d_zero_1862 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1862 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1994 (coe v0)
-- Data.Nat.Properties._.IsRightBolLoop.//-cong
d_'47''47''45'cong_1866 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1866 = erased
-- Data.Nat.Properties._.IsRightBolLoop.\\-cong
d_'92''92''45'cong_1872 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1872 = erased
-- Data.Nat.Properties._.IsRightBolLoop.identity
d_identity_1878 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1878 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Nat.Properties._.IsRightBolLoop.isEquivalence
d_isEquivalence_1884 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1884 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))))
-- Data.Nat.Properties._.IsRightBolLoop.isLoop
d_isLoop_1886 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1886 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)
-- Data.Nat.Properties._.IsRightBolLoop.isMagma
d_isMagma_1888 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1888 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Nat.Properties._.IsRightBolLoop.isQuasigroup
d_isQuasigroup_1892 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1892 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Nat.Properties._.IsRightBolLoop.leftDivides
d_leftDivides_1894 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1894 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Nat.Properties._.IsRightBolLoop.rightBol
d_rightBol_1904 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1904 = erased
-- Data.Nat.Properties._.IsRightBolLoop.rightDivides
d_rightDivides_1906 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1906 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Nat.Properties._.IsRightBolLoop.∙-cong
d_'8729''45'cong_1918 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1918 = erased
-- Data.Nat.Properties._.IsRing.*-assoc
d_'42''45'assoc_1928 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1928 = erased
-- Data.Nat.Properties._.IsRing.*-cong
d_'42''45'cong_1930 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1930 = erased
-- Data.Nat.Properties._.IsRing.*-identity
d_'42''45'identity_1936 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1936 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424 (coe v0)
-- Data.Nat.Properties._.IsRing.assoc
d_assoc_1948 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1948 = erased
-- Data.Nat.Properties._.IsRing.comm
d_comm_1950 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1950 = erased
-- Data.Nat.Properties._.IsRing.∙-cong
d_'8729''45'cong_1952 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1952 = erased
-- Data.Nat.Properties._.IsRing.identity
d_identity_1958 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1958 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v0))))
-- Data.Nat.Properties._.IsRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1964 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1964 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe v0)
-- Data.Nat.Properties._.IsRing.isGroup
d_isGroup_1972 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1972 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe v0))
-- Data.Nat.Properties._.IsRing.isMagma
d_isMagma_1978 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1978 v0
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
-- Data.Nat.Properties._.IsRing.isMonoid
d_isMonoid_1980 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1980 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v0)))
-- Data.Nat.Properties._.IsRing.isSemigroup
d_isSemigroup_1982 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1982 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v0))))
-- Data.Nat.Properties._.IsRing.⁻¹-cong
d_'8315''185''45'cong_1986 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1986 = erased
-- Data.Nat.Properties._.IsRing.inverse
d_inverse_1988 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1988 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v0)))
-- Data.Nat.Properties._.IsRing.distrib
d_distrib_1994 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1994 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2426 (coe v0)
-- Data.Nat.Properties._.IsRing.isEquivalence
d_isEquivalence_2000 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2000 v0
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
-- Data.Nat.Properties._.IsRing.zero
d_zero_2026 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2026 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2428 (coe v0)
-- Data.Nat.Properties._.IsRingWithoutOne.*-assoc
d_'42''45'assoc_2036 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2036 = erased
-- Data.Nat.Properties._.IsRingWithoutOne.*-cong
d_'42''45'cong_2038 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2038 = erased
-- Data.Nat.Properties._.IsRingWithoutOne.assoc
d_assoc_2048 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2048 = erased
-- Data.Nat.Properties._.IsRingWithoutOne.comm
d_comm_2050 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2050 = erased
-- Data.Nat.Properties._.IsRingWithoutOne.∙-cong
d_'8729''45'cong_2052 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2052 = erased
-- Data.Nat.Properties._.IsRingWithoutOne.identity
d_identity_2058 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe v0))))
-- Data.Nat.Properties._.IsRingWithoutOne.+-isAbelianGroup
d_'43''45'isAbelianGroup_2064 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_2064 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
      (coe v0)
-- Data.Nat.Properties._.IsRingWithoutOne.isGroup
d_isGroup_2072 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_2072 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe v0))
-- Data.Nat.Properties._.IsRingWithoutOne.isMagma
d_isMagma_2078 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2078 v0
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
-- Data.Nat.Properties._.IsRingWithoutOne.isMonoid
d_isMonoid_2080 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2080 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe v0)))
-- Data.Nat.Properties._.IsRingWithoutOne.isSemigroup
d_isSemigroup_2082 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2082 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe v0))))
-- Data.Nat.Properties._.IsRingWithoutOne.⁻¹-cong
d_'8315''185''45'cong_2086 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_2086 = erased
-- Data.Nat.Properties._.IsRingWithoutOne.inverse
d_inverse_2088 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_2088 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe v0)))
-- Data.Nat.Properties._.IsRingWithoutOne.distrib
d_distrib_2094 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2094 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2082 (coe v0)
-- Data.Nat.Properties._.IsRingWithoutOne.isEquivalence
d_isEquivalence_2100 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2100 v0
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
-- Data.Nat.Properties._.IsRingWithoutOne.zero
d_zero_2118 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2118 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2084 (coe v0)
-- Data.Nat.Properties._.IsSelectiveMagma.isEquivalence
d_isEquivalence_2126 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2126 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0))
-- Data.Nat.Properties._.IsSelectiveMagma.isMagma
d_isMagma_2128 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2128 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0)
-- Data.Nat.Properties._.IsSelectiveMagma.sel
d_sel_2136 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_2136 v0
  = coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0)
-- Data.Nat.Properties._.IsSelectiveMagma.∙-cong
d_'8729''45'cong_2144 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2144 = erased
-- Data.Nat.Properties._.IsSemigroup.assoc
d_assoc_2152 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2152 = erased
-- Data.Nat.Properties._.IsSemigroup.isEquivalence
d_isEquivalence_2154 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2154 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0))
-- Data.Nat.Properties._.IsSemigroup.isMagma
d_isMagma_2156 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2156 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0)
-- Data.Nat.Properties._.IsSemigroup.∙-cong
d_'8729''45'cong_2170 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2170 = erased
-- Data.Nat.Properties._.IsSemimedialMagma.isEquivalence
d_isEquivalence_2178 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2178 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0))
-- Data.Nat.Properties._.IsSemimedialMagma.isMagma
d_isMagma_2180 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2180 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0)
-- Data.Nat.Properties._.IsSemimedialMagma.semiMedial
d_semiMedial_2188 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_semiMedial_2188 v0
  = coe MAlonzo.Code.Algebra.Structures.d_semiMedial_370 (coe v0)
-- Data.Nat.Properties._.IsSemimedialMagma.∙-cong
d_'8729''45'cong_2200 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2200 = erased
-- Data.Nat.Properties._.IsSemiring.*-assoc
d_'42''45'assoc_2208 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2208 = erased
-- Data.Nat.Properties._.IsSemiring.*-cong
d_'42''45'cong_2210 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2210 = erased
-- Data.Nat.Properties._.IsSemiring.*-identity
d_'42''45'identity_2216 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2216 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Nat.Properties._.IsSemiring.assoc
d_assoc_2228 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2228 = erased
-- Data.Nat.Properties._.IsSemiring.comm
d_comm_2230 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2230 = erased
-- Data.Nat.Properties._.IsSemiring.∙-cong
d_'8729''45'cong_2232 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2232 = erased
-- Data.Nat.Properties._.IsSemiring.identity
d_identity_2238 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2238 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v0))))
-- Data.Nat.Properties._.IsSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2246 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2246 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Nat.Properties._.IsSemiring.isMagma
d_isMagma_2250 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2250 v0
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
-- Data.Nat.Properties._.IsSemiring.isMonoid
d_isMonoid_2252 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2252 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe v0)))
-- Data.Nat.Properties._.IsSemiring.isSemigroup
d_isSemigroup_2254 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v0))))
-- Data.Nat.Properties._.IsSemiring.distrib
d_distrib_2258 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2258 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Nat.Properties._.IsSemiring.isEquivalence
d_isEquivalence_2264 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2264 v0
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
-- Data.Nat.Properties._.IsSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2270 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2270 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe v0)
-- Data.Nat.Properties._.IsSemiring.zero
d_zero_2284 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2284 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1388 (coe v0)
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.*-assoc
d_'42''45'assoc_2292 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2292 = erased
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.*-cong
d_'42''45'cong_2294 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2294 = erased
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.*-identity
d_'42''45'identity_2300 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2300 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296 (coe v0)
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.assoc
d_assoc_2312 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2312 = erased
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.comm
d_comm_2314 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2314 = erased
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.∙-cong
d_'8729''45'cong_2316 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2316 = erased
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.identity
d_identity_2322 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2322 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe v0)))
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2330 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2330 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe v0)
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.isMagma
d_isMagma_2334 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2334 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe v0))))
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.isMonoid
d_isMonoid_2336 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2336 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v0))
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.isSemigroup
d_isSemigroup_2338 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2338 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe v0)))
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.distrib
d_distrib_2342 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2342 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1298 (coe v0)
-- Data.Nat.Properties._.IsSemiringWithoutAnnihilatingZero.isEquivalence
d_isEquivalence_2348 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2348 v0
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
-- Data.Nat.Properties._.IsSemiringWithoutOne.*-assoc
d_'42''45'assoc_2364 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2364 = erased
-- Data.Nat.Properties._.IsSemiringWithoutOne.*-cong
d_'42''45'cong_2366 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2366 = erased
-- Data.Nat.Properties._.IsSemiringWithoutOne.comm
d_comm_2376 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2376 = erased
-- Data.Nat.Properties._.IsSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2380 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2380 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe v0)
-- Data.Nat.Properties._.IsSemiringWithoutOne.isMonoid
d_isMonoid_2384 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2384 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe v0))
-- Data.Nat.Properties._.IsSemiringWithoutOne.distrib
d_distrib_2386 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2386 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1166 (coe v0)
-- Data.Nat.Properties._.IsSemiringWithoutOne.isEquivalence
d_isEquivalence_2388 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2388 v0
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
-- Data.Nat.Properties._.IsSemiringWithoutOne.zero
d_zero_2392 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2392 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1168 (coe v0)
-- Data.Nat.Properties._.IsUnitalMagma.identity
d_identity_2400 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2400 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_568 (coe v0)
-- Data.Nat.Properties._.IsUnitalMagma.isEquivalence
d_isEquivalence_2406 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2406 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0))
-- Data.Nat.Properties._.IsUnitalMagma.isMagma
d_isMagma_2408 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2408 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0)
-- Data.Nat.Properties._.IsUnitalMagma.∙-cong
d_'8729''45'cong_2422 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2422 = erased
-- Data.Nat.Properties.nonZero?
d_nonZero'63'_2428 ::
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_nonZero'63'_2428 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe
                   MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
                   (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
-- Data.Nat.Properties.suc-injective
d_suc'45'injective_2436 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_suc'45'injective_2436 = erased
-- Data.Nat.Properties.≡ᵇ⇒≡
d_'8801''7495''8658''8801'_2442 ::
  Integer ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8801''7495''8658''8801'_2442 = erased
-- Data.Nat.Properties.≡⇒≡ᵇ
d_'8801''8658''8801''7495'_2454 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_'8801''8658''8801''7495'_2454 v0 ~v1 ~v2
  = du_'8801''8658''8801''7495'_2454 v0
du_'8801''8658''8801''7495'_2454 :: Integer -> AgdaAny
du_'8801''8658''8801''7495'_2454 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe du_'8801''8658''8801''7495'_2454 (coe v1)
-- Data.Nat.Properties._≟_
d__'8799'__2464 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__2464 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      erased
      (coe
         MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
         (coe eqInt (coe v0) (coe v1)))
-- Data.Nat.Properties.≡-irrelevant
d_'8801''45'irrelevant_2470 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8801''45'irrelevant_2470 = erased
-- Data.Nat.Properties.≟-diag
d_'8799''45'diag_2478 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8799''45'diag_2478 = erased
-- Data.Nat.Properties.≡-isDecEquivalence
d_'8801''45'isDecEquivalence_2480 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_'8801''45'isDecEquivalence_2480
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecEquivalence'46'constructor_2293
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (coe d__'8799'__2464)
-- Data.Nat.Properties.≡-decSetoid
d_'8801''45'decSetoid_2482 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8801''45'decSetoid_2482
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecSetoid'46'constructor_1373
      d_'8801''45'isDecEquivalence_2480
-- Data.Nat.Properties.0≢1+n
d_0'8802'1'43'n_2486 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_0'8802'1'43'n_2486 = erased
-- Data.Nat.Properties.1+n≢0
d_1'43'n'8802'0_2490 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_1'43'n'8802'0_2490 = erased
-- Data.Nat.Properties.1+n≢n
d_1'43'n'8802'n_2494 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_1'43'n'8802'n_2494 = erased
-- Data.Nat.Properties.<ᵇ⇒<
d_'60''7495''8658''60'_2502 ::
  Integer ->
  Integer -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''7495''8658''60'_2502 v0 ~v1 ~v2
  = du_'60''7495''8658''60'_2502 v0
du_'60''7495''8658''60'_2502 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''7495''8658''60'_2502 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_'60''7495''8658''60'_2502 (coe v1))
-- Data.Nat.Properties.<⇒<ᵇ
d_'60''8658''60''7495'_2518 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny
d_'60''8658''60''7495'_2518 ~v0 ~v1 v2
  = du_'60''8658''60''7495'_2518 v2
du_'60''8658''60''7495'_2518 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny
du_'60''8658''60''7495'_2518 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3
        -> case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> coe
                    du_'60''8658''60''7495'_2518
                    (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.<ᵇ-reflects-<
d_'60''7495''45'reflects'45''60'_2526 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Reflects.T_Reflects_18
d_'60''7495''45'reflects'45''60'_2526 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Reflects.du_fromEquivalence_110
      (coe ltInt (coe v0) (coe v1))
      (\ v2 -> coe du_'60''7495''8658''60'_2502 (coe v0))
-- Data.Nat.Properties.≤ᵇ⇒≤
d_'8804''7495''8658''8804'_2536 ::
  Integer ->
  Integer -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''7495''8658''8804'_2536 v0 ~v1 ~v2
  = du_'8804''7495''8658''8804'_2536 v0
du_'8804''7495''8658''8804'_2536 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''7495''8658''8804'_2536 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe du_'60''7495''8658''60'_2502 (coe v1)
-- Data.Nat.Properties.≤⇒≤ᵇ
d_'8804''8658''8804''7495'_2552 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny
d_'8804''8658''8804''7495'_2552 ~v0 ~v1 v2
  = du_'8804''8658''8804''7495'_2552 v2
du_'8804''8658''8804''7495'_2552 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny
du_'8804''8658''8804''7495'_2552 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3
        -> coe
             du_'60''8658''60''7495'_2518
             (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.≤ᵇ-reflects-≤
d_'8804''7495''45'reflects'45''8804'_2560 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Reflects.T_Reflects_18
d_'8804''7495''45'reflects'45''8804'_2560 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Reflects.du_fromEquivalence_110
      (coe
         MAlonzo.Code.Data.Nat.Base.d__'8804''7495'__10 (coe v0) (coe v1))
      (\ v2 -> coe du_'8804''7495''8658''8804'_2536 (coe v0))
-- Data.Nat.Properties.≤-reflexive
d_'8804''45'reflexive_2566 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'reflexive_2566 v0 ~v1 ~v2
  = du_'8804''45'reflexive_2566 v0
du_'8804''45'reflexive_2566 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'reflexive_2566 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_'8804''45'reflexive_2566 (coe v1))
-- Data.Nat.Properties.≤-refl
d_'8804''45'refl_2570 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'refl_2570 v0 = coe du_'8804''45'reflexive_2566 (coe v0)
-- Data.Nat.Properties.≤-antisym
d_'8804''45'antisym_2572 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'antisym_2572 = erased
-- Data.Nat.Properties.≤-trans
d_'8804''45'trans_2578 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'trans_2578 ~v0 ~v1 ~v2 v3 v4
  = du_'8804''45'trans_2578 v3 v4
du_'8804''45'trans_2578 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'trans_2578 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_'8804''45'trans_2578 (coe v4) (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.≤-total
d_'8804''45'total_2584 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8804''45'total_2584 v0 v1
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  let v4 = d_'8804''45'total_2584 (coe v2) (coe v3) in
                  case coe v4 of
                    MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
                      -> coe
                           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
                    MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5
                      -> coe
                           MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
                    _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.≤-irrelevant
d_'8804''45'irrelevant_2606 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'irrelevant_2606 = erased
-- Data.Nat.Properties._≤?_
d__'8804''63'__2612 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__2612 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (\ v2 -> coe du_'8804''7495''8658''8804'_2536 (coe v0))
      (coe
         MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
         (coe
            MAlonzo.Code.Data.Nat.Base.d__'8804''7495'__10 (coe v0) (coe v1)))
-- Data.Nat.Properties._≥?_
d__'8805''63'__2618 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8805''63'__2618 v0 v1
  = coe d__'8804''63'__2612 (coe v1) (coe v0)
-- Data.Nat.Properties.≤-isPreorder
d_'8804''45'isPreorder_2620 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8804''45'isPreorder_2620
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'8804''45'reflexive_2566 v0)
      (\ v0 v1 v2 v3 v4 -> coe du_'8804''45'trans_2578 v3 v4)
-- Data.Nat.Properties.≤-isTotalPreorder
d_'8804''45'isTotalPreorder_2622 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_'8804''45'isTotalPreorder_2622
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalPreorder'46'constructor_7157
      (coe d_'8804''45'isPreorder_2620) (coe d_'8804''45'total_2584)
-- Data.Nat.Properties.≤-isPartialOrder
d_'8804''45'isPartialOrder_2624 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8804''45'isPartialOrder_2624
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe d_'8804''45'isPreorder_2620) erased
-- Data.Nat.Properties.≤-isTotalOrder
d_'8804''45'isTotalOrder_2626 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_'8804''45'isTotalOrder_2626
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe d_'8804''45'isPartialOrder_2624) (coe d_'8804''45'total_2584)
-- Data.Nat.Properties.≤-isDecTotalOrder
d_'8804''45'isDecTotalOrder_2628 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_'8804''45'isDecTotalOrder_2628
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe d_'8804''45'isTotalOrder_2626) (coe d__'8799'__2464)
      (coe d__'8804''63'__2612)
-- Data.Nat.Properties.≤-preorder
d_'8804''45'preorder_2630 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8804''45'preorder_2630
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      d_'8804''45'isPreorder_2620
-- Data.Nat.Properties.≤-totalPreorder
d_'8804''45'totalPreorder_2632 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
d_'8804''45'totalPreorder_2632
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalPreorder'46'constructor_3645
      d_'8804''45'isTotalPreorder_2622
-- Data.Nat.Properties.≤-poset
d_'8804''45'poset_2634 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8804''45'poset_2634
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      d_'8804''45'isPartialOrder_2624
-- Data.Nat.Properties.≤-totalOrder
d_'8804''45'totalOrder_2636 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_'8804''45'totalOrder_2636
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      d_'8804''45'isTotalOrder_2626
-- Data.Nat.Properties.≤-decTotalOrder
d_'8804''45'decTotalOrder_2638 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_'8804''45'decTotalOrder_2638
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      d_'8804''45'isDecTotalOrder_2628
-- Data.Nat.Properties.s≤s-injective
d_s'8804's'45'injective_2648 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_s'8804's'45'injective_2648 = erased
-- Data.Nat.Properties.≤-pred
d_'8804''45'pred_2654 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'pred_2654 ~v0 ~v1 v2 = du_'8804''45'pred_2654 v2
du_'8804''45'pred_2654 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'pred_2654 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m≤n⇒m≤1+n
d_m'8804'n'8658'm'8804'1'43'n_2662 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'8658'm'8804'1'43'n_2662 ~v0 ~v1 v2
  = du_m'8804'n'8658'm'8804'1'43'n_2662 v2
du_m'8804'n'8658'm'8804'1'43'n_2662 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8804'n'8658'm'8804'1'43'n_2662 v0 = coe v0
-- Data.Nat.Properties.n≤1+n
d_n'8804'1'43'n_2668 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_n'8804'1'43'n_2668 v0 = coe d_'8804''45'refl_2570 (coe v0)
-- Data.Nat.Properties.1+n≰n
d_1'43'n'8816'n_2672 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_1'43'n'8816'n_2672 = erased
-- Data.Nat.Properties.n≤0⇒n≡0
d_n'8804'0'8658'n'8801'0_2678 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'8804'0'8658'n'8801'0_2678 = erased
-- Data.Nat.Properties.n≤1⇒n≡0∨n≡1
d_n'8804'1'8658'n'8801'0'8744'n'8801'1_2682 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_n'8804'1'8658'n'8801'0'8744'n'8801'1_2682 ~v0 v1
  = du_n'8804'1'8658'n'8801'0'8744'n'8801'1_2682 v1
du_n'8804'1'8658'n'8801'0'8744'n'8801'1_2682 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_n'8804'1'8658'n'8801'0'8744'n'8801'1_2682 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3
        -> coe
             seq (coe v3) (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.<⇒≤
d_'60''8658''8804'_2684 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''8658''8804'_2684 ~v0 ~v1 v2 = du_'60''8658''8804'_2684 v2
du_'60''8658''8804'_2684 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''8658''8804'_2684 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3
        -> case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       du_'60''8658''8804'_2684
                       (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.<⇒≢
d_'60''8658''8802'_2688 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8802'_2688 = erased
-- Data.Nat.Properties.>⇒≢
d_'62''8658''8802'_2692 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'62''8658''8802'_2692 = erased
-- Data.Nat.Properties.≤⇒≯
d_'8804''8658''8815'_2694 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8804''8658''8815'_2694 = erased
-- Data.Nat.Properties.<⇒≱
d_'60''8658''8817'_2700 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8817'_2700 = erased
-- Data.Nat.Properties.<⇒≯
d_'60''8658''8815'_2706 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8815'_2706 = erased
-- Data.Nat.Properties.≰⇒≮
d_'8816''8658''8814'_2712 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8816''8658''8814'_2712 = erased
-- Data.Nat.Properties.≰⇒>
d_'8816''8658''62'_2718 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8816''8658''62'_2718 v0 v1 ~v2 = du_'8816''8658''62'_2718 v0 v1
du_'8816''8658''62'_2718 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8816''8658''62'_2718 v0 v1
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_'8816''8658''62'_2718 (coe v2) (coe v3))
-- Data.Nat.Properties.≰⇒≥
d_'8816''8658''8805'_2730 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8816''8658''8805'_2730 v0 v1 ~v2
  = du_'8816''8658''8805'_2730 v0 v1
du_'8816''8658''8805'_2730 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8816''8658''8805'_2730 v0 v1
  = coe
      du_'60''8658''8804'_2684
      (coe du_'8816''8658''62'_2718 (coe v0) (coe v1))
-- Data.Nat.Properties.≮⇒≥
d_'8814''8658''8805'_2732 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8814''8658''8805'_2732 v0 v1 ~v2
  = du_'8814''8658''8805'_2732 v0 v1
du_'8814''8658''8805'_2732 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8814''8658''8805'_2732 v0 v1
  = case coe v1 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           case coe v0 of
             0 -> coe
                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
             _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_'8814''8658''8805'_2732 (coe v3) (coe v2))
-- Data.Nat.Properties.≤∧≢⇒<
d_'8804''8743''8802''8658''60'_2748 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''8743''8802''8658''60'_2748 ~v0 v1 v2 ~v3
  = du_'8804''8743''8802''8658''60'_2748 v1 v2
du_'8804''8743''8802''8658''60'_2748 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''8743''8802''8658''60'_2748 v0 v1
  = case coe v0 of
      0 -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_'8804''8743''8802''8658''60'_2748 (coe v2) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.≤∧≮⇒≡
d_'8804''8743''8814''8658''8801'_2766 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''8743''8814''8658''8801'_2766 = erased
-- Data.Nat.Properties.≤-<-connex
d_'8804''45''60''45'connex_2772 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8804''45''60''45'connex_2772 v0 v1
  = let v2 = d__'8804''63'__2612 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v3 v4
        -> if coe v3
             then case coe v4 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v5
                      -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v5)
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v4)
                    (coe
                       MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                       (coe du_'8816''8658''62'_2718 (coe v0) (coe v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.≥->-connex
d_'8805''45''62''45'connex_2794 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8805''45''62''45'connex_2794 v0 v1
  = coe d_'8804''45''60''45'connex_2772 (coe v1) (coe v0)
-- Data.Nat.Properties.<-≤-connex
d_'60''45''8804''45'connex_2796 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'60''45''8804''45'connex_2796
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_flip'45'Connex_854
      (coe d_'8804''45''60''45'connex_2772)
-- Data.Nat.Properties.>-≥-connex
d_'62''45''8805''45'connex_2798 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'62''45''8805''45'connex_2798
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_flip'45'Connex_854
      (coe d_'8805''45''62''45'connex_2794)
-- Data.Nat.Properties.<-irrefl
d_'60''45'irrefl_2800 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'irrefl_2800 = erased
-- Data.Nat.Properties.<-asym
d_'60''45'asym_2804 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'asym_2804 = erased
-- Data.Nat.Properties.<-trans
d_'60''45'trans_2810 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'trans_2810 ~v0 v1 ~v2 v3 v4
  = du_'60''45'trans_2810 v1 v3 v4
du_'60''45'trans_2810 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''45'trans_2810 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
        -> let v6 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v2 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       du_'8804''45'trans_2578 (coe v5)
                       (coe
                          du_'8804''45'trans_2578 (coe d_n'8804'1'43'n_2668 (coe v6))
                          (coe v9)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.<-transʳ
d_'60''45'trans'691'_2816 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'trans'691'_2816 ~v0 ~v1 ~v2 v3 v4
  = du_'60''45'trans'691'_2816 v3 v4
du_'60''45'trans'691'_2816 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''45'trans'691'_2816 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_'8804''45'trans_2578 (coe v0) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.<-transˡ
d_'60''45'trans'737'_2822 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'trans'737'_2822 ~v0 ~v1 ~v2 v3 v4
  = du_'60''45'trans'737'_2822 v3 v4
du_'60''45'trans'737'_2822 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''45'trans'737'_2822 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_'8804''45'trans_2578 (coe v4) (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.<-cmp
d_'60''45'cmp_2828 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_'60''45'cmp_2828 v0 v1
  = let v2 = d__'8799'__2464 (coe v0) (coe v1) in
    let v3
          = MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
              (coe ltInt (coe v0) (coe v1)) in
    case coe v2 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v4 v5
        -> if coe v4
             then case coe v5 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v6
                      -> coe MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v6
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v5)
                    (case coe v3 of
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v6 v7
                         -> if coe v6
                              then coe
                                     seq (coe v7)
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
                                        (coe du_'60''7495''8658''60'_2502 (coe v0)))
                              else coe
                                     seq (coe v7)
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
                                        (coe
                                           du_'8804''8743''8802''8658''60'_2748 (coe v0)
                                           (coe du_'8814''8658''8805'_2732 (coe v0) (coe v1))))
                       _ -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties._<?_
d__'60''63'__2860 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__2860 v0 v1
  = coe
      d__'8804''63'__2612 (coe addInt (coe (1 :: Integer)) (coe v0))
      (coe v1)
-- Data.Nat.Properties._>?_
d__'62''63'__2866 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'62''63'__2866 v0 v1 = coe d__'60''63'__2860 (coe v1) (coe v0)
-- Data.Nat.Properties.<-irrelevant
d_'60''45'irrelevant_2868 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''45'irrelevant_2868 = erased
-- Data.Nat.Properties.<-resp₂-≡
d_'60''45'resp'8322''45''8801'_2870 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'8322''45''8801'_2870
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe (\ v0 v1 v2 v3 v4 -> v4)) (coe (\ v0 v1 v2 v3 v4 -> v4))
-- Data.Nat.Properties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_2876 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_2876
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45'trans_2810 v1 v3 v4)
      d_'60''45'resp'8322''45''8801'_2870
-- Data.Nat.Properties.<-isStrictTotalOrder
d_'60''45'isStrictTotalOrder_2878 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder_2878
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45'trans_2810 v1 v3 v4)
      (coe d_'60''45'cmp_2828)
-- Data.Nat.Properties.<-strictPartialOrder
d_'60''45'strictPartialOrder_2880 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_2880
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      d_'60''45'isStrictPartialOrder_2876
-- Data.Nat.Properties.<-strictTotalOrder
d_'60''45'strictTotalOrder_2882 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder_2882
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      d_'60''45'isStrictTotalOrder_2878
-- Data.Nat.Properties.s<s-injective
d_s'60's'45'injective_2892 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_s'60's'45'injective_2892 = erased
-- Data.Nat.Properties.<-pred
d_'60''45'pred_2898 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'pred_2898 ~v0 ~v1 v2 = du_'60''45'pred_2898 v2
du_'60''45'pred_2898 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''45'pred_2898 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m<n⇒m<1+n
d_m'60'n'8658'm'60'1'43'n_2906 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'm'60'1'43'n_2906 ~v0 ~v1 v2
  = du_m'60'n'8658'm'60'1'43'n_2906 v2
du_m'60'n'8658'm'60'1'43'n_2906 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'8658'm'60'1'43'n_2906 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3
        -> case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       du_m'60'n'8658'm'60'1'43'n_2906
                       (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.n≮0
d_n'8814'0_2912 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_n'8814'0_2912 = erased
-- Data.Nat.Properties.n≮n
d_n'8814'n_2916 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_n'8814'n_2916 = erased
-- Data.Nat.Properties.0<1+n
d_0'60'1'43'n_2922 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_0'60'1'43'n_2922 ~v0 = du_0'60'1'43'n_2922
du_0'60'1'43'n_2922 :: MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_0'60'1'43'n_2922
  = coe
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
      (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
-- Data.Nat.Properties.n<1+n
d_n'60'1'43'n_2926 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_n'60'1'43'n_2926 v0
  = coe
      d_'8804''45'refl_2570 (coe addInt (coe (1 :: Integer)) (coe v0))
-- Data.Nat.Properties.n<1⇒n≡0
d_n'60'1'8658'n'8801'0_2932 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'60'1'8658'n'8801'0_2932 = erased
-- Data.Nat.Properties.n>0⇒n≢0
d_n'62'0'8658'n'8802'0_2938 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_n'62'0'8658'n'8802'0_2938 = erased
-- Data.Nat.Properties.n≢0⇒n>0
d_n'8802'0'8658'n'62'0_2944 ::
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_n'8802'0'8658'n'62'0_2944 v0 ~v1
  = du_n'8802'0'8658'n'62'0_2944 v0
du_n'8802'0'8658'n'62'0_2944 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_n'8802'0'8658'n'62'0_2944 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      _ -> coe du_0'60'1'43'n_2922
-- Data.Nat.Properties.m<n⇒0<n
d_m'60'n'8658'0'60'n_2954 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'0'60'n_2954 ~v0 ~v1 = du_m'60'n'8658'0'60'n_2954
du_m'60'n'8658'0'60'n_2954 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'8658'0'60'n_2954
  = coe du_'8804''45'trans_2578 (coe du_0'60'1'43'n_2922)
-- Data.Nat.Properties.m<n⇒n≢0
d_m'60'n'8658'n'8802'0_2960 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'60'n'8658'n'8802'0_2960 = erased
-- Data.Nat.Properties.m<n⇒m≤1+n
d_m'60'n'8658'm'8804'1'43'n_2968 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'm'8804'1'43'n_2968 ~v0 ~v1 v2
  = du_m'60'n'8658'm'8804'1'43'n_2968 v2
du_m'60'n'8658'm'8804'1'43'n_2968 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'8658'm'8804'1'43'n_2968 v0
  = coe du_'60''8658''8804'_2684 (coe v0)
-- Data.Nat.Properties.m<1+n⇒m<n∨m≡n
d_m'60'1'43'n'8658'm'60'n'8744'm'8801'n_2974 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_m'60'1'43'n'8658'm'60'n'8744'm'8801'n_2974 v0 v1 v2
  = case coe v0 of
      0 -> case coe v1 of
             0 -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
             _ -> coe
                    MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe du_0'60'1'43'n_2922)
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v2 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> let v7 = subInt (coe v1) (coe (1 :: Integer)) in
                  let v8
                        = d_m'60'1'43'n'8658'm'60'n'8744'm'8801'n_2974
                            (coe v3) (coe v7) (coe v6) in
                  case coe v8 of
                    MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v9
                      -> coe
                           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9)
                    MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v9
                      -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m≤n⇒m<n∨m≡n
d_m'8804'n'8658'm'60'n'8744'm'8801'n_3008 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_m'8804'n'8658'm'60'n'8744'm'8801'n_3008 v0 v1 v2
  = coe
      d_m'60'1'43'n'8658'm'60'n'8744'm'8801'n_2974 (coe v0) (coe v1)
      (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v2)
-- Data.Nat.Properties.m<1+n⇒m≤n
d_m'60'1'43'n'8658'm'8804'n_3016 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'1'43'n'8658'm'8804'n_3016 ~v0 ~v1 v2
  = du_m'60'1'43'n'8658'm'8804'n_3016 v2
du_m'60'1'43'n'8658'm'8804'n_3016 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'1'43'n'8658'm'8804'n_3016 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.∀[m≤n⇒m≢o]⇒n<o
d_'8704''91'm'8804'n'8658'm'8802'o'93''8658'n'60'o_3026 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8704''91'm'8804'n'8658'm'8802'o'93''8658'n'60'o_3026 v0 v1 ~v2
  = du_'8704''91'm'8804'n'8658'm'8802'o'93''8658'n'60'o_3026 v0 v1
du_'8704''91'm'8804'n'8658'm'8802'o'93''8658'n'60'o_3026 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8704''91'm'8804'n'8658'm'8802'o'93''8658'n'60'o_3026 v0 v1
  = case coe v1 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           case coe v0 of
             0 -> coe du_0'60'1'43'n_2922
             _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       du_'8704''91'm'8804'n'8658'm'8802'o'93''8658'n'60'o_3026 (coe v3)
                       (coe v2))
-- Data.Nat.Properties._.rec
d_rec_3044 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_rec_3044 = erased
-- Data.Nat.Properties.∀[m<n⇒m≢o]⇒n≤o
d_'8704''91'm'60'n'8658'm'8802'o'93''8658'n'8804'o_3054 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8704''91'm'60'n'8658'm'8802'o'93''8658'n'8804'o_3054 v0 v1 ~v2
  = du_'8704''91'm'60'n'8658'm'8802'o'93''8658'n'8804'o_3054 v0 v1
du_'8704''91'm'60'n'8658'm'8802'o'93''8658'n'8804'o_3054 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8704''91'm'60'n'8658'm'8802'o'93''8658'n'8804'o_3054 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       du_'8704''91'm'60'n'8658'm'8802'o'93''8658'n'8804'o_3054 (coe v2)
                       (coe v3))
-- Data.Nat.Properties._.rec
d_rec_3074 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_rec_3074 = erased
-- Data.Nat.Properties.≤-Reasoning._._IsRelatedTo_
d__IsRelatedTo__3082 a0 a1 = ()
-- Data.Nat.Properties.≤-Reasoning._._∎
d__'8718'_3084 ::
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d__'8718'_3084
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
      (coe d_'8804''45'isPreorder_2620)
-- Data.Nat.Properties.≤-Reasoning._._≡⟨⟩_
d__'8801''10216''10217'__3086 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d__'8801''10216''10217'__3086 v0 = coe v0
-- Data.Nat.Properties.≤-Reasoning._.IsEquality
d_IsEquality_3088 a0 a1 a2 = ()
-- Data.Nat.Properties.≤-Reasoning._.IsEquality?
d_IsEquality'63'_3090 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsEquality'63'_3090 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_IsEquality'63'_142
      v2
-- Data.Nat.Properties.≤-Reasoning._.IsStrict
d_IsStrict_3092 a0 a1 a2 = ()
-- Data.Nat.Properties.≤-Reasoning._.IsStrict?
d_IsStrict'63'_3094 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsStrict'63'_3094 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_IsStrict'63'_108
      v2
-- Data.Nat.Properties.≤-Reasoning._.begin_
d_begin__3096 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_begin__3096
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_2620)
      (\ v0 v1 v2 -> coe du_'60''8658''8804'_2684 v2)
-- Data.Nat.Properties.≤-Reasoning._.begin-equality_
d_begin'45'equality__3098 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_begin'45'equality__3098 = erased
-- Data.Nat.Properties.≤-Reasoning._.begin-strict_
d_begin'45'strict__3100 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_begin'45'strict__3100 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      v2
-- Data.Nat.Properties.≤-Reasoning._.extractEquality
d_extractEquality_3104 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T_IsEquality_126 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_extractEquality_3104 = erased
-- Data.Nat.Properties.≤-Reasoning._.extractStrict
d_extractStrict_3106 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T_IsStrict_92 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_extractStrict_3106 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_extractStrict_118
      v2 v3
-- Data.Nat.Properties.≤-Reasoning._.step-<
d_step'45''60'_3114 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''60'_3114
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45'trans_2810 v1 v3 v4)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45'trans'737'_2822 v3 v4)
-- Data.Nat.Properties.≤-Reasoning._.step-≡
d_step'45''8801'_3116 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8801'_3116 ~v0 ~v1 ~v2 v3 ~v4
  = du_step'45''8801'_3116 v3
du_step'45''8801'_3116 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
du_step'45''8801'_3116 v0 = coe v0
-- Data.Nat.Properties.≤-Reasoning._.step-≡˘
d_step'45''8801''728'_3118 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8801''728'_3118 ~v0 ~v1 ~v2 v3 ~v4
  = du_step'45''8801''728'_3118 v3
du_step'45''8801''728'_3118 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
du_step'45''8801''728'_3118 v0 = coe v0
-- Data.Nat.Properties.≤-Reasoning._.step-≤
d_step'45''8804'_3120 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8804'_3120
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
      (coe d_'8804''45'isPreorder_2620)
      (\ v0 v1 v2 v3 v4 -> coe du_'60''45'trans'691'_2816 v3 v4)
-- Data.Nat.Properties.+-suc
d_'43''45'suc_3144 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'suc_3144 = erased
-- Data.Nat.Properties.+-assoc
d_'43''45'assoc_3152 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'assoc_3152 = erased
-- Data.Nat.Properties.+-identityˡ
d_'43''45'identity'737'_3160 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'737'_3160 = erased
-- Data.Nat.Properties.+-identityʳ
d_'43''45'identity'691'_3162 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'691'_3162 = erased
-- Data.Nat.Properties.+-identity
d_'43''45'identity_3166 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'identity_3166
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.+-comm
d_'43''45'comm_3168 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'comm_3168 = erased
-- Data.Nat.Properties.+-cancelˡ-≡
d_'43''45'cancel'737''45''8801'_3176 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'cancel'737''45''8801'_3176 = erased
-- Data.Nat.Properties.+-cancelʳ-≡
d_'43''45'cancel'691''45''8801'_3184 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'cancel'691''45''8801'_3184 = erased
-- Data.Nat.Properties.+-cancel-≡
d_'43''45'cancel'45''8801'_3186 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'cancel'45''8801'_3186
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.+-isMagma
d_'43''45'isMagma_3188 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'43''45'isMagma_3188
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.Nat.Properties.+-isSemigroup
d_'43''45'isSemigroup_3190 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'43''45'isSemigroup_3190
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'43''45'isMagma_3188) erased
-- Data.Nat.Properties.+-isCommutativeSemigroup
d_'43''45'isCommutativeSemigroup_3192 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'43''45'isCommutativeSemigroup_3192
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemigroup'46'constructor_10975
      (coe d_'43''45'isSemigroup_3190) erased
-- Data.Nat.Properties.+-0-isMonoid
d_'43''45'0'45'isMonoid_3194 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'0'45'isMonoid_3194
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'43''45'isSemigroup_3190) (coe d_'43''45'identity_3166)
-- Data.Nat.Properties.+-0-isCommutativeMonoid
d_'43''45'0'45'isCommutativeMonoid_3196 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'0'45'isCommutativeMonoid_3196
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'43''45'0'45'isMonoid_3194) erased
-- Data.Nat.Properties.+-magma
d_'43''45'magma_3198 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'43''45'magma_3198
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187 addInt
      d_'43''45'isMagma_3188
-- Data.Nat.Properties.+-semigroup
d_'43''45'semigroup_3200 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'43''45'semigroup_3200
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557 addInt
      d_'43''45'isSemigroup_3190
-- Data.Nat.Properties.+-commutativeSemigroup
d_'43''45'commutativeSemigroup_3202 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'43''45'commutativeSemigroup_3202
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemigroup'46'constructor_10763
      addInt d_'43''45'isCommutativeSemigroup_3192
-- Data.Nat.Properties.+-0-monoid
d_'43''45'0'45'monoid_3204 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'43''45'0'45'monoid_3204
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309 addInt
      (0 :: Integer) d_'43''45'0'45'isMonoid_3194
-- Data.Nat.Properties.+-0-commutativeMonoid
d_'43''45'0'45'commutativeMonoid_3206 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'0'45'commutativeMonoid_3206
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      addInt (0 :: Integer) d_'43''45'0'45'isCommutativeMonoid_3196
-- Data.Nat.Properties.∸-magma
d_'8760''45'magma_3208 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8760''45'magma_3208
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Algebra.du_magma_20
      (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22)
-- Data.Nat.Properties.m≢1+m+n
d_m'8802'1'43'm'43'n_3214 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'8802'1'43'm'43'n_3214 = erased
-- Data.Nat.Properties.m≢1+n+m
d_m'8802'1'43'n'43'm_3224 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'8802'1'43'n'43'm_3224 = erased
-- Data.Nat.Properties.m+1+n≢m
d_m'43'1'43'n'8802'm_3234 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'43'1'43'n'8802'm_3234 = erased
-- Data.Nat.Properties.m+1+n≢n
d_m'43'1'43'n'8802'n_3242 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'43'1'43'n'8802'n_3242 = erased
-- Data.Nat.Properties.m+1+n≢0
d_m'43'1'43'n'8802'0_3256 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'43'1'43'n'8802'0_3256 = erased
-- Data.Nat.Properties.m+n≡0⇒m≡0
d_m'43'n'8801'0'8658'm'8801'0_3270 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'43'n'8801'0'8658'm'8801'0_3270 = erased
-- Data.Nat.Properties.m+n≡0⇒n≡0
d_m'43'n'8801'0'8658'n'8801'0_3278 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'43'n'8801'0'8658'n'8801'0_3278 = erased
-- Data.Nat.Properties.+-cancelˡ-≤
d_'43''45'cancel'737''45''8804'_3286 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'cancel'737''45''8804'_3286 v0 ~v1 ~v2 v3
  = du_'43''45'cancel'737''45''8804'_3286 v0 v3
du_'43''45'cancel'737''45''8804'_3286 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'cancel'737''45''8804'_3286 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
               -> coe du_'43''45'cancel'737''45''8804'_3286 (coe v2) (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.+-cancelʳ-≤
d_'43''45'cancel'691''45''8804'_3294 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'cancel'691''45''8804'_3294 v0 ~v1 ~v2 v3
  = du_'43''45'cancel'691''45''8804'_3294 v0 v3
du_'43''45'cancel'691''45''8804'_3294 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'cancel'691''45''8804'_3294 v0 v1
  = coe du_'43''45'cancel'737''45''8804'_3286 (coe v0) (coe v1)
-- Data.Nat.Properties.+-cancel-≤
d_'43''45'cancel'45''8804'_3304 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'cancel'45''8804'_3304
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 v1 v2 v3 -> coe du_'43''45'cancel'737''45''8804'_3286 v0 v3)
      (\ v0 v1 v2 v3 -> coe du_'43''45'cancel'691''45''8804'_3294 v0 v3)
-- Data.Nat.Properties.+-cancelˡ-<
d_'43''45'cancel'737''45''60'_3306 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'cancel'737''45''60'_3306 v0 ~v1 ~v2 v3
  = du_'43''45'cancel'737''45''60'_3306 v0 v3
du_'43''45'cancel'737''45''60'_3306 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'cancel'737''45''60'_3306 v0 v1
  = coe du_'43''45'cancel'737''45''8804'_3286 (coe v0) (coe v1)
-- Data.Nat.Properties.+-cancelʳ-<
d_'43''45'cancel'691''45''60'_3316 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'cancel'691''45''60'_3316 v0 ~v1 ~v2 v3
  = du_'43''45'cancel'691''45''60'_3316 v0 v3
du_'43''45'cancel'691''45''60'_3316 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'cancel'691''45''60'_3316 v0 v1
  = coe du_'43''45'cancel'691''45''8804'_3294 (coe v0) (coe v1)
-- Data.Nat.Properties.+-cancel-<
d_'43''45'cancel'45''60'_3326 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'cancel'45''60'_3326
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 v1 v2 v3 -> coe du_'43''45'cancel'737''45''60'_3306 v0 v3)
      (\ v0 v1 v2 v3 -> coe du_'43''45'cancel'691''45''60'_3316 v0 v3)
-- Data.Nat.Properties.m≤n⇒m≤o+n
d_m'8804'n'8658'm'8804'o'43'n_3334 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'8658'm'8804'o'43'n_3334 ~v0 ~v1 ~v2 v3
  = du_m'8804'n'8658'm'8804'o'43'n_3334 v3
du_m'8804'n'8658'm'8804'o'43'n_3334 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8804'n'8658'm'8804'o'43'n_3334 v0 = coe v0
-- Data.Nat.Properties.m≤n⇒m≤n+o
d_m'8804'n'8658'm'8804'n'43'o_3348 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'8658'm'8804'n'43'o_3348 ~v0 ~v1 ~v2 v3
  = du_m'8804'n'8658'm'8804'n'43'o_3348 v3
du_m'8804'n'8658'm'8804'n'43'o_3348 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8804'n'8658'm'8804'n'43'o_3348 v0 = coe v0
-- Data.Nat.Properties.m≤m+n
d_m'8804'm'43'n_3362 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'm'43'n_3362 v0 ~v1 = du_m'8804'm'43'n_3362 v0
du_m'8804'm'43'n_3362 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8804'm'43'n_3362 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_m'8804'm'43'n_3362 (coe v1))
-- Data.Nat.Properties.m≤n+m
d_m'8804'n'43'm_3374 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'43'm_3374 v0 ~v1 = du_m'8804'n'43'm_3374 v0
du_m'8804'n'43'm_3374 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8804'n'43'm_3374 v0 = coe du_m'8804'm'43'n_3362 (coe v0)
-- Data.Nat.Properties.m+n≤o⇒m≤o
d_m'43'n'8804'o'8658'm'8804'o_3388 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'43'n'8804'o'8658'm'8804'o_3388 v0 ~v1 ~v2 v3
  = du_m'43'n'8804'o'8658'm'8804'o_3388 v0 v3
du_m'43'n'8804'o'8658'm'8804'o_3388 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'43'n'8804'o'8658'm'8804'o_3388 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_m'43'n'8804'o'8658'm'8804'o_3388 (coe v2) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m+n≤o⇒n≤o
d_m'43'n'8804'o'8658'n'8804'o_3402 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'43'n'8804'o'8658'n'8804'o_3402 v0 ~v1 ~v2 v3
  = du_m'43'n'8804'o'8658'n'8804'o_3402 v0 v3
du_m'43'n'8804'o'8658'n'8804'o_3402 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'43'n'8804'o'8658'n'8804'o_3402 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             du_m'43'n'8804'o'8658'n'8804'o_3402 (coe v2)
             (coe du_'60''8658''8804'_2684 (coe v1))
-- Data.Nat.Properties.+-mono-≤
d_'43''45'mono'45''8804'_3410 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'mono'45''8804'_3410 ~v0 ~v1 ~v2 v3 v4 v5
  = du_'43''45'mono'45''8804'_3410 v3 v4 v5
du_'43''45'mono'45''8804'_3410 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'mono'45''8804'_3410 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe
             du_'8804''45'trans_2578 (coe v2)
             (coe du_m'8804'n'43'm_3374 (coe v0))
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_'43''45'mono'45''8804'_3410 (coe v0) (coe v5) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.+-monoˡ-≤
d_'43''45'mono'737''45''8804'_3424 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'mono'737''45''8804'_3424 v0 ~v1 ~v2 v3
  = du_'43''45'mono'737''45''8804'_3424 v0 v3
du_'43''45'mono'737''45''8804'_3424 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'mono'737''45''8804'_3424 v0 v1
  = coe
      du_'43''45'mono'45''8804'_3410 (coe v0) (coe v1)
      (coe d_'8804''45'refl_2570 (coe v0))
-- Data.Nat.Properties.+-monoʳ-≤
d_'43''45'mono'691''45''8804'_3434 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'mono'691''45''8804'_3434 v0 ~v1 v2 v3
  = du_'43''45'mono'691''45''8804'_3434 v0 v2 v3
du_'43''45'mono'691''45''8804'_3434 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'mono'691''45''8804'_3434 v0 v1 v2
  = coe
      du_'43''45'mono'45''8804'_3410 (coe v1)
      (coe d_'8804''45'refl_2570 (coe v0)) (coe v2)
-- Data.Nat.Properties.+-mono-<-≤
d_'43''45'mono'45''60''45''8804'_3440 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'mono'45''60''45''8804'_3440 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_'43''45'mono'45''60''45''8804'_3440 v4 v5
du_'43''45'mono'45''60''45''8804'_3440 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'mono'45''60''45''8804'_3440 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
        -> case coe v4 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v1
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       du_'43''45'mono'45''60''45''8804'_3440
                       (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7) (coe v1))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.+-mono-≤-<
d_'43''45'mono'45''8804''45''60'_3450 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'mono'45''8804''45''60'_3450 ~v0 ~v1 ~v2 v3 v4 v5
  = du_'43''45'mono'45''8804''45''60'_3450 v3 v4 v5
du_'43''45'mono'45''8804''45''60'_3450 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'mono'45''8804''45''60'_3450 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe
             du_'8804''45'trans_2578 (coe v2)
             (coe du_m'8804'n'43'm_3374 (coe v0))
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe
                du_'43''45'mono'45''8804''45''60'_3450 (coe v0) (coe v5) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.+-mono-<
d_'43''45'mono'45''60'_3460 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'mono'45''60'_3460 ~v0 ~v1 ~v2 v3 v4
  = du_'43''45'mono'45''60'_3460 v3 v4
du_'43''45'mono'45''60'_3460 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'mono'45''60'_3460 v0 v1
  = coe
      du_'43''45'mono'45''8804''45''60'_3450 (coe v0)
      (coe du_'60''8658''8804'_2684 (coe v1))
-- Data.Nat.Properties.+-monoˡ-<
d_'43''45'mono'737''45''60'_3468 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'mono'737''45''60'_3468 v0 ~v1 ~v2
  = du_'43''45'mono'737''45''60'_3468 v0
du_'43''45'mono'737''45''60'_3468 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'mono'737''45''60'_3468 v0
  = coe du_'43''45'mono'737''45''8804'_3424 (coe v0)
-- Data.Nat.Properties.+-monoʳ-<
d_'43''45'mono'691''45''60'_3476 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'43''45'mono'691''45''60'_3476 v0 ~v1 ~v2 v3
  = du_'43''45'mono'691''45''60'_3476 v0 v3
du_'43''45'mono'691''45''60'_3476 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'43''45'mono'691''45''60'_3476 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_'43''45'mono'691''45''60'_3476 (coe v2) (coe v1))
-- Data.Nat.Properties.m+1+n≰m
d_m'43'1'43'n'8816'm_3488 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'43'1'43'n'8816'm_3488 = erased
-- Data.Nat.Properties.m<m+n
d_m'60'm'43'n_3498 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'm'43'n_3498 v0 ~v1 v2 = du_m'60'm'43'n_3498 v0 v2
du_m'60'm'43'n_3498 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'm'43'n_3498 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_m'60'm'43'n_3498 (coe v2) (coe v1))
-- Data.Nat.Properties.m<n+m
d_m'60'n'43'm_3510 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'43'm_3510 v0 ~v1 v2 = du_m'60'n'43'm_3510 v0 v2
du_m'60'n'43'm_3510 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'43'm_3510 v0 v1
  = coe du_m'60'm'43'n_3498 (coe v0) (coe v1)
-- Data.Nat.Properties.m+n≮n
d_m'43'n'8814'n_3526 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'43'n'8814'n_3526 = erased
-- Data.Nat.Properties.m+n≮m
d_m'43'n'8814'm_3540 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'43'n'8814'm_3540 = erased
-- Data.Nat.Properties.*-suc
d_'42''45'suc_3552 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'suc_3552 = erased
-- Data.Nat.Properties.*-identityˡ
d_'42''45'identity'737'_3564 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'737'_3564 = erased
-- Data.Nat.Properties.*-identityʳ
d_'42''45'identity'691'_3568 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'691'_3568 = erased
-- Data.Nat.Properties.*-identity
d_'42''45'identity_3572 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_3572
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.*-zeroˡ
d_'42''45'zero'737'_3574 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'zero'737'_3574 = erased
-- Data.Nat.Properties.*-zeroʳ
d_'42''45'zero'691'_3576 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'zero'691'_3576 = erased
-- Data.Nat.Properties.*-zero
d_'42''45'zero_3580 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'zero_3580
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.*-comm
d_'42''45'comm_3582 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_3582 = erased
-- Data.Nat.Properties.*-distribʳ-+
d_'42''45'distrib'691''45''43'_3592 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''43'_3592 = erased
-- Data.Nat.Properties.*-distribˡ-+
d_'42''45'distrib'737''45''43'_3606 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''43'_3606 = erased
-- Data.Nat.Properties.*-distrib-+
d_'42''45'distrib'45''43'_3608 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'distrib'45''43'_3608
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.*-assoc
d_'42''45'assoc_3610 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_3610 = erased
-- Data.Nat.Properties.*-isMagma
d_'42''45'isMagma_3624 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_3624
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.Nat.Properties.*-isSemigroup
d_'42''45'isSemigroup_3626 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_3626
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'42''45'isMagma_3624) erased
-- Data.Nat.Properties.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_3628 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_3628
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemigroup'46'constructor_10975
      (coe d_'42''45'isSemigroup_3626) erased
-- Data.Nat.Properties.*-1-isMonoid
d_'42''45'1'45'isMonoid_3630 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'1'45'isMonoid_3630
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'42''45'isSemigroup_3626) (coe d_'42''45'identity_3572)
-- Data.Nat.Properties.*-1-isCommutativeMonoid
d_'42''45'1'45'isCommutativeMonoid_3632 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'1'45'isCommutativeMonoid_3632
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'42''45'1'45'isMonoid_3630) erased
-- Data.Nat.Properties.+-*-isSemiring
d_'43''45''42''45'isSemiring_3634 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_'43''45''42''45'isSemiring_3634
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
         (coe d_'43''45'0'45'isCommutativeMonoid_3196) erased erased
         (coe d_'42''45'identity_3572) (coe d_'42''45'distrib'45''43'_3608))
      (coe d_'42''45'zero_3580)
-- Data.Nat.Properties.+-*-isCommutativeSemiring
d_'43''45''42''45'isCommutativeSemiring_3636 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'43''45''42''45'isCommutativeSemiring_3636
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe d_'43''45''42''45'isSemiring_3634) erased
-- Data.Nat.Properties.*-magma
d_'42''45'magma_3638 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'42''45'magma_3638
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187 mulInt
      d_'42''45'isMagma_3624
-- Data.Nat.Properties.*-semigroup
d_'42''45'semigroup_3640 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'42''45'semigroup_3640
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557 mulInt
      d_'42''45'isSemigroup_3626
-- Data.Nat.Properties.*-commutativeSemigroup
d_'42''45'commutativeSemigroup_3642 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'42''45'commutativeSemigroup_3642
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemigroup'46'constructor_10763
      mulInt d_'42''45'isCommutativeSemigroup_3628
-- Data.Nat.Properties.*-1-monoid
d_'42''45'1'45'monoid_3644 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'1'45'monoid_3644
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309 mulInt
      (1 :: Integer) d_'42''45'1'45'isMonoid_3630
-- Data.Nat.Properties.*-1-commutativeMonoid
d_'42''45'1'45'commutativeMonoid_3646 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'1'45'commutativeMonoid_3646
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      mulInt (1 :: Integer) d_'42''45'1'45'isCommutativeMonoid_3632
-- Data.Nat.Properties.+-*-semiring
d_'43''45''42''45'semiring_3648 ::
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_'43''45''42''45'semiring_3648
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semiring'46'constructor_35691 addInt
      mulInt (0 :: Integer) (1 :: Integer)
      d_'43''45''42''45'isSemiring_3634
-- Data.Nat.Properties.+-*-commutativeSemiring
d_'43''45''42''45'commutativeSemiring_3650 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'43''45''42''45'commutativeSemiring_3650
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      addInt mulInt (0 :: Integer) (1 :: Integer)
      d_'43''45''42''45'isCommutativeSemiring_3636
-- Data.Nat.Properties.*-cancelʳ-≡
d_'42''45'cancel'691''45''8801'_3660 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'691''45''8801'_3660 = erased
-- Data.Nat.Properties.*-cancelˡ-≡
d_'42''45'cancel'737''45''8801'_3682 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'737''45''8801'_3682 = erased
-- Data.Nat.Properties.m*n≡0⇒m≡0∨n≡0
d_m'42'n'8801'0'8658'm'8801'0'8744'n'8801'0_3702 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_m'42'n'8801'0'8658'm'8801'0'8744'n'8801'0_3702 v0 ~v1 ~v2
  = du_m'42'n'8801'0'8658'm'8801'0'8744'n'8801'0_3702 v0
du_m'42'n'8801'0'8658'm'8801'0'8744'n'8801'0_3702 ::
  Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_m'42'n'8801'0'8658'm'8801'0'8744'n'8801'0_3702 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
      _ -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
-- Data.Nat.Properties.m*n≢0
d_m'42'n'8802'0_3720 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_m'42'n'8802'0_3720 ~v0 ~v1 ~v2 ~v3 = du_m'42'n'8802'0_3720
du_m'42'n'8802'0_3720 :: MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_m'42'n'8802'0_3720
  = coe
      MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Nat.Properties.m*n≡0⇒m≡0
d_m'42'n'8801'0'8658'm'8801'0_3732 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'8801'0'8658'm'8801'0_3732 = erased
-- Data.Nat.Properties.m*n≡1⇒m≡1
d_m'42'n'8801'1'8658'm'8801'1_3740 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'8801'1'8658'm'8801'1_3740 = erased
-- Data.Nat.Properties.m*n≡1⇒n≡1
d_m'42'n'8801'1'8658'n'8801'1_3754 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'8801'1'8658'n'8801'1_3754 = erased
-- Data.Nat.Properties.[m*n]*[o*p]≡[m*o]*[n*p]
d_'91'm'42'n'93''42''91'o'42'p'93''8801''91'm'42'o'93''42''91'n'42'p'93'_3770 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'm'42'n'93''42''91'o'42'p'93''8801''91'm'42'o'93''42''91'n'42'p'93'_3770
  = erased
-- Data.Nat.Properties.*-cancelʳ-≤
d_'42''45'cancel'691''45''8804'_3858 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'cancel'691''45''8804'_3858 v0 ~v1 ~v2 ~v3 ~v4
  = du_'42''45'cancel'691''45''8804'_3858 v0
du_'42''45'cancel'691''45''8804'_3858 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'cancel'691''45''8804'_3858 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_'42''45'cancel'691''45''8804'_3858 (coe v1))
-- Data.Nat.Properties.*-cancelˡ-≤
d_'42''45'cancel'737''45''8804'_3878 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'cancel'737''45''8804'_3878 v0 ~v1 ~v2 ~v3
  = du_'42''45'cancel'737''45''8804'_3878 v0
du_'42''45'cancel'737''45''8804'_3878 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'cancel'737''45''8804'_3878 v0 v1
  = coe du_'42''45'cancel'691''45''8804'_3858 (coe v0)
-- Data.Nat.Properties.*-mono-≤
d_'42''45'mono'45''8804'_3894 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'mono'45''8804'_3894 ~v0 v1 ~v2 v3 v4 v5
  = du_'42''45'mono'45''8804'_3894 v1 v3 v4 v5
du_'42''45'mono'45''8804'_3894 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'mono'45''8804'_3894 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
        -> let v7 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             du_'43''45'mono'45''8804'_3410 (coe mulInt (coe v7) (coe v1))
             (coe v3)
             (coe
                du_'42''45'mono'45''8804'_3894 (coe v7) (coe v1) (coe v6) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.*-monoˡ-≤
d_'42''45'mono'737''45''8804'_3904 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'mono'737''45''8804'_3904 v0 ~v1 v2 v3
  = du_'42''45'mono'737''45''8804'_3904 v0 v2 v3
du_'42''45'mono'737''45''8804'_3904 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'mono'737''45''8804'_3904 v0 v1 v2
  = coe
      du_'42''45'mono'45''8804'_3894 (coe v1) (coe v0) (coe v2)
      (coe d_'8804''45'refl_2570 (coe v0))
-- Data.Nat.Properties.*-monoʳ-≤
d_'42''45'mono'691''45''8804'_3914 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'mono'691''45''8804'_3914 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''8804'_3914 v0 v2 v3
du_'42''45'mono'691''45''8804'_3914 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'mono'691''45''8804'_3914 v0 v1 v2
  = coe
      du_'42''45'mono'45''8804'_3894 (coe v0) (coe v1)
      (coe d_'8804''45'refl_2570 (coe v0)) (coe v2)
-- Data.Nat.Properties.*-mono-<
d_'42''45'mono'45''60'_3920 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'mono'45''60'_3920 ~v0 v1 ~v2 v3 v4 v5
  = du_'42''45'mono'45''60'_3920 v1 v3 v4 v5
du_'42''45'mono'45''60'_3920 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'mono'45''60'_3920 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
        -> case coe v6 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe seq (coe v3) (coe du_0'60'1'43'n_2922)
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
               -> case coe v3 of
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v12
                      -> coe
                           du_'43''45'mono'45''60'_3460
                           (mulInt (coe subInt (coe v0) (coe (1 :: Integer))) (coe v1))
                           (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v12)
                           (coe
                              du_'42''45'mono'45''60'_3920
                              (coe subInt (coe v0) (coe (1 :: Integer))) (coe v1)
                              (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9)
                              (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v12))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.*-monoˡ-<
d_'42''45'mono'737''45''60'_3934 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'mono'737''45''60'_3934 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''60'_3934 v0 v2 v3 v4
du_'42''45'mono'737''45''60'_3934 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'mono'737''45''60'_3934 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
        -> case coe v6 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22 -> coe du_0'60'1'43'n_2922
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
               -> let v10 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    du_'43''45'mono'45''8804''45''60'_3450
                    (coe
                       MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                       (\ v11 v12 -> v12) (\ v11 -> mulInt (coe v11) (coe v0)) v10
                       (subInt (coe v2) (coe (1 :: Integer))))
                    (coe d_'8804''45'refl_2570 (coe v0))
                    (coe
                       du_'42''45'mono'737''45''60'_3934 (coe v0) (coe v10)
                       (coe subInt (coe v2) (coe (1 :: Integer)))
                       (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.*-monoʳ-<
d_'42''45'mono'691''45''60'_3948 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'mono'691''45''60'_3948 v0 ~v1 ~v2 v3 v4
  = du_'42''45'mono'691''45''60'_3948 v0 v3 v4
du_'42''45'mono'691''45''60'_3948 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'mono'691''45''60'_3948 v0 v1 v2
  = case coe v0 of
      1 -> case coe v2 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
               -> coe
                    du_'43''45'mono'45''8804'_3410 (coe (0 :: Integer))
                    (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> case coe v2 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
               -> coe
                    du_'43''45'mono'45''8804'_3410
                    (coe mulInt (coe subInt (coe v0) (coe (1 :: Integer))) (coe v1))
                    (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)
                    (coe
                       du_'60''8658''8804'_2684
                       (coe
                          du_'42''45'mono'691''45''60'_3948
                          (coe subInt (coe v0) (coe (1 :: Integer))) (coe v1)
                          (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5)))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m≤m*n
d_m'8804'm'42'n_3962 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'm'42'n_3962 v0 v1 ~v2 = du_m'8804'm'42'n_3962 v0 v1
du_m'8804'm'42'n_3962 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8804'm'42'n_3962 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_2620)
      (\ v2 v3 v4 -> coe du_'60''8658''8804'_2684 v4) (coe v0)
      (coe mulInt (coe v0) (coe v1))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_2620)
         (\ v2 v3 v4 v5 v6 -> coe du_'60''45'trans'691'_2816 v5 v6)
         (coe mulInt (coe v0) (coe (1 :: Integer)))
         (coe mulInt (coe v0) (coe v1)) (coe mulInt (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe d_'8804''45'isPreorder_2620) (coe mulInt (coe v0) (coe v1)))
         (coe
            du_'42''45'mono'691''45''8804'_3914 (coe v0) (coe v1)
            (coe du_0'60'1'43'n_2922)))
-- Data.Nat.Properties.m≤n*m
d_m'8804'n'42'm_3974 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'42'm_3974 v0 v1 ~v2 = du_m'8804'n'42'm_3974 v0 v1
du_m'8804'n'42'm_3974 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8804'n'42'm_3974 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_2620)
      (\ v2 v3 v4 -> coe du_'60''8658''8804'_2684 v4) (coe v0)
      (coe mulInt (coe v1) (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe d_'8804''45'isPreorder_2620)
         (\ v2 v3 v4 v5 v6 -> coe du_'60''45'trans'691'_2816 v5 v6) (coe v0)
         (coe mulInt (coe v0) (coe v1)) (coe mulInt (coe v1) (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe d_'8804''45'isPreorder_2620) (coe mulInt (coe v1) (coe v0)))
         (coe du_m'8804'm'42'n_3962 (coe v0) (coe v1)))
-- Data.Nat.Properties.m<m*n
d_m'60'm'42'n_3986 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'm'42'n_3986 v0 v1 ~v2 v3 = du_m'60'm'42'n_3986 v0 v1 v3
du_m'60'm'42'n_3986 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'm'42'n_3986 v0 v1 v2
  = let v3 = subInt (coe v0) (coe (1 :: Integer)) in
    case coe v2 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
        -> coe
             seq (coe v6)
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                   (\ v7 v8 v9 v10 v11 -> coe du_'60''45'trans_2810 v8 v10 v11)
                   (coe
                      MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                   (\ v7 v8 v9 v10 v11 -> coe du_'60''45'trans'737'_2822 v10 v11)
                   (coe v0) (coe addInt (coe v1) (coe v3))
                   (coe mulInt (coe v0) (coe v1))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                      (coe d_'8804''45'isPreorder_2620)
                      (\ v7 v8 v9 v10 v11 -> coe du_'60''45'trans'691'_2816 v10 v11)
                      (coe addInt (coe v1) (coe v3))
                      (coe addInt (coe v1) (coe mulInt (coe v3) (coe v1)))
                      (coe mulInt (coe v0) (coe v1))
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                         (coe d_'8804''45'isPreorder_2620) (coe mulInt (coe v0) (coe v1)))
                      (coe
                         du_'43''45'mono'691''45''8804'_3434 (coe v1)
                         (coe mulInt (coe v3) (coe v1))
                         (coe du_m'8804'm'42'n_3962 (coe v3) (coe v1))))
                   (coe
                      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                      (coe
                         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                         (coe du_m'8804'n'43'm_3374 (coe v3))))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m<n⇒m<n*o
d_m'60'n'8658'm'60'n'42'o_4004 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'm'60'n'42'o_4004 ~v0 v1 v2 ~v3 v4
  = du_m'60'n'8658'm'60'n'42'o_4004 v1 v2 v4
du_m'60'n'8658'm'60'n'42'o_4004 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'8658'm'60'n'42'o_4004 v0 v1 v2
  = coe
      du_'60''45'trans'737'_2822 (coe v2)
      (coe du_m'8804'm'42'n_3962 (coe v0) (coe v1))
-- Data.Nat.Properties.m<n⇒m<o*n
d_m'60'n'8658'm'60'o'42'n_4022 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'm'60'o'42'n_4022 v0 v1 v2 ~v3 v4
  = du_m'60'n'8658'm'60'o'42'n_4022 v0 v1 v2 v4
du_m'60'n'8658'm'60'o'42'n_4022 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'8658'm'60'o'42'n_4022 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
         (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans_2810 v5 v7 v8)
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
         (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans'737'_2822 v7 v8) (coe v0)
         (coe mulInt (coe v1) (coe v2)) (coe mulInt (coe v2) (coe v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe d_'8804''45'isPreorder_2620) (coe mulInt (coe v2) (coe v1)))
         (coe du_m'60'n'8658'm'60'n'42'o_4004 (coe v1) (coe v2) (coe v3)))
-- Data.Nat.Properties.*-cancelʳ-<
d_'42''45'cancel'691''45''60'_4032 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'cancel'691''45''60'_4032 v0 v1 v2 ~v3
  = du_'42''45'cancel'691''45''60'_4032 v0 v1 v2
du_'42''45'cancel'691''45''60'_4032 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'42''45'cancel'691''45''60'_4032 v0 v1 v2
  = let v3
          = let v3 = subInt (coe v1) (coe (1 :: Integer)) in
            let v4 = subInt (coe v2) (coe (1 :: Integer)) in
            coe
              MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
              (coe
                 du_'42''45'cancel'691''45''60'_4032 (coe v0) (coe v3) (coe v4)) in
    coe
      seq (coe v0)
      (case coe v1 of
         0 -> case coe v2 of
                _ | coe geqInt (coe v2) (coe (1 :: Integer)) ->
                    coe du_0'60'1'43'n_2922
                _ -> coe v3
         _ -> let v4 = subInt (coe v1) (coe (1 :: Integer)) in
              case coe v2 of
                _ | coe geqInt (coe v2) (coe (1 :: Integer)) ->
                    let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                    coe
                      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                      (coe
                         du_'42''45'cancel'691''45''60'_4032 (coe v0) (coe v4) (coe v5))
                _ -> coe v3)
-- Data.Nat.Properties.*-cancelˡ-<
d_'42''45'cancel'737''45''60'_4048 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'42''45'cancel'737''45''60'_4048 v0 v1 v2 v3
  = coe
      du_'42''45'cancel'691''45''60'_4032 (coe v0) (coe v1) (coe v2)
-- Data.Nat.Properties.*-cancel-<
d_'42''45'cancel'45''60'_4064 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'cancel'45''60'_4064
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_'42''45'cancel'737''45''60'_4048)
      (\ v0 v1 v2 v3 -> coe du_'42''45'cancel'691''45''60'_4032 v0 v1 v2)
-- Data.Nat.Properties.^-identityʳ
d_'94''45'identity'691'_4066 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'94''45'identity'691'_4066 = erased
-- Data.Nat.Properties.^-zeroˡ
d_'94''45'zero'737'_4070 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'94''45'zero'737'_4070 = erased
-- Data.Nat.Properties.^-distribˡ-+-*
d_'94''45'distrib'737''45''43''45''42'_4080 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'94''45'distrib'737''45''43''45''42'_4080 = erased
-- Data.Nat.Properties.^-semigroup-morphism
d_'94''45'semigroup'45'morphism_4098 ::
  Integer -> MAlonzo.Code.Algebra.Morphism.T_IsSemigroupMorphism_148
d_'94''45'semigroup'45'morphism_4098 ~v0
  = du_'94''45'semigroup'45'morphism_4098
du_'94''45'semigroup'45'morphism_4098 ::
  MAlonzo.Code.Algebra.Morphism.T_IsSemigroupMorphism_148
du_'94''45'semigroup'45'morphism_4098
  = coe
      MAlonzo.Code.Algebra.Morphism.C_IsSemigroupMorphism'46'constructor_1081
      erased erased
-- Data.Nat.Properties.^-monoid-morphism
d_'94''45'monoid'45'morphism_4106 ::
  Integer -> MAlonzo.Code.Algebra.Morphism.T_IsMonoidMorphism_306
d_'94''45'monoid'45'morphism_4106 ~v0
  = du_'94''45'monoid'45'morphism_4106
du_'94''45'monoid'45'morphism_4106 ::
  MAlonzo.Code.Algebra.Morphism.T_IsMonoidMorphism_306
du_'94''45'monoid'45'morphism_4106
  = coe
      MAlonzo.Code.Algebra.Morphism.C_IsMonoidMorphism'46'constructor_2137
      (coe du_'94''45'semigroup'45'morphism_4098) erased
-- Data.Nat.Properties.^-*-assoc
d_'94''45''42''45'assoc_4114 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'94''45''42''45'assoc_4114 = erased
-- Data.Nat.Properties.m^n≡0⇒m≡0
d_m'94'n'8801'0'8658'm'8801'0_4136 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'94'n'8801'0'8658'm'8801'0_4136 = erased
-- Data.Nat.Properties.m^n≡1⇒n≡0∨m≡1
d_m'94'n'8801'1'8658'n'8801'0'8744'm'8801'1_4148 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_m'94'n'8801'1'8658'n'8801'0'8744'm'8801'1_4148 ~v0 v1 ~v2
  = du_m'94'n'8801'1'8658'n'8801'0'8744'm'8801'1_4148 v1
du_m'94'n'8801'1'8658'n'8801'0'8744'm'8801'1_4148 ::
  Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_m'94'n'8801'1'8658'n'8801'0'8744'm'8801'1_4148 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
      _ -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
-- Data.Nat.Properties.m^n≢0
d_m'94'n'8802'0_4164 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_m'94'n'8802'0_4164 v0 v1 ~v2 = du_m'94'n'8802'0_4164 v0 v1
du_m'94'n'8802'0_4164 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_m'94'n'8802'0_4164 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Base.du_'8802''45'nonZero_102
      (coe MAlonzo.Code.Data.Nat.Base.d__'94'__206 (coe v0) (coe v1))
-- Data.Nat.Properties.2^n>0
d_2'94'n'62'0_4172 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_2'94'n'62'0_4172 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             du_'8804''45'trans_2578 (coe d_2'94'n'62'0_4172 (coe v1))
             (coe
                du_m'8804'm'43'n_3362
                (coe
                   MAlonzo.Code.Data.Nat.Base.d__'94'__206 (coe (2 :: Integer))
                   (coe v1)))
-- Data.Nat.Properties.m≤n⇒m⊔n≡n
d_m'8804'n'8658'm'8852'n'8801'n_4180 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658'm'8852'n'8801'n_4180 = erased
-- Data.Nat.Properties.m≥n⇒m⊔n≡m
d_m'8805'n'8658'm'8852'n'8801'm_4190 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8805'n'8658'm'8852'n'8801'm_4190 = erased
-- Data.Nat.Properties.m≤n⇒m⊓n≡m
d_m'8804'n'8658'm'8851'n'8801'm_4204 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658'm'8851'n'8801'm_4204 = erased
-- Data.Nat.Properties.m≥n⇒m⊓n≡n
d_m'8805'n'8658'm'8851'n'8801'n_4214 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8805'n'8658'm'8851'n'8801'n_4214 = erased
-- Data.Nat.Properties.⊓-operator
d_'8851''45'operator_4224 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84
d_'8851''45'operator_4224
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MinOperator'46'constructor_973
      (coe MAlonzo.Code.Data.Nat.Base.d__'8851'__166) erased erased
-- Data.Nat.Properties.⊔-operator
d_'8852''45'operator_4226 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MaxOperator_114
d_'8852''45'operator_4226
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MaxOperator'46'constructor_1501
      (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138) erased erased
-- Data.Nat.Properties.⊔≡⊔′
d_'8852''8801''8852''8242'_4232 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''8801''8852''8242'_4232 = erased
-- Data.Nat.Properties.⊓≡⊓′
d_'8851''8801''8851''8242'_4258 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''8801''8851''8242'_4258 = erased
-- Data.Nat.Properties.⊓-⊔-properties.antimono-≤-distrib-⊓
d_antimono'45''8804''45'distrib'45''8851'_4282 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8851'_4282 = erased
-- Data.Nat.Properties.⊓-⊔-properties.antimono-≤-distrib-⊔
d_antimono'45''8804''45'distrib'45''8852'_4284 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8852'_4284 = erased
-- Data.Nat.Properties.⊓-⊔-properties.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_4286 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8851'_4286 = erased
-- Data.Nat.Properties.⊓-⊔-properties.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_4288 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8852'_4288 = erased
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≤x
d_x'8851'y'8804'x_4290 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8804'x_4290
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_4292 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8804'y'8658'x'8851'z'8804'y_4292
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_4294 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8804'y'8658'z'8851'x'8804'y_4294
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_4296 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8804'y'8658'x'8851'z'8804'y_4296
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_4298 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8804'y'8658'z'8851'x'8804'y_4298
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_4300 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8804'y'8851'z'8658'x'8804'y_4300
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_4302 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8804'y'8851'z'8658'x'8804'z_4302
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≤y
d_x'8851'y'8804'y_4304 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8804'y_4304
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_4306 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8776'x'8658'x'8804'y_4306
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_4308 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8776'y'8658'y'8804'x_4308
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≤x
d_x'8851'y'8804'x_4310 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8804'x_4310
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≤x⊔y
d_x'8851'y'8804'x'8852'y_4312 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8804'x'8852'y_4312
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_x'8851'y'8804'x'8852'y_2996
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≤y
d_x'8851'y'8804'y_4314 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8804'y_4314
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_4316 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8776'x'8658'x'8804'y_4316
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_4318 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8851'y'8776'y'8658'y'8804'x_4318
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_4320 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8804'y'8851'z'8658'x'8804'y_4320
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_4322 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_x'8804'y'8851'z'8658'x'8804'z_4322
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-absorbs-⊔
d_'8851''45'absorbs'45''8852'_4324 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'absorbs'45''8852'_4324 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-assoc
d_'8851''45'assoc_4326 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'assoc_4326 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-band
d_'8851''45'band_4328 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_4328
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-comm
d_'8851''45'comm_4330 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'comm_4330 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_4332 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_4332
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-distrib-⊔
d_'8851''45'distrib'45''8852'_4340 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'distrib'45''8852'_4340
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45'distrib'45''8852'_2816
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-distribʳ-⊔
d_'8851''45'distrib'691''45''8852'_4342 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'distrib'691''45''8852'_4342 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-distribˡ-⊔
d_'8851''45'distrib'737''45''8852'_4344 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'distrib'737''45''8852'_4344 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-glb
d_'8851''45'glb_4346 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'glb_4346
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-idem
d_'8851''45'idem_4348 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'idem_4348 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isBand
d_'8851''45'isBand_4356 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_4356
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_4358 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_4358
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isMagma
d_'8851''45'isMagma_4360 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_4360
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_4364 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_4364
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isSemigroup
d_'8851''45'isSemigroup_4366 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_4366
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-magma
d_'8851''45'magma_4368 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_4368
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-mono-≤
d_'8851''45'mono'45''8804'_4370 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'mono'45''8804'_4370
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_4374 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'mono'691''45''8804'_4374
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_4376 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'mono'737''45''8804'_4376
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-sel
d_'8851''45'sel_4380 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_4380
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-selectiveMagma
d_'8851''45'selectiveMagma_4382 ::
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_4382
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-semigroup
d_'8851''45'semigroup_4384 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_4384
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-properties.⊓-triangulate
d_'8851''45'triangulate_4386 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'triangulate_4386 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-⊔-absorptive
d_'8851''45''8852''45'absorptive_4394 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45''8852''45'absorptive_4394
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'absorptive_2896
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-properties.⊔-absorbs-⊓
d_'8852''45'absorbs'45''8851'_4396 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'absorbs'45''8851'_4396 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-assoc
d_'8851''45'assoc_4398 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'assoc_4398 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-band
d_'8851''45'band_4400 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_4400
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-comm
d_'8851''45'comm_4402 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'comm_4402 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_4404 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_4404
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊔-distrib-⊓
d_'8852''45'distrib'45''8851'_4412 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45'distrib'45''8851'_4412
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45'distrib'45''8851'_2848
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-properties.⊔-distribʳ-⊓
d_'8852''45'distrib'691''45''8851'_4414 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'distrib'691''45''8851'_4414 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊔-distribˡ-⊓
d_'8852''45'distrib'737''45''8851'_4416 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'distrib'737''45''8851'_4416 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-idem
d_'8851''45'idem_4418 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'idem_4418 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isBand
d_'8851''45'isBand_4426 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_4426
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_4428 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_4428
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isMagma
d_'8851''45'isMagma_4430 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_4430
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_4434 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_4434
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-isSemigroup
d_'8851''45'isSemigroup_4436 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_4436
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-glb
d_'8851''45'glb_4438 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'glb_4438
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-magma
d_'8851''45'magma_4440 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_4440
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-mono-≤
d_'8851''45'mono'45''8804'_4442 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'mono'45''8804'_4442
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_4446 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'mono'691''45''8804'_4446
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_4448 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'mono'737''45''8804'_4448
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-sel
d_'8851''45'sel_4450 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_4450
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-selectiveMagma
d_'8851''45'selectiveMagma_4452 ::
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_4452
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-semigroup
d_'8851''45'semigroup_4454 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_4454
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-properties.⊓-triangulate
d_'8851''45'triangulate_4456 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'triangulate_4456 = erased
-- Data.Nat.Properties.⊓-⊔-properties.⊔-⊓-absorptive
d_'8852''45''8851''45'absorptive_4464 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45''8851''45'absorptive_4464
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'absorptive_2894
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊓-isSemilattice
d_'8851''45'isSemilattice_4468 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8851''45'isSemilattice_4468
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'isSemilattice_586
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊓-semilattice
d_'8851''45'semilattice_4470 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8851''45'semilattice_4470
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'semilattice_588
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊓-⊔-distributiveLattice
d_'8851''45''8852''45'distributiveLattice_4472 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8851''45''8852''45'distributiveLattice_4472
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'distributiveLattice_770
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊓-⊔-isDistributiveLattice
d_'8851''45''8852''45'isDistributiveLattice_4474 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8851''45''8852''45'isDistributiveLattice_4474
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'isDistributiveLattice_760
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊓-⊔-isLattice
d_'8851''45''8852''45'isLattice_4476 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8851''45''8852''45'isLattice_4476
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'isLattice_758
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊓-⊔-lattice
d_'8851''45''8852''45'lattice_4478 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8851''45''8852''45'lattice_4478
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'lattice_766
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊓-isSemilattice
d_'8851''45'isSemilattice_4480 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8851''45'isSemilattice_4480
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'isSemilattice_586
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊓-semilattice
d_'8851''45'semilattice_4482 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8851''45'semilattice_4482
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'semilattice_588
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊔-⊓-distributiveLattice
d_'8852''45''8851''45'distributiveLattice_4484 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8852''45''8851''45'distributiveLattice_4484
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'distributiveLattice_768
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊔-⊓-isDistributiveLattice
d_'8852''45''8851''45'isDistributiveLattice_4486 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8852''45''8851''45'isDistributiveLattice_4486
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'isDistributiveLattice_762
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊔-⊓-isLattice
d_'8852''45''8851''45'isLattice_4488 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8852''45''8851''45'isLattice_4488
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'isLattice_756
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊓-⊔-latticeProperties.⊔-⊓-lattice
d_'8852''45''8851''45'lattice_4490 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8852''45''8851''45'lattice_4490
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'lattice_764
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226)
-- Data.Nat.Properties.⊔-identityˡ
d_'8852''45'identity'737'_4492 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'identity'737'_4492 = erased
-- Data.Nat.Properties.⊔-identityʳ
d_'8852''45'identity'691'_4494 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'identity'691'_4494 = erased
-- Data.Nat.Properties.⊔-identity
d_'8852''45'identity_4498 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45'identity_4498
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.⊔-0-isMonoid
d_'8852''45'0'45'isMonoid_4500 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8852''45'0'45'isMonoid_4500
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (let v0 = d_'8804''45'totalPreorder_2632 in
       let v1 = d_'8852''45'operator_4226 in
       coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
         (coe
            MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
            (coe v1)))
      (coe d_'8852''45'identity_4498)
-- Data.Nat.Properties.⊔-0-isCommutativeMonoid
d_'8852''45'0'45'isCommutativeMonoid_4502 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'8852''45'0'45'isCommutativeMonoid_4502
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'8852''45'0'45'isMonoid_4500)
      (let v0 = d_'8804''45'totalPreorder_2632 in
       let v1 = d_'8852''45'operator_4226 in
       coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'comm_2604
         (coe
            MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
            (coe v1)))
-- Data.Nat.Properties.⊔-0-monoid
d_'8852''45'0'45'monoid_4504 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'8852''45'0'45'monoid_4504
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (0 :: Integer)
      d_'8852''45'0'45'isMonoid_4500
-- Data.Nat.Properties.⊔-0-commutativeMonoid
d_'8852''45'0'45'commutativeMonoid_4506 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'8852''45'0'45'commutativeMonoid_4506
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (0 :: Integer)
      d_'8852''45'0'45'isCommutativeMonoid_4502
-- Data.Nat.Properties.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_4514 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8852'_4514 = erased
-- Data.Nat.Properties.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_4524 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8851'_4524 = erased
-- Data.Nat.Properties.antimono-≤-distrib-⊓
d_antimono'45''8804''45'distrib'45''8851'_4534 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8851'_4534 = erased
-- Data.Nat.Properties.antimono-≤-distrib-⊔
d_antimono'45''8804''45'distrib'45''8852'_4544 ::
  (Integer -> Integer) ->
  (Integer ->
   Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18) ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8852'_4544 = erased
-- Data.Nat.Properties.m<n⇒m<n⊔o
d_m'60'n'8658'm'60'n'8852'o_4554 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'm'60'n'8852'o_4554 v0 v1
  = let v2 = d_'8804''45'totalPreorder_2632 in
    let v3 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v2))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v3))
      (coe v1) (coe addInt (coe (1 :: Integer)) (coe v0))
-- Data.Nat.Properties.m<n⇒m<o⊔n
d_m'60'n'8658'm'60'o'8852'n_4562 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'm'60'o'8852'n_4562 v0 v1
  = let v2 = d_'8804''45'totalPreorder_2632 in
    let v3 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v2))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v3))
      (coe v1) (coe addInt (coe (1 :: Integer)) (coe v0))
-- Data.Nat.Properties.m⊔n<o⇒m<o
d_m'8852'n'60'o'8658'm'60'o_4570 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8852'n'60'o'8658'm'60'o_4570 v0 v1 ~v2 v3
  = du_m'8852'n'60'o'8658'm'60'o_4570 v0 v1 v3
du_m'8852'n'60'o'8658'm'60'o_4570 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8852'n'60'o'8658'm'60'o_4570 v0 v1 v2
  = coe
      du_'60''45'trans'691'_2816
      (let v3 = d_'8804''45'totalPreorder_2632 in
       let v4 = d_'8852''45'operator_4226 in
       coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
         (coe
            MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
            (coe v4))
         (coe v0) (coe v1))
      (coe v2)
-- Data.Nat.Properties.m⊔n<o⇒n<o
d_m'8852'n'60'o'8658'n'60'o_4584 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8852'n'60'o'8658'n'60'o_4584 v0 v1 ~v2 v3
  = du_m'8852'n'60'o'8658'n'60'o_4584 v0 v1 v3
du_m'8852'n'60'o'8658'n'60'o_4584 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8852'n'60'o'8658'n'60'o_4584 v0 v1 v2
  = coe
      du_'60''45'trans'691'_2816
      (let v3 = d_'8804''45'totalPreorder_2632 in
       let v4 = d_'8852''45'operator_4226 in
       coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
         (coe
            MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
            (coe v4))
         (coe v0) (coe v1))
      (coe v2)
-- Data.Nat.Properties.⊔-mono-<
d_'8852''45'mono'45''60'_4592 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8852''45'mono'45''60'_4592 v0 v1 v2 v3
  = let v4 = d_'8804''45'totalPreorder_2632 in
    let v5 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v4))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v5))
      (coe v1) (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v3)
      (coe addInt (coe (1 :: Integer)) (coe v2))
-- Data.Nat.Properties.⊔-pres-<m
d_'8852''45'pres'45''60'm_4600 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8852''45'pres'45''60'm_4600 v0 v1 v2 v3 v4
  = coe d_'8852''45'mono'45''60'_4592 v1 v0 v2 v0 v3 v4
-- Data.Nat.Properties.+-distribˡ-⊔
d_'43''45'distrib'737''45''8852'_4610 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'distrib'737''45''8852'_4610 = erased
-- Data.Nat.Properties.+-distribʳ-⊔
d_'43''45'distrib'691''45''8852'_4622 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'distrib'691''45''8852'_4622 = erased
-- Data.Nat.Properties.+-distrib-⊔
d_'43''45'distrib'45''8852'_4624 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'distrib'45''8852'_4624
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.m⊔n≤m+n
d_m'8852'n'8804'm'43'n_4630 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8852'n'8804'm'43'n_4630 v0 v1
  = let v2
          = let v2 = d_'8804''45'totalPreorder_2632 in
            let v3 = d_'8852''45'operator_4226 in
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
              (coe
                 MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
                 (coe v2))
              (coe
                 MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
                 (coe v3))
              (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v3
        -> coe du_m'8804'm'43'n_3362 (coe v0)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v3
        -> coe du_m'8804'n'43'm_3374 (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.*-distribˡ-⊔
d_'42''45'distrib'737''45''8852'_4660 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8852'_4660 = erased
-- Data.Nat.Properties.*-distribʳ-⊔
d_'42''45'distrib'691''45''8852'_4682 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8852'_4682 = erased
-- Data.Nat.Properties.*-distrib-⊔
d_'42''45'distrib'45''8852'_4684 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'distrib'45''8852'_4684
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.⊓-zeroˡ
d_'8851''45'zero'737'_4686 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'zero'737'_4686 = erased
-- Data.Nat.Properties.⊓-zeroʳ
d_'8851''45'zero'691'_4688 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'zero'691'_4688 = erased
-- Data.Nat.Properties.⊓-zero
d_'8851''45'zero_4692 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'zero_4692
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.⊔-⊓-isSemiringWithoutOne
d_'8852''45''8851''45'isSemiringWithoutOne_4694 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_'8852''45''8851''45'isSemiringWithoutOne_4694
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutOne'46'constructor_33063
      (coe d_'8852''45'0'45'isCommutativeMonoid_4502) erased
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'assoc_2692
         (coe d_'8804''45'totalPreorder_2632)
         (coe d_'8851''45'operator_4224))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45'distrib'45''8852'_2816
         (coe d_'8804''45'totalPreorder_2632)
         (coe d_'8851''45'operator_4224) (coe d_'8852''45'operator_4226))
      (coe d_'8851''45'zero_4692)
-- Data.Nat.Properties.⊔-⊓-isCommutativeSemiringWithoutOne
d_'8852''45''8851''45'isCommutativeSemiringWithoutOne_4696 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_'8852''45''8851''45'isCommutativeSemiringWithoutOne_4696
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiringWithoutOne'46'constructor_36241
      (coe d_'8852''45''8851''45'isSemiringWithoutOne_4694)
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'comm_2604
         (coe d_'8804''45'totalPreorder_2632)
         (coe d_'8851''45'operator_4224))
-- Data.Nat.Properties.⊔-⊓-commutativeSemiringWithoutOne
d_'8852''45''8851''45'commutativeSemiringWithoutOne_4698 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiringWithoutOne_1726
d_'8852''45''8851''45'commutativeSemiringWithoutOne_4698
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiringWithoutOne'46'constructor_31105
      MAlonzo.Code.Data.Nat.Base.d__'8852'__138
      MAlonzo.Code.Data.Nat.Base.d__'8851'__166 (0 :: Integer)
      d_'8852''45''8851''45'isCommutativeSemiringWithoutOne_4696
-- Data.Nat.Properties.m<n⇒m⊓o<n
d_m'60'n'8658'm'8851'o'60'n_4706 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'm'8851'o'60'n_4706 v0 ~v1 v2 v3
  = du_m'60'n'8658'm'8851'o'60'n_4706 v0 v2 v3
du_m'60'n'8658'm'8851'o'60'n_4706 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'8658'm'8851'o'60'n_4706 v0 v1 v2
  = coe
      du_'60''45'trans'691'_2816
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
         (coe d_'8804''45'totalPreorder_2632)
         (coe d_'8851''45'operator_4224) (coe v0) (coe v1))
      (coe v2)
-- Data.Nat.Properties.m<n⇒o⊓m<n
d_m'60'n'8658'o'8851'm'60'n_4718 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'o'8851'm'60'n_4718 v0 ~v1 v2 v3
  = du_m'60'n'8658'o'8851'm'60'n_4718 v0 v2 v3
du_m'60'n'8658'o'8851'm'60'n_4718 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'8658'o'8851'm'60'n_4718 v0 v1 v2
  = coe
      du_'60''45'trans'691'_2816
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
         (coe d_'8804''45'totalPreorder_2632)
         (coe d_'8851''45'operator_4224) (coe v1) (coe v0))
      (coe v2)
-- Data.Nat.Properties.m<n⊓o⇒m<n
d_m'60'n'8851'o'8658'm'60'n_4730 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8851'o'8658'm'60'n_4730 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224)
      (coe addInt (coe (1 :: Integer)) (coe v0))
-- Data.Nat.Properties.m<n⊓o⇒m<o
d_m'60'n'8851'o'8658'm'60'o_4738 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8851'o'8658'm'60'o_4738 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224)
      (coe addInt (coe (1 :: Integer)) (coe v0))
-- Data.Nat.Properties.⊓-mono-<
d_'8851''45'mono'45''60'_4740 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'mono'45''60'_4740 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe d_'8804''45'totalPreorder_2632)
      (coe d_'8851''45'operator_4224)
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
      (coe addInt (coe (1 :: Integer)) (coe v2)) (coe v3)
-- Data.Nat.Properties.⊓-pres-m<
d_'8851''45'pres'45'm'60'_4748 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'pres'45'm'60'_4748 v0 v1 v2 v3 v4
  = coe d_'8851''45'mono'45''60'_4740 v0 v1 v0 v2 v3 v4
-- Data.Nat.Properties.+-distribˡ-⊓
d_'43''45'distrib'737''45''8851'_4758 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'distrib'737''45''8851'_4758 = erased
-- Data.Nat.Properties.+-distribʳ-⊓
d_'43''45'distrib'691''45''8851'_4770 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'distrib'691''45''8851'_4770 = erased
-- Data.Nat.Properties.+-distrib-⊓
d_'43''45'distrib'45''8851'_4772 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'distrib'45''8851'_4772
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.m⊓n≤m+n
d_m'8851'n'8804'm'43'n_4778 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8851'n'8804'm'43'n_4778 v0 v1
  = let v2
          = let v2 = d_'8804''45'totalPreorder_2632 in
            let v3 = d_'8851''45'operator_4224 in
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
              (coe v2) (coe v3) (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v3
        -> coe du_m'8804'm'43'n_3362 (coe v0)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v3
        -> coe du_m'8804'n'43'm_3374 (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.*-distribˡ-⊓
d_'42''45'distrib'737''45''8851'_4808 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8851'_4808 = erased
-- Data.Nat.Properties.*-distribʳ-⊓
d_'42''45'distrib'691''45''8851'_4830 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8851'_4830 = erased
-- Data.Nat.Properties.*-distrib-⊓
d_'42''45'distrib'45''8851'_4832 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'distrib'45''8851'_4832
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.0∸n≡0
d_0'8760'n'8801'0_4834 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'8760'n'8801'0_4834 = erased
-- Data.Nat.Properties.n∸n≡0
d_n'8760'n'8801'0_4838 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'8760'n'8801'0_4838 = erased
-- Data.Nat.Properties.pred[m∸n]≡m∸[1+n]
d_pred'91'm'8760'n'93''8801'm'8760''91'1'43'n'93'_4846 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pred'91'm'8760'n'93''8801'm'8760''91'1'43'n'93'_4846 = erased
-- Data.Nat.Properties.m∸n≤m
d_m'8760'n'8804'm_4860 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8760'n'8804'm_4860 v0 v1
  = case coe v1 of
      0 -> coe
             d_'8804''45'refl_2570
             (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v0 (0 :: Integer))
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           case coe v0 of
             0 -> coe
                    d_'8804''45'refl_2570
                    (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 (0 :: Integer) v1)
             _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
                  coe
                    du_'8804''45'trans_2578
                    (coe d_m'8760'n'8804'm_4860 (coe v3) (coe v2))
                    (coe d_n'8804'1'43'n_2668 (coe v3))
-- Data.Nat.Properties.m≮m∸n
d_m'8814'm'8760'n_4874 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'8814'm'8760'n_4874 = erased
-- Data.Nat.Properties.1+m≢m∸n
d_1'43'm'8802'm'8760'n_4886 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_1'43'm'8802'm'8760'n_4886 = erased
-- Data.Nat.Properties.∸-mono
d_'8760''45'mono_4894 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8760''45'mono_4894 v0 v1 v2 v3 v4 v5
  = let v6
          = seq
              (coe v5)
              (coe
                 du_'8804''45'trans_2578
                 (coe d_m'8760'n'8804'm_4860 (coe v0) (coe v2)) (coe v4)) in
    case coe v4 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> case coe v5 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v10
               -> case coe v2 of
                    _ | coe geqInt (coe v2) (coe (1 :: Integer)) ->
                        case coe v3 of
                          _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                              coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
                          _ -> coe v6
                    _ -> coe v6
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe
                    du_'8804''45'trans_2578
                    (coe d_m'8760'n'8804'm_4860 (coe v0) (coe v2))
                    (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
        -> case coe v0 of
             _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
                 let v10 = subInt (coe v0) (coe (1 :: Integer)) in
                 case coe v1 of
                   _ | coe geqInt (coe v1) (coe (1 :: Integer)) ->
                       let v11 = subInt (coe v1) (coe (1 :: Integer)) in
                       case coe v5 of
                         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v14
                           -> case coe v2 of
                                _ | coe geqInt (coe v2) (coe (1 :: Integer)) ->
                                    let v15 = subInt (coe v2) (coe (1 :: Integer)) in
                                    case coe v3 of
                                      _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                                          let v16 = subInt (coe v3) (coe (1 :: Integer)) in
                                          coe
                                            d_'8760''45'mono_4894 (coe v10) (coe v11) (coe v15)
                                            (coe v16) (coe v9) (coe v14)
                                      _ -> coe v6
                                _ -> coe v6
                         MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
                           -> coe
                                du_'8804''45'trans_2578
                                (coe d_m'8760'n'8804'm_4860 (coe v0) (coe v2))
                                (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9)
                         _ -> MAlonzo.RTE.mazUnreachableError
                   _ -> coe v6
             _ -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.∸-monoˡ-≤
d_'8760''45'mono'737''45''8804'_4912 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8760''45'mono'737''45''8804'_4912 v0 v1 v2 v3
  = coe
      d_'8760''45'mono_4894 (coe v0) (coe v1) (coe v2) (coe v2) (coe v3)
      (coe d_'8804''45'refl_2570 (coe v2))
-- Data.Nat.Properties.∸-monoʳ-≤
d_'8760''45'mono'691''45''8804'_4924 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8760''45'mono'691''45''8804'_4924 v0 v1 v2 v3
  = coe
      d_'8760''45'mono_4894 (coe v2) (coe v2) (coe v1) (coe v0)
      (coe d_'8804''45'refl_2570 (coe v2)) (coe v3)
-- Data.Nat.Properties.∸-monoˡ-<
d_'8760''45'mono'737''45''60'_4934 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8760''45'mono'737''45''60'_4934 ~v0 v1 ~v2 v3 v4
  = du_'8760''45'mono'737''45''60'_4934 v1 v3 v4
du_'8760''45'mono'737''45''60'_4934 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8760''45'mono'737''45''60'_4934 v0 v1 v2
  = case coe v0 of
      0 -> coe v1
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> case coe v2 of
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
                      -> coe
                           du_'8760''45'mono'737''45''60'_4934 (coe v3) (coe v6) (coe v9)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.∸-monoʳ-<
d_'8760''45'mono'691''45''60'_4960 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8760''45'mono'691''45''60'_4960 v0 v1 v2 v3 v4
  = let v5 = subInt (coe v1) (coe (1 :: Integer)) in
    case coe v2 of
      0 -> coe
             seq (coe v3)
             (coe
                seq (coe v4)
                (coe
                   MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                   (d_m'8760'n'8804'm_4860
                      (coe subInt (coe v0) (coe (1 :: Integer))) (coe v5))))
      _ -> let v6 = subInt (coe v2) (coe (1 :: Integer)) in
           case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v9
               -> case coe v4 of
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v12
                      -> let v13 = subInt (coe v0) (coe (1 :: Integer)) in
                         coe
                           d_'8760''45'mono'691''45''60'_4960 (coe v13) (coe v5) (coe v6)
                           (coe v9) (coe v12)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.∸-cancelʳ-≤
d_'8760''45'cancel'691''45''8804'_4982 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8760''45'cancel'691''45''8804'_4982 ~v0 v1 ~v2 v3 ~v4
  = du_'8760''45'cancel'691''45''8804'_4982 v1 v3
du_'8760''45'cancel'691''45''8804'_4982 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8760''45'cancel'691''45''8804'_4982 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
        -> case coe v0 of
             0 -> coe
                    MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
             _ -> let v5 = subInt (coe v0) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_'8760''45'cancel'691''45''8804'_4982 (coe v5) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.∸-cancelʳ-<
d_'8760''45'cancel'691''45''60'_5002 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8760''45'cancel'691''45''60'_5002 v0 v1 ~v2 ~v3
  = du_'8760''45'cancel'691''45''60'_5002 v0 v1
du_'8760''45'cancel'691''45''60'_5002 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8760''45'cancel'691''45''60'_5002 v0 v1
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe du_0'60'1'43'n_2922
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_'8760''45'cancel'691''45''60'_5002 (coe v2) (coe v3))
-- Data.Nat.Properties.∸-cancelˡ-≡
d_'8760''45'cancel'737''45''8801'_5028 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8760''45'cancel'737''45''8801'_5028 = erased
-- Data.Nat.Properties.∸-cancelʳ-≡
d_'8760''45'cancel'691''45''8801'_5050 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8760''45'cancel'691''45''8801'_5050 = erased
-- Data.Nat.Properties.m∸n≡0⇒m≤n
d_m'8760'n'8801'0'8658'm'8804'n_5064 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8760'n'8801'0'8658'm'8804'n_5064 v0 ~v1 ~v2
  = du_m'8760'n'8801'0'8658'm'8804'n_5064 v0
du_m'8760'n'8801'0'8658'm'8804'n_5064 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8760'n'8801'0'8658'm'8804'n_5064 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe du_m'8760'n'8801'0'8658'm'8804'n_5064 (coe v1))
-- Data.Nat.Properties.m≤n⇒m∸n≡0
d_m'8804'n'8658'm'8760'n'8801'0_5076 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658'm'8760'n'8801'0_5076 = erased
-- Data.Nat.Properties.m<n⇒0<n∸m
d_m'60'n'8658'0'60'n'8760'm_5086 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'8658'0'60'n'8760'm_5086 v0 ~v1 v2
  = du_m'60'n'8658'0'60'n'8760'm_5086 v0 v2
du_m'60'n'8658'0'60'n'8760'm_5086 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'8658'0'60'n'8760'm_5086 v0 v1
  = case coe v0 of
      0 -> coe du_0'60'1'43'n_2922
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
               -> coe du_m'60'n'8658'0'60'n'8760'm_5086 (coe v2) (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m∸n≢0⇒n<m
d_m'8760'n'8802'0'8658'n'60'm_5100 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8760'n'8802'0'8658'n'60'm_5100 v0 v1 ~v2
  = du_m'8760'n'8802'0'8658'n'60'm_5100 v0 v1
du_m'8760'n'8802'0'8658'n'60'm_5100 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8760'n'8802'0'8658'n'60'm_5100 v0 v1
  = let v2 = d__'60''63'__2860 (coe v1) (coe v0) in
    case coe v2 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v3 v4
        -> if coe v3
             then case coe v4 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v5 -> coe v5
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v4)
                    (coe
                       MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m>n⇒m∸n≢0
d_m'62'n'8658'm'8760'n'8802'0_5132 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'62'n'8658'm'8760'n'8802'0_5132 = erased
-- Data.Nat.Properties.m≤n⇒n∸m≤n
d_m'8804'n'8658'n'8760'm'8804'n_5142 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'8658'n'8760'm'8804'n_5142 ~v0 v1 v2
  = du_m'8804'n'8658'n'8760'm'8804'n_5142 v1 v2
du_m'8804'n'8658'n'8760'm'8804'n_5142 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8804'n'8658'n'8760'm'8804'n_5142 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe
             d_'8804''45'refl_2570
             (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v0 (0 :: Integer))
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
        -> let v5 = subInt (coe v0) (coe (1 :: Integer)) in
           coe du_m'8804'n'8658'n'8760'm'8804'n_5142 (coe v5) (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.+-∸-comm
d_'43''45''8760''45'comm_5152 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45''8760''45'comm_5152 = erased
-- Data.Nat.Properties.∸-+-assoc
d_'8760''45''43''45'assoc_5170 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8760''45''43''45'assoc_5170 = erased
-- Data.Nat.Properties.+-∸-assoc
d_'43''45''8760''45'assoc_5194 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45''8760''45'assoc_5194 = erased
-- Data.Nat.Properties.m≤n+m∸n
d_m'8804'n'43'm'8760'n_5214 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'43'm'8760'n_5214 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe d_'8804''45'refl_2570 (coe v0)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (d_m'8804'n'43'm'8760'n_5214 (coe v2) (coe v3))
-- Data.Nat.Properties.m+n∸n≡m
d_m'43'n'8760'n'8801'm_5228 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'43'n'8760'n'8801'm_5228 = erased
-- Data.Nat.Properties.m+n∸m≡n
d_m'43'n'8760'm'8801'n_5240 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'43'n'8760'm'8801'n_5240 = erased
-- Data.Nat.Properties.m+[n∸m]≡n
d_m'43''91'n'8760'm'93''8801'n_5252 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'43''91'n'8760'm'93''8801'n_5252 = erased
-- Data.Nat.Properties.m∸n+n≡m
d_m'8760'n'43'n'8801'm_5266 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8760'n'43'n'8801'm_5266 = erased
-- Data.Nat.Properties.m∸[m∸n]≡n
d_m'8760''91'm'8760'n'93''8801'n_5278 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8760''91'm'8760'n'93''8801'n_5278 = erased
-- Data.Nat.Properties.[m+n]∸[m+o]≡n∸o
d_'91'm'43'n'93''8760''91'm'43'o'93''8801'n'8760'o_5294 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'm'43'n'93''8760''91'm'43'o'93''8801'n'8760'o_5294 = erased
-- Data.Nat.Properties.*-distribʳ-∸
d_'42''45'distrib'691''45''8760'_5306 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8760'_5306 = erased
-- Data.Nat.Properties.*-distribˡ-∸
d_'42''45'distrib'737''45''8760'_5326 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8760'_5326 = erased
-- Data.Nat.Properties.*-distrib-∸
d_'42''45'distrib'45''8760'_5328 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'distrib'45''8760'_5328
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.even≢odd
d_even'8802'odd_5334 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_even'8802'odd_5334 = erased
-- Data.Nat.Properties.m⊓n+n∸m≡n
d_m'8851'n'43'n'8760'm'8801'n_5350 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8851'n'43'n'8760'm'8801'n_5350 = erased
-- Data.Nat.Properties.[m∸n]⊓[n∸m]≡0
d_'91'm'8760'n'93''8851''91'n'8760'm'93''8801'0_5364 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'm'8760'n'93''8851''91'n'8760'm'93''8801'0_5364 = erased
-- Data.Nat.Properties.∸-distribˡ-⊓-⊔
d_'8760''45'distrib'737''45''8851''45''8852'_5380 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8760''45'distrib'737''45''8851''45''8852'_5380 = erased
-- Data.Nat.Properties.∸-distribʳ-⊓
d_'8760''45'distrib'691''45''8851'_5388 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8760''45'distrib'691''45''8851'_5388 = erased
-- Data.Nat.Properties.∸-distribˡ-⊔-⊓
d_'8760''45'distrib'737''45''8852''45''8851'_5402 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8760''45'distrib'737''45''8852''45''8851'_5402 = erased
-- Data.Nat.Properties.∸-distribʳ-⊔
d_'8760''45'distrib'691''45''8852'_5410 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8760''45'distrib'691''45''8852'_5410 = erased
-- Data.Nat.Properties.pred-mono
d_pred'45'mono_5418 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_pred'45'mono_5418 v0 v1 v2
  = coe
      d_'8760''45'mono_4894 (coe v0) (coe v1) (coe (1 :: Integer))
      (coe (1 :: Integer)) (coe v2)
      (coe d_'8804''45'refl_2570 (coe (1 :: Integer)))
-- Data.Nat.Properties.pred[n]≤n
d_pred'91'n'93''8804'n_5424 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_pred'91'n'93''8804'n_5424 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe d_n'8804'1'43'n_2668 (coe v1)
-- Data.Nat.Properties.≤pred⇒≤
d_'8804'pred'8658''8804'_5432 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804'pred'8658''8804'_5432 ~v0 v1 v2
  = du_'8804'pred'8658''8804'_5432 v1 v2
du_'8804'pred'8658''8804'_5432 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804'pred'8658''8804'_5432 v0 v1 = coe seq (coe v0) (coe v1)
-- Data.Nat.Properties.≤⇒pred≤
d_'8804''8658'pred'8804'_5448 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''8658'pred'8804'_5448 v0 ~v1 v2
  = du_'8804''8658'pred'8804'_5448 v0 v2
du_'8804''8658'pred'8804'_5448 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''8658'pred'8804'_5448 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             du_'8804''45'trans_2578 (coe d_n'8804'1'43'n_2668 (coe v2))
             (coe v1)
-- Data.Nat.Properties.<⇒≤pred
d_'60''8658''8804'pred_5460 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''8658''8804'pred_5460 ~v0 ~v1 v2
  = du_'60''8658''8804'pred_5460 v2
du_'60''8658''8804'pred_5460 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''8658''8804'pred_5460 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.suc-pred
d_suc'45'pred_5468 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_suc'45'pred_5468 = erased
-- Data.Nat.Properties.m≡n⇒∣m-n∣≡0
d_m'8801'n'8658''8739'm'45'n'8739''8801'0_5476 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8801'n'8658''8739'm'45'n'8739''8801'0_5476 = erased
-- Data.Nat.Properties.∣m-n∣≡0⇒m≡n
d_'8739'm'45'n'8739''8801'0'8658'm'8801'n_5484 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'm'45'n'8739''8801'0'8658'm'8801'n_5484 = erased
-- Data.Nat.Properties.m≤n⇒∣n-m∣≡n∸m
d_m'8804'n'8658''8739'n'45'm'8739''8801'n'8760'm_5498 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658''8739'n'45'm'8739''8801'n'8760'm_5498 = erased
-- Data.Nat.Properties.m≤n⇒∣m-n∣≡n∸m
d_m'8804'n'8658''8739'm'45'n'8739''8801'n'8760'm_5508 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658''8739'm'45'n'8739''8801'n'8760'm_5508 = erased
-- Data.Nat.Properties.∣m-n∣≡m∸n⇒n≤m
d_'8739'm'45'n'8739''8801'm'8760'n'8658'n'8804'm_5518 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8739'm'45'n'8739''8801'm'8760'n'8658'n'8804'm_5518 v0 v1 ~v2
  = du_'8739'm'45'n'8739''8801'm'8760'n'8658'n'8804'm_5518 v0 v1
du_'8739'm'45'n'8739''8801'm'8760'n'8658'n'8804'm_5518 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8739'm'45'n'8739''8801'm'8760'n'8658'n'8804'm_5518 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       du_'8739'm'45'n'8739''8801'm'8760'n'8658'n'8804'm_5518 (coe v2)
                       (coe v3))
-- Data.Nat.Properties.∣n-n∣≡0
d_'8739'n'45'n'8739''8801'0_5534 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'n'45'n'8739''8801'0_5534 = erased
-- Data.Nat.Properties.∣m-m+n∣≡n
d_'8739'm'45'm'43'n'8739''8801'n_5542 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'm'45'm'43'n'8739''8801'n_5542 = erased
-- Data.Nat.Properties.∣m+n-m+o∣≡∣n-o∣
d_'8739'm'43'n'45'm'43'o'8739''8801''8739'n'45'o'8739'_5556 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'm'43'n'45'm'43'o'8739''8801''8739'n'45'o'8739'_5556
  = erased
-- Data.Nat.Properties.m∸n≤∣m-n∣
d_m'8760'n'8804''8739'm'45'n'8739'_5572 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8760'n'8804''8739'm'45'n'8739'_5572 v0 v1
  = let v2 = d_'8804''45'total_2584 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v3
        -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v3
        -> coe
             d_'8804''45'refl_2570
             (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v0 v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.∣m-n∣≤m⊔n
d_'8739'm'45'n'8739''8804'm'8852'n_5602 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8739'm'45'n'8739''8804'm'8852'n_5602 v0 v1
  = case coe v0 of
      0 -> coe
             d_'8804''45'refl_2570
             (coe
                MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214
                (coe (0 :: Integer)) (coe v1))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    d_'8804''45'refl_2570
                    (coe
                       MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                       (coe (0 :: Integer)))
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe d_'8739'm'45'n'8739''8804'm'8852'n_5602 (coe v2) (coe v3)
-- Data.Nat.Properties.∣-∣-identityˡ
d_'8739''45''8739''45'identity'737'_5612 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45''8739''45'identity'737'_5612 = erased
-- Data.Nat.Properties.∣-∣-identityʳ
d_'8739''45''8739''45'identity'691'_5616 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45''8739''45'identity'691'_5616 = erased
-- Data.Nat.Properties.∣-∣-identity
d_'8739''45''8739''45'identity_5620 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8739''45''8739''45'identity_5620
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.∣-∣-comm
d_'8739''45''8739''45'comm_5622 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45''8739''45'comm_5622 = erased
-- Data.Nat.Properties.∣m-n∣≡[m∸n]∨[n∸m]
d_'8739'm'45'n'8739''8801''91'm'8760'n'93''8744''91'n'8760'm'93'_5636 ::
  Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8739'm'45'n'8739''8801''91'm'8760'n'93''8744''91'n'8760'm'93'_5636 v0
                                                                      v1
  = let v2 = d_'8804''45'total_2584 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v3
        -> coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                   (coe d_'8804''45'isPreorder_2620)
                   (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1 v0)))
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v3
        -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.*-distribˡ-∣-∣-aux
d_'42''45'distrib'737''45''8739''45''8739''45'aux_5664 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8739''45''8739''45'aux_5664 = erased
-- Data.Nat.Properties.*-distribˡ-∣-∣
d_'42''45'distrib'737''45''8739''45''8739'_5676 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8739''45''8739'_5676 = erased
-- Data.Nat.Properties.*-distribʳ-∣-∣
d_'42''45'distrib'691''45''8739''45''8739'_5706 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8739''45''8739'_5706 = erased
-- Data.Nat.Properties.*-distrib-∣-∣
d_'42''45'distrib'45''8739''45''8739'_5708 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'distrib'45''8739''45''8739'_5708
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.Properties.m≤n+∣n-m∣
d_m'8804'n'43''8739'n'45'm'8739'_5714 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'43''8739'n'45'm'8739'_5714 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe d_'8804''45'refl_2570 (coe v0)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (d_m'8804'n'43''8739'n'45'm'8739'_5714 (coe v2) (coe v3))
-- Data.Nat.Properties.m≤n+∣m-n∣
d_m'8804'n'43''8739'm'45'n'8739'_5728 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804'n'43''8739'm'45'n'8739'_5728 v0 v1
  = coe d_m'8804'n'43''8739'n'45'm'8739'_5714 (coe v0) (coe v1)
-- Data.Nat.Properties.m≤∣m-n∣+n
d_m'8804''8739'm'45'n'8739''43'n_5742 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8804''8739'm'45'n'8739''43'n_5742 v0 v1
  = coe d_m'8804'n'43''8739'm'45'n'8739'_5728 (coe v0) (coe v1)
-- Data.Nat.Properties.∣-∣-triangle
d_'8739''45''8739''45'triangle_5750 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8739''45''8739''45'triangle_5750 v0 v1 v2
  = case coe v0 of
      0 -> coe d_m'8804'n'43''8739'n'45'm'8739'_5714 (coe v2) (coe v1)
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                    (coe d_'8804''45'isPreorder_2620)
                    (\ v4 v5 v6 -> coe du_'60''8658''8804'_2684 v6)
                    (coe
                       MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                       (coe v2))
                    (coe
                       addInt
                       (coe
                          MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214
                          (coe (0 :: Integer)) (coe v2))
                       (coe
                          MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                          (coe (0 :: Integer))))
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                       (coe d_'8804''45'isPreorder_2620)
                       (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans'691'_2816 v7 v8)
                       (coe
                          MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                          (coe v2))
                       (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v2))
                       (coe
                          addInt
                          (coe
                             MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214
                             (coe (0 :: Integer)) (coe v2))
                          (coe
                             MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                             (coe (0 :: Integer))))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                          (coe d_'8804''45'isPreorder_2620)
                          (\ v4 v5 v6 v7 v8 -> coe du_'60''45'trans'691'_2816 v7 v8)
                          (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v2))
                          (coe addInt (coe v0) (coe v2))
                          (coe
                             addInt
                             (coe
                                MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214
                                (coe (0 :: Integer)) (coe v2))
                             (coe
                                MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                                (coe (0 :: Integer))))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                             (coe d_'8804''45'isPreorder_2620)
                             (coe
                                addInt
                                (coe
                                   MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                                   (coe (0 :: Integer)))
                                (coe v2)))
                          (coe d_m'8852'n'8804'm'43'n_4630 (coe v0) (coe v2)))
                       (coe d_'8739'm'45'n'8739''8804'm'8852'n_5602 (coe v0) (coe v2)))
             _ -> let v4 = subInt (coe v1) (coe (1 :: Integer)) in
                  case coe v2 of
                    0 -> coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                           (coe d_'8804''45'isPreorder_2620)
                           (\ v5 v6 v7 -> coe du_'60''8658''8804'_2684 v7)
                           (coe
                              MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                              (coe (0 :: Integer)))
                           (coe
                              addInt
                              (coe
                                 MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                                 (coe v1))
                              (coe
                                 MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v1)
                                 (coe (0 :: Integer))))
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                              (coe d_'8804''45'isPreorder_2620)
                              (\ v5 v6 v7 v8 v9 -> coe du_'60''45'trans'691'_2816 v8 v9) (coe v0)
                              (coe
                                 addInt
                                 (coe
                                    MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                                    (coe v1))
                                 (coe v1))
                              (coe
                                 addInt
                                 (coe
                                    MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                                    (coe v1))
                                 (coe
                                    MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v1)
                                    (coe (0 :: Integer))))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                 (coe d_'8804''45'isPreorder_2620)
                                 (coe
                                    addInt
                                    (coe
                                       MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v0)
                                       (coe v1))
                                    (coe
                                       MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214 (coe v1)
                                       (coe (0 :: Integer)))))
                              (coe d_m'8804''8739'm'45'n'8739''43'n_5742 (coe v0) (coe v1)))
                    _ -> let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                         coe d_'8739''45''8739''45'triangle_5750 (coe v3) (coe v4) (coe v5)
-- Data.Nat.Properties.∣-∣≡∣-∣′
d_'8739''45''8739''8801''8739''45''8739''8242'_5782 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45''8739''8801''8739''45''8739''8242'_5782 = erased
-- Data.Nat.Properties.∣-∣-isProtoMetric
d_'8739''45''8739''45'isProtoMetric_5804 ::
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_'8739''45''8739''45'isProtoMetric_5804
  = coe
      MAlonzo.Code.Function.Metric.Structures.C_IsProtoMetric'46'constructor_2109
      (coe d_'8804''45'isPartialOrder_2624)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
      (coe (\ v0 v1 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
-- Data.Nat.Properties.∣-∣-isPreMetric
d_'8739''45''8739''45'isPreMetric_5806 ::
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_'8739''45''8739''45'isPreMetric_5806
  = coe
      MAlonzo.Code.Function.Metric.Structures.C_IsPreMetric'46'constructor_6061
      (coe d_'8739''45''8739''45'isProtoMetric_5804) erased
-- Data.Nat.Properties.∣-∣-isQuasiSemiMetric
d_'8739''45''8739''45'isQuasiSemiMetric_5808 ::
  MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
d_'8739''45''8739''45'isQuasiSemiMetric_5808
  = coe
      MAlonzo.Code.Function.Metric.Structures.C_IsQuasiSemiMetric'46'constructor_9549
      (coe d_'8739''45''8739''45'isPreMetric_5806) erased
-- Data.Nat.Properties.∣-∣-isSemiMetric
d_'8739''45''8739''45'isSemiMetric_5810 ::
  MAlonzo.Code.Function.Metric.Structures.T_IsSemiMetric_232
d_'8739''45''8739''45'isSemiMetric_5810
  = coe
      MAlonzo.Code.Function.Metric.Structures.C_IsSemiMetric'46'constructor_13167
      (coe d_'8739''45''8739''45'isQuasiSemiMetric_5808) erased
-- Data.Nat.Properties.∣-∣-isMetric
d_'8739''45''8739''45'isMetric_5812 ::
  MAlonzo.Code.Function.Metric.Structures.T_IsGeneralMetric_308
d_'8739''45''8739''45'isMetric_5812
  = coe
      MAlonzo.Code.Function.Metric.Structures.C_IsGeneralMetric'46'constructor_17141
      (coe d_'8739''45''8739''45'isSemiMetric_5810)
      (coe d_'8739''45''8739''45'triangle_5750)
-- Data.Nat.Properties.∣-∣-quasiSemiMetric
d_'8739''45''8739''45'quasiSemiMetric_5814 ::
  MAlonzo.Code.Function.Metric.Nat.Bundles.T_QuasiSemiMetric_174
d_'8739''45''8739''45'quasiSemiMetric_5814
  = coe
      MAlonzo.Code.Function.Metric.Nat.Bundles.C_QuasiSemiMetric'46'constructor_3025
      MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214
      d_'8739''45''8739''45'isQuasiSemiMetric_5808
-- Data.Nat.Properties.∣-∣-semiMetric
d_'8739''45''8739''45'semiMetric_5816 ::
  MAlonzo.Code.Function.Metric.Nat.Bundles.T_SemiMetric_266
d_'8739''45''8739''45'semiMetric_5816
  = coe
      MAlonzo.Code.Function.Metric.Nat.Bundles.C_SemiMetric'46'constructor_4649
      MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214
      d_'8739''45''8739''45'isSemiMetric_5810
-- Data.Nat.Properties.∣-∣-preMetric
d_'8739''45''8739''45'preMetric_5818 ::
  MAlonzo.Code.Function.Metric.Nat.Bundles.T_PreMetric_90
d_'8739''45''8739''45'preMetric_5818
  = coe
      MAlonzo.Code.Function.Metric.Nat.Bundles.C_PreMetric'46'constructor_1511
      MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214
      d_'8739''45''8739''45'isPreMetric_5806
-- Data.Nat.Properties.∣-∣-metric
d_'8739''45''8739''45'metric_5820 ::
  MAlonzo.Code.Function.Metric.Nat.Bundles.T_Metric_364
d_'8739''45''8739''45'metric_5820
  = coe
      MAlonzo.Code.Function.Metric.Nat.Bundles.C_Metric'46'constructor_6343
      MAlonzo.Code.Data.Nat.Base.d_'8739'_'45'_'8739'_214
      d_'8739''45''8739''45'isMetric_5812
-- Data.Nat.Properties.⌊n/2⌋-mono
d_'8970'n'47'2'8971''45'mono_5822 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8970'n'47'2'8971''45'mono_5822 ~v0 ~v1 v2
  = du_'8970'n'47'2'8971''45'mono_5822 v2
du_'8970'n'47'2'8971''45'mono_5822 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8970'n'47'2'8971''45'mono_5822 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v3
        -> case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe du_'8970'n'47'2'8971''45'mono_5822 (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.⌈n/2⌉-mono
d_'8968'n'47'2'8969''45'mono_5826 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8968'n'47'2'8969''45'mono_5826 ~v0 ~v1 v2
  = du_'8968'n'47'2'8969''45'mono_5826 v2
du_'8968'n'47'2'8969''45'mono_5826 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8968'n'47'2'8969''45'mono_5826 v0
  = coe
      du_'8970'n'47'2'8971''45'mono_5822
      (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v0)
-- Data.Nat.Properties.⌊n/2⌋≤⌈n/2⌉
d_'8970'n'47'2'8971''8804''8968'n'47'2'8969'_5832 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8970'n'47'2'8971''8804''8968'n'47'2'8969'_5832 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      1 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (2 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (d_'8970'n'47'2'8971''8804''8968'n'47'2'8969'_5832 (coe v1))
-- Data.Nat.Properties.⌊n/2⌋+⌈n/2⌉≡n
d_'8970'n'47'2'8971''43''8968'n'47'2'8969''8801'n_5838 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8970'n'47'2'8971''43''8968'n'47'2'8969''8801'n_5838 = erased
-- Data.Nat.Properties.⌊n/2⌋≤n
d_'8970'n'47'2'8971''8804'n_5844 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8970'n'47'2'8971''8804'n_5844 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      1 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (2 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (d_'8970'n'47'2'8971''8804'n_5844 (coe v1))
-- Data.Nat.Properties.⌊n/2⌋<n
d_'8970'n'47'2'8971''60'n_5850 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8970'n'47'2'8971''60'n_5850 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe
                MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                (d_'8970'n'47'2'8971''8804'n_5844 (coe v1)))
-- Data.Nat.Properties.n≡⌊n+n/2⌋
d_n'8801''8970'n'43'n'47'2'8971'_5856 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'8801''8970'n'43'n'47'2'8971'_5856 = erased
-- Data.Nat.Properties.⌈n/2⌉≤n
d_'8968'n'47'2'8969''8804'n_5864 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8968'n'47'2'8969''8804'n_5864 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (d_'8970'n'47'2'8971''8804'n_5844 (coe v1))
-- Data.Nat.Properties.⌈n/2⌉<n
d_'8968'n'47'2'8969''60'n_5870 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8968'n'47'2'8969''60'n_5870 v0
  = coe
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
      (d_'8970'n'47'2'8971''60'n_5850 (coe v0))
-- Data.Nat.Properties.n≡⌈n+n/2⌉
d_n'8801''8968'n'43'n'47'2'8969'_5876 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'8801''8968'n'43'n'47'2'8969'_5876 = erased
-- Data.Nat.Properties.1≤n!
d_1'8804'n'33'_5884 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_1'8804'n'33'_5884 v0
  = case coe v0 of
      0 -> coe d_'8804''45'refl_2570 (coe (1 :: Integer))
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             du_'42''45'mono'45''8804'_3894 (coe v0)
             (coe MAlonzo.Code.Data.Nat.Base.d__'33'_266 (coe v1))
             (coe du_m'8804'm'43'n_3362 (coe (1 :: Integer)))
             (coe d_1'8804'n'33'_5884 (coe v1))
-- Data.Nat.Properties._!≢0
d__'33''8802'0_5890 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d__'33''8802'0_5890 v0
  = coe
      MAlonzo.Code.Data.Nat.Base.du_'62''45'nonZero_112
      (coe d_1'8804'n'33'_5884 (coe v0))
-- Data.Nat.Properties._!*_!≢0
d__'33''42'_'33''8802'0_5898 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d__'33''42'_'33''8802'0_5898 ~v0 ~v1
  = du__'33''42'_'33''8802'0_5898
du__'33''42'_'33''8802'0_5898 ::
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du__'33''42'_'33''8802'0_5898 = coe du_m'42'n'8802'0_3720
-- Data.Nat.Properties.≤′-trans
d_'8804''8242''45'trans_5904 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_'8804''8242''45'trans_5904 ~v0 ~v1 ~v2 v3 v4
  = du_'8804''8242''45'trans_5904 v3 v4
du_'8804''8242''45'trans_5904 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
du_'8804''8242''45'trans_5904 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276 -> coe v0
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v3
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
             (coe du_'8804''8242''45'trans_5904 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.z≤′n
d_z'8804''8242'n_5914 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_z'8804''8242'n_5914 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
             (d_z'8804''8242'n_5914 (coe v1))
-- Data.Nat.Properties.s≤′s
d_s'8804''8242's_5922 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_s'8804''8242's_5922 ~v0 ~v1 v2 = du_s'8804''8242's_5922 v2
du_s'8804''8242's_5922 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
du_s'8804''8242's_5922 v0 = coe v0
-- Data.Nat.Properties.≤′⇒≤
d_'8804''8242''8658''8804'_5926 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''8242''8658''8804'_5926 v0 ~v1 v2
  = du_'8804''8242''8658''8804'_5926 v0 v2
du_'8804''8242''8658''8804'_5926 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''8242''8658''8804'_5926 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
        -> coe d_'8804''45'refl_2570 (coe v0)
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v3
        -> coe du_'8804''8242''8658''8804'_5926 (coe v0) (coe v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.≤⇒≤′
d_'8804''8658''8804''8242'_5930 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_'8804''8658''8804''8242'_5930 ~v0 v1 v2
  = du_'8804''8658''8804''8242'_5930 v1 v2
du_'8804''8658''8804''8242'_5930 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
du_'8804''8658''8804''8242'_5930 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
        -> coe d_z'8804''8242'n_5914 (coe v0)
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
        -> let v5 = subInt (coe v0) (coe (1 :: Integer)) in
           coe du_'8804''8658''8804''8242'_5930 (coe v5) (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.≤′-step-injective
d_'8804''8242''45'step'45'injective_5942 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''8242''45'step'45'injective_5942 = erased
-- Data.Nat.Properties.z<′s
d_z'60''8242's_5946 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_z'60''8242's_5946 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
             (d_z'60''8242's_5946 (coe v1))
-- Data.Nat.Properties.s<′s
d_s'60''8242's_5954 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_s'60''8242's_5954 ~v0 ~v1 v2 = du_s'60''8242's_5954 v2
du_s'60''8242's_5954 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
du_s'60''8242's_5954 v0 = coe v0
-- Data.Nat.Properties.<⇒<′
d_'60''8658''60''8242'_5962 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_'60''8658''60''8242'_5962 ~v0 v1 v2
  = du_'60''8658''60''8242'_5962 v1 v2
du_'60''8658''60''8242'_5962 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
du_'60''8658''60''8242'_5962 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
        -> let v5 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v4 of
             MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22
               -> coe d_z'60''8242's_5946 (coe v5)
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
               -> coe
                    du_'60''8658''60''8242'_5962
                    (coe subInt (coe v0) (coe (1 :: Integer)))
                    (coe MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.<′⇒<
d_'60''8242''8658''60'_5970 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''8242''8658''60'_5970 v0 ~v1 v2
  = du_'60''8242''8658''60'_5970 v0 v2
du_'60''8242''8658''60'_5970 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'60''8242''8658''60'_5970 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
        -> coe d_n'60'1'43'n_2926 (coe v0)
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v3
        -> coe
             du_m'60'n'8658'm'60'1'43'n_2906
             (coe du_'60''8242''8658''60'_5970 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.m<1+n⇒m<n∨m≡n′
d_m'60'1'43'n'8658'm'60'n'8744'm'8801'n'8242'_5978 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_m'60'1'43'n'8658'm'60'n'8744'm'8801'n'8242'_5978 v0 v1 v2
  = let v3
          = coe
              du_'60''8658''60''8242'_5962
              (coe addInt (coe (1 :: Integer)) (coe v1)) (coe v2) in
    case coe v3 of
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
        -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v5
        -> coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
             (coe du_'60''8242''8658''60'_5970 (coe v0) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties._≤′?_
d__'8804''8242''63'__5992 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''8242''63'__5992 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (coe du_'8804''8658''8804''8242'_5930 (coe v1))
      (coe d__'8804''63'__2612 (coe v0) (coe v1))
-- Data.Nat.Properties._<′?_
d__'60''8242''63'__5998 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''8242''63'__5998 v0 v1
  = coe
      d__'8804''8242''63'__5992
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
-- Data.Nat.Properties._≥′?_
d__'8805''8242''63'__6004 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8805''8242''63'__6004 v0 v1
  = coe d__'8804''8242''63'__5992 (coe v1) (coe v0)
-- Data.Nat.Properties._>′?_
d__'62''8242''63'__6006 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'62''8242''63'__6006 v0 v1
  = coe d__'60''8242''63'__5998 (coe v1) (coe v0)
-- Data.Nat.Properties.m≤′m+n
d_m'8804''8242'm'43'n_6012 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_m'8804''8242'm'43'n_6012 v0 v1
  = coe
      du_'8804''8658''8804''8242'_5930 (coe addInt (coe v0) (coe v1))
      (coe du_m'8804'm'43'n_3362 (coe v0))
-- Data.Nat.Properties.n≤′m+n
d_n'8804''8242'm'43'n_6022 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_n'8804''8242'm'43'n_6022 v0 ~v1 = du_n'8804''8242'm'43'n_6022 v0
du_n'8804''8242'm'43'n_6022 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
du_n'8804''8242'm'43'n_6022 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
             (coe du_n'8804''8242'm'43'n_6022 (coe v1))
-- Data.Nat.Properties.⌈n/2⌉≤′n
d_'8968'n'47'2'8969''8804''8242'n_6032 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_'8968'n'47'2'8969''8804''8242'n_6032 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
      1 -> coe MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
      _ -> let v1 = subInt (coe v0) (coe (2 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
             (d_'8968'n'47'2'8969''8804''8242'n_6032 (coe v1))
-- Data.Nat.Properties.⌊n/2⌋≤′n
d_'8970'n'47'2'8971''8804''8242'n_6038 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_'8970'n'47'2'8971''8804''8242'n_6038 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
             (d_'8968'n'47'2'8969''8804''8242'n_6032 (coe v1))
-- Data.Nat.Properties.m<ᵇn⇒1+m+[n-1+m]≡n
d_m'60''7495'n'8658'1'43'm'43''91'n'45'1'43'm'93''8801'n_6046 ::
  Integer ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'60''7495'n'8658'1'43'm'43''91'n'45'1'43'm'93''8801'n_6046
  = erased
-- Data.Nat.Properties.m<ᵇ1+m+n
d_m'60''7495'1'43'm'43'n_6058 :: Integer -> Integer -> AgdaAny
d_m'60''7495'1'43'm'43'n_6058 v0 ~v1
  = du_m'60''7495'1'43'm'43'n_6058 v0
du_m'60''7495'1'43'm'43'n_6058 :: Integer -> AgdaAny
du_m'60''7495'1'43'm'43'n_6058 v0
  = coe
      du_'60''8658''60''7495'_2518
      (coe
         du_m'8804'm'43'n_3362 (coe addInt (coe (1 :: Integer)) (coe v0)))
-- Data.Nat.Properties.<ᵇ⇒<″
d_'60''7495''8658''60''8243'_6066 ::
  Integer ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314
d_'60''7495''8658''60''8243'_6066 v0 v1 ~v2
  = du_'60''7495''8658''60''8243'_6066 v0 v1
du_'60''7495''8658''60''8243'_6066 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314
du_'60''7495''8658''60''8243'_6066 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328
      (coe
         MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1
         (addInt (coe (1 :: Integer)) (coe v0)))
-- Data.Nat.Properties.<″⇒<ᵇ
d_'60''8243''8658''60''7495'_6078 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 -> AgdaAny
d_'60''8243''8658''60''7495'_6078 v0 ~v1 v2
  = du_'60''8243''8658''60''7495'_6078 v0 v2
du_'60''8243''8658''60''7495'_6078 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 -> AgdaAny
du_'60''8243''8658''60''7495'_6078 v0 v1
  = coe
      seq (coe v1)
      (coe
         du_'60''8658''60''7495'_2518
         (coe
            du_m'8804'm'43'n_3362 (coe addInt (coe (1 :: Integer)) (coe v0))))
-- Data.Nat.Properties.≤″⇒≤
d_'8804''8243''8658''8804'_6082 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''8243''8658''8804'_6082 v0 ~v1 v2
  = du_'8804''8243''8658''8804'_6082 v0 v2
du_'8804''8243''8658''8804'_6082 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''8243''8658''8804'_6082 v0 v1
  = case coe v0 of
      0 -> coe
             seq (coe v1) (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v3
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       du_'8804''8243''8658''8804'_6082 (coe v2)
                       (coe
                          MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v3))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.≤⇒≤″
d_'8804''8658''8804''8243'_6086 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314
d_'8804''8658''8804''8243'_6086 v0 v1 ~v2
  = du_'8804''8658''8804''8243'_6086 v0 v1
du_'8804''8658''8804''8243'_6086 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314
du_'8804''8658''8804''8243'_6086 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328
      (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1 v0)
-- Data.Nat.Properties._<″?_
d__'60''8243''63'__6088 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''8243''63'__6088 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (\ v2 -> coe du_'60''7495''8658''60''8243'_6066 (coe v0) (coe v1))
      (coe
         MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
         (coe ltInt (coe v0) (coe v1)))
-- Data.Nat.Properties._≤″?_
d__'8804''8243''63'__6094 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''8243''63'__6094 v0 v1
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe
                   MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v1))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe d__'60''8243''63'__6088 (coe v2) (coe v1)
-- Data.Nat.Properties._≥″?_
d__'8805''8243''63'__6102 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8805''8243''63'__6102 v0 v1
  = coe d__'8804''8243''63'__6094 (coe v1) (coe v0)
-- Data.Nat.Properties._>″?_
d__'62''8243''63'__6104 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'62''8243''63'__6104 v0 v1
  = coe d__'60''8243''63'__6088 (coe v1) (coe v0)
-- Data.Nat.Properties.≤″-irrelevant
d_'8804''8243''45'irrelevant_6106 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''8243''45'irrelevant_6106 = erased
-- Data.Nat.Properties.<″-irrelevant
d_'60''8243''45'irrelevant_6124 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''8243''45'irrelevant_6124 = erased
-- Data.Nat.Properties.>″-irrelevant
d_'62''8243''45'irrelevant_6126 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'62''8243''45'irrelevant_6126 = erased
-- Data.Nat.Properties.≥″-irrelevant
d_'8805''8243''45'irrelevant_6128 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8805''8243''45'irrelevant_6128 = erased
-- Data.Nat.Properties.≤‴⇒≤″
d_'8804''8244''8658''8804''8243'_6134 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314
d_'8804''8244''8658''8804''8243'_6134 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Nat.Base.C_'8804''8244''45'refl_352
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328
             (0 :: Integer)
      MAlonzo.Code.Data.Nat.Base.C_'8804''8244''45'step_358 v5
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328
             (addInt
                (coe (1 :: Integer))
                (coe
                   MAlonzo.Code.Data.Nat.Base.d_k_324
                   (coe d_ind_6146 (coe v0) (coe v1) (coe v5))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties._.ind
d_ind_6146 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314
d_ind_6146 v0 v1 v2
  = coe
      d_'8804''8244''8658''8804''8243'_6134
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1) (coe v2)
-- Data.Nat.Properties.m≤‴m+k
d_m'8804''8244'm'43'k_6154 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
d_m'8804''8244'm'43'k_6154 ~v0 ~v1 v2 ~v3
  = du_m'8804''8244'm'43'k_6154 v2
du_m'8804''8244'm'43'k_6154 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
du_m'8804''8244'm'43'k_6154 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C_'8804''8244''45'refl_352
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.Base.C_'8804''8244''45'step_358
             (coe du_m'8804''8244'm'43'k_6154 (coe v1))
-- Data.Nat.Properties.≤″⇒≤‴
d_'8804''8243''8658''8804''8244'_6170 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
d_'8804''8243''8658''8804''8244'_6170 ~v0 ~v1 v2
  = du_'8804''8243''8658''8804''8244'_6170 v2
du_'8804''8243''8658''8804''8244'_6170 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
du_'8804''8243''8658''8804''8244'_6170 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v1
        -> coe du_m'8804''8244'm'43'k_6154 (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.0≤‴n
d_0'8804''8244'n_6178 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
d_0'8804''8244'n_6178 v0 = coe du_m'8804''8244'm'43'k_6154 (coe v0)
-- Data.Nat.Properties.<ᵇ⇒<‴
d_'60''7495''8658''60''8244'_6186 ::
  Integer ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
d_'60''7495''8658''60''8244'_6186 v0 v1 ~v2
  = du_'60''7495''8658''60''8244'_6186 v0 v1
du_'60''7495''8658''60''8244'_6186 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
du_'60''7495''8658''60''8244'_6186 v0 v1
  = coe
      du_'8804''8243''8658''8804''8244'_6170
      (coe du_'60''7495''8658''60''8243'_6066 (coe v0) (coe v1))
-- Data.Nat.Properties.<‴⇒<ᵇ
d_'60''8244''8658''60''7495'_6198 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348 -> AgdaAny
d_'60''8244''8658''60''7495'_6198 v0 v1 v2
  = coe
      du_'60''8243''8658''60''7495'_6078 (coe v0)
      (coe
         d_'8804''8244''8658''8804''8243'_6134
         (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1) (coe v2))
-- Data.Nat.Properties._<‴?_
d__'60''8244''63'__6202 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''8244''63'__6202 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (\ v2 -> coe du_'60''7495''8658''60''8244'_6186 (coe v0) (coe v1))
      (coe
         MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
         (coe ltInt (coe v0) (coe v1)))
-- Data.Nat.Properties._≤‴?_
d__'8804''8244''63'__6208 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''8244''63'__6208 v0 v1
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe d_0'8804''8244'n_6178 (coe v1)))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe d__'60''8244''63'__6202 (coe v2) (coe v1)
-- Data.Nat.Properties._≥‴?_
d__'8805''8244''63'__6216 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8805''8244''63'__6216 v0 v1
  = coe d__'8804''8244''63'__6208 (coe v1) (coe v0)
-- Data.Nat.Properties._>‴?_
d__'62''8244''63'__6218 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'62''8244''63'__6218 v0 v1
  = coe d__'60''8244''63'__6202 (coe v1) (coe v0)
-- Data.Nat.Properties.≤⇒≤‴
d_'8804''8658''8804''8244'_6220 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
d_'8804''8658''8804''8244'_6220 v0 v1 ~v2
  = du_'8804''8658''8804''8244'_6220 v0 v1
du_'8804''8658''8804''8244'_6220 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348
du_'8804''8658''8804''8244'_6220 v0 v1
  = coe
      du_'8804''8243''8658''8804''8244'_6170
      (coe du_'8804''8658''8804''8243'_6086 (coe v0) (coe v1))
-- Data.Nat.Properties.≤‴⇒≤
d_'8804''8244''8658''8804'_6222 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8244'__348 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''8244''8658''8804'_6222 v0 v1 v2
  = coe
      du_'8804''8243''8658''8804'_6082 (coe v0)
      (coe
         d_'8804''8244''8658''8804''8243'_6134 (coe v0) (coe v1) (coe v2))
-- Data.Nat.Properties.eq?
d_eq'63'_6228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_eq'63'_6228 ~v0 ~v1 v2 = du_eq'63'_6228 v2
du_eq'63'_6228 ::
  MAlonzo.Code.Function.Bundles.T_Injection_704 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_eq'63'_6228 v0
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.du_via'45'injection_124
      (coe v0) (coe d__'8799'__2464)
-- Data.Nat.Properties._.anyUpTo?
d_anyUpTo'63'_6246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (Integer -> ()) ->
  (Integer ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_anyUpTo'63'_6246 ~v0 ~v1 v2 v3 = du_anyUpTo'63'_6246 v2 v3
du_anyUpTo'63'_6246 ::
  (Integer ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_anyUpTo'63'_6246 v0 v1
  = case coe v1 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           let v3 = coe v0 v2 in
           let v4 = coe du_anyUpTo'63'_6246 (coe v0) (coe v2) in
           let v5
                 = case coe v4 of
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v5 v6
                       -> coe
                            seq (coe v5)
                            (case coe v6 of
                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v7
                                 -> case coe v7 of
                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                                        -> coe
                                             seq (coe v9)
                                             (coe
                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                                (coe v6))
                                      _ -> MAlonzo.RTE.mazUnreachableError
                               _ -> MAlonzo.RTE.mazUnreachableError)
                     _ -> MAlonzo.RTE.mazUnreachableError in
           case coe v3 of
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v6 v7
               -> let v8
                        = case coe v4 of
                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v8 v9
                              -> case coe v8 of
                                   MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                                     -> case coe v9 of
                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v10
                                            -> case coe v10 of
                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
                                                   -> coe seq (coe v12) (coe v4)
                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                          _ -> coe v5
                                   _ -> coe v5
                            _ -> MAlonzo.RTE.mazUnreachableError in
                  if coe v6
                    then case coe v7 of
                           MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v9
                             -> coe
                                  MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                  (coe v6)
                                  (coe
                                     MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                           (coe d_'8804''45'refl_2570 (coe v1)) (coe v9))))
                           _ -> coe v8
                    else (case coe v4 of
                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
                              -> if coe v9
                                   then case coe v10 of
                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v11
                                            -> case coe v11 of
                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v12 v13
                                                   -> coe seq (coe v13) (coe v4)
                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                          _ -> coe v8
                                   else (case coe v7 of
                                           MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                             -> case coe v10 of
                                                  MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                                    -> coe
                                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                         (coe v9)
                                                         (coe
                                                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                                                  _ -> coe v8
                                           _ -> coe v8)
                            _ -> MAlonzo.RTE.mazUnreachableError)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties._._.¬Pn<1+v
d_'172'Pn'60'1'43'v_6280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (Integer -> ()) ->
  (Integer ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'172'Pn'60'1'43'v_6280 = erased
-- Data.Nat.Properties._.allUpTo?
d_allUpTo'63'_6310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (Integer -> ()) ->
  (Integer ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_allUpTo'63'_6310 ~v0 ~v1 v2 v3 = du_allUpTo'63'_6310 v2 v3
du_allUpTo'63'_6310 ::
  (Integer ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_allUpTo'63'_6310 v0 v1
  = case coe v1 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           let v3 = coe v0 v2 in
           let v4 = coe du_allUpTo'63'_6310 (coe v0) (coe v2) in
           let v5
                 = case coe v4 of
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v5 v6
                       -> coe
                            seq (coe v5)
                            (coe
                               seq (coe v6)
                               (coe
                                  MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                  (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                  (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)))
                     _ -> MAlonzo.RTE.mazUnreachableError in
           case coe v3 of
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v6 v7
               -> let v8
                        = case coe v4 of
                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v8 v9
                              -> case coe v8 of
                                   MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                                     -> case coe v9 of
                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                            -> coe
                                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                 (coe v8)
                                                 (coe
                                                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                                          _ -> coe v5
                                   _ -> coe v5
                            _ -> MAlonzo.RTE.mazUnreachableError in
                  if coe v6
                    then case coe v4 of
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
                             -> if coe v9
                                  then case coe v7 of
                                         MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v11
                                           -> case coe v10 of
                                                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v12
                                                  -> coe
                                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                       (coe v9)
                                                       (coe
                                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                                          (coe
                                                             du_Pn'60'1'43'v_6342 (coe v2) (coe v11)
                                                             (coe v12)))
                                                _ -> coe v8
                                         _ -> coe v8
                                  else (case coe v10 of
                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                                            -> coe
                                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                 (coe v9)
                                                 (coe
                                                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                                          _ -> coe v8)
                           _ -> MAlonzo.RTE.mazUnreachableError
                    else (case coe v7 of
                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
                              -> coe
                                   MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                   (coe v6)
                                   (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                            _ -> coe v8)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties._._.Pn<1+v
d_Pn'60'1'43'v_6342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (Integer -> ()) ->
  (Integer ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  AgdaAny ->
  (Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny) ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny
d_Pn'60'1'43'v_6342 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7
  = du_Pn'60'1'43'v_6342 v3 v4 v5 v6 v7
du_Pn'60'1'43'v_6342 ::
  Integer ->
  AgdaAny ->
  (Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny) ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny
du_Pn'60'1'43'v_6342 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v7
        -> let v8 = d__'8799'__2464 (coe v3) (coe v0) in
           case coe v8 of
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
               -> if coe v9
                    then coe seq (coe v10) (coe v1)
                    else coe
                           seq (coe v10)
                           (coe
                              v2 v3 (coe du_'8804''8743''8802''8658''60'_2748 (coe v0) (coe v7)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Properties.∀[m≤n⇒m≢o]⇒o<n
d_'8704''91'm'8804'n'8658'm'8802'o'93''8658'o'60'n_6368 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8704''91'm'8804'n'8658'm'8802'o'93''8658'o'60'n_6368 v0 v1 v2
  = coe
      du_'8704''91'm'8804'n'8658'm'8802'o'93''8658'n'60'o_3026 v0 v1
-- Data.Nat.Properties.∀[m<n⇒m≢o]⇒o≤n
d_'8704''91'm'60'n'8658'm'8802'o'93''8658'o'8804'n_6376 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8704''91'm'60'n'8658'm'8802'o'93''8658'o'8804'n_6376 v0 v1 v2
  = coe
      du_'8704''91'm'60'n'8658'm'8802'o'93''8658'n'8804'o_3054 v0 v1
-- Data.Nat.Properties.*-+-isSemiring
d_'42''45''43''45'isSemiring_6378 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_'42''45''43''45'isSemiring_6378
  = coe d_'43''45''42''45'isSemiring_3634
-- Data.Nat.Properties.*-+-isCommutativeSemiring
d_'42''45''43''45'isCommutativeSemiring_6380 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'42''45''43''45'isCommutativeSemiring_6380
  = coe d_'43''45''42''45'isCommutativeSemiring_3636
-- Data.Nat.Properties.*-+-semiring
d_'42''45''43''45'semiring_6382 ::
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_'42''45''43''45'semiring_6382
  = coe d_'43''45''42''45'semiring_3648
-- Data.Nat.Properties.*-+-commutativeSemiring
d_'42''45''43''45'commutativeSemiring_6384 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'42''45''43''45'commutativeSemiring_6384
  = coe d_'43''45''42''45'commutativeSemiring_3650
-- Data.Nat.Properties.∣m+n-m+o∣≡∣n-o|
d_'8739'm'43'n'45'm'43'o'8739''8801''8739'n'45'o'124'_6386 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'm'43'n'45'm'43'o'8739''8801''8739'n'45'o'124'_6386 = erased
-- Data.Nat.Properties.m≤n⇒n⊔m≡n
d_m'8804'n'8658'n'8852'm'8801'n_6388 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658'n'8852'm'8801'n_6388 = erased
-- Data.Nat.Properties.m≤n⇒n⊓m≡m
d_m'8804'n'8658'n'8851'm'8801'm_6390 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658'n'8851'm'8801'm_6390 = erased
-- Data.Nat.Properties.n⊔m≡m⇒n≤m
d_n'8852'm'8801'm'8658'n'8804'm_6392 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_n'8852'm'8801'm'8658'n'8804'm_6392
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.n⊔m≡n⇒m≤n
d_n'8852'm'8801'n'8658'm'8804'n_6394 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_n'8852'm'8801'n'8658'm'8804'n_6394
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.n≤m⊔n
d_n'8804'm'8852'n_6396 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_n'8804'm'8852'n_6396
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊔-least
d_'8852''45'least_6398 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8852''45'least_6398
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-greatest
d_'8851''45'greatest_6400 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'greatest_6400
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊔-pres-≤m
d_'8852''45'pres'45''8804'm_6402 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8852''45'pres'45''8804'm_6402
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8852''45'operator_4226 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Nat.Properties.⊓-pres-m≤
d_'8851''45'pres'45'm'8804'_6404 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8851''45'pres'45'm'8804'_6404
  = let v0 = d_'8804''45'totalPreorder_2632 in
    let v1 = d_'8851''45'operator_4224 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe v0) (coe v1)
-- Data.Nat.Properties.⊔-abs-⊓
d_'8852''45'abs'45''8851'_6406 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'abs'45''8851'_6406 = erased
-- Data.Nat.Properties.⊓-abs-⊔
d_'8851''45'abs'45''8852'_6408 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'abs'45''8852'_6408 = erased
-- Data.Nat.Properties.suc[pred[n]]≡n
d_suc'91'pred'91'n'93''93''8801'n_6412 ::
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_suc'91'pred'91'n'93''93''8801'n_6412 = erased
-- Data.Nat.Properties.≤-step
d_'8804''45'step_6418 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'step_6418 ~v0 ~v1 v2 = du_'8804''45'step_6418 v2
du_'8804''45'step_6418 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'step_6418 v0 = coe v0
-- Data.Nat.Properties.≤-stepsˡ
d_'8804''45'steps'737'_6420 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'steps'737'_6420 ~v0 ~v1 ~v2 v3
  = du_'8804''45'steps'737'_6420 v3
du_'8804''45'steps'737'_6420 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'steps'737'_6420 v0 = coe v0
-- Data.Nat.Properties.≤-stepsʳ
d_'8804''45'steps'691'_6422 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8804''45'steps'691'_6422 ~v0 ~v1 ~v2 v3
  = du_'8804''45'steps'691'_6422 v3
du_'8804''45'steps'691'_6422 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8804''45'steps'691'_6422 v0 = coe v0
-- Data.Nat.Properties.<-step
d_'60''45'step_6424 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'step_6424 v0 v1 v2
  = coe du_m'60'n'8658'm'60'1'43'n_2906 v2
