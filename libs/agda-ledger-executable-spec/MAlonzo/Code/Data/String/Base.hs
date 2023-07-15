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

module MAlonzo.Code.Data.String.Base where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Char
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Char.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.NonEmpty.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.String.Base._≈_
d__'8776'__6 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> ()
d__'8776'__6 = erased
-- Data.String.Base._<_
d__'60'__8 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> ()
d__'60'__8 = erased
-- Data.String.Base._≤_
d__'8804'__10 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> ()
d__'8804'__10 = erased
-- Data.String.Base.head
d_head_12 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  Maybe MAlonzo.Code.Agda.Builtin.Char.T_Char_6
d_head_12
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe
         MAlonzo.Code.Data.Maybe.Base.du_map_68
         (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0))))
      (coe MAlonzo.Code.Agda.Builtin.String.d_primStringUncons_10)
-- Data.String.Base.tail
d_tail_14 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  Maybe MAlonzo.Code.Agda.Builtin.String.T_String_6
d_tail_14
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe
         MAlonzo.Code.Data.Maybe.Base.du_map_68
         (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0))))
      (coe MAlonzo.Code.Agda.Builtin.String.d_primStringUncons_10)
-- Data.String.Base.fromChar
d_fromChar_16 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_fromChar_16
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe MAlonzo.Code.Agda.Builtin.String.d_primStringFromList_14)
      (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306)
-- Data.String.Base.fromList⁺
d_fromList'8314'_18 ::
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_fromList'8314'_18
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe MAlonzo.Code.Agda.Builtin.String.d_primStringFromList_14)
      (coe MAlonzo.Code.Data.List.NonEmpty.Base.du_toList_60)
-- Data.String.Base._++_
d__'43''43'__20 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d__'43''43'__20
  = coe MAlonzo.Code.Agda.Builtin.String.d_primStringAppend_16
-- Data.String.Base.length
d_length_22 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> Integer
d_length_22 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_length_304
      (coe MAlonzo.Code.Agda.Builtin.String.d_primStringToList_12 v0)
-- Data.String.Base.replicate
d_replicate_24 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_replicate_24 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.String.d_primStringFromList_14
      (coe
         MAlonzo.Code.Data.List.Base.du_replicate_314 (coe v0) (coe v1))
-- Data.String.Base.concat
d_concat_28 ::
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_concat_28
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242 (coe d__'43''43'__20)
      (coe ("" :: Data.Text.Text))
-- Data.String.Base.intersperse
d_intersperse_30 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_intersperse_30 v0
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216 (coe d_concat_28)
      (coe MAlonzo.Code.Data.List.Base.du_intersperse_72 (coe v0))
-- Data.String.Base.unwords
d_unwords_34 ::
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_unwords_34 = coe d_intersperse_30 (coe (" " :: Data.Text.Text))
-- Data.String.Base.unlines
d_unlines_36 ::
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_unlines_36 = coe d_intersperse_30 (coe ("\n" :: Data.Text.Text))
-- Data.String.Base.parens
d_parens_38 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_parens_38 v0
  = coe
      d__'43''43'__20 ("(" :: Data.Text.Text)
      (coe d__'43''43'__20 v0 (")" :: Data.Text.Text))
-- Data.String.Base.braces
d_braces_42 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_braces_42 v0
  = coe
      d__'43''43'__20 ("{" :: Data.Text.Text)
      (coe d__'43''43'__20 v0 ("}" :: Data.Text.Text))
-- Data.String.Base._<+>_
d__'60''43''62'__46 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d__'60''43''62'__46 v0 v1
  = let v2
          = let v2
                  = coe
                      d__'43''43'__20 v0
                      (coe d__'43''43'__20 (" " :: Data.Text.Text) v1) in
            case coe v1 of
              l | (==) l ("" :: Data.Text.Text) -> coe v0
              _ -> coe v2 in
    case coe v0 of
      l | (==) l ("" :: Data.Text.Text) -> coe v1
      _ -> coe v2
-- Data.String.Base.padLeft
d_padLeft_56 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_padLeft_56 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1
              (d_length_22 (coe v2)) in
    let v4
          = coe d__'43''43'__20 (d_replicate_24 (coe v3) (coe v0)) v2 in
    case coe v3 of
      0 -> coe v2
      _ -> coe v4
-- Data.String.Base.padRight
d_padRight_82 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_padRight_82 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1
              (d_length_22 (coe v2)) in
    let v4
          = coe d__'43''43'__20 v2 (d_replicate_24 (coe v3) (coe v0)) in
    case coe v3 of
      0 -> coe v2
      _ -> coe v4
-- Data.String.Base.padBoth
d_padBoth_108 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_padBoth_108 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v2
              (d_length_22 (coe v3)) in
    let v5
          = coe
              d__'43''43'__20
              (d_replicate_24
                 (coe MAlonzo.Code.Data.Nat.Base.d_'8970'_'47'2'8971'_198 (coe v4))
                 (coe v0))
              (coe
                 d__'43''43'__20 v3
                 (d_replicate_24
                    (coe MAlonzo.Code.Data.Nat.Base.d_'8968'_'47'2'8969'_202 (coe v4))
                    (coe v1))) in
    case coe v4 of
      0 -> coe v3
      _ -> coe v5
-- Data.String.Base.Alignment
d_Alignment_140 = ()
data T_Alignment_140 = C_Left_142 | C_Center_144 | C_Right_146
-- Data.String.Base.fromAlignment
d_fromAlignment_148 ::
  T_Alignment_140 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_fromAlignment_148 v0
  = case coe v0 of
      C_Left_142 -> coe d_padRight_82 (coe ' ')
      C_Center_144 -> coe d_padBoth_108 (coe ' ') (coe ' ')
      C_Right_146 -> coe d_padLeft_56 (coe ' ')
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.String.Base.wordsByᵇ
d_wordsBy'7495'_150 ::
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6]
d_wordsBy'7495'_150 v0 v1
  = coe
      MAlonzo.Code.Data.List.Base.du_map_22
      (coe MAlonzo.Code.Agda.Builtin.String.d_primStringFromList_14)
      (coe
         MAlonzo.Code.Data.List.Base.du_wordsBy'7495'_708 v0
         (coe MAlonzo.Code.Agda.Builtin.String.d_primStringToList_12 v1))
-- Data.String.Base.wordsBy
d_wordsBy_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6]
d_wordsBy_158 ~v0 ~v1 v2 = du_wordsBy_158 v2
du_wordsBy_158 ::
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6]
du_wordsBy_158 v0
  = coe
      d_wordsBy'7495'_150
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.String.Base.words
d_words_162 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6]
d_words_162
  = coe
      d_wordsBy'7495'_150
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primIsSpace_14)
-- Data.String.Base.linesByᵇ
d_linesBy'7495'_166 ::
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6]
d_linesBy'7495'_166 v0 v1
  = coe
      MAlonzo.Code.Data.List.Base.du_map_22
      (coe MAlonzo.Code.Agda.Builtin.String.d_primStringFromList_14)
      (coe
         MAlonzo.Code.Data.List.Base.du_linesBy'7495'_672 v0
         (coe MAlonzo.Code.Agda.Builtin.String.d_primStringToList_12 v1))
-- Data.String.Base.linesBy
d_linesBy_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6]
d_linesBy_174 ~v0 ~v1 v2 = du_linesBy_174 v2
du_linesBy_174 ::
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6]
du_linesBy_174 v0
  = coe
      d_linesBy'7495'_166
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.String.Base.lines
d_lines_178 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6]
d_lines_178
  = coe
      d_linesBy'7495'_166
      (coe MAlonzo.Code.Data.Char.Base.d__'8776''7495'__14 (coe '\n'))
