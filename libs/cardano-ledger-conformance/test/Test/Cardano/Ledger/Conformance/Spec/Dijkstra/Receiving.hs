{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conformance.Spec.Dijkstra.Receiving (spec) where

import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), Ptr, StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxBody (
  receivingKeyHashes,
  receivingScriptHashes,
  receivingScriptTargets,
 )
import Cardano.Ledger.Val (inject)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance (SpecTranslate (..), runSpecTransM)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

spec :: Spec
spec = describe "Receiving executable specification" $ do
  prop "protection changes identity and preserves payment and stake" $
    \(credential :: Credential Payment) (stake :: Maybe (Credential Staking)) ->
      let
        reference = maybe StakeRefNull StakeRefBase stake
        ordinary = translate (Addr Testnet credential reference)
        protected = translate (AddrProtected Testnet credential reference)
       in
        case (ordinary, protected) of
          (Left a, Left b) ->
            conjoin
              [ property (a /= b)
              , Agda.basePay a === Agda.basePay b
              , Agda.baseStake a === Agda.baseStake b
              , Agda.baseProtected a === False
              , Agda.baseProtected b === True
              ]
          _ -> counterexample "Shelley addresses must translate to BaseAddr" False
  prop "protected stake pointers cannot silently enter the model" $
    \(credential :: Credential Payment) (pointer :: Ptr) ->
      property $
        isLeft $
          runSpecTransM () $
            toSpecRep @DijkstraEra (AddrProtected Testnet credential (StakeRefPtr pointer))
  prop "body-local output domains and raw pointers agree with independently extracted Agda" $
    \(targets :: [(Bool, Credential Payment, Maybe (Credential Staking))]) ->
      let
        outputs = fmap mkOutput (targetVariants targets)
        body = mkBasicTxBody @DijkstraEra @TopTx & outputsTxBodyL .~ StrictSeq.fromList outputs
        tx = mkBasicTx body
       in
        case runSpecTransM () (toSpecRep @DijkstraEra tx) of
          Left err -> counterexample (show err) False
          Right specTx ->
            let
              Agda.MkHSSet specScripts = Agda.receivingScriptHashes specTx
              Agda.MkHSSet specKeys = Agda.receivingKeyHashes specTx
              scriptHashes = Set.toAscList $ receivingScriptHashes body
              indexed = indexedScriptOutputs outputs
              Agda.MkHSSet specOutputs = Agda.receivingOutputs specTx
             in
              conjoin
                [ Set.fromList specScripts === Set.fromList (fmap translate scriptHashes)
                , Set.fromList specKeys === Set.fromList (fmap translate $ Set.toList $ receivingKeyHashes body)
                , receivingScriptTargets body
                    === [ (i, h)
                        | (i, receivingOutput) <- indexed
                        , AddrProtected _ (ScriptHashObj h) _ <- [receivingOutput ^. addrTxOutL]
                        ]
                , Map.fromList specOutputs
                    === Map.fromList [(toInteger i, translate receivingOutput) | (i, receivingOutput) <- indexed]
                , conjoin
                    [ Agda.receivingPointer specTx (toInteger i)
                        === Just (Agda.Receive, toInteger i)
                        .&&. redeemerPointer @DijkstraEra body (DijkstraReceiving (AsItem i))
                        === SJust (DijkstraReceiving (AsIx i))
                        .&&. redeemerPointerInverse @DijkstraEra body (DijkstraReceiving (AsIx i))
                        === SJust (DijkstraReceiving (AsIxItem i i))
                    | (i, _) <- indexed
                    ]
                , Agda.receivingPointer specTx (toInteger $ length outputs) === Nothing
                , redeemerPointerInverse @DijkstraEra body (DijkstraReceiving (AsIx (fromIntegral $ length outputs)))
                    === SNothing
                , conjoin
                    [ Agda.receivingPointer specTx (toInteger i)
                        === Nothing
                        .&&. redeemerPointer @DijkstraEra body (DijkstraReceiving (AsItem i))
                        === SNothing
                    | (i, _) <- zip [0 ..] outputs
                    , i `notElem` fmap fst indexed
                    ]
                ]
  prop "child Receiving domains use the child's outputs" $
    \(targets :: [(Bool, Credential Payment, Maybe (Credential Staking))]) ->
      let
        outputs = fmap mkOutput (targetVariants targets)
        body =
          mkBasicTxBody @DijkstraEra @SubTx
            & outputsTxBodyL .~ StrictSeq.fromList outputs
        tx = mkBasicTx body
       in
        case runSpecTransM () (toSpecRep @DijkstraEra tx) of
          Left err -> counterexample (show err) False
          Right specTx ->
            let
              Agda.MkHSSet specScripts = Agda.subReceivingScriptHashes specTx
              Agda.MkHSSet specKeys = Agda.subReceivingKeyHashes specTx
              scripts = Set.toAscList $ receivingScriptHashes body
              indexed = indexedScriptOutputs outputs
              Agda.MkHSSet specOutputs = Agda.subReceivingOutputs specTx
             in
              conjoin
                [ Set.fromList specScripts === Set.fromList (fmap translate scripts)
                , Set.fromList specKeys === Set.fromList (fmap translate $ Set.toList $ receivingKeyHashes body)
                , receivingScriptTargets body
                    === [ (i, h)
                        | (i, receivingOutput) <- indexed
                        , AddrProtected _ (ScriptHashObj h) _ <- [receivingOutput ^. addrTxOutL]
                        ]
                , Map.fromList specOutputs
                    === Map.fromList [(toInteger i, translate receivingOutput) | (i, receivingOutput) <- indexed]
                , conjoin
                    [ Agda.subReceivingPointer specTx (toInteger i)
                        === Just (Agda.Receive, toInteger i)
                        .&&. redeemerPointer @DijkstraEra body (DijkstraReceiving (AsItem i))
                        === SJust (DijkstraReceiving (AsIx i))
                        .&&. redeemerPointerInverse @DijkstraEra body (DijkstraReceiving (AsIx i))
                        === SJust (DijkstraReceiving (AsIxItem i i))
                    | (i, _) <- indexed
                    ]
                , Agda.subReceivingPointer specTx (toInteger $ length outputs) === Nothing
                , redeemerPointerInverse @DijkstraEra body (DijkstraReceiving (AsIx (fromIntegral $ length outputs)))
                    === SNothing
                , conjoin
                    [ Agda.subReceivingPointer specTx (toInteger i)
                        === Nothing
                        .&&. redeemerPointer @DijkstraEra body (DijkstraReceiving (AsItem i))
                        === SNothing
                    | (i, _) <- zip [0 ..] outputs
                    , i `notElem` fmap fst indexed
                    ]
                ]
  where
    indexedScriptOutputs outputs =
      [ (i, receivingOutput)
      | (i, receivingOutput) <- zip [0 ..] outputs
      , AddrProtected _ (ScriptHashObj _) _ <- [receivingOutput ^. addrTxOutL]
      ]
    targetVariants targets =
      targets
        <> reverse targets
        <> [(protected, credential, Nothing) | (protected, credential, _) <- targets]
    mkOutput (protected, credential, stake) =
      mkBasicTxOut @DijkstraEra
        ( (if protected then AddrProtected else Addr)
            Testnet
            credential
            (maybe StakeRefNull StakeRefBase stake)
        )
        (inject (Coin 1))

translate ::
  (SpecTranslate DijkstraEra a, SpecContext DijkstraEra a ~ ()) => a -> SpecRep DijkstraEra a
translate x = either (error . show) id $ runSpecTransM () (toSpecRep @DijkstraEra x)
