{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conformance.Spec.Dijkstra.Receiving (spec) where

import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (SJust))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), Ptr, StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxBody (receivingKeyHashes, receivingScriptHashes)
import Cardano.Ledger.Val (inject)
import Data.Either (isLeft)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
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
  prop "body-local grouped domains and pointers agree with independently extracted Agda" $
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
              indexed = zip [0 ..] scriptHashes
             in
              conjoin
                [ Set.fromList specScripts === Set.fromList (fmap translate scriptHashes)
                , Set.fromList specKeys === Set.fromList (fmap translate $ Set.toList $ receivingKeyHashes body)
                , conjoin
                    [ Agda.receivingPointer specTx (translate sh)
                        === Just (Agda.Receive, toInteger ix)
                        .&&. redeemerPointer @DijkstraEra body (DijkstraReceiving (AsItem sh))
                        === SJust (DijkstraReceiving (AsIx ix))
                        .&&. redeemerPointerInverse @DijkstraEra body (DijkstraReceiving (AsIx ix))
                        === SJust (DijkstraReceiving (AsIxItem ix sh))
                    | (ix, sh) <- indexed
                    ]
                ]
  prop "child Receiving domains use the child's outputs" $
    \(targets :: [(Bool, Credential Payment, Maybe (Credential Staking))]) ->
      let
        body =
          mkBasicTxBody @DijkstraEra @SubTx
            & outputsTxBodyL .~ StrictSeq.fromList (fmap mkOutput (targetVariants targets))
        tx = mkBasicTx body
       in
        case runSpecTransM () (toSpecRep @DijkstraEra tx) of
          Left err -> counterexample (show err) False
          Right specTx ->
            let
              Agda.MkHSSet specScripts = Agda.subReceivingScriptHashes specTx
              Agda.MkHSSet specKeys = Agda.subReceivingKeyHashes specTx
              scripts = Set.toAscList $ receivingScriptHashes body
             in
              conjoin
                [ Set.fromList specScripts === Set.fromList (fmap translate scripts)
                , Set.fromList specKeys === Set.fromList (fmap translate $ Set.toList $ receivingKeyHashes body)
                , conjoin
                    [ Agda.subReceivingPointer specTx (translate sh) === Just (Agda.Receive, ix)
                    | (ix, sh) <- zip [0 ..] scripts
                    ]
                ]
  where
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
