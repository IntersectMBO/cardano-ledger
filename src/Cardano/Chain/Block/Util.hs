{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Util
       ( checkBodyProof
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (..))
import           Formatting (build, sformat, (%))
import qualified Formatting.Buildable as B


checkBodyProof
  :: (B.Buildable bodyProof, Eq bodyProof, MonadError Text m)
  => (body -> bodyProof)
  -> body
  -> bodyProof
  -> m ()
checkBodyProof mkProof body proof = do
  let calculatedProof = mkProof body
  let
    errMsg = sformat
      ( "Incorrect proof of body. "
      % "Proof in header: "
      % build
      % ", calculated proof: "
      % build
      )
      proof
      calculatedProof
  unless (calculatedProof == proof) $ throwError errMsg
