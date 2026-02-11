{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Incremental Blake2b-256 hashing.
--
-- TODO: delete once we bump @cardano-crypto-class@ to @>=2.3@
module Cardano.Ledger.Shelley.Crypto.Blake2b (
  Blake2bContext,
  withBlake2bHash,
  blake2bUpdate,
) where

import Cardano.Crypto.Hash (Blake2b_256)
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Crypto.Libsodium.C (
  CRYPTO_BLAKE2B_256_STATE_SIZE,
  c_crypto_generichash_blake2b_final,
  c_crypto_generichash_blake2b_init,
  c_crypto_generichash_blake2b_update,
 )
import Cardano.Crypto.Libsodium.MLockedBytes.Internal (
  MLockedSizedBytes,
  mlsbFinalize,
  mlsbNew,
  mlsbUseAsSizedPtr,
 )
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Foreign.C.Error (errnoToIOError, getErrno)
import Foreign.C.Types (CULLong)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, nullPtr)
import GHC.IO.Exception (ioException)

-- | Opaque incremental Blake2b-256 hash context.
newtype Blake2bContext = Blake2bContext (MLockedSizedBytes CRYPTO_BLAKE2B_256_STATE_SIZE)

-- | Run an action that feeds data into a Blake2b-256 context, then finalize
-- and return both the action's result and the computed hash.
--
-- The context is allocated before the callback and securely freed afterwards.
withBlake2bHash ::
  MonadIO m =>
  (Blake2bContext -> m a) ->
  m (a, Hash.Hash Blake2b_256 b)
withBlake2bHash f = do
  ctx <- blake2bInit
  res <- f ctx
  h <- blake2bFinalize ctx
  pure (res, h)

-- | Feed a 'BS.ByteString' chunk into the hash context.
blake2bUpdate :: MonadIO m => Blake2bContext -> BS.ByteString -> m ()
blake2bUpdate (Blake2bContext mlsb) chunk = liftIO $
  mlsbUseAsSizedPtr mlsb $ \sizedPtr ->
    BS.useAsCStringLen chunk $ \(inptr, inlen) -> do
      res <-
        c_crypto_generichash_blake2b_update
          sizedPtr
          (castPtr inptr)
          (fromIntegral @Int @CULLong inlen)
      unless (res == 0) $ do
        errno <- getErrno
        ioException $
          errnoToIOError "blake2bUpdate: c_crypto_generichash_blake2b_update" errno Nothing Nothing

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

blake2bInit :: MonadIO m => m Blake2bContext
blake2bInit = liftIO $ do
  mlsb <- mlsbNew @CRYPTO_BLAKE2B_256_STATE_SIZE
  mlsbUseAsSizedPtr mlsb $ \sizedPtr -> do
    res <-
      c_crypto_generichash_blake2b_init
        sizedPtr
        nullPtr -- no key
        0 -- key length
        32 -- output length (Blake2b-256)
    unless (res == 0) $ do
      errno <- getErrno
      ioException $
        errnoToIOError "blake2bInit: c_crypto_generichash_blake2b_init" errno Nothing Nothing
  pure $ Blake2bContext mlsb

blake2bFinalize :: MonadIO m => Blake2bContext -> m (Hash.Hash Blake2b_256 a)
blake2bFinalize (Blake2bContext mlsb) = liftIO $ do
  hashBytes <- allocaBytes 32 $ \outptr ->
    mlsbUseAsSizedPtr mlsb $ \sizedPtr -> do
      res <-
        c_crypto_generichash_blake2b_final
          sizedPtr
          (castPtr outptr)
          32
      unless (res == 0) $ do
        errno <- getErrno
        ioException $
          errnoToIOError "blake2bFinalize: c_crypto_generichash_blake2b_final" errno Nothing Nothing
      BS.packCStringLen (castPtr outptr, 32)
  mlsbFinalize mlsb
  case Hash.hashFromBytes hashBytes of
    Just h -> pure h
    Nothing ->
      ioException $
        userError "blake2bFinalize: unexpected hash size mismatch"
