{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Incremental Blake2b hashing
--
-- TODO: upstream to @cardano-crypto-class@ once stable.
-- (see the TODO at @Cardano.Crypto.Libsodium.Hash.Class@)
module Cardano.Ledger.Crypto.Blake2b.Incremental (
  Blake2bContext,
  blake2bInit,
  blake2bUpdate,
  blake2bFinalize,
  blake2b256Finalize,
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
  mlsbNew,
  mlsbUseAsSizedPtr,
 )
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Foreign.C.Error (errnoToIOError, getErrno)
import Foreign.C.Types (CSize, CULLong)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, nullPtr)
import GHC.IO.Exception (ioException)
import GHC.TypeLits (KnownNat, Nat, natVal)

-- | Opaque incremental Blake2b hash context.
--
-- @n@ is the output hash size in bytes (e.g. 32 for Blake2b-256).
newtype Blake2bContext (n :: Nat) = Blake2bContext (MLockedSizedBytes CRYPTO_BLAKE2B_256_STATE_SIZE)

-- | Init a new Blake2b hash context with @n@ bytes of output length.
blake2bInit :: forall n m. (KnownNat n, MonadIO m) => m (Blake2bContext n)
blake2bInit = liftIO $ do
  let outLen = fromInteger @CSize $ natVal (Proxy @n)
  mlsb <- mlsbNew @CRYPTO_BLAKE2B_256_STATE_SIZE
  mlsbUseAsSizedPtr mlsb $ \sizedPtr -> do
    res <-
      c_crypto_generichash_blake2b_init
        sizedPtr
        nullPtr -- no key
        0 -- key length
        outLen -- output length
    unless (res == 0) $ do
      errno <- getErrno
      ioException $
        errnoToIOError "blake2bInit: c_crypto_generichash_blake2b_init" errno Nothing Nothing
  pure $ Blake2bContext mlsb

-- | Feed a 'ByteString' chunk into the hash context.
blake2bUpdate :: MonadIO m => Blake2bContext n -> BS.ByteString -> m ()
blake2bUpdate (Blake2bContext mlsb) chunk = liftIO $ do
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

-- | Finalize the hash context, returning the raw hash bytes.
--
-- Note: the context must not be used after calling this function.
blake2bFinalize :: forall n m. (KnownNat n, MonadIO m) => Blake2bContext n -> m BS.ByteString
blake2bFinalize (Blake2bContext mlsb) = liftIO $ do
  let outlen = fromInteger @Int $ natVal (Proxy @n)
  allocaBytes outlen $ \outptr ->
    mlsbUseAsSizedPtr mlsb $ \sizedPtr -> do
      res <-
        c_crypto_generichash_blake2b_final
          sizedPtr
          (castPtr outptr)
          (fromIntegral @Int @CSize outlen)
      unless (res == 0) $ do
        errno <- getErrno
        ioException $
          errnoToIOError "blake2bFinalize: c_crypto_generichash_blake2b_final" errno Nothing Nothing
      BS.packCStringLen (castPtr outptr, outlen)

-- | Finalize a Blake2b-256 context, returning a @Hash Blake2b_256 a@.
--
-- Note: the context must not be used after calling this function.
blake2b256Finalize :: MonadIO m => Blake2bContext 32 -> m (Hash.Hash Blake2b_256 BS.ByteString)
blake2b256Finalize ctx = do
  hashBytes <- blake2bFinalize ctx
  case Hash.hashFromBytes hashBytes of
    Just h -> pure h
    Nothing -> error "blake2b256Finalize: unexpected hash size mismatch"
