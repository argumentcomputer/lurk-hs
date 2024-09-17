{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This library is not tied to a particular VM key. But it provides tools that
-- help users to manage VM keys in their applications.

module PlonkBn254.Verify
( VMKey(..)
, Proof(..)
, ProgramId(..)
, PublicParameter(..)
, PublicParameterHash(..)
, mkPublicParameterHash
, verify
, verifyPrehashed
) where

import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Short qualified as BS

import Foreign.C.String
import Foreign.C.Types

import Language.Haskell.TH.Syntax (Lift)

import System.IO.Temp
import Data.Bits

-- -------------------------------------------------------------------------- --
-- Plonk Verifier FFI

foreign import ccall safe "__c_verify_plonk_bn254"
    verify_plonk_bn254 :: CString -> CString -> CString -> CString -> IO (CUInt)

foreign import ccall safe "__c_verify_plonk_bn254_prehashed"
    verify_plonk_bn254_prehashed :: CString -> CString -> CString -> CString -> IO (CUInt)

-- -------------------------------------------------------------------------- --
-- API Types

-- | The VM key identifies the circuit of the VM that is used to execute and
-- proof the program. Intuitively, one can think of the key as a version
-- identifier of the VM.
--
-- When the VM circuit is updated the key changes. Applications should record
-- what key a proofs depends on for verification. The module "EmbedVMKeys"
-- provides tools for embedding existing keys from files into applications
-- binaries.
--
newtype VMKey = VMKey
    { _vmKey :: B.ByteString }
    deriving (Show, Eq, Ord)
    deriving (Lift)

-- | The actual proof. This is opaque cryptographic object that allows the
-- verifier to establish that a program was executed with a particular list of
-- public parameters in the context of the VM that is idendified with a
-- particular VM key.
--
newtype Proof = Proof
    { _proof :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

-- | A unique identifier of a program.
--
newtype ProgramId = ProgramId
    { _programId :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

-- | The public parameters of a program invocation.
--
newtype PublicParameter = PublicParameter
    { _publicParameter :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

-- | The hash of the public parameters of a program invocation.
--
newtype PublicParameterHash = PublicParameterHash
    { _publicParameterHash :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

-- | Helper function that checks some invariants on the format of public
-- parameters digest. It is valid only for Plonk over curve BN254.
--
mkPublicParameterHash :: BS.ShortByteString -> Either String PublicParameterHash
mkPublicParameterHash bytes
    | BS.length bytes /= 32 = Left $ "wrong length; expected: 32, actual: " <> show (BS.length bytes)
    | BS.head bytes .&. 0xe0 /= 0 = Left "first three bit are not set to 0"
    | otherwise = Right $ PublicParameterHash bytes

-- -------------------------------------------------------------------------- --
-- Plonk Verifier API

-- | Verify the claim that the program with the given id was invoked with the
-- given list of public parameters.
--
verify
    :: VMKey
        -- ^ The VM key in bytes. This key represents the particular version of
        -- the verifier. It must match the respective version of the prover that
        -- was used to generate the proof. Otherwise verification fails.
    -> Proof
        -- ^ The proof object for the invocation of program with the respective
        -- public parameters.
    -> ProgramId
        -- ^ The program identifier. A program is valid only in the context of a
        -- particular VM key. It also the number and types of public parameters
        -- are well defined.
    -> PublicParameter
        -- ^ The public parameters of the program execution. The encoding of the
        -- parameters depends on the program.
    -> IO Bool
verify (VMKey vk) (Proof proof) (ProgramId pid) (PublicParameter params) = do
    withSystemTempDirectory "plonk-verifier" $ \path -> do
        B.writeFile (path <> "/" <> "vk.bin") vk
        withCString path $ \cpath ->
            useAsHexCString (proof) $ \cproof ->
                useAsHexCString pid $ \cpid ->
                    useAsHexCString params $ \cparams ->
                        (== 1) <$> verify_plonk_bn254 cpath cproof cpid cparams
  where
    useAsHexCString = B.useAsCString . B16.encode . BS.fromShort

-- | Verify the claim that the program with the given id was invoked with the
-- list of public parameters with the given digest.
--
verifyPrehashed
    :: VMKey
        -- ^ The VM key in bytes. This key represents the particular version of
        -- the verifier. It must match the respective version of the prover that
        -- was used to generate the proof. Otherwise verification fails.
    -> Proof
        -- ^ The proof object for the invocation of program with the respective
        -- public parameters.
    -> ProgramId
        -- ^ The program identifier. A program is valid only in the context of a
        -- particular VM key. It also the number and types of public parameters
        -- are well defined.
    -> PublicParameterHash
        -- ^ The digest of the public parameters as computed by
        -- 'hashPublicParameters'.
    -> IO Bool
verifyPrehashed (VMKey vk) (Proof proof) (ProgramId pid) (PublicParameterHash paramHash) = do
    withSystemTempDirectory "plonk-verifier" $ \path -> do
        B.writeFile (path <> "/" <> "vk.bin") vk
        withCString path $ \cpath ->
            useAsHexCString (proof) $ \cproof ->
                useAsHexCString pid $ \cpid ->
                    useAsHexCString paramHash $ \cparamHash ->
                        (== 1) <$> verify_plonk_bn254_prehashed cpath cproof cpid cparamHash
  where
    useAsHexCString = B.useAsCString . B16.encode . BS.fromShort

