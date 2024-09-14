{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

-- This library is not tight to a particular verifying key. But it provides
-- tools that help users to manage verifying keys in their applications.

module PlonkVerify
( verify_plonk_bn254
, VKey(..)
, Proof(..)
, ProgramId(..)
, PublicParameter(..)
, PublicParameterHash(..)
, mkPublicParameterHash
, hashPublicParameters
, verifyPlonkBn254
, verifyPlonkBn254'
) where

import Crypto.Hash.SHA256 qualified as H

import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Short qualified as BS
import Data.Foldable

import Foreign.C.String
import Foreign.C.Types

import Language.Haskell.TH.Syntax (Lift)

import System.IO.Temp
import Data.Bits

-- -------------------------------------------------------------------------- --
-- Plonk Verifier FFI

foreign import ccall safe "__c_verify_plonk_bn254"
    verify_plonk_bn254 :: CString -> CString -> CString -> CString -> IO (CUInt)

-- -------------------------------------------------------------------------- --
-- API Types

-- | The verifying key identifies the circuit of the VM that is used to execute
-- and proof the program. Intuitively, one can think of the key as a version
-- identifier of the VM.
--
-- When the VM circuit is updated the key changes. Applications should record
-- what key a proofs depends on for verification. The module "EmbedVKey"
-- provides tools for embedding existing keys from files into applications
-- binaries.
--
newtype VKey = VKey
    { _vKey :: B.ByteString }
    deriving (Show, Eq, Ord)
    deriving (Lift)

-- | The actual proof. This is opaque cryptographic object that allows the
-- verifier to establish that a program was executed with a particular list of
-- public parameters in the context of the VM that is idendified with a
-- particular verifying key.
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

mkPublicParameterHash :: BS.ShortByteString -> Either String PublicParameterHash
mkPublicParameterHash bytes
    | BS.length bytes /= 32 = Left $ "wrong length; expected: 32, actual: " <> show (BS.length bytes)
    | BS.head bytes .&. 0xe0 /= 0 = Left "first three bit are not set to 0"
    | otherwise = Right $ PublicParameterHash bytes

-- | Compute the 'PublicParameterHash' from a list of 'PublicParameter's.
--
-- In the context of verification the public parameters are just the
-- concatenation of the bytes that the program outputs.
--
-- The digest is computed as SHA256 hash with the first three bits set to 0.
--
hashPublicParameters :: Foldable f => f PublicParameter -> PublicParameterHash
hashPublicParameters l = PublicParameterHash
    $ BS.toShort
    $ clearBits
    $ H.finalize
    $ H.updates H.init
    $ fmap (BS.fromShort . _publicParameter)
    $ toList
    $ l
  where
    clearBits x = B.cons (0x1f .&. B.head x) (B.tail x)

-- -------------------------------------------------------------------------- --
-- Plonk Verifier API

-- | Verify the claim that the program with the given id was invoked with the
-- given list of public parameters.
--
verifyPlonkBn254
    :: VKey
        -- ^ The verifying key in bytes. This key represents the particular
        -- version of the verifier. It must match the respective version of the
        -- prover that was used to generate the proof. Otherwise verification
        -- fails.
    -> Proof
        -- ^ The proof object for the invocation of program with the respective
        -- public parameters.
    -> ProgramId
        -- ^ The program identifier. A program is valid only in the context of a
        -- particular verifying key. It also the number and types of public
        -- parameters are well defined.
    -> [PublicParameter]
        -- ^ The list of public parameters. Note, that the order of the
        -- parameter matters. The number and types of parameters is determined
        -- by the program id.
        --
        -- The actual encoding of the parameters depends on the program.
    -> IO Bool
verifyPlonkBn254 vk proof pid params =
    verifyPlonkBn254' vk proof pid (hashPublicParameters params)

-- | Verify the claim that the program with the given id was invoked with the
-- list of public parameters with the given digest.
--
verifyPlonkBn254'
    :: VKey
        -- ^ The verifying key in bytes. This key represents the particular
        -- version of the verifier. It must match the respective version of the
        -- prover that was used to generate the proof. Otherwise verification
        -- fails.
    -> Proof
        -- ^ The proof object for the invocation of program with the respective
        -- public parameters.
    -> ProgramId
        -- ^ The program identifier. A program is valid only in the context of a
        -- particular verifying key. It also the number and types of public
        -- parameters are well defined.
    -> PublicParameterHash
        -- ^ The digest of the public parameters as computed by
        -- 'hashPublicParameters'.
    -> IO Bool
verifyPlonkBn254' (VKey vk) (Proof proof) (ProgramId pid) (PublicParameterHash paramHash) = do
    withSystemTempDirectory "plonk-verifier" $ \path -> do
        B.writeFile (path <> "/" <> "vk.bin") vk
        withCString path $ \cpath ->
            useAsHexCString (proof) $ \cproof ->
                useAsHexCString pid $ \cpid ->
                    useAsHexCString paramHash $ \cparamHash ->
                        (== 1) <$> verify_plonk_bn254 cpath cproof cpid cparamHash
  where
    useAsHexCString = B.useAsCString . B16.encode . BS.fromShort

