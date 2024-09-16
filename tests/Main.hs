{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
) where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Short qualified as BS
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Test.Hspec

-- internal modules

import EmbedVKeys
import PlonkVerify

-- -------------------------------------------------------------------------- --
-- main

verifyingKeyName :: String
verifyingKeyName = "vk"

verifyingKeys :: [(FilePath, VKey)]
verifyingKeys = $$(embedVKeys "bin" "verifier-assets/v1.0.8-testnet")

main :: IO ()
main = hspec $ describe "examples" $ do
    testExample "fibonacci_fixture"
    testExample "epoch_change"
    testExample "inclusion_fixture"

-- -------------------------------------------------------------------------- --
-- Orphans

deriving via HexEncoded instance FromJSON Proof
deriving via HexEncoded instance FromJSON ProgramId
deriving via HexEncoded instance FromJSON PublicParameter

instance FromJSON PublicParameterHash where
    parseJSON = parseJSON >=> \x -> case mkPublicParameterHash (_hexEncoded x) of
        Right r -> return r
        Left e -> fail $ "invalid public parameter hash bytes: " <> e

-- -------------------------------------------------------------------------- --
-- Test Proof Claims

data ProofClaim = ProofClaim
    { _claimProgramId :: !ProgramId
        -- ^ Identifies the RISC-V program that is proven. A program is valid
        -- only in the context of a particular verifying key. Each program has a
        -- well defined set of public parameters.

    , _claimPublicParameters :: !(Either PublicParameterHash PublicParameter)
        -- ^ The public parameters of the respective program. For verification
        -- the parameters are encoded and hashes.
        --
        -- In the context of this test suite a the length of used as heuristics
        -- for whether the value is a digest or a list.

    , _claimProof :: !Proof
        -- ^ The actual proof object.
    }

instance FromJSON ProofClaim where
    parseJSON = withObject "ProofClaim" $ \o -> ProofClaim
        <$> o .: "vkey"
        <*> parseParameters o
        <*> o .: "proof"
      where
        parseParameters o
            = (Left <$> o .: "publicValues")
            <|> (Right <$> o .: "publicValues")

readProof :: String -> IO ProofClaim
readProof name = eitherDecodeFileStrict' ("./assets/" <> name <> ".json") >>= \case
    Left e -> fail $ "failed to load proof " <> name <> ": " <> e
    Right p -> return p

-- --------------------------------------------------------------------------
-- Run Example

testExample :: String -> SpecWith ()
testExample name = it name $ do
    r <- runExample name
    shouldBe r True

runExample :: String -> IO Bool
runExample name = case lookup verifyingKeyName verifyingKeys of
    Just vk -> do
        p <- readProof name
        case _claimPublicParameters p of
            Left pp -> verifyPlonkBn254' vk
                (_claimProof p)
                (_claimProgramId p)
                pp
            Right pp -> verifyPlonkBn254 vk
                (_claimProof p)
                (_claimProgramId p)
                pp
    Nothing -> error $ "missing verifying key: " <> verifyingKeyName

-- -------------------------------------------------------------------------- --
-- Utils

-- | Helper for hex encodings
--
newtype HexEncoded = HexEncoded { _hexEncoded :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

instance FromJSON HexEncoded where
    parseJSON = withText "HexEncoded" $ \str -> do
        let sstr = fromMaybe str $ T.stripPrefix "0x" str
        case B16.decode (T.encodeUtf8 sstr) of
           Left e -> fail $ "decodeing hex string failed: " <> e
           Right bytes -> return (HexEncoded $ BS.toShort bytes)

