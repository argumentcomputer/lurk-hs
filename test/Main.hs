{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Monad

import Data.Aeson

import Foreign.C.String

-- internal modules
import PlonkVerify

-- -------------------------------------------------------------------------- --
-- main

main :: IO ()
main = do
    example "fibonacci_fixture"
    example "epoch_change"
    example "inclusion_fixture"

-- --------------------------------------------------------------------------
-- Run Example

example :: String -> IO ()
example name = do
    p <- readProof name
    dataDir <- newCString "./assets"
    proof <- newCString (_proofProof p)
    vkeyHash <- newCString (_proofVKey p)
    committedValuesDigest <- newCString (_proofPublicValues p)

    rawRes <- verify_plonk_bn254
        dataDir
        proof
        vkeyHash
        committedValuesDigest

    when (rawRes == 0) $ error "Verification failed"
    return ()

-- -------------------------------------------------------------------------- --
-- Utils

data Proof = Proof
    { _proofVKey :: !String
    , _proofPublicValues :: !String
    , _proofProof :: !String
    }

instance FromJSON Proof where
    parseJSON = withObject "Proof" $ \o -> Proof
        <$> o .: "vkey"
        <*> o .: "publicValues"
        <*> o .: "proof"

readProof :: String -> IO Proof
readProof name = eitherDecodeFileStrict' ("./assets/" <> name <> ".json") >>= \case
    Left e -> fail $ "failed to load proof " <> name <> ": " <> e
    Right p -> return p

