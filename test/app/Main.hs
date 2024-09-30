{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Short qualified as BS
import Data.List (stripPrefix)
import Data.Maybe

import PlonkBn254.Verify

main :: IO ()
main = do
    -- This directory contains only the necessary artifacts for verification
    -- Alternatively, the parameters might also be present at `~/.sp1/circuits/plonk_bn254/<VERSION>`
    let circuitPathStr = "./verifier-assets/v1.0.8.2-testnet"
    vk <- VMKey <$> B.readFile (circuitPathStr <> "/vk.bin")

    -- Convert Haskell strings to CStrings
    -- Call the FFI function
    result <- verifyPrehashed vk proof progId publicInputs

    -- Print the result
    putStrLn ("Verification result: " ++ show result)

  where

    proof = Proof $ fromHex $
        "0x070ebe597360e09c92562b810cfd411a4cadac13904a6f5ef80597d31992b55611cc3d8557ad399cb5153cf87460901db1fd8dc92f16c5ea2ba1e8f357a1c5dd192120e7e19011e6e8028677870b4d59c1688fc0e729a547691ff7b9760aefbf242cfd52dbf3b98ddc003d31c8b86f2acf5f02ac891f990accba9a9c8477eb701d9f43bbd2c0566e7f48469c84a1769c17e74b8ed53602ce12f7f9a0181e9ab70548028c15e5a49611db64135b80545bf38afa4195e95d1289e20b945d7139ef1a94705f2ddbd481817316ef262053e9ee6a2419c63de7e3f339cac54bfa8c222e740ee8470ead68edb38aae2e9519fe01ce2f20707d6f81bc8643f0306c61ed1d37a65d87c876e13b3353bb42f02ab86423b4da43341e7bc5461b185a3946691d1015cb04fd6ffc38f9e7f52c92cffc3c44de7c58c9728d71e9f4c8266a831c2484b36f3ea92dd827d668dcf59ab7a0dee33f46fe289b90df06b15e4d288a5f1bee1bc0214e6a77647db6a2f4f51df8b100b1ddaef494618b3a3b0369ca18631f2d81effd2f9185afed9a0712d302b1d439c31d44a2ca3f20fd5f70a95eb30c132f39448df14f1e5311b906d22068ca160ac7d574df55605555df41208c9ab41634986f88e8af3b475ca428eb66e17f35c5d5808f358ec9884317db04d11a2d07200c82c257f1fdf6de2265f8cfd280c9728303f36dd0af7fc797b5ac232ae60000000720e4c44119a39f2303913d419ccff44f27639a1efe8634c93d82e581a4af1dee26b61ebb2d1eac5fe7e918786ced3a2a6b441dee0e8574341bb2e5bcbb27dc4f294ecca8cf684af42cecc403954747d3fb9b8ffb2a8081f2a809d3315f96bfca10d8e46e711998c3cd612849af775cb156caebcd3e2ce2bc7f6c7825dc536e310d69f3e8a506635f24f681b32fc30435c93d82ee0f93c83d7daf695d33acd87e1dbd2d55668d9a07c45d1f261173e9155bf35ce61b73becdc86ea76ca228f45a264f228a3b4f5b02022cd03bd8604a5ad62fd3a3706713bbc4f9846b31856b9608e7cdef78fc19f96cf6d5ada170bd3332f797f3a67756b83fa53e628f1db9a121042af248f08e8df4c9b046dc8807f7b514eb1dd7e6e2b6b80d060097a296cf22b56debdb623976250d281713f6e34b96ef5f71e92e748106d84675e30f9dc6000000010a0b17cbd37bd5c99d4ee00bf0a469c39494d8df9bbdbf57342e5d43e167dba20b659c2534a428edb9063ad60ddfb4867608b0520457348b535d1e5c1d040704"
    progId = ProgramId $ fromHex $
        "0x2acfe95638a7aa71feaa167c1d53339708571ba2082f000d39bedebac6c872"
    publicInputs = PublicParameterHash $ fromHex
        "0x190e5a3ed690bd3b533c087ef7339d00e25113d217f7dac4575a01b79dacd553"

fromHex :: String -> BS.ShortByteString
fromHex s = case B16.decode $ B8.pack $ fromMaybe s $ stripPrefix "0x" s of
    Left e -> error $ "failed to decode hex string: " <> e
    Right r -> BS.toShort r

