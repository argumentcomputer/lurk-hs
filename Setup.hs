import Control.Monad
import Data.Maybe
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
  ( Args
  , UserHooks (confHook, preConf)
  , defaultMainWithHooks
  , simpleUserHooks
  )
import Distribution.Simple.Program
  ( ghcProgram
  )
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (localPkgDescr)
  , buildDir
  , compiler
  )
import Distribution.Simple.Setup
  ( BuildFlags (buildVerbosity)
  , ConfigFlags (configVerbosity)
  , CleanFlags(..)
  , fromFlag
  )
import Distribution.Simple.UserHooks
  ( UserHooks (buildHook, confHook, cleanHook)
  )
import Distribution.Simple.Utils
  ( notice
  , copyFileVerbose
  , createDirectoryIfMissingVerbose
  , rawSystemExit
  , info
  )
import System.Directory (getCurrentDirectory)
import Distribution.Text (display)
import Distribution.Simple.Compiler
import Distribution.System (buildPlatform, buildOS, OS(..))
import Distribution.Simple.BuildPaths
  ( mkGenericSharedBundledLibName
  , mkGenericStaticLibName
  , dllExtension
  )

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks { buildHook = rustBuildHook }

rustBuildHook
  :: PD.PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
rustBuildHook description localBuildInfo hooks flags = do
  -- run Rust build
  -- FIXME add cargo and rust to program db during configure step
  -- FIXME: add `--target $TARGET` flag to support cross-compiling to $TARGET
  notice verbosity "Call `cargo build --release` to build a dependency written in Rust"
  rawSystemExit (fromFlag $ buildVerbosity flags) "cargo"
    [ "build"
    , "--release"
    , "--target-dir"
    , rustTargetDir
    ]
  -- rawSystemExit (fromFlag $ buildVerbosity flags) "cargo"
  --   [ "+nightly"
  --   , "build"
  --   , "--release"
  --   , "--target-dir"
  --   , rustTargetDir
  --   , "--out-dir"
  --   , targetBuildDir
  --   , "-Z"
  --   , "unstable-options"
  --   ]

  -- Install build results into cabal build directory
  createDirectoryIfMissingVerbose verbosity True targetBuildDir
  copyFileVerbose verbosity staticSource staticTarget
  copyFileVerbose verbosity dynSource dynTarget
  -- addLibraryPath buildPlatform
  when (buildOS == OSX) $
    rawSystemExit verbosity "install_name_tool"
        [ "-id"
        , "@rpath/" <> mkGenericSharedBundledLibName buildPlatform (compilerId c) targetLibname
        , dynTarget
        ]

  info verbosity "rustc compilation succeeded"
  buildHook simpleUserHooks description localBuildInfo hooks flags
 where
  verbosity = fromFlag $ buildVerbosity flags
  c = compiler localBuildInfo

  sourceLibname = "plonk_verify"
  targetLibname = "C" <> sourceLibname
  rustTargetDir = buildDir localBuildInfo <> "/rust-target"
  sourceBuildDir = rustTargetDir <> "/release"
  targetBuildDir = buildDir localBuildInfo
  staticSource = sourceBuildDir <> "/" <> mkGenericStaticLibName sourceLibname
  staticTarget = targetBuildDir <> "/" <> mkGenericStaticLibName targetLibname
  dynSource = sourceBuildDir <> "/" <> "lib" <> sourceLibname <> "." <> dllExtension buildPlatform
  dynTarget = targetBuildDir <> "/" <> mkGenericSharedBundledLibName buildPlatform (compilerId c) targetLibname

