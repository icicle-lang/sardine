{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Sardine.Compiler.Data where

import           Control.Exception (finally)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Text (Text)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P hiding (mod)

import           Sardine.Compiler.Error
import           Sardine.Compiler.Module
import           Sardine.Compiler.Monad
import           Sardine.Haskell.Pretty
import           Sardine.Pretty (Doc, line)

import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, copyFile)
import           System.Exit (ExitCode(..))
import           System.FilePath (FilePath, (</>))
import           System.IO (IO)
import           System.IO.Temp (createTempDirectory)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process (CreateProcess(cwd), createProcess, waitForProcess, shell)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Sardine.Arbitrary

import           Text.Shakespeare.Text (sbt)

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (hoistEither, left)


prop_no_compile_errors (ThriftIDL p) =
  testIO . testEitherT (\x -> "\n" <> renderTestError x) $ do
    mod <- firstT CompilerError . hoistEither . runCompiler $
      moduleOfProgram "generated.thrift" p
    src <- firstT PrettyError . hoistEither $
      ppModule mod

    -- Uncomment to debug tests
    -- liftIO $ T.putStrLn . T.pack $ show src

    dir <- createEnv
    liftIO $ createDirectoryIfMissing False (dir </> "src")
    liftIO $ T.writeFile (dir </> "src/Generated.hs") (T.pack (show src))
    mafia dir src
    return True

data TestError =
    MafiaFailed Int Doc
  | CompilerError (CompilerError X)
  | PrettyError PrettyError
    deriving (Show)

renderTestError :: TestError -> Text
renderTestError = \case
  MafiaFailed code doc ->
    "Mafia failed (exit code: " <> T.pack (show code) <> ") when compiling the following code:" <>
    T.pack (show doc)
  PrettyError perr ->
    renderPrettyError perr
  CompilerError cerr ->
    "Failed to compile Thrift IDL: " <> T.pack (show cerr)

mafia :: FilePath -> Doc -> EitherT TestError IO ()
mafia dir src = do
  (Nothing, Nothing, Nothing, h) <-
    liftIO . createProcess $ (shell "./mafia build") { cwd = Just dir }
  result <- liftIO $ waitForProcess h
  case result of
    ExitFailure code ->
      left $ MafiaFailed code (line <> src)
    ExitSuccess ->
      return ()

envRef :: IORef (Maybe FilePath)
envRef =
  unsafePerformIO (newIORef Nothing)

createEnv :: MonadIO m => m FilePath
createEnv = liftIO $ do
  menv <- readIORef envRef
  case menv of
    Just env ->
      return env
    Nothing -> do
      let tmp = "tmp"
      createDirectoryIfMissing False tmp
      dir <- createTempDirectory tmp "sardine-"
      writeIORef envRef (Just dir)
      createDirectoryIfMissing False (dir </> "src")
      createDirectoryIfMissing False (dir </> "main")
      copyFile "mafia" (dir </> "mafia")
      T.writeFile (dir </> "sardine-test.cabal") cabalFile
      T.writeFile (dir </> "sardine-test.submodules") submodulesFile
      T.writeFile (dir </> "main/sardine-test.hs") mainFile
      return dir

deleteEnvAfterTests :: IO a -> IO a
deleteEnvAfterTests io = let
  cleanup = do
    menv <- readIORef envRef
    case menv of
      Nothing ->
        return ()
      Just env -> do
        removeDirectoryRecursive env
        writeIORef envRef Nothing
  in
    io `finally` cleanup

mainFile :: Text
mainFile =
  [sbt|{-# OPTIONS_GHC -fno-warn-unused-imports #-}
      |import Generated
      |
      |main :: IO ()
      |main =
      |  return ()
      |]

submodulesFile :: Text
submodulesFile =
  "sardine-runtime"

cabalFile :: Text
cabalFile =
  [sbt|name:                  sardine-test
      |version:               0.0.1
      |license:               AllRightsReserved
      |cabal-version:         >= 1.8
      |build-type:            Simple
      |
      |executable sardine-test
      |  ghc-options:
      |                    -Wall -O0
      |
      |  hs-source-dirs:
      |                    src
      |
      |  main-is:
      |                    ../main/sardine-test.hs
      |
      |  build-depends:
      |                      base                            >= 3          && < 5
      |                    , ambiata-sardine-runtime
      |                    , bytestring                      == 0.10.*
      |                    , containers                      == 0.5.*
      |                    , hybrid-vectors                  == 0.2.*
      |                    , text                            == 1.2.*
      |                    , vector                          == 0.11.*
      |]

return []
tests :: IO Bool
tests =
  deleteEnvAfterTests $
    $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 30, maxSize = 20 })
