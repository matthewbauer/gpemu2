module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import           Data.Default
import           Data.Maybe
import           Language.Javascript.JSaddle.WebKitGTK (run)
import           Libretro.CFFI
import qualified Libretro.Core as Core
import           Libretro.Downloader
import           System.Environment
import           System.FilePath

getGameInfo :: FilePath -> IO Core.RetroGameInfo
getGameInfo path =
  Core.RetroGameInfo <$> pure path <*> B.readFile path <*> pure ""

play :: FilePath -> FilePath -> IO ()
play core game = bracket (Core.init (undefined :: RetroCFFICore) core def) Core.deinit $ \core -> do
  _ <- Core.loadGame core =<< getGameInfo game

  Core.RetroSystemAVInfo _ (Core.RetroSystemTiming fps _ )
    <- Core.getSystemAVInfo core

  -- print avinfo

  -- _ <- Core.getMemory core Core.SAVE_RAM
  -- Core.reset core

  _ <- forever $ do
    _ <- Core.run core
    threadDelay $ floor $ 1000000 / fps

main :: IO ()
main = do
  args <- getArgs

  when (length args > 1) $ error "Only one argument is necessary."

  case listToMaybe args of
    Nothing -> error "Need to provide one argument of the game path."
    Just gamePath -> doesFileExist gamePath >>= \case
      False -> error $ unwords [gamePath, "does not exist."]
      True -> case splitExtension gamePath of
        (_, "") -> error $ unwords ["No extension found for", gamePath]
        (_, ext) -> (getCorePath $ getBackendFromExt $ tail ext) >>= \case
          Left a -> error a
          Right corePath -> doesFileExist corePath >>= \case
            False -> error $ unwords [corePath, "does not exist."]
            True -> run $ play corePath gamePath
