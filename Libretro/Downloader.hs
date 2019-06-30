{-# LANGUAGE LambdaCase, DeriveGeneric, TupleSections #-}
module Libretro.Downloader where

import           Debug.Trace
import           Libretro.Core
import           System.FilePath
import           System.IO
import           System.Info
import           System.Process
import           Text.Printf

data Backend = FBA | FCEumm | Snes9x | Nestopia | Stella
  -- VBAM | VECX | BNES | FUSE | Mednafen | Higan | PicoDrive | Gambatte
  deriving (Bounded, Enum, Show)

getCoreSystemInfo :: Backend -> RetroSystemInfo
getCoreSystemInfo = \case
  FBA -> RetroSystemInfo {
           library_name = "FB Alpha 2012"
         , library_version = "v0.2.97.29"
         , valid_extensions = ["iso", "zip", "7z"]
         , need_fullpath = True
         , block_extract = True }
  FCEumm -> RetroSystemInfo {
              library_name = "FCEUmm"
            , library_version = "(SVN)"
            , valid_extensions = ["fds", "nes", "unf", "unif"]
            , need_fullpath = False
            , block_extract = False }
  Snes9x -> RetroSystemInfo {
              library_name = "Snes9x"
            , library_version = "1.54.1"
            , valid_extensions = ["smc", "sfc", "swc", "fig"]
            , need_fullpath = False
            , block_extract = False }
  Stella -> RetroSystemInfo {
              library_name = "Stella"
            , library_version = "3.9.3"
            , valid_extensions = ["a26", "bin"]
            , need_fullpath = False
            , block_extract = False }
  Nestopia -> RetroSystemInfo {
                library_name = "Snes9x"
              , library_version = "1.54.1"
              , valid_extensions = ["smc", "sfc", "swc", "fig"]
              , need_fullpath = False
              , block_extract = False }
  -- Gambatte -> RetroSystemInfo {
  --               library_name = "Gambatte"
  --             , library_version = "v0.5.0-netlink"
  --             , valid_extensions = ["gb","gbc","dmg"]
  --             , need_fullpath = False
  --             , block_extract = False }

toBoundedMap :: (Bounded k, Enum k) => (k -> a) -> [(k, a)]
toBoundedMap f = fmap (\k -> (k, f k)) [minBound..maxBound]

getBackendFromExt :: String -> Maybe Backend
getBackendFromExt ext = lookup ext $ concat $
  fmap (\(k, RetroSystemInfo _ _ exts _ _) -> fmap (, k) exts) $
    toBoundedMap getCoreSystemInfo

getCorePath :: Backend -> IO (Either String FilePath)
getCorePath backend =
  withCreateProcess (proc "nix-build"
                              [ "-A"
                              , printf "libretro.%s" attr
                              , "<nixpkgs>"
                              , "--no-out-link", "--no-gc-warning" ])
                        { std_out = CreatePipe } $ \_ (Just hout) _ _ -> do
    ineof <- hIsEOF hout
    if ineof then pure $ Left "EOF"
    else do
      drv <- hGetLine hout
      pure $ Right $ drv </> "lib" </> "retroarch" </> "cores"
                         </> (attr <> "_libretro") <.> sharedExt
  where
    sharedExt = case os of
      "darwin" -> "dylib"
      "windows" -> "dll"
      _ -> "so"

    attr = getCoreAttr backend
    getCoreAttr = \case
      FBA -> "fba"
      FCEumm -> "fceumm"
      Snes9x -> "snes9x"
      Stella -> "stella"
      Nestopia -> "nestopia"
      -- Gambatte -> "gambatte"
