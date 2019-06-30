{-# LANGUAGE LambdaCase #-}
module Properties where

import           Prelude hiding (init)

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Default
import           Libretro.CFFI
import qualified Libretro.Core as Libretro
import           Libretro.Core hiding (run)
import           Libretro.Downloader
import           System.FilePath
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

instance Arbitrary Backend where arbitrary = arbitraryBoundedEnum

getCoreDefaultRegion :: Backend -> RetroRegion
getCoreDefaultRegion _ = NTSC

data Game = Suite240p | NWarpDaisakusen | SuperBossGaiden | SheepItUp
          | GunmanClive | Infinity | MonaAndTheWitchHat | TobuTobuGirl
  deriving (Bounded, Enum, Show)

instance Arbitrary Game where arbitrary = arbitraryBoundedEnum

getCoreDefaultSystemAVInfo :: Backend -> RetroSystemAVInfo
getCoreDefaultSystemAVInfo = \case
  FBA -> RetroSystemAVInfo
              (RetroGameGeometry {
                           base_width = 256
                         , base_height = 224
                         , max_width = 512
                         , max_height = 478
                         , aspect_ratio = 1.3061225 })
              (RetroSystemTiming 60.098811862348406 32040.0)
  FCEumm -> RetroSystemAVInfo
              (RetroGameGeometry {
                           base_width = 256
                         , base_height = 224
                         , max_width = 512
                         , max_height = 478
                         , aspect_ratio = 1.3061225 })
              (RetroSystemTiming 60.098811862348406 32040.0)
  Snes9x -> RetroSystemAVInfo
              (RetroGameGeometry {
                           base_width = 256
                         , base_height = 224
                         , max_width = 512
                         , max_height = 478
                         , aspect_ratio = 1.3061225 })
              (RetroSystemTiming 60.098811862348406 32040.0)
  Stella -> RetroSystemAVInfo
              (RetroGameGeometry {
                           base_width = 320
                         , base_height = 210
                         , max_width = 320
                         , max_height = 256
                         , aspect_ratio = 1.3333334 })
              (RetroSystemTiming 59.91999816894531 31400.0)
  Nestopia -> RetroSystemAVInfo
                (RetroGameGeometry {
                           base_width = 256
                         , base_height = 224
                         , max_width = 512
                         , max_height = 478
                         , aspect_ratio = 1.3061225 })
                (RetroSystemTiming 60.098811862348406 32040.0)
  Gambatte -> RetroSystemAVInfo
                (RetroGameGeometry {
                   base_width = 160
                 , base_height = 144
                 , max_width = 160
                 , max_height = 144
                 , aspect_ratio = 0.0})
                (RetroSystemTiming 59.72750056960583 32768.0)

getGameBackend :: Game -> Backend
getGameBackend = \case
  Suite240p -> Snes9x
  NWarpDaisakusen -> Snes9x
  SuperBossGaiden -> Snes9x
  SheepItUp -> Stella
  -- GunmanClive -> Gambatte
  -- Infinity -> Gambatte
  -- MonaAndTheWitchHat -> Gambatte
  -- TobuTobuGirl -> Gambatte

getGamePath :: Game -> FilePath
getGamePath = \case
  Suite240p ->
    -- http://buildbot.libretro.com/assets/cores/Nintendo%20-%20Super%20Nintendo%20Entertainment%20System/240pSuite.sfc
    "./tests" </> "roms" </> "snes" </> "240pSuite.sfc"
  NWarpDaisakusen ->
    -- http://buildbot.libretro.com/assets/cores/Nintendo%20-%20Super%20Nintendo%20Entertainment%20System/N-Warp%20Daisakusen%20%28Europe%29.zip
    "./tests" </> "roms" </> "snes" </> "N-Warp Daisakusen (Europe).sfc"
  SuperBossGaiden ->
    -- http://buildbot.libretro.com/assets/cores/Nintendo%20-%20Super%20Nintendo%20Entertainment%20System/Super%20Boss%20Gaiden%20%28Japan%29.zip
    "./tests" </> "roms" </> "snes" </> "Super Boss Gaiden (J) (V1.0).sfc"
  SheepItUp ->
    -- http://buildbot.libretro.com/assets/cores/Atari%20-%202600/Sheep%20It%20Up.zip
    "./tests" </> "roms" </> "atari2600" </> "sheepitup_ntsc.a26"
  -- GunmanClive ->
  --   "./tests" </> "roms" </> "gameboy" </> "Gunman Clive (USA) (Demo).gb"
  -- Infinity ->
  --   "./tests" </> "roms" </> "gameboy" </> "Infinity (USA) (Preview).gb"
  -- MonaAndTheWitchHat ->
  --   "./tests" </> "roms" </> "gameboy" </> "Mona_And_The_Witch_Hat.gb"
  -- TobuTobuGirl ->
  --   "./tests" </> "roms" </> "gameboy" </> "tobu_tobu_girl.gb"

getGameMeta :: Game -> String
getGameMeta _ = ""

getGameInfo :: Game -> IO RetroGameInfo
getGameInfo backend =
  RetroGameInfo
    <$> pure (getGamePath backend)
    <*> B.readFile (getGamePath backend)
    <*> pure (getGameMeta backend)

getGameSystemAVInfo :: Game -> RetroSystemAVInfo
getGameSystemAVInfo = getCoreDefaultSystemAVInfo . getGameBackend

prop_loadCore :: Backend -> Property
prop_loadCore backend = monadicIO $ do
  Right path <- run $ getCorePath backend

  core <- run $ Libretro.init (undefined :: RetroCFFICore) path def
  info <- run $ Libretro.getSystemInfo core
  region <- run $ Libretro.getRegion core
  run $ Libretro.deinit core

  assert $ info == getCoreSystemInfo backend
  assert $ region == getCoreDefaultRegion backend

prop_loadGame :: Game -> Property
prop_loadGame game = monadicIO $ do
  let backend = getGameBackend game
  Right path <- run $ getCorePath backend

  core <- run $ Libretro.init (undefined :: RetroCFFICore) path def

  result <- run $ Libretro.loadGame core =<< getGameInfo game
  assert result

  avinfo <- run $ Libretro.getSystemAVInfo core
  assert $ avinfo == getGameSystemAVInfo game

  run $ replicateM_ 10 $ Libretro.run core

  run $ Libretro.deinit core

tests :: TestTree
tests = testProperties "cores" [ ("loadCore", withMaxSuccess 5 prop_loadCore)
                               , ("loadGame", withMaxSuccess 3 prop_loadGame) ]
