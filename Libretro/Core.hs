module Libretro.Core where

import Data.ByteString
import Data.Default
import Data.Int
import Data.Word

--
-- Retro type synonyms
--

type RetroPort = Word32

--
-- Retro enums
--

data RetroPixelFormat = RetroPixelFormat0RGB1555
                      | RetroPixelFormatXRGB8888
                      | RetroPixelFormatRGB565
  deriving (Enum, Bounded, Show, Read, Eq)
data RetroDevice = DEVICE_NONE | DEVICE_JOYPAD | DEVICE_MOUSE | DEVICE_KEYBOARD
                 | DEVICE_LIGHTGUN | DEVICE_ANALOG | DEVICE_POINTER
  deriving (Enum, Bounded, Show, Read, Eq)
data RetroRegion = NTSC | PAL
  deriving (Enum, Bounded, Show, Read, Eq)
data RetroMemoryRegion = SAVE_RAM | RTC | SYSTEM_RAM | VIDEO_RAM
  deriving (Enum, Bounded, Show, Read, Eq)
data RetroLanguage = ENGLISH | JAPANESE | FRENCH | SPANISH | GERMAN
  | ITALIAN | DUTCH | PORTUGUESE_BRAZIL | PORTUGUESE_PORTUGAL
  | RUSSIAN | KOREAN | CHINESE_TRADITIONAL | CHINESE_SIMPLIFIED
  | ESPERANTO | POLISH | VIETNAMESE | ARABIC | GREEK | TURKISH
  deriving (Enum, Bounded, Show, Read, Eq)

data RetroJoypad = JOYPAD_B | JOYPAD_Y | JOYPAD_SELECT
  | JOYPAD_START | JOYPAD_UP | JOYPAD_DOWN | JOYPAD_LEFT | JOYPAD_RIGHT
  | JOYPAD_A | JOYPAD_X | JOYPAD_L | JOYPAD_R | JOYPAD_L2 | JOYPAD_R2
  | JOYPAD_L3 | JOYPAD_R3
  deriving (Enum, Bounded, Show, Read, Eq)

data RetroAnalogIndex = ANALOG_LEFT | ANALOG_RIGHT | ANALOG_BUTTON
  deriving (Enum, Bounded, Show, Read, Eq)
data RetroAnalogId = ANALOG_X | ANALOG_Y
  deriving (Enum, Bounded, Show, Read, Eq)

data RetroMouse = MOUSE_X | MOUSE_Y | MOUSE_LEFT | MOUSE_RIGHT
  | MOUSE_WHEELUP | MOUSE_WHEELDOWN | MOUSE_MIDDLE | MOUSE_HORIZ_WHEELUP
  | MOUSE_HORIZ_WHEELDOWN | MOUSE_BUTTON_4 | MOUSE_BUTTON_5
  deriving (Enum, Bounded, Show, Read, Eq)

data RetroLightgun = LIGHTGUN_X | LIGHTGUN_Y | LIGHTGUN_TRIGGER
  | LIGHTGUN_AUX_A | LIGHTGUN_AUX_B | LIGHTGUN_PAUSE | LIGHTGUN_START
  | LIGHTGUN_SELECT | LIGHTGUN_AUX_C | LIGHTGUN_DPAD_UP | LIGHTGUN_DPAD_DOWN
  | LIGHTGUN_DPAD_LEFT | LIGHTGUN_DPAD_RIGHT
  | LIGHTGUN_SCREEN_X | LIGHTGUN_SCREEN_Y
  | LIGHTGUN_IS_OFFSCREEN | LIGHTGUN_RELOAD
  deriving (Enum, Bounded, Show, Read, Eq)

data RetroPointer = POINTER_X | POINTER_Y | POINTER_PRESSED
  deriving (Enum, Bounded, Show, Read, Eq)

data RetroKey = RETROK_UNKNOWN | RETROK_FIRST | RETROK_BACKSPACE | RETROK_TAB
  | RETROK_CLEAR | RETROK_RETURN | RETROK_PAUSE | RETROK_ESCAPE | RETROK_SPACE
  | RETROK_EXCLAIM | RETROK_QUOTEDBL | RETROK_HASH | RETROK_DOLLAR
  | RETROK_AMPERSAND | RETROK_QUOTE | RETROK_LEFTPAREN | RETROK_RIGHTPAREN
  | RETROK_ASTERISK | RETROK_PLUS | RETROK_COMMA | RETROK_MINUS
  | RETROK_PERIOD | RETROK_SLASH | RETROK_0 | RETROK_1 | RETROK_2 | RETROK_3
  | RETROK_4 | RETROK_5 | RETROK_6 | RETROK_7 | RETROK_8 | RETROK_9
  | RETROK_COLON | RETROK_SEMICOLON | RETROK_LESS | RETROK_EQUALS
  | RETROK_GREATER | RETROK_QUESTION | RETROK_AT | RETROK_LEFTBRACKET
  | RETROK_BACKSLASH | RETROK_RIGHTBRACKET | RETROK_CARET | RETROK_UNDERSCORE
  | RETROK_BACKQUOTE | RETROK_a | RETROK_b | RETROK_c | RETROK_d | RETROK_e
  | RETROK_f | RETROK_g | RETROK_h | RETROK_i | RETROK_j | RETROK_k | RETROK_l
  | RETROK_m | RETROK_n | RETROK_o | RETROK_p | RETROK_q | RETROK_r | RETROK_s
  | RETROK_t | RETROK_u | RETROK_v | RETROK_w | RETROK_x | RETROK_y | RETROK_z
  | RETROK_LEFTBRACE | RETROK_BAR | RETROK_RIGHTBRACE | RETROK_TILDE
  | RETROK_DELETE | RETROK_KP0 | RETROK_KP1 | RETROK_KP2 | RETROK_KP3
  | RETROK_KP4 | RETROK_KP5 | RETROK_KP6 | RETROK_KP7 | RETROK_KP8
  | RETROK_KP9 | RETROK_KP_PERIOD | RETROK_KP_DIVIDE | RETROK_KP_MULTIPLY
  | RETROK_KP_MINUS | RETROK_KP_PLUS | RETROK_KP_ENTER | RETROK_KP_EQUALS
  | RETROK_UP | RETROK_DOWN | RETROK_RIGHT | RETROK_LEFT | RETROK_INSERT
  | RETROK_HOME | RETROK_END | RETROK_PAGEUP | RETROK_PAGEDOWN | RETROK_F1
  | RETROK_F2 | RETROK_F3 | RETROK_F4 | RETROK_F5 | RETROK_F6 | RETROK_F7
  | RETROK_F8 | RETROK_F9 | RETROK_F10 | RETROK_F11 | RETROK_F12 | RETROK_F13
  | RETROK_F14 | RETROK_F15 | RETROK_NUMLOCK | RETROK_CAPSLOCK
  | RETROK_SCROLLOCK | RETROK_RSHIFT | RETROK_LSHIFT | RETROK_RCTRL
  | RETROK_LCTRL | RETROK_RALT | RETROK_LALT | RETROK_RMETA | RETROK_LMETA
  | RETROK_LSUPER | RETROK_RSUPER | RETROK_MODE | RETROK_COMPOSE
  | RETROK_HELP | RETROK_PRINT | RETROK_SYSREQ | RETROK_BREAK | RETROK_MENU
  | RETROK_POWER | RETROK_EURO | RETROK_UNDO | RETROK_OEM_102
  deriving (Enum, Bounded, Show, Read, Eq)

data RetroMod = MOD_NONE | MOD_SHIFT | MOD_CTRL | MOD_ALT | MOD_META
  | MOD_NUMSLOCK | MOD_CAPSLOCK | MOD_SCROLLOCK
  deriving (Enum, Bounded, Show, Read, Eq)

data RetroEnvironment = SET_ROTATION | GET_OVERSCAN | GET_CAN_DUPE
  | GET_VARIABLE_OLD | SET_VARIABLES_OLD | SET_MESSAGE | SHUTDOWN
  | SET_PERFORMANCE_LEVEL | GET_SYSTEM_DIRECTORY | SET_PIXEL_FORMAT
  | SET_INPUT_DESCRIPTORS | SET_KEYBOARD_CALLBACK | SET_DISK_CONTROL_INTERFACE
  | SET_HW_RENDER | GET_VARIABLE | SET_VARIABLES | GET_VARIABLE_UPDATE
  | SET_SUPPORT_NO_GAME | SET_AUDIO_CALLBACK_OLD
  | GET_LIBRETRO_PATH | SET_FRAME_TIME_CALLBACK | SET_AUDIO_CALLBACK
  | GET_RUMBLE_INTERFACE | GET_INPUT_DEVICE_CAPABILITIES | GET_SENSOR_INTERFACE
  | GET_CAMERA_INTERFACE | GET_LOG_INTERFACE | GET_PERF_INTERFACE
  | GET_LOCATION_INTERFACE | GET_CORE_ASSETS_DIRECTORY
  | GET_SAVE_DIRECTORY | SET_SYSTEM_AV_INFO | SET_PROC_ADDRESS_CALLBACK
  | SET_SUBSYSTEM_INFO | SET_CONTROLLER_INFO | SET_MEMORY_MAPS | SET_GEOMETRY
  | GET_USERNAME | GET_LANGUAGE | GET_CURRENT_SOFTWARE_FRAMEBUFFER
  | GET_HW_RENDER_INTERFACE | SET_SUPPORT_ACHIEVEMENTS
  | SET_HW_RENDER_CONTEXT_NEGOTATION_INTEFACE | SET_SERIALIZATION_QUIRKS
  | SET_HW_SHARED_CONTEXT | GET_VFS_INTERFACE | GET_LED_INTERFACE
  | GET_AUDIO_VIDEO_ENABLED | GET_TARGET_REFRESH_RATE
  deriving (Enum, Bounded, Show, Read, Eq)

--
-- Retro data structs
--

data RetroSystemTiming = RetroSystemTiming Double Double
  deriving (Show, Read, Eq)
data RetroGameGeometry = RetroGameGeometry
  { base_width :: Word32
  , base_height :: Word32
  , max_width :: Word32
  , max_height :: Word32
  , aspect_ratio :: Float }
  deriving (Show, Read, Eq)
data RetroSystemAVInfo = RetroSystemAVInfo RetroGameGeometry RetroSystemTiming
  deriving (Show, Read, Eq)
data RetroSystemInfo = RetroSystemInfo
  { library_name :: String
  , library_version :: String
  , valid_extensions :: [String]
  , need_fullpath :: Bool
  , block_extract :: Bool }
  deriving (Show, Read, Eq)
data RetroGameInfo = RetroGameInfo FilePath ByteString String
  deriving (Show, Read, Eq)
data RetroMessage = RetroMessage String Word32
  deriving (Show, Read, Eq)
data RetroInputDescriptor = RetroInputDescriptor
  RetroPort RetroDevice Word32 Word32
  deriving (Show, Read, Eq)
data RetroVariable = RetroVariable (String, String)
  deriving (Show, Read, Eq)
data RetroControllerDescription = RetroControllerDescription String RetroDevice
  deriving (Show, Read, Eq)
data RetroControllerInfo = RetroControllerInfo [RetroControllerDescription]
  deriving (Show, Read, Eq)
data RetroControllers = RetroControllers [RetroControllerInfo]
  deriving (Show, Read, Eq)

--
-- Main stuff
--

-- | Represents a Libretro core implementation
class RetroCore a where
  init :: a -> FilePath -> RetroCallbacks -> IO a
  deinit :: a -> IO ()
  getSystemInfo :: a -> IO RetroSystemInfo
  getSystemAVInfo :: a -> IO RetroSystemAVInfo
  setControllerPortDevice :: a -> RetroPort -> RetroDevice -> IO ()
  reset :: a -> IO ()
  run :: a -> IO ()
  serialize :: a -> IO (Maybe ByteString)
  unserialize :: a -> ByteString -> IO Bool
  cheatReset :: a -> IO ()
  cheatSet :: a -> Word32 -> Bool -> String -> IO ()
  loadGame :: a -> RetroGameInfo -> IO Bool
  loadGameSpecial :: a -> Word32 -> RetroGameInfo -> Word64 -> IO Bool
  unloadGame :: a -> IO ()
  getRegion :: a -> IO RetroRegion
  getMemory :: a -> RetroMemoryRegion -> IO ByteString

-- | Callbacks to Haskell from Libretro Core
data RetroCallbacks = RetroCallbacks
  { videoRefresh :: ByteString -> Word32 -> Word32 -> IO ()
  , audioSample :: Int16 -> Int16 -> IO ()
  , audioSampleBatch :: ByteString -> IO Word64
  , inputPoll :: IO ()
  , inputState :: RetroPort -> RetroDevice -> Word32 -> Word32 -> IO Int16

  , setRotation :: Word32 -> IO ()
  , getOverscan :: IO Bool
  , getCanDupe :: IO Bool
  , setMessage :: RetroMessage -> IO ()
  , shutdown :: IO ()
  , setPerformanceLevel :: Word32 -> IO ()
  , getSystemDirectory :: IO String
  , setPixelFormat :: RetroPixelFormat -> IO ()
  -- , setInputDescriptors :: RetroInputDescriptors -> IO ()
  -- , setKeyboardCallback :: RetroKeyboardCallback -> IO ()
  -- , setDiskConrolInterface :: RetroDiskControlCallback -> IO ()
  -- , setHWRender :: RetroHWRenderCallback -> IO ()
  , setVariables :: [RetroVariable] -> IO ()
  , getVariable :: String -> IO String
  , getVariableUpdate :: IO Bool
  , setSupportNoGame :: Bool -> IO ()
  , getLibretroPath :: IO String
  -- , setFrameTimeCallback :: RetroFrameTimeCallback -> IO ()
  -- , setAudioCallback :: RetroAudioCallback -> IO ()
  -- , getRumbleInterface :: IO RetroRumbleInterface
  , getInputDeviceCapabilities :: IO Word64
  -- , getSensorInterface :: IO RetroSensorInterface
  -- , getCameraInterface :: IO RetroCameraCallback
  -- , getLogInterface :: IO RetroLogCallback
  -- , getPerfInterface :: IO RetroPerfCallback
  -- , getLocationInterface :: IO RetroLocationCallback
  , getCoreAssetsDirectory :: IO String
  , getSaveDirectory :: IO String
  , setSystemAVInfo :: RetroSystemAVInfo -> IO ()
  -- , setProcAddressCallback :: RetroProcAddressCallback -> IO ()
  -- , setSubsystemInfo :: RetroSubsystemInfo -> IO ()
  , setControllerInfo :: RetroControllerInfo -> IO ()
  -- , setMemoryMaps :: RetroMemoryMap -> IO ()
  , setGeometry :: RetroGameGeometry -> IO ()
  , getUsername :: IO String
  , getLanguage :: IO RetroLanguage
  -- , getCurrentSoftwareFramebuffer :: IO RetroFramebuffer
  -- , getHWRenderInterface :: IO RetroHWRenderInterface
  , setSupportAchievements :: Bool -> IO ()
  -- , setHWRenderContextNegotationInterface ::
  --     RetroHWRenderContextNegotiationInterface -> IO ()
  , setSerializationQuirks :: Word32 -> IO ()
  , setHWSharedContext :: IO ()
  -- , getVFSInterface :: IO RetroVFSInterface
  -- , getLEDInterface :: IO RetroLEDInterface
  -- , getAudioVideoEnableInterface :: IO Int32
  -- , getMIDIInterface :: IO RetroMIDIInterface
  , getFastforwarding :: IO Bool
  , getTargetRefreshRate :: IO Float
  }

-- | Default callbacks for Libretro. All no-ops.
instance Default RetroCallbacks where
  def = RetroCallbacks
    { videoRefresh = const $ const $ const $ pure ()
    , audioSample = const $ const $ pure ()
    , audioSampleBatch = const $ pure 0
    , inputPoll = pure ()
    , inputState = const $ const $ const $ const $ pure 0

    -- , log = const $

    , setRotation = const $ pure ()
    , getOverscan = pure False
    , getCanDupe = pure False
    , setMessage = const $ pure ()
    , shutdown = pure ()
    , setPerformanceLevel = const $ pure ()
    , getSystemDirectory = pure ""
    , setPixelFormat = const $ pure ()
    , setVariables = const $ pure ()
    , getVariable = const $ pure ""
    , getVariableUpdate = pure False
    , setSupportNoGame = const $ pure ()
    , getLibretroPath = pure ""
    , getInputDeviceCapabilities = pure 0
    , getCoreAssetsDirectory = pure ""
    , getSaveDirectory = pure ""
    , setSystemAVInfo = const $ pure ()
    , setControllerInfo = const $ pure ()
    , setGeometry = const $ pure ()
    , getUsername = pure ""
    , getLanguage = pure ENGLISH
    , setSupportAchievements = const $ pure ()
    , setSerializationQuirks = const $ pure ()
    , setHWSharedContext = pure ()
    , getFastforwarding = pure False
    , getTargetRefreshRate = pure 0
    }
