{-# LANGUAGE ForeignFunctionInterface, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Implementation of C-based libretro core.
module Libretro.CFFI (RetroCFFICore, RetroCallbacks) where

import           Control.Monad
import           Data.Bits
import           Data.ByteString
import qualified Data.ByteString.Internal as BI
import           Data.List
import           Data.List.Split
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           Libretro.Core
import           System.Posix.DynamicLinker

-- | Convert commands to RetroEnvironment enum. This ignores the
--   libretro “environment” and “private” flags so that it can cleanly
--   fit into toEnum.
toRetroEnvironment :: CUInt -> RetroEnvironment
toRetroEnvironment cmd = toEnum $ fromIntegral
  (pred cmd) .&. (complement $ retroEnvironmentExperimental .|.
                               retroEnvironmentPrivate)
  where
    retroEnvironmentExperimental = 0x10000
    retroEnvironmentPrivate = 0x20000

-- | Implementation of retro_set_environment callbacks.
setEnvironment :: RetroCallbacks -> CUInt -> Ptr () -> IO Bool
setEnvironment cbs cmd ptr =
  case toRetroEnvironment cmd of
    GET_CORE_ASSETS_DIRECTORY -> do
      dir <- getCoreAssetsDirectory cbs
      withCString dir $ poke $ castPtr ptr
      pure True
    GET_SYSTEM_DIRECTORY -> do
      dir <- getSystemDirectory cbs
      withCString dir $ poke $ castPtr ptr
      pure True
    GET_USERNAME -> do
      username <- getUsername cbs
      withCString username $ poke $ castPtr ptr
      pure True
    GET_VARIABLE -> do
      key <- peekCString =<< peek (castPtr ptr)
      value <- getVariable cbs key
      pokeByteOff (castPtr ptr) (sizeOf (undefined :: Ptr CChar))
        =<< newCString value
      pure True
    GET_VARIABLE_UPDATE -> do
      poke (castPtr ptr :: Ptr CBool) . fromBool =<< getVariableUpdate cbs
      pure True
    GET_CAN_DUPE -> do
      poke (castPtr ptr :: Ptr CBool) . fromBool =<< getCanDupe cbs
      pure True
    SET_ROTATION -> do
      setRotation cbs =<< peek (castPtr ptr)
      pure True
    SET_SUPPORT_NO_GAME -> do
      setSupportNoGame cbs . toBool =<< peek (castPtr ptr :: Ptr CBool)
      pure True
    SET_GEOMETRY -> do
      setGeometry cbs =<< peek (castPtr ptr)
      pure True
    SET_SERIALIZATION_QUIRKS -> do
      setSerializationQuirks cbs =<< peek (castPtr ptr)
      pure True
    SET_PERFORMANCE_LEVEL -> do
      setPerformanceLevel cbs =<< peek (castPtr ptr)
      pure True
    SET_SUPPORT_ACHIEVEMENTS -> do
      setSupportAchievements cbs . toBool =<< peek (castPtr ptr :: Ptr CBool)
      pure True
    SET_PIXEL_FORMAT -> do
      setPixelFormat cbs =<< peek (castPtr ptr)
      pure True
    SET_CONTROLLER_INFO -> do
      setControllerInfo cbs =<< peek (castPtr ptr)
      pure True
    SET_VARIABLES -> do
      setVariables cbs =<< peekArray0 (RetroVariable ("", "")) (castPtr ptr)
      pure True

    -- SET_INPUT_DESCRIPTORS -> do
    -- GET_LOG_INTERFACE -> do
    -- GET_RUMBLE_INTERFACE -> do
    -- SET_MEMORY_MAPS -> do

    unknown -> do
      Prelude.putStrLn $ "Unknown command: " <> show unknown
      pure False

instance RetroCore RetroCFFICore where

  init _ fp cbs = do
    dl <- dlopen fp [RTLD_LAZY]

    a <- wrapRetroEnvironmentCallback $ \cmd ptr -> fmap fromBool $
      setEnvironment cbs cmd ptr
    b <- wrapRetroVideoRefreshCallback $ \buf width height pitch -> do
      cstr <- packCStringLen (buf, fromIntegral pitch * fromIntegral height)
      videoRefresh cbs cstr (fromIntegral height) (fromIntegral width)
    c <- wrapRetroAudioSampleCallback $ \left right ->
      audioSample cbs (fromIntegral left) (fromIntegral right)
    d <- wrapRetroAudioSampleBatchCallback $ \samples size' -> do
      fmap fromIntegral <$> audioSampleBatch cbs
        =<< packCStringLen (samples, fromIntegral size' * 4)
    e <- wrapRetroInputPollCallback $ inputPoll cbs
    f <- wrapRetroInputStateCallback $ \port' device' index' id' ->
      fromIntegral <$> inputState cbs (fromIntegral port')
                                      (toEnum $ fromIntegral device')
                                      (fromIntegral index')
                                      (fromIntegral id')

    flip mkRetroSetEnvironmentFun a =<< dlsym dl "retro_set_environment"
    flip mkRetroSetVideoRefreshFun b =<< dlsym dl "retro_set_video_refresh"
    flip mkRetroSetAudioSampleFun c =<< dlsym dl "retro_set_audio_sample"
    flip mkRetroSetAudioSampleBatchFun d =<<
      dlsym dl "retro_set_audio_sample_batch"
    flip mkRetroSetInputPollFun e =<< dlsym dl "retro_set_input_poll"
    flip mkRetroSetInputStateFun f =<< dlsym dl "retro_set_input_state"

    mkRetroInitFun =<< dlsym dl "retro_init"

    version <- mkRetroApiVersionFun =<< dlsym dl "retro_api_version"
    unless (version == 1) $ error "Not compatible with core version."

    RetroCFFICore
      <$> pure dl
      <*> dlsym dl "retro_deinit"
      <*> dlsym dl "retro_get_system_info"
      <*> dlsym dl "retro_get_system_av_info"
      <*> dlsym dl "retro_set_controller_port_device"
      <*> dlsym dl "retro_reset"
      <*> dlsym dl "retro_run"
      <*> dlsym dl "retro_serialize_size"
      <*> dlsym dl "retro_serialize"
      <*> dlsym dl "retro_unserialize"
      <*> dlsym dl "retro_cheat_reset"
      <*> dlsym dl "retro_cheat_set"
      <*> dlsym dl "retro_load_game"
      <*> dlsym dl "retro_load_game_special"
      <*> dlsym dl "retro_unload_game"
      <*> dlsym dl "retro_get_region"
      <*> dlsym dl "retro_get_memory_data"
      <*> dlsym dl "retro_get_memory_size"

  deinit core = do
    mkRetroDeinitFun $ retro_deinit core
    dlclose $ retro_dl core

  getSystemInfo core = alloca $ \info -> do
    mkRetroGetSystemInfoFun (retro_get_system_info core) info
    peek info

  getSystemAVInfo core = alloca $ \avinfo -> do
    mkRetroGetSystemAVInfoFun (retro_get_system_av_info core) avinfo
    peek avinfo

  run = mkRetroRunFun . retro_run

  reset = mkRetroResetFun . retro_reset

  unloadGame = mkRetroUnloadGameFun . retro_unload_game

  getRegion = fmap (toEnum . fromIntegral) <$>
    mkRetroGetRegionFun . retro_get_region

  setControllerPortDevice core port' device' =
    mkRetroSetControllerPortDeviceFun (retro_set_controller_port_device core)
                                      (fromIntegral port')
                                      (fromIntegral $ fromEnum device')

  serialize core = do
    size <- mkRetroSerializeSizeFun $ retro_serialize_size core
    allocaBytes (fromIntegral size) $ \ptr -> do
      ok <- toBool <$> mkRetroSerializeFun (retro_serialize core) (castPtr ptr)
      if ok then Just <$> packCStringLen (ptr, fromIntegral size)
            else pure Nothing

  unserialize core data' = fmap toBool <$> useAsCString data' $
    mkRetroUnserializeFun (retro_unserialize core) . castPtr

  cheatSet core index' enabled code =
    withCString code $ mkRetroCheatSetFun (retro_cheat_set core)
                                          (fromIntegral index')
                                          (fromBool enabled)

  cheatReset = mkRetroCheatResetFun . retro_cheat_reset

  getMemory core region = do
    size <- mkRetroGetMemorySizeFun (retro_get_memory_size core)
                                    (fromIntegral $ fromEnum region)
    ptr <- mkRetroGetMemoryDataFun (retro_get_memory_data core)
                                   (fromIntegral $ fromEnum region)
    packCStringLen (castPtr ptr, fromIntegral size)

  loadGame core info = toBool <$>
    with info (mkRetroLoadGameFun $ retro_load_game core)

  loadGameSpecial core type_ info num_info = toBool <$>
    with info (\ptr -> mkRetroLoadGameSpecialFun (retro_load_game_special core)
                                                 (fromIntegral type_) ptr
                                                 (fromIntegral num_info))

-- | State for Libretro core. Binds C symbols.
data RetroCFFICore = RetroCFFICore
  { retro_dl :: DL
  , retro_deinit :: FunPtr RetroDeinit
  , retro_get_system_info :: FunPtr RetroGetSystemInfo
  , retro_get_system_av_info :: FunPtr RetroGetSystemAVInfo
  , retro_set_controller_port_device :: FunPtr RetroSetControllerPortDevice
  , retro_reset :: FunPtr RetroReset
  , retro_run :: FunPtr RetroRun
  , retro_serialize_size :: FunPtr RetroSerializeSize
  , retro_serialize :: FunPtr RetroSerialize
  , retro_unserialize :: FunPtr RetroUnserialize
  , retro_cheat_reset :: FunPtr RetroCheatReset
  , retro_cheat_set :: FunPtr RetroCheatSet
  , retro_load_game :: FunPtr RetroLoadGame
  , retro_load_game_special :: FunPtr RetroLoadGameSpecial
  , retro_unload_game :: FunPtr RetroUnloadGame
  , retro_get_region :: FunPtr RetroGetRegion
  , retro_get_memory_data :: FunPtr RetroGetMemoryData
  , retro_get_memory_size :: FunPtr RetroGetMemorySize
  }

--
-- Type synonyms for functions and callbacks.
--

type RetroEnvironmentCallback = CUInt -> Ptr () -> IO CBool
type RetroSetEnvironment = FunPtr RetroEnvironmentCallback -> IO ()
type RetroVideoRefreshCallback = CString -> CUInt -> CUInt -> CSize -> IO ()
type RetroSetVideoRefresh = FunPtr RetroVideoRefreshCallback -> IO ()
type RetroAudioSampleCallback = CShort -> CShort -> IO ()
type RetroSetAudioSample = FunPtr RetroAudioSampleCallback -> IO ()
type RetroAudioSampleBatchCallback = CString -> CSize -> IO CSize
type RetroSetAudioSampleBatch = FunPtr RetroAudioSampleBatchCallback -> IO ()
type RetroInputPollCallback = IO ()
type RetroSetInputPoll = FunPtr RetroInputPollCallback -> IO ()
type RetroInputStateCallback = CUInt -> CUInt -> CUInt -> CUInt -> IO CShort
type RetroSetInputState = FunPtr RetroInputStateCallback -> IO ()

type RetroInit = IO ()
type RetroDeinit = IO ()
type RetroAPIVersion = IO CUInt

type RetroGetSystemInfo = Ptr RetroSystemInfo -> IO ()
type RetroGetSystemAVInfo = Ptr RetroSystemAVInfo -> IO ()

type RetroSetControllerPortDevice = CUInt -> CUInt -> IO ()

type RetroReset = IO ()
type RetroRun = IO ()

type RetroSerializeSize = IO CSize
type RetroSerialize = Ptr () -> IO CBool
type RetroUnserialize = Ptr () -> IO CBool

type RetroCheatReset = IO ()
type RetroCheatSet = CUInt -> CBool -> CString -> IO ()

type RetroLoadGame = Ptr RetroGameInfo -> IO CBool
type RetroLoadGameSpecial = CUInt -> Ptr RetroGameInfo -> CSize -> IO CBool
type RetroUnloadGame = IO ()

type RetroGetRegion = IO CUInt

type RetroGetMemoryData = CUInt -> IO (Ptr ())
type RetroGetMemorySize = CUInt -> IO CSize

--
-- Static functions in libretro.h
--

foreign import ccall "dynamic"
  mkRetroSetEnvironmentFun :: FunPtr RetroSetEnvironment
                           -> RetroSetEnvironment
foreign import ccall "dynamic"
  mkRetroSetVideoRefreshFun :: FunPtr RetroSetVideoRefresh
                            -> RetroSetVideoRefresh
foreign import ccall "dynamic"
  mkRetroSetAudioSampleFun :: FunPtr RetroSetAudioSample -> RetroSetAudioSample
foreign import ccall "dynamic"
  mkRetroSetAudioSampleBatchFun :: FunPtr RetroSetAudioSampleBatch
                                -> RetroSetAudioSampleBatch
foreign import ccall "dynamic"
  mkRetroSetInputPollFun :: FunPtr RetroSetInputPoll -> RetroSetInputPoll
foreign import ccall "dynamic"
  mkRetroSetInputStateFun :: FunPtr RetroSetInputState -> RetroSetInputState
foreign import ccall "dynamic" mkRetroInitFun :: FunPtr RetroInit -> RetroInit
foreign import ccall "dynamic"
  mkRetroDeinitFun :: FunPtr RetroDeinit -> RetroDeinit
foreign import ccall "dynamic"
  mkRetroApiVersionFun :: FunPtr RetroAPIVersion -> RetroAPIVersion
foreign import ccall "dynamic"
  mkRetroGetSystemInfoFun :: FunPtr RetroGetSystemInfo -> RetroGetSystemInfo
foreign import ccall "dynamic"
  mkRetroGetSystemAVInfoFun :: FunPtr RetroGetSystemAVInfo
                            -> RetroGetSystemAVInfo
foreign import ccall "dynamic"
  mkRetroSetControllerPortDeviceFun :: FunPtr RetroSetControllerPortDevice
                                    -> RetroSetControllerPortDevice
foreign import ccall "dynamic" mkRetroResetFun :: FunPtr RetroReset -> RetroReset
foreign import ccall "dynamic" mkRetroRunFun :: FunPtr RetroRun -> RetroRun
foreign import ccall "dynamic"
  mkRetroSerializeSizeFun :: FunPtr RetroSerializeSize -> RetroSerializeSize
foreign import ccall "dynamic"
  mkRetroSerializeFun :: FunPtr RetroSerialize -> RetroSerialize
foreign import ccall "dynamic"
  mkRetroUnserializeFun :: FunPtr RetroUnserialize -> RetroUnserialize
foreign import ccall "dynamic"
  mkRetroCheatResetFun :: FunPtr RetroCheatReset -> RetroCheatReset
foreign import ccall "dynamic"
  mkRetroCheatSetFun :: FunPtr RetroCheatSet -> RetroCheatSet
foreign import ccall "dynamic"
  mkRetroLoadGameFun :: FunPtr RetroLoadGame -> RetroLoadGame
foreign import ccall "dynamic"
  mkRetroLoadGameSpecialFun :: FunPtr RetroLoadGameSpecial
                            -> RetroLoadGameSpecial
foreign import ccall "dynamic"
  mkRetroUnloadGameFun :: FunPtr RetroUnloadGame -> RetroUnloadGame
foreign import ccall "dynamic"
  mkRetroGetRegionFun :: FunPtr RetroGetRegion -> RetroGetRegion
foreign import ccall "dynamic"
  mkRetroGetMemoryDataFun :: FunPtr RetroGetMemoryData -> RetroGetMemoryData
foreign import ccall "dynamic"
  mkRetroGetMemorySizeFun :: FunPtr RetroGetMemorySize -> RetroGetMemorySize

--
-- Callbacks in libretro.h
--

foreign import ccall "wrapper"
  wrapRetroEnvironmentCallback :: RetroEnvironmentCallback
                               -> IO (FunPtr RetroEnvironmentCallback)
foreign import ccall "wrapper"
  wrapRetroVideoRefreshCallback :: RetroVideoRefreshCallback
                                -> IO (FunPtr RetroVideoRefreshCallback)
foreign import ccall "wrapper"
  wrapRetroAudioSampleCallback :: RetroAudioSampleCallback
                               -> IO (FunPtr RetroAudioSampleCallback)
foreign import ccall "wrapper"
  wrapRetroAudioSampleBatchCallback :: RetroAudioSampleBatchCallback
                                    -> IO (FunPtr RetroAudioSampleBatchCallback)
foreign import ccall "wrapper"
  wrapRetroInputPollCallback :: RetroInputPollCallback
                             -> IO (FunPtr RetroInputPollCallback)
foreign import ccall "wrapper"
  wrapRetroInputStateCallback :: RetroInputStateCallback
                              -> IO (FunPtr RetroInputStateCallback)

-- Hack to create new CString from ByteString
newCStringFromBS :: ByteString -> IO CString
newCStringFromBS (BI.PS fp o l) = do
  buf <- mallocBytes (l + 1)
  withForeignPtr fp $ \p -> do
    BI.memcpy buf (p `plusPtr` o) (fromIntegral l)
    pokeByteOff buf l (0::Word8)
    return $ castPtr buf

--
-- Storable instances for Libretro.Core data.
--

-- XXX: make these generic, not assumming pointer size is 8.
-- Alignments, sizeOf, and offsets will change on 32-bit architecture.

instance Storable RetroGameInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = RetroGameInfo
          <$> (peekCString =<< peekByteOff ptr 0)
          <*> do
            a <- peekByteOff ptr 8
            b <- peekByteOff ptr 16 :: IO CSize
            packCStringLen (a, fromIntegral b)
          <*> (peekCString =<< peekByteOff ptr 24)
  poke ptr (RetroGameInfo a b c) = do
    pokeByteOff ptr 0 =<< newCString a
    pokeByteOff ptr 8 =<< newCStringFromBS b
    pokeByteOff ptr 16 (fromIntegral (Data.ByteString.length b) :: CSize)
    pokeByteOff ptr 24 =<< newCString c

instance Storable RetroSystemInfo where
  sizeOf _ = 26
  alignment _ = 1
  peek ptr = RetroSystemInfo
          <$> (peekCString =<< peekByteOff ptr 0)
          <*> (peekCString =<< peekByteOff ptr 8)
          <*> (splitOn "|" <$> (peekCString =<< peekByteOff ptr 16))
          <*> (toBool <$> (peekByteOff ptr 24 :: IO CBool))
          <*> (toBool <$> (peekByteOff ptr 25 :: IO CBool))
  poke ptr (RetroSystemInfo a b c d e) = do
    pokeByteOff ptr 0 =<< newCString a
    pokeByteOff ptr 8 =<< newCString b
    pokeByteOff ptr 16 =<< newCString (Data.List.intercalate "|" c)
    pokeByteOff ptr 24 (fromBool d :: CBool)
    pokeByteOff ptr 25 (fromBool e :: CBool)

instance Storable RetroSystemTiming where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = RetroSystemTiming <$> peekByteOff ptr 0 <*> peekByteOff ptr 8
  poke ptr (RetroSystemTiming a b) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b

instance Storable RetroGameGeometry where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = RetroGameGeometry
          <$> peekByteOff ptr 0
          <*> peekByteOff ptr 4
          <*> peekByteOff ptr 8
          <*> peekByteOff ptr 12
          <*> peekByteOff ptr 16
  poke ptr (RetroGameGeometry a b c d e) = do
    pokeByteOff ptr 0 (fromIntegral a :: CUInt)
    pokeByteOff ptr 4 (fromIntegral b :: CUInt)
    pokeByteOff ptr 8 (fromIntegral c :: CUInt)
    pokeByteOff ptr 12 (fromIntegral d :: CUInt)
    pokeByteOff ptr 16 e

instance Storable RetroSystemAVInfo where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = RetroSystemAVInfo <$> peekByteOff ptr 0 <*> peekByteOff ptr 24
  poke ptr (RetroSystemAVInfo a b) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 24 b

-- XXX: add poke methods

instance Storable RetroPixelFormat where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr =
    toEnum . fromIntegral . pred <$> peek (castPtr ptr :: Ptr CInt)

instance Storable RetroDevice where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr CInt)

instance Storable RetroControllerDescription where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = do
    -- RetroControllerDescription
    --       <$> (peekCString =<< peekByteOff ptr 0)
    --       <*> (peekByteOff ptr 8)
    pure $ RetroControllerDescription "" DEVICE_NONE

instance Storable RetroControllerInfo where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = do
    lookahead <- peek (castPtr ptr :: Ptr (Ptr ()))
    if lookahead == nullPtr then pure $ RetroControllerInfo []
    else do
      addr <- peekByteOff (castPtr ptr) 0
      num_types <- peekByteOff (castPtr ptr :: Ptr CUInt) 8
      RetroControllerInfo <$> peekArray num_types (castPtr addr)

instance Storable RetroControllers where
  sizeOf _ = 8
  alignment _ = 8
  peek ptr = RetroControllers <$>
    peekArray0 (RetroControllerInfo []) (castPtr ptr)

instance Storable RetroVariable where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = do
    lookahead <- peek (castPtr ptr :: Ptr (Ptr ()))
    if lookahead == nullPtr then pure $ RetroVariable ("", "")
    else do
      key <- peekCString =<< peekByteOff ptr 0
      value <- peekCString =<< peekByteOff ptr 8
      pure $ RetroVariable (key, value)
