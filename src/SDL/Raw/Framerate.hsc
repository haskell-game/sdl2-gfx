{-|

Module      : SDL.Raw.Framerate
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Raw bindings to the @SDL2_gfx@ library, specifically the framerate management
functionality from @SDL2_framerate.h@.

-}

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}

module SDL.Raw.Framerate
  ( Manager(..)
  , init
  , framerateDelay
  , setFramerate
  , getFramerate
  , getFramecount
  ) where

#include "SDL2_framerate.h"

import Control.Monad.IO.Class  (MonadIO, liftIO)
import Data.Typeable           (Typeable)
import Foreign.C.Types         (CFloat(..), CInt(..))
import Foreign.Ptr             (Ptr)
import Foreign.Storable        (Storable(..))
import GHC.Word                (Word32)
import Prelude          hiding (init)

pattern FPS_DEFAULT     = (#const FPS_DEFAULT)
pattern FPS_LOWER_LIMIT = (#const FPS_LOWER_LIMIT)
pattern FPS_UPPER_LIMIT = (#const FPS_UPPER_LIMIT)

data Manager = Manager
  { frameCount :: Word32
  , rateTicks  :: CFloat
  , baseTicks  :: Word32
  , lastTicks  :: Word32
  , rate       :: Word32
  } deriving (Eq, Show, Typeable)

instance Storable Manager where
  alignment = sizeOf
  sizeOf _ = (#size FPSmanager)

  peek ptr =
    Manager
      <$> (#peek FPSmanager, framecount) ptr
      <*> (#peek FPSmanager, rateticks)  ptr
      <*> (#peek FPSmanager, baseticks)  ptr
      <*> (#peek FPSmanager, lastticks)  ptr
      <*> (#peek FPSmanager, rate)       ptr

  poke ptr (Manager {..}) = do
    (#poke FPSmanager, framecount) ptr frameCount
    (#poke FPSmanager, rateticks)  ptr rateTicks
    (#poke FPSmanager, baseticks)  ptr baseTicks
    (#poke FPSmanager, lastticks)  ptr lastTicks
    (#poke FPSmanager, rate)       ptr rate

foreign import ccall "SDL2_framerate.h SDL_initFramerate"
  init' :: Ptr Manager -> IO ()

init :: MonadIO m => Ptr Manager -> m ()
init = liftIO . init'

foreign import ccall "SDL2_framerate.h SDL_getFramecount"
  getFramecount' :: Ptr Manager -> IO CInt

getFramecount :: MonadIO m => Ptr Manager -> m CInt
getFramecount = liftIO . getFramecount'

foreign import ccall "SDL2_framerate.h SDL_framerateDelay"
  framerateDelay' :: Ptr Manager -> IO Word32

framerateDelay :: MonadIO m => Ptr Manager -> m Word32
framerateDelay = liftIO . framerateDelay'

foreign import ccall "SDL2_framerate.h SDL_getFramerate"
  getFramerate' :: Ptr Manager -> IO CInt

getFramerate :: MonadIO m => Ptr Manager -> m CInt
getFramerate = liftIO . getFramerate'

foreign import ccall "SDL2_framerate.h SDL_setFramerate"
  setFramerate' :: Ptr Manager -> Word32 -> IO CInt

setFramerate :: MonadIO m => Ptr Manager -> Word32 -> m CInt
setFramerate fps = liftIO . setFramerate' fps
