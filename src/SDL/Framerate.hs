{-|

Module      : SDL.Framerate
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Bindings to @SDL2_gfx@'s framerate management functionality. These functions
should allow you to set, manage and query a target application framerate.

-}

{-# LANGUAGE LambdaCase #-}

module SDL.Framerate
  ( Framerate
  , Manager(..)
  , manager
  , set
  , delay
  , minimum
  , maximum
  , get
  , count
  ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable          (Typeable)
import Foreign.Marshal.Alloc  (malloc, free)
import Foreign.Ptr            (Ptr)
import Prelude         hiding (minimum, maximum)

import qualified SDL.Raw.Framerate

-- | A framerate manager, counting frames and keeping track of time delays
-- necessary to reach a certain target framerate.
newtype Manager = Manager (Ptr SDL.Raw.Framerate.Manager)
  deriving (Eq, Typeable)

-- | A certain number of frames per second.
type Framerate = Int

-- | Create a new framerate 'Manager' using the default settings. You have to
-- take care to call 'destroyManager' yourself. It's recommended to use 'with'
-- instead.
manager :: MonadIO m => m Manager
manager =
  fmap Manager . liftIO $ do
    ptr <- malloc
    SDL.Raw.Framerate.init ptr
    return ptr

-- | The smallest allowed framerate.
minimum :: Framerate
minimum = SDL.Raw.Framerate.FPS_LOWER_LIMIT

-- | The largest allowed framerate.
maximum :: Framerate
maximum = SDL.Raw.Framerate.FPS_UPPER_LIMIT

-- | Set a target framerate and reset delay interpolation. Note that the given
-- framerate must be within the allowed range -- otherwise the minimum or
-- maximum allowed framerate is used instead.
set :: MonadIO m => Manager -> Framerate -> m ()
set (Manager ptr) = void . set' . min maximum . max minimum
  where
    set' = SDL.Raw.Framerate.setFramerate ptr . fromIntegral

-- | Get the currently set framerate.
get :: MonadIO m => Manager -> m Framerate
get (Manager ptr) = fmap fromIntegral $ SDL.Raw.Framerate.getFramerate ptr

-- | Returns the framecount. Each time 'delay' is called, a frame is counted.
count :: MonadIO m => Manager -> m Int
count (Manager ptr) = fmap fromIntegral $ SDL.Raw.Framerate.getFramecount ptr

-- | Generate and apply a delay in order to maintain a constant target
-- framerate. This should be called once per rendering loop. Delay will
-- automatically be set to zero if the computer cannot keep up (if rendering is
-- too slow). Returns the number of milliseconds since the last time 'delay'
-- was called (possibly zero).
delay :: MonadIO m => Manager -> m Int
delay (Manager ptr) = fmap fromIntegral $ SDL.Raw.Framerate.framerateDelay ptr
