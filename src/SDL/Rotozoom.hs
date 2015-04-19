{-|

Module      : SDL.Rotozoom
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Bindings to @SDL2_gfx@'s surface rotation and zoom functionality.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module SDL.Rotozoom
  ( Angle
  , Zoom
  , Smooth(..)
  , rotozoom
  , rotozoomXY
  , Size
  , rotozoomSize
  , rotozoomSizeXY
  , zoom
  , zoomXY
  , zoomSize
  , zoomSizeXY
  , shrink
  , rotate90
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.Types        (CInt)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Storable       (peek)
import Linear                 (V2(..))
import GHC.Generics           (Generic)
import SDL                    (Surface(..))

import qualified SDL
import qualified SDL.Raw.Rotozoom

-- | Desired rotation in degrees.
type Angle = Double

-- | A dimension scaling factor.
type Zoom = Double

-- | Whether resulting 'Surface's are anti-aliased or not.
data Smooth = Smooth | Rough
  deriving (Eq, Enum, Ord, Bounded, Generic, Read, Show)

smoothToCInt :: Smooth -> CInt
smoothToCInt = \case
  Smooth -> SDL.Raw.Rotozoom.SMOOTHING_ON
  Rough  -> SDL.Raw.Rotozoom.SMOOTHING_OFF

-- | Rotates and zooms a 32 or 8-bit 'Surface'. If the 'Surface' isn't 8-bit or
-- 32-bit RGBA/ABGR, it will be converted into 32-bit RGBA.
rotozoom :: MonadIO m => Surface -> Angle -> Zoom -> Smooth -> m Surface
rotozoom (Surface p) a z s =
  fmap SDL.Surface $
    SDL.Raw.Rotozoom.rotozoom p (realToFrac a) (realToFrac z) (smoothToCInt s)

-- | Same as 'rotozoom', but applies different horizontal and vertical scaling
-- factors. The 'Zoom' arguments are the horizontal and vertical zoom,
-- respectively.
rotozoomXY :: MonadIO m => Surface -> Angle -> Zoom -> Zoom -> Smooth -> m Surface
rotozoomXY (Surface p) a zx zy s =
  fmap SDL.Surface $
    SDL.Raw.Rotozoom.rotozoomXY
      p (realToFrac a) (realToFrac zx) (realToFrac zy) (smoothToCInt s)

-- | A surface size, packing width and height.
type Size = V2 CInt

-- | Given the 'Size' of an input 'Surface', returns the 'Size' of a 'Surface'
-- resulting from a 'rotozoom' call.
rotozoomSize :: MonadIO m => Size -> Angle -> Zoom -> m Size
rotozoomSize (V2 w h) a z =
  liftIO .
    alloca $ \w' ->
      alloca $ \h' -> do
        SDL.Raw.Rotozoom.rotozoomSize w h (realToFrac a) (realToFrac z) w' h'
        V2 <$> peek w' <*> peek h'

-- | Same as 'rotozoomSize', but for different horizontal and vertical scaling
-- factors.
rotozoomSizeXY :: MonadIO m => Size -> Angle -> Zoom -> Zoom -> m Size
rotozoomSizeXY (V2 w h) a zx zy =
  liftIO .
    alloca $ \w' ->
      alloca $ \h' -> do
        SDL.Raw.Rotozoom.rotozoomSizeXY
          w h (realToFrac a) (realToFrac zx) (realToFrac zy) w' h'
        V2 <$> peek w' <*> peek h'

{-# INLINE zoom #-}
-- | Same as 'rotozoom', but only performs the zoom. If a 'Zoom' factor is
-- negative, it flips the image on both axes.
zoom :: MonadIO m => Surface -> Zoom -> Smooth -> m Surface
zoom surface z = zoomXY surface z z

-- | Same as 'zoom', but applies different horizontal and vertical scaling
-- factors. If a 'Zoom' factor is negative, it flips the image on its
-- corresponding axis.
zoomXY :: MonadIO m => Surface -> Zoom -> Zoom -> Smooth -> m Surface
zoomXY (Surface p) zx zy s =
  fmap SDL.Surface $
    SDL.Raw.Rotozoom.zoom p (realToFrac zx) (realToFrac zy) (smoothToCInt s)

{-# INLINE zoomSize #-}
-- | Calculates the 'Size' of a resulting 'Surface' for a 'zoom' call.
zoomSize :: MonadIO m => Size -> Zoom -> m Size
zoomSize size z = zoomSizeXY size z z

-- | Same as 'zoomSize', but for different horizontal and vertical scaling
-- factors.
zoomSizeXY :: MonadIO m => Size -> Angle -> Zoom -> m Size
zoomSizeXY (V2 w h) zx zy =
  liftIO .
    alloca $ \w' ->
      alloca $ \h' -> do
        SDL.Raw.Rotozoom.zoomSize w h (realToFrac zx) (realToFrac zy) w' h'
        V2 <$> peek w' <*> peek h'

-- | Shrink a surface by an integer ratio. The two 'CInt' arguments are the
-- horizontal and vertical shrinking ratios: 2 halves a dimension, 5 makes it a
-- fifth of its original size etc. The resulting 'Surface' is anti-aliased and,
-- if the input wasn't 8-bit or 32-bit, converted to a 32-bit RGBA format.
shrink :: MonadIO m => Surface -> CInt -> CInt -> m Surface
shrink (Surface p) rx ry =
  fmap SDL.Surface $
    SDL.Raw.Rotozoom.shrink p rx ry

-- | Given a number of clockwise rotations to perform, rotates 'Surface' in
-- increments of 90 degrees. Since no interpolation is done, this is faster
-- than 'rotozoomer'.
rotate90 :: MonadIO m => Surface -> Int -> m Surface
rotate90 (Surface p) =
  fmap SDL.Surface . SDL.Raw.Rotozoom.rotate90 p . fromIntegral . (`rem` 4)
