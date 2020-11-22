{-|

Module      : SDL.Rotozoom
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
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
  , zoom
  , zoomXY
  , shrink
  , rotate90
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.Types        (CInt)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Ptr            (Ptr)
import Foreign.Storable       (peek)
import GHC.Generics           (Generic)
import SDL                    (Surface(..))
import SDL.Vect               (V2(..))

import qualified SDL.Raw
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

-- | A helper for unmanaged 'Surface's, since it is not exposed by SDL itself.
unmanaged :: Ptr SDL.Raw.Surface -> Surface
unmanaged p = Surface p Nothing

-- | Rotates and zooms a 32 or 8-bit 'Surface'.
--
-- If the 'Surface' isn't 8-bit or 32-bit RGBA/ABGR, it will be converted into
-- 32-bit RGBA.
rotozoom :: MonadIO m => Surface -> Angle -> Zoom -> Smooth -> m Surface
rotozoom (Surface p _) a z s =
  unmanaged <$>
    SDL.Raw.Rotozoom.rotozoom p (realToFrac a) (realToFrac z) (smoothToCInt s)

-- | Same as 'rotozoom', but applies different horizontal and vertical scaling
-- factors.
--
-- The 'Zoom' arguments are the horizontal and vertical zoom, respectively.
rotozoomXY :: MonadIO m => Surface -> Angle -> Zoom -> Zoom -> Smooth -> m Surface
rotozoomXY (Surface p _) a zx zy s =
  unmanaged <$>
    SDL.Raw.Rotozoom.rotozoomXY
      p (realToFrac a) (realToFrac zx) (realToFrac zy) (smoothToCInt s)

-- | A surface size, packing width and height.
type Size = V2 CInt

{-# INLINE zoom #-}
-- | Same as 'rotozoom', but only performs the zoom.
--
-- If a 'Zoom' factor is negative, it flips the image on both axes.
zoom :: MonadIO m => Surface -> Zoom -> Smooth -> m Surface
zoom surface z = zoomXY surface z z

-- | Same as 'zoom', but applies different horizontal and vertical scaling
-- factors.
--
-- If a 'Zoom' factor is negative, it flips the image on its corresponding
-- axis.
zoomXY :: MonadIO m => Surface -> Zoom -> Zoom -> Smooth -> m Surface
zoomXY (Surface p _) zx zy s =
  unmanaged <$>
    SDL.Raw.Rotozoom.zoom p (realToFrac zx) (realToFrac zy) (smoothToCInt s)

-- | Shrink a surface by an integer ratio.
--
-- The two 'CInt' arguments are the horizontal and vertical shrinking ratios: 2
-- halves a dimension, 5 makes it a fifth of its original size etc.
--
-- The resulting 'Surface' is anti-aliased and, if the input wasn't 8-bit or
-- 32-bit, converted to a 32-bit RGBA format.
shrink :: MonadIO m => Surface -> CInt -> CInt -> m Surface
shrink (Surface p _) rx ry = unmanaged <$> SDL.Raw.Rotozoom.shrink p rx ry

-- | Given a number of clockwise rotations to perform, rotates 'Surface' in
-- increments of 90 degrees.
--
-- Since no interpolation is done, this is faster than 'rotozoomer'.
rotate90 :: MonadIO m => Surface -> Int -> m Surface
rotate90 (Surface p _) =
  fmap unmanaged . SDL.Raw.Rotozoom.rotate90 p . fromIntegral . (`rem` 4)
