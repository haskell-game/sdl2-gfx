{-|

Module      : SDL.Raw.Rotozoom
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
Stability   : experimental

Raw bindings to the @SDL2_gfx@ library, specifically the surface rotation and
zoom functionality from @SDL2_rotozoom.h@.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Rotozoom
  ( pattern SMOOTHING_ON
  , pattern SMOOTHING_OFF
  , rotozoom
  , rotozoomXY
  , rotozoomSize
  , rotozoomSizeXY
  , zoom
  , zoomSize
  , shrink
  , rotate90
  ) where

#include "SDL2_rotozoom.h"

import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Ptr     (Ptr)
import SDL.Raw.Helper  (liftF)
import SDL.Raw.Types   (Surface(..))

pattern SMOOTHING_OFF = (#const SMOOTHING_OFF)
pattern SMOOTHING_ON  = (#const SMOOTHING_ON)

liftF "rotozoom" "rotozoomSurface"
  [t|Ptr Surface -> CDouble -> CDouble -> CInt -> IO (Ptr Surface)|]

liftF "rotozoomXY" "rotozoomSurfaceXY"
  [t|Ptr Surface -> CDouble -> CDouble -> CDouble -> CInt -> IO (Ptr Surface)|]

liftF "rotozoomSize" "rotozoomSurfaceSize"
  [t|CInt -> CInt -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> IO ()|]

liftF "rotozoomSizeXY" "rotozoomSurfaceSizeXY"
  [t|CInt -> CInt -> CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> IO ()|]

liftF "zoom" "zoomSurface"
  [t|Ptr Surface -> CDouble -> CDouble -> CInt -> IO (Ptr Surface)|]

liftF "zoomSize" "zoomSurfaceSize"
  [t|CInt -> CInt -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> IO ()|]

liftF "shrink" "shrinkSurface"
  [t|Ptr Surface -> CInt -> CInt -> IO (Ptr Surface)|]

liftF "rotate90" "rotateSurface90Degrees"
  [t|Ptr Surface -> CInt -> IO (Ptr Surface)|]
