{-|

Module      : SDL.Raw.Primitive
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Raw bindings to the @SDL2_gfx@ library, specifically the primitives drawing
functionality from @SDL2_gfxPrimitives.h@.

-}

-- {-# LANGUAGE DeriveDataTypeable  #-}
-- {-# LANGUAGE PatternSynonyms     #-}
-- {-# LANGUAGE RecordWildCards     #-}

module SDL.Raw.Primitive
  ( X
  , Y
  , ColorF
  , pixel
  , W
  , hline
  , H
  , vline
  , line
  , aaLine
  , thickLine
  , R
  , N
  , bezier
  , rect
  , roundRect
  , box
  , roundBox
  , circle
  , aaCircle
  , fillCircle
  , arc
  , ellipse
  , aaEllipse
  , fillEllipse
  , pie
  , fillPie
  , trigon
  , aaTrigon
  , fillTrigon
  , polygon
  , aaPolygon
  , fillPolygon
  , texPolygon
  ) where

#include "SDL2_gfxPrimitives.h"

import Control.Monad.IO.Class  (MonadIO, liftIO)
import Foreign.C.Types         (CInt(..))
import Foreign.Ptr             (Ptr)
import Data.Word               (Word8)
import Data.Int                (Int16)
import SDL.Raw                 (Renderer, Surface)

-- | The position of something on the x-axis.
type X = Int16

-- | Same as 'X', but for the y-axis.
type Y = Int16

-- | All of the actions here accept as their last arguments the RGBA color
-- they're drawing with, while also returning a 'CInt'. This makes 'ColorF' a
-- useful shorthand. The components accepted are R, G, B and A, respectively.
type ColorF m = Word8 -> Word8 -> Word8 -> Word8 -> m CInt

foreign import ccall "SDL2_gfxPrimitives.h pixelRGBA"
  pixel' :: Ptr Renderer -> X -> Y -> ColorF IO

{-# INLINE pixel #-}
pixel :: MonadIO m => Ptr Renderer -> X -> Y -> ColorF m
pixel rndr x y r g b = liftIO . pixel' rndr x y r g b

-- | A width.
type W = Int16

foreign import ccall "SDL2_gfxPrimitives.h hlineRGBA"
  hline' :: Ptr Renderer -> X -> Y -> W -> ColorF IO

{-# INLINE hline #-}
hline :: MonadIO m => Ptr Renderer -> X -> Y -> W -> ColorF m
hline rndr x y w r g b = liftIO . hline' rndr x y w r g b

-- | A height.
type H = Int16

foreign import ccall "SDL2_gfxPrimitives.h vlineRGBA"
  vline' :: Ptr Renderer -> X -> Y -> H -> ColorF IO

{-# INLINE vline #-}
vline :: MonadIO m => Ptr Renderer -> X -> Y -> H -> ColorF m
vline rndr x y h r g b = liftIO . vline' rndr x y h r g b

foreign import ccall "SDL2_gfxPrimitives.h rectangleRGBA"
  rect' :: Ptr Renderer -> X -> Y -> X -> Y -> ColorF IO

{-# INLINE rect #-}
rect :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> ColorF m
rect rndr x y x2 y2 r g b = liftIO . rect' rndr x y x2 y2 r g b

-- | A radius.
type R = Int16

foreign import ccall "SDL2_gfxPrimitives.h roundedRectangleRGBA"
  roundRect' :: Ptr Renderer -> X -> Y -> X -> Y -> R -> ColorF IO

{-# INLINE roundRect #-}
roundRect :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> R -> ColorF m
roundRect rndr x y x2 y2 s r g b = liftIO . roundRect' rndr x y x2 y2 s r g b

foreign import ccall "SDL2_gfxPrimitives.h boxRGBA"
  box' :: Ptr Renderer -> X -> Y -> X -> Y -> ColorF IO

{-# INLINE box #-}
box :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> ColorF m
box rndr x y x2 y2 r g b = liftIO . box' rndr x y x2 y2 r g b

foreign import ccall "SDL2_gfxPrimitives.h roundedBoxRGBA"
  roundBox' :: Ptr Renderer -> X -> Y -> X -> Y -> R -> ColorF IO

{-# INLINE roundBox #-}
roundBox :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> R -> ColorF m
roundBox rndr x y x2 y2 s r g b = liftIO . roundBox' rndr x y x2 y2 s r g b

foreign import ccall "SDL2_gfxPrimitives.h lineRGBA"
  line' :: Ptr Renderer -> X -> Y -> X -> Y -> ColorF IO

{-# INLINE line #-}
line :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> ColorF m
line rndr x y x2 y2 r g b = liftIO . line' rndr x y x2 y2 r g b

foreign import ccall "SDL2_gfxPrimitives.h aalineRGBA"
  aaLine' :: Ptr Renderer -> X -> Y -> X -> Y -> ColorF IO

{-# INLINE aaLine #-}
aaLine :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> ColorF m
aaLine rndr x y x2 y2 r g b = liftIO . aaLine' rndr x y x2 y2 r g b

foreign import ccall "SDL2_gfxPrimitives.h thickLineRGBA"
  thickLine' :: Ptr Renderer -> X -> Y -> X -> Y -> W -> ColorF IO

{-# INLINE thickLine #-}
thickLine :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> W -> ColorF m
thickLine rndr x y x2 y2 w r g b = liftIO . thickLine' rndr x y x2 y2 w r g b

foreign import ccall "SDL2_gfxPrimitives.h circleRGBA"
  circle' :: Ptr Renderer -> X -> Y -> R -> ColorF IO

{-# INLINE circle #-}
circle :: MonadIO m => Ptr Renderer -> X -> Y -> R -> ColorF m
circle rndr x y rad r g b = liftIO . circle' rndr x y rad r g b

foreign import ccall "SDL2_gfxPrimitives.h arcRGBA"
  arc' :: Ptr Renderer -> X -> Y -> R -> R -> R -> ColorF IO

{-# INLINE arc #-}
arc :: MonadIO m => Ptr Renderer -> X -> Y -> R -> R -> R -> ColorF m
arc rndr x y rad s e r g b = liftIO . arc' rndr x y rad s e r g b

foreign import ccall "SDL2_gfxPrimitives.h aacircleRGBA"
  aaCircle' :: Ptr Renderer -> X -> Y -> R -> ColorF IO

{-# INLINE aaCircle #-}
aaCircle :: MonadIO m => Ptr Renderer -> X -> Y -> R -> ColorF m
aaCircle rndr x y rad r g b = liftIO . aaCircle' rndr x y rad r g b

foreign import ccall "SDL2_gfxPrimitives.h filledCircleRGBA"
  fillCircle' :: Ptr Renderer -> X -> Y -> R -> ColorF IO

{-# INLINE fillCircle #-}
fillCircle :: MonadIO m => Ptr Renderer -> X -> Y -> R -> ColorF m
fillCircle rndr x y rad r g b = liftIO . fillCircle' rndr x y rad r g b

foreign import ccall "SDL2_gfxPrimitives.h ellipseRGBA"
  ellipse' :: Ptr Renderer -> X -> Y -> R -> R -> ColorF IO

{-# INLINE ellipse #-}
ellipse :: MonadIO m => Ptr Renderer -> X -> Y -> R -> R -> ColorF m
ellipse rndr x y rx ry r g b = liftIO . ellipse' rndr x y rx ry r g b

foreign import ccall "SDL2_gfxPrimitives.h aaellipseRGBA"
  aaEllipse' :: Ptr Renderer -> X -> Y -> R -> R -> ColorF IO

{-# INLINE aaEllipse #-}
aaEllipse :: MonadIO m => Ptr Renderer -> X -> Y -> R -> R -> ColorF m
aaEllipse rndr x y rx ry r g b = liftIO . aaEllipse' rndr x y rx ry r g b

foreign import ccall "SDL2_gfxPrimitives.h filledEllipseRGBA"
  fillEllipse' :: Ptr Renderer -> X -> Y -> R -> R -> ColorF IO

{-# INLINE fillEllipse #-}
fillEllipse :: MonadIO m => Ptr Renderer -> X -> Y -> R -> R -> ColorF m
fillEllipse rndr x y rx ry r g b = liftIO . fillEllipse' rndr x y rx ry r g b

foreign import ccall "SDL2_gfxPrimitives.h pieRGBA"
  pie' :: Ptr Renderer -> X -> Y -> R -> R -> R -> ColorF IO

{-# INLINE pie #-}
pie :: MonadIO m => Ptr Renderer -> X -> Y -> R -> R -> R -> ColorF m
pie rndr x y rad s e r g b = liftIO . pie' rndr x y rad s e r g b

foreign import ccall "SDL2_gfxPrimitives.h filledPieRGBA"
  fillPie' :: Ptr Renderer -> X -> Y -> R -> R -> R -> ColorF IO

{-# INLINE fillPie #-}
fillPie :: MonadIO m => Ptr Renderer -> X -> Y -> R -> R -> R -> ColorF m
fillPie rndr x y rad s e r g b = liftIO . fillPie' rndr x y rad s e r g b

foreign import ccall "SDL2_gfxPrimitives.h trigonRGBA"
  trigon' :: Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> ColorF IO

{-# INLINE trigon #-}
trigon :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> ColorF m
trigon rndr x y x2 y2 x3 y3 r g b = liftIO . trigon' rndr x y x2 y2 x3 y3 r g b

foreign import ccall "SDL2_gfxPrimitives.h aatrigonRGBA"
  aaTrigon' :: Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> ColorF IO

{-# INLINE aaTrigon #-}
aaTrigon :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> ColorF m
aaTrigon rndr x y x2 y2 x3 y3 r g b =
  liftIO . aaTrigon' rndr x y x2 y2 x3 y3 r g b

foreign import ccall "SDL2_gfxPrimitives.h filledTrigonRGBA"
  fillTrigon' :: Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> ColorF IO

{-# INLINE fillTrigon #-}
fillTrigon
  :: MonadIO m => Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> ColorF m
fillTrigon rndr x y x2 y2 x3 y3 r g b =
  liftIO . fillTrigon' rndr x y x2 y2 x3 y3 r g b

-- | How many of a certain thing, i.e. how many points.
type N = CInt

foreign import ccall "SDL2_gfxPrimitives.h polygonRGBA"
  polygon' :: Ptr Renderer -> Ptr X -> Ptr Y -> N -> ColorF IO

{-# INLINE polygon #-}
polygon :: MonadIO m => Ptr Renderer -> Ptr X -> Ptr Y -> N -> ColorF m
polygon rndr xs ys n r g b = liftIO . polygon' rndr xs ys n r g b

foreign import ccall "SDL2_gfxPrimitives.h aapolygonRGBA"
  aaPolygon' :: Ptr Renderer -> Ptr X -> Ptr Y -> N -> ColorF IO

{-# INLINE aaPolygon #-}
aaPolygon :: MonadIO m => Ptr Renderer -> Ptr X -> Ptr Y -> N -> ColorF m
aaPolygon rndr xs ys n r g b = liftIO . aaPolygon' rndr xs ys n r g b

foreign import ccall "SDL2_gfxPrimitives.h filledPolygonRGBA"
  fillPolygon' :: Ptr Renderer -> Ptr X -> Ptr Y -> N -> ColorF IO

{-# INLINE fillPolygon #-}
fillPolygon :: MonadIO m => Ptr Renderer -> Ptr X -> Ptr Y -> N -> ColorF m
fillPolygon rndr xs ys n r g b = liftIO . fillPolygon' rndr xs ys n r g b

foreign import ccall "SDL2_gfxPrimitives.h texturedPolygonRGBA"
  texPolygon'
    :: Ptr Renderer ->
       Ptr X -> Ptr Y -> N ->
       Ptr Surface ->
       X -> Y ->
       ColorF IO

{-# INLINE texPolygon #-}
texPolygon
  :: MonadIO m =>
     Ptr Renderer ->
     Ptr X -> Ptr Y -> N ->
     Ptr SDL.Raw.Surface ->
     X -> Y ->
     ColorF m
texPolygon rndr xs ys n s dx dy r g b =
  liftIO . texPolygon' rndr xs ys n s dx dy r g b

foreign import ccall "SDL2_gfxPrimitives.h bezierRGBA"
  bezier' :: Ptr Renderer -> Ptr X -> Ptr Y -> N -> N -> ColorF IO

{-# INLINE bezier #-}
{-| The 4th argument is the number of interpolation steps, minimum being 2. -}
bezier :: MonadIO m => Ptr Renderer -> Ptr X -> Ptr Y -> N -> N -> ColorF m
bezier rndr xs ys n s r g b = liftIO . bezier' rndr xs ys n s r g b
