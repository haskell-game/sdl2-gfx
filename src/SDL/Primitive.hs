{-|

Module      : SDL.Primitive
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Bindings to @SDL2_gfx@'s primitives drawing functionality. These functions
should allow you to render various simple shapes such as lines, ellipses or
polygons.

All of the monadic functions within this module are capable of throwing an
'SDL.Exception.SDLException' if they encounter an error.

-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module SDL.Primitive
  (
  -- * Pixels
    Pos
  , Color
  , pixel

  -- * Lines
  , line
  , Length
  , horizontalLine
  , verticalLine
  , smoothLine
  , Width
  , thickLine

  -- * Triangles
  , triangle
  , smoothTriangle
  , fillTriangle

  -- * Rectangles
  , rectangle
  , Radius
  , roundRectangle
  , fillRectangle
  , fillRoundRectangle

  -- * Curves
  , Start
  , End
  , arc
  , circle
  , smoothCircle
  , fillCircle
  , ellipse
  , smoothEllipse
  , fillEllipse
  , pie
  , fillPie
  , Steps
  , bezier

  -- * Polygons
  , polygon
  , smoothPolygon
  , fillPolygon
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int               (Int16)
import Data.Vector.Storable   (Vector, unsafeWith, length)
import Data.Word              (Word8)
import Foreign.C.Types        (CInt)
import Linear                 (V4(..), V2(..))
import Prelude         hiding (length)
import SDL.Exception          (throwIfNeg_)
import SDL.Internal.Types     (Renderer(..))

import qualified SDL.Raw.Primitive

-- | A position as a two-dimensional vector.
type Pos = V2 CInt

-- | A color as an RGBA byte-vector.
type Color = V4 Word8

-- The SDL2_gfx API expects Int16, while SDL2 uses CInt. We could force Int16,
-- but that would cause issues for the end user always having to convert
-- between vector types in order to use both SDL2 and SDL2_gfx. I'm therefore
-- currently opting to accept CInt and convert to Int16, overflows be damned.
cint :: CInt -> Int16
cint = fromIntegral

-- | Renders a single pixel at a given position.
pixel :: MonadIO m => Renderer -> Pos -> Color -> m ()
pixel (Renderer p) (V2 x y) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.pixel" "pixelRGBA" $
    SDL.Raw.Primitive.pixel
      p (cint x) (cint y) r g b a

-- | Renders a line between two points.
line :: MonadIO m => Renderer -> Pos -> Pos -> Color -> m ()
line (Renderer p) (V2 x y) (V2 u v) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.line" "lineRGBA" $
    SDL.Raw.Primitive.line
      p (cint x) (cint y) (cint u) (cint v) r g b a

-- | A width in pixels.
type Width = CInt

-- | Same as 'line', but the rendered line is of a given 'Width'.
thickLine :: MonadIO m => Renderer -> Pos -> Pos -> Width -> Color -> m ()
thickLine (Renderer p) (V2 x y) (V2 u v) w (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.thickLine" "thickLineRGBA" $
    SDL.Raw.Primitive.thickLine
      p (cint x) (cint y) (cint u) (cint v) (cint w) r g b a

-- | Renders an anti-aliased line between two points.
smoothLine :: MonadIO m => Renderer -> Pos -> Pos -> Color -> m ()
smoothLine (Renderer p) (V2 x y) (V2 u v) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.smoothLine" "aalineRGBA" $
    SDL.Raw.Primitive.aaLine
      p (cint x) (cint y) (cint u) (cint v) r g b a

-- | A length in pixels.
type Length = CInt

-- | Renders a horizontal line of a certain 'Length', its left and starting
-- point corresponding to a given 'Pos'.
horizontalLine :: MonadIO m => Renderer -> Pos -> Length -> Color -> m ()
horizontalLine (Renderer p) (V2 x y) w (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.horizontalLine" "hlineRGBA" $
    SDL.Raw.Primitive.hline
      p (cint x) (cint $ x + w) (cint y) r g b a

-- | Renders a vertical line of a certain 'Length', its top and starting point
-- corresponding to a given 'Pos'.
verticalLine :: MonadIO m => Renderer -> Pos -> Length -> Color -> m ()
verticalLine (Renderer p) (V2 x y) h (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.verticalLine" "vlineRGBA" $
    SDL.Raw.Primitive.vline
      p (cint x) (cint y) (cint $ y + h) r g b a

-- | Renders a transparent rectangle spanning two points, bordered by a line of
-- a given 'Color'.
rectangle :: MonadIO m => Renderer -> Pos -> Pos -> Color -> m ()
rectangle (Renderer p) (V2 x y) (V2 u v) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.rectangle" "rectangleRGBA" $
    SDL.Raw.Primitive.rectangle
      p (cint x) (cint y) (cint u) (cint v) r g b a

-- | A radius in pixels.
type Radius = CInt

-- | Same as 'rectangle', but the rectangle's corners are rounded. Control the
-- roundness using the 'Radius' argument, defining the radius of the corner
-- arcs.
roundRectangle :: MonadIO m => Renderer -> Pos -> Pos -> Radius -> Color -> m ()
roundRectangle (Renderer p) (V2 x y) (V2 u v) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.roundRectangle" "roundedRectangleRGBA" $
    SDL.Raw.Primitive.roundedRectangle
      p (cint x) (cint y) (cint u) (cint v) (cint rad) r g b a

-- | Same as 'rectangle', but the rectangle is filled by the given 'Color'.
fillRectangle :: MonadIO m => Renderer -> Pos -> Pos -> Color -> m ()
fillRectangle (Renderer p) (V2 x y) (V2 u v) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.fillRectangle" "boxRGBA" $
    SDL.Raw.Primitive.box
      p (cint x) (cint y) (cint u) (cint v) r g b a

-- | Same as 'roundRectangle', but the rectangle is filled by the given 'Color'.
fillRoundRectangle :: MonadIO m => Renderer -> Pos -> Pos -> Radius -> Color -> m ()
fillRoundRectangle (Renderer p) (V2 x y) (V2 u v) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.fillRoundRectangle" "roundedBoxRGBA" $
    SDL.Raw.Primitive.roundedBox
      p (cint x) (cint y) (cint u) (cint v) (cint rad) r g b a

-- | A starting position in degrees.
type Start = CInt

-- | An ending position in degrees.
type End = CInt

-- | Render an arc, its 'Pos' being its center. The 'Start' and 'End' arguments
-- define the starting and ending points of the arc in degrees, zero degrees
-- being south and increasing counterclockwise.
arc :: MonadIO m => Renderer -> Pos -> Radius -> Start -> End -> Color -> m ()
arc (Renderer p) (V2 x y) rad start end (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.arc" "arcRGBA" $
    SDL.Raw.Primitive.arc
      p (cint x) (cint y) (cint rad) (cint start) (cint end) r g b a

-- | Renders a transparent circle, bordered by a line of a given 'Color'.
circle :: MonadIO m => Renderer -> Pos -> Radius -> Color -> m ()
circle (Renderer p) (V2 x y) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.circle" "circleRGBA" $
    SDL.Raw.Primitive.circle
      p (cint x) (cint y) (cint rad) r g b a

-- | Same as 'circle', but fills it with the given 'Color' instead.
fillCircle :: MonadIO m => Renderer -> Pos -> Radius -> Color -> m ()
fillCircle (Renderer p) (V2 x y) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.filledCircle" "filledCircleRGBA" $
    SDL.Raw.Primitive.filledCircle
      p (cint x) (cint y) (cint rad) r g b a

-- | Same as 'circle', but the border is anti-aliased.
smoothCircle :: MonadIO m => Renderer -> Pos -> Radius -> Color -> m ()
smoothCircle (Renderer p) (V2 x y) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.aaCircle" "aacircleRGBA" $
    SDL.Raw.Primitive.aaCircle
      p (cint x) (cint y) (cint rad) r g b a

-- | Renders a transparent ellipse, bordered by a line of a given 'Color'. The
-- 'Radius' arguments are the horizontal and vertical radius of the ellipse
-- respectively, in pixels.
ellipse :: MonadIO m => Renderer -> Pos -> Radius -> Radius -> Color -> m ()
ellipse (Renderer p) (V2 x y) rx ry (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.ellipse" "ellipseRGBA" $
    SDL.Raw.Primitive.ellipse
      p (cint x) (cint y) (cint rx) (cint ry) r g b a

-- | Same as 'ellipse', but makes the border anti-aliased.
smoothEllipse :: MonadIO m => Renderer -> Pos -> Radius -> Radius -> Color -> m ()
smoothEllipse (Renderer p) (V2 x y) rx ry (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.smoothEllipse" "aaellipseRGBA" $
    SDL.Raw.Primitive.aaEllipse
      p (cint x) (cint y) (cint rx) (cint ry) r g b a

-- | Same as 'ellipse', but fills it with the given 'Color' instead.
fillEllipse :: MonadIO m => Renderer -> Pos -> Radius -> Radius -> Color -> m ()
fillEllipse (Renderer p) (V2 x y) rx ry (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.fillEllipse" "filledEllipseRGBA" $
    SDL.Raw.Primitive.filledEllipse
      p (cint x) (cint y) (cint rx) (cint ry) r g b a

-- | Render a pie outline, its 'Pos' being its center. The 'Start' and 'End'
-- arguments define the starting and ending points of the pie in degrees, zero
-- degrees being east and increasing counterclockwise.
pie :: MonadIO m => Renderer -> Pos -> Radius -> Start -> End -> Color -> m ()
pie (Renderer p) (V2 x y) rad start end (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.pie" "pieRGBA" $
    SDL.Raw.Primitive.pie
      p (cint x) (cint y) (cint rad) (cint start) (cint end) r g b a

-- | Same as 'pie', but fills it with the given 'Color' instead.
fillPie :: MonadIO m => Renderer -> Pos -> Radius -> Start -> End -> Color -> m ()
fillPie (Renderer p) (V2 x y) rad start end (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.fillPie" "filledPieRGBA" $
    SDL.Raw.Primitive.filledPie
      p (cint x) (cint y) (cint rad) (cint start) (cint end) r g b a

-- | How many interpolation steps when rendering a bezier curve? The higher
-- this is, the smoother the curve and more resource-intensive the render.
type Steps = CInt

-- | Renders a bezier curve of a given 'Color'. The input vectors contain the
-- bezier curve's point locations on the x and y-axis, respectively. The input
-- vectors need to be the same length, and those lengths must be at least 3,
-- otherwise 'bezier' might raise an 'SDL.Exception.SDLException'. The same
-- applies for the number of interpolation 'Steps': it must be at least 2.
bezier :: MonadIO m => Renderer -> Vector Int16 -> Vector Int16 -> Steps -> Color -> m ()
bezier (Renderer p) xs ys steps (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.bezier" "bezierRGBA" $
    liftIO .
      unsafeWith xs $ \xs' ->
        unsafeWith ys $ \ys' ->
          SDL.Raw.Primitive.bezier
            p xs' ys' (fromIntegral $ length xs) steps r g b a

-- | Render a transparent triangle, its edges being of a given 'Color'.
triangle :: MonadIO m => Renderer -> Pos -> Pos -> Pos -> Color -> m ()
triangle (Renderer p) (V2 x y) (V2 u v) (V2 t z) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.triangle" "trigonRGBA" $
    SDL.Raw.Primitive.trigon
      p (cint x) (cint y) (cint u) (cint v) (cint t) (cint z) r g b a

-- | Same as 'triangle', but the edges are anti-aliased.
smoothTriangle :: MonadIO m => Renderer -> Pos -> Pos -> Pos -> Color -> m ()
smoothTriangle (Renderer p) (V2 x y) (V2 u v) (V2 t z) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.smoothTriangle" "aatrigonRGBA" $
    SDL.Raw.Primitive.aaTrigon
      p (cint x) (cint y) (cint u) (cint v) (cint t) (cint z) r g b a

-- | Same as 'triangle', but the triangle is filled with the given 'Color'
-- instead.
fillTriangle :: MonadIO m => Renderer -> Pos -> Pos -> Pos -> Color -> m ()
fillTriangle (Renderer p) (V2 x y) (V2 u v) (V2 t z) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.fillTriangle" "filledTrigonRGBA" $
    SDL.Raw.Primitive.filledTrigon
      p (cint x) (cint y) (cint u) (cint v) (cint t) (cint z) r g b a

-- | Render a transparent polygon, its edges of a given 'Color'. The input
-- vectors contain the points' locations on the x and y-axis, respectively. The
-- input vectors need to be the of the same length, and the lengths must be at
-- least 3, otherwise 'polygon' might raise an 'SDL.Exception.SDLException'.
polygon :: MonadIO m => Renderer -> Vector Int16 -> Vector Int16 -> Color -> m ()
polygon (Renderer p) xs ys (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.polygon" "polygonRGBA" $
    liftIO .
      unsafeWith xs $ \xs' ->
        unsafeWith ys $ \ys' ->
          SDL.Raw.Primitive.polygon
            p xs' ys' (fromIntegral $ length xs) r g b a

-- | Same as 'polygon', but the edges are drawn anti-aliased.
smoothPolygon :: MonadIO m => Renderer -> Vector Int16 -> Vector Int16 -> Color -> m ()
smoothPolygon (Renderer p) xs ys (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.smoothPolygon" "aapolygonRGBA" $
    liftIO .
      unsafeWith xs $ \xs' ->
        unsafeWith ys $ \ys' ->
          SDL.Raw.Primitive.aaPolygon
            p xs' ys' (fromIntegral $ length xs) r g b a

-- | Same as 'polygon', but the polygon is filled with the given 'Color'.
fillPolygon :: MonadIO m => Renderer -> Vector Int16 -> Vector Int16 -> Color -> m ()
fillPolygon (Renderer p) xs ys (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.fillPolygon" "filledPolygonRGBA" $
    liftIO .
      unsafeWith xs $ \xs' ->
        unsafeWith ys $ \ys' ->
          SDL.Raw.Primitive.filledPolygon
            p xs' ys' (fromIntegral $ length xs) r g b a
