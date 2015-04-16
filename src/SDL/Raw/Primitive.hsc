{-|

Module      : SDL.Raw.Primitive
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Raw bindings to the @SDL2_gfx@ library, specifically the primitives drawing
functionality from @SDL2_gfxPrimitives.h@.

-}

{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Primitive
  ( X
  , Y
  , R
  , G
  , B
  , A
  , pixel
  , W
  , hline
  , H
  , vline
  , line
  , aaLine
  , thickLine
  , Rad
  , N
  , bezier
  , rect
  , roundRect
  , box
  , roundBox
  , circle
  , aaCircle
  , filledCircle
  , arc
  , ellipse
  , aaEllipse
  , filledEllipse
  , pie
  , filledPie
  , trigon
  , aaTrigon
  , filledTrigon
  , polygon
  , aaPolygon
  , filledPolygon
  , texturedPolygon
  ) where

import Data.Int                (Int16)
import Data.Word               (Word8)
import Foreign.C.Types         (CInt(..))
import Foreign.Ptr             (Ptr)
import SDL.Raw                 (Renderer, Surface)
import SDL.Raw.Helper          (liftF)

-- | The position of something on the x-axis.
type X = Int16

-- | Same as 'X', but for the y-axis.
type Y = Int16

-- | The red color component.
type R = Word8

-- | The green color component.
type G = Word8

-- | The blue color component.
type B = Word8

-- | The alpha color component.
type A = Word8

liftF "pixel" "SDL2_gfxPrimitives.h pixelRGBA"
  [t|Ptr Renderer -> X -> Y -> R -> G -> B -> A -> IO CInt|]

-- | A width.
type W = Int16

liftF "hline" "SDL2_gfxPrimitives.h hlineRGBA"
  [t|Ptr Renderer -> X -> Y -> W -> R -> G -> B -> A -> IO CInt|]

-- | A height.
type H = Int16

liftF "vline" "SDL2_gfxPrimitives.h vlineRGBA"
  [t|Ptr Renderer -> X -> Y -> H -> R -> G -> B -> A -> IO CInt|]

liftF "rect" "SDL2_gfxPrimitives.h rectangleRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

-- | A radius.
type Rad = Int16

liftF "roundedRectangle" "SDL2_gfxPrimitives.h roundedRectangleRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "box" "SDL2_gfxPrimitives.h boxRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "roundBox" "SDL2_gfxPrimitives.h roundedBoxRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "line" "SDL2_gfxPrimitives.h lineRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "aaLine" "SDL2_gfxPrimitives.h aalineRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "thickLine" "SDL2_gfxPrimitives.h thickLineRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> W -> R -> G -> B -> A -> IO CInt|]

liftF "circle" "SDL2_gfxPrimitives.h circleRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "arc" "SDL2_gfxPrimitives.h arcRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "aaCircle" "SDL2_gfxPrimitives.h aacircleRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "filledCircle" "SDL2_gfxPrimitives.h filledCircleRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "ellipse" "SDL2_gfxPrimitives.h ellipseRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "aaEllipse" "SDL2_gfxPrimitives.h aaellipseRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "filledEllipse" "SDL2_gfxPrimitives.h filledEllipseRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "pie" "SDL2_gfxPrimitives.h pieRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "filledPie" "SDL2_gfxPrimitives.h filledPieRGBA"
  [t|Ptr Renderer -> X -> Y -> Rad -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "trigon" "SDL2_gfxPrimitives.h trigonRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "aaTrigon" "SDL2_gfxPrimitives.h aatrigonRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "filledTrigon" "SDL2_gfxPrimitives.h filledTrigonRGBA"
  [t|Ptr Renderer -> X -> Y -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

-- | How many of a certain thing, e.g. how many points.
type N = CInt

liftF "polygon" "SDL2_gfxPrimitives.h polygonRGBA"
  [t|Ptr Renderer -> Ptr X -> Ptr Y -> N -> R -> G -> B -> A -> IO CInt|]

liftF "aaPolygon" "SDL2_gfxPrimitives.h aapolygonRGBA"
  [t|Ptr Renderer -> Ptr X -> Ptr Y -> N -> R -> G -> B -> A -> IO CInt|]

liftF "filledPolygon" "SDL2_gfxPrimitives.h filledPolygonRGBA"
  [t|Ptr Renderer -> Ptr X -> Ptr Y -> N -> R -> G -> B -> A -> IO CInt|]

liftF "texturedPolygon" "SDL2_gfxPrimitives.h texturedPolygonRGBA"
  [t|Ptr Renderer -> Ptr X -> Ptr Y -> N -> Ptr Surface -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "bezier" "SDL2_gfxPrimitives.h bezierRGBA"
  [t|Ptr Renderer -> Ptr X -> Ptr Y -> N -> N -> R -> G -> B -> A -> IO CInt|]
