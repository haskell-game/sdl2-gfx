{-|

Module      : SDL.Raw.Primitive
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
Stability   : experimental

Raw bindings to the @SDL2_gfx@ library, specifically the primitives drawing
functionality from @SDL2_gfxPrimitives.h@.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Primitive
  ( X
  , Y
  , Rad
  , R
  , N
  , G
  , B
  , A
  , pixel
  , L
  , hline
  , vline
  , line
  , aaLine
  , W
  , thickLine
  , bezier
  , rectangle
  , roundedRectangle
  , box
  , roundedBox
  , circle
  , aaCircle
  , filledCircle
  , Deg
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

liftF "pixel" "pixelRGBA"
  [t|Renderer -> X -> Y -> R -> G -> B -> A -> IO CInt|]

-- | A length.
type L = Int16

liftF "hline" "hlineRGBA"
  [t|Renderer -> X -> Y -> L -> R -> G -> B -> A -> IO CInt|]

liftF "vline" "vlineRGBA"
  [t|Renderer -> X -> Y -> L -> R -> G -> B -> A -> IO CInt|]

liftF "rectangle" "rectangleRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

-- | A radius.
type Rad = Int16

liftF "roundedRectangle" "roundedRectangleRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "box" "boxRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "roundedBox" "roundedBoxRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "line" "lineRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "aaLine" "aalineRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

-- | A width.
type W = Int16

liftF "thickLine" "thickLineRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> W -> R -> G -> B -> A -> IO CInt|]

liftF "circle" "circleRGBA"
  [t|Renderer -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

-- | Degrees.
type Deg = Int16

liftF "arc" "arcRGBA"
  [t|Renderer -> X -> Y -> Rad -> Deg -> Deg -> R -> G -> B -> A -> IO CInt|]

liftF "aaCircle" "aacircleRGBA"
  [t|Renderer -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "filledCircle" "filledCircleRGBA"
  [t|Renderer -> X -> Y -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "ellipse" "ellipseRGBA"
  [t|Renderer -> X -> Y -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "aaEllipse" "aaellipseRGBA"
  [t|Renderer -> X -> Y -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "filledEllipse" "filledEllipseRGBA"
  [t|Renderer -> X -> Y -> Rad -> Rad -> R -> G -> B -> A -> IO CInt|]

liftF "pie" "pieRGBA"
  [t|Renderer -> X -> Y -> Rad -> Deg -> Deg -> R -> G -> B -> A -> IO CInt|]

liftF "filledPie" "filledPieRGBA"
  [t|Renderer -> X -> Y -> Rad -> Deg -> Deg -> R -> G -> B -> A -> IO CInt|]

liftF "trigon" "trigonRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "aaTrigon" "aatrigonRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

liftF "filledTrigon" "filledTrigonRGBA"
  [t|Renderer -> X -> Y -> X -> Y -> X -> Y -> R -> G -> B -> A -> IO CInt|]

-- | How many of a certain thing, e.g. how many points, or steps.
type N = CInt

liftF "polygon" "polygonRGBA"
  [t|Renderer -> Ptr X -> Ptr Y -> N -> R -> G -> B -> A -> IO CInt|]

liftF "aaPolygon" "aapolygonRGBA"
  [t|Renderer -> Ptr X -> Ptr Y -> N -> R -> G -> B -> A -> IO CInt|]

liftF "filledPolygon" "filledPolygonRGBA"
  [t|Renderer -> Ptr X -> Ptr Y -> N -> R -> G -> B -> A -> IO CInt|]

liftF "texturedPolygon" "texturedPolygon"
  [t|Renderer -> Ptr X -> Ptr Y -> N -> Ptr Surface -> X -> Y -> IO CInt|]

liftF "bezier" "bezierRGBA"
  [t|Renderer -> Ptr X -> Ptr Y -> N -> N -> R -> G -> B -> A -> IO CInt|]
