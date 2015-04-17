{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad        (when)
import Data.Vector.Storable (fromList)
import Foreign.C.Types      (CInt)
import Linear               (V2(..), V4(..))

import qualified SDL
import qualified SDL.Framerate
import qualified SDL.Primitive

red :: SDL.Primitive.Color
red = V4 255 50 50 255

green :: SDL.Primitive.Color
green = V4 50 255 50 255

blue :: SDL.Primitive.Color
blue = V4 50 50 255 255

black :: SDL.Primitive.Color
black = V4 0 0 0 255

white :: SDL.Primitive.Color
white = V4 255 255 255 255

main :: IO ()
main = do

  SDL.initialize [SDL.InitVideo]
  w <- SDL.createWindow "sdl2-gfx-example" SDL.defaultWindow
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  SDL.showWindow w

  let fps   = 60  -- How fast do we aim to render?
  let limit = 120 -- How many frames will we render?

  SDL.Framerate.with fps $ loopFor limit r

  SDL.destroyWindow w
  SDL.quit

loopFor :: CInt -> SDL.Renderer -> SDL.Framerate.Manager -> IO ()
loopFor limit r fpsm = loop'
  where
    loop' :: IO ()
    loop' = do

      -- How many frames have we drawn until now?
      frames <- fromIntegral `fmap` SDL.Framerate.count fpsm

      -- Clear the screen!
      SDL.setRenderDrawColor r black
      SDL.renderClear r

      -- Run each of the functions from SDL.Primitives.
      -- For added chaos, move everything by framecount.
      SDL.Primitive.pixel r (V2 (100+frames) 100) green
      SDL.Primitive.line r (V2 10 10) (V2 (25+frames) (25+frames)) red
      SDL.Primitive.thickLine r (V2 10 15) (V2 (10+frames) 120) 3 white
      SDL.Primitive.smoothLine r (V2 100 frames) (V2 300 (20-frames)) white
      SDL.Primitive.horizontalLine r (V2 40 (350+frames)) (2*frames) blue
      SDL.Primitive.verticalLine r (V2 40 (350+frames)) (5*frames) green
      SDL.Primitive.rectangle r (V2 (75+frames) (90+frames)) (V2 (100+frames) (100+frames)) white
      SDL.Primitive.roundRectangle r (V2 110 300) (V2 (170+frames) (400+frames)) 10 white
      SDL.Primitive.fillRectangle r (V2 (175+frames) (190+frames)) (V2 (200+frames) (200+frames)) red
      SDL.Primitive.fillRoundRectangle r (V2 120 310) (V2 (160+frames) (390+frames)) 5 blue
      SDL.Primitive.arc r (V2 320 240) 100 0 (frames*360 `div` limit) red
      SDL.Primitive.circle r (V2 320 240) 80 white
      SDL.Primitive.smoothCircle r (V2 320 240) 70 white
      SDL.Primitive.fillCircle r (V2 320 240) 50 white
      SDL.Primitive.ellipse r (V2 500 200) 70 (10+frames) green
      SDL.Primitive.smoothEllipse r (V2 500 200) 60 (5+frames) white
      SDL.Primitive.fillEllipse r (V2 500 200) 40 frames blue
      SDL.Primitive.pie r (V2 640 500) 80 0 (frames*360 `div` limit) red
      SDL.Primitive.fillPie r (V2 640 400) 60 0 (frames*360 `div` limit) blue
      SDL.Primitive.triangle r (V2 700 10) (V2 750 10) (V2 750 60) red
      SDL.Primitive.smoothTriangle r (V2 700 5) (V2 740 5) (V2 740 70) green
      SDL.Primitive.fillTriangle r (V2 650 40) (V2 690 50) (V2 700 90) blue
      SDL.Primitive.polygon r (fromList [100, 220, 430, 317, 50]) (fromList [30, 70, 200, 300, 500]) green
      SDL.Primitive.smoothPolygon r (fromList $ map (+40) [100, 220, 430, 317, 50]) (fromList $ map (+40) [30, 70, 200, 300, 500]) blue
      SDL.Primitive.fillPolygon r (fromList $ map (+300) [100, 220, 430, 317, 50]) (fromList $ map (+400) [30, 70, 200, 300, 500]) $ V4 200 20 20 128
      SDL.Primitive.bezier r (fromList [70, 43, 23, 388, 239, 584, 444]) (fromList [546, 323, 110, 5, 483, 673, 332]) 5 $ V4 255 0 255 255

      SDL.renderPresent r

      SDL.Framerate.delay_ fpsm -- Delay to keep framerate constant.

      when (frames < limit) loop'
