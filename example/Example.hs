{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)

import qualified SDL
import qualified SDL.Framerate

main :: IO ()
main = do

  SDL.initialize [SDL.InitVideo]
  w <- SDL.createWindow "sdl2-gfx-example" SDL.defaultWindow
  SDL.showWindow w

  fpsm <- SDL.Framerate.manager
  SDL.Framerate.set fpsm 25 -- Run at 25 frames per second.

  -- Draw bunch of stuff, repeat for 100 frames.
  -- If everything OK, should run 100/25=4 seconds.
  let loop = do
        frames <- SDL.Framerate.count fpsm
        _ <- SDL.Framerate.delay fpsm
        when (frames < 100) loop

  loop

  SDL.destroyWindow w
  SDL.quit
